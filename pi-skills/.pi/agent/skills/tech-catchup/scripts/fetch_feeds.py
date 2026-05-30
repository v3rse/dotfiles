#!/usr/bin/env python3
"""Fetch RSS/Atom feeds in parallel; emit JSONL.

Improvements over v1:
- Conditional GET via ETag / If-Modified-Since (cache in .feed-cache.json)
- One retry with backoff on transient failures
- Extracts the *real* article URL for HN/Reddit items (link vs discussion)
- Canonicalizes all URLs (strips utm_*, trailing slashes, etc.)
- Compact one-sentence summaries by default

Usage:
  fetch_feeds.py --feeds <file>           # one URL per line, # comments OK
  fetch_feeds.py URL [URL...]
  echo URL | fetch_feeds.py -

Options:
  --since YYYY-MM-DD     drop entries older than this (default: 7 days ago)
  --max-per-feed N       cap entries per feed (default: 15)
  --timeout S            per-fetch timeout seconds (default: 10)
  --concurrency N        parallel fetches (default: 16)
  --no-cache             disable conditional GET (force full fetch)
  --full-summaries       keep full summaries instead of first sentence

Output: JSONL on stdout, one item per line:
  {"feed":"...", "feed_title":"...", "title":"...",
   "link":"<canonical primary article URL>",
   "discussion":"<HN/Reddit comments URL or empty>",
   "published":"2026-05-07T10:00:00Z", "summary":"..."}

Stderr: per-feed log: "OK <n> <url>", "CACHED <url>", or "ERR <reason> <url>".
"""
from __future__ import annotations
import argparse, json, os, sys, re, html, time
from concurrent.futures import ThreadPoolExecutor, as_completed
from datetime import datetime, timedelta, timezone
from pathlib import Path
from urllib.request import Request, urlopen
from urllib.error import URLError, HTTPError
from xml.etree import ElementTree as ET

from urllib.parse import urlparse

sys.path.insert(0, str(Path(__file__).resolve().parent))
from _lib import canonicalize_url, parse_date, first_sentence, title_fingerprint, load_tastemakers  # noqa: E402

UA = "Mozilla/5.0 tech-catchup-skill/2.0"
NS = {
    "atom": "http://www.w3.org/2005/Atom",
    "dc": "http://purl.org/dc/elements/1.1/",
    "content": "http://purl.org/rss/1.0/modules/content/",
}
NEWS = Path(os.environ.get("TECH_CATCHUP_DIR", str(Path.home() / "org" / "news")))
CACHE_PATH = NEWS / ".feed-cache.json"


# ---- HTTP cache -----------------------------------------------------------

def _load_cache() -> dict:
    if not CACHE_PATH.exists():
        return {}
    try:
        return json.loads(CACHE_PATH.read_text())
    except json.JSONDecodeError:
        return {}


def _save_cache(d: dict) -> None:
    NEWS.mkdir(parents=True, exist_ok=True)
    CACHE_PATH.write_text(json.dumps(d, indent=2, sort_keys=True))


# ---- HTTP fetch with retry + conditional GET -----------------------------

def fetch(url: str, timeout: int, cache_entry: dict | None) -> tuple[bytes | None, dict]:
    """Return (body_bytes_or_None, new_cache_entry).
    body_bytes is None if HTTP 304 (use stored body — but we don't store it).
    Caller handles 304 = "no new items".
    """
    headers = {
        "User-Agent": UA,
        "Accept": ("application/rss+xml, application/atom+xml, "
                   "application/xml, text/xml, */*"),
    }
    if cache_entry:
        if cache_entry.get("etag"):
            headers["If-None-Match"] = cache_entry["etag"]
        if cache_entry.get("last_modified"):
            headers["If-Modified-Since"] = cache_entry["last_modified"]

    last_err: Exception | None = None
    for attempt in (0, 1):
        try:
            req = Request(url, headers=headers)
            with urlopen(req, timeout=timeout) as r:
                body = r.read()
                new_cache = {
                    "etag": r.headers.get("ETag", ""),
                    "last_modified": r.headers.get("Last-Modified", ""),
                    "fetched": datetime.now(timezone.utc).isoformat(),
                }
                return body, new_cache
        except HTTPError as e:
            if e.code == 304:
                return None, (cache_entry or {})
            # 4xx/5xx: don't retry 4xx; retry 5xx once
            if 400 <= e.code < 500 or attempt == 1:
                raise
            last_err = e
        except (URLError, TimeoutError) as e:
            last_err = e
            if attempt == 1:
                raise
        time.sleep(0.8)  # tiny backoff
    if last_err:
        raise last_err
    raise RuntimeError("unreachable")



# ---- tastemaker endorsement extraction ----------------------------------

# Pattern: Simon Willison's "Quoting" blogmarks have <blockquote cite="URL">
_BLOCKQUOTE_CITE_RE = re.compile(r'<blockquote\s+cite="(https?://[^"]+)"', re.IGNORECASE)
# Generic: first external <a href="URL"> that isn't the feed's own domain
_LINK_RE = re.compile(r'<a\s+href="(https?://[^"]+)"', re.IGNORECASE)


def _extract_endorsements(feed_url: str, title: str, raw_html: str) -> list[str]:
    """Extract external URLs that a tastemaker post endorses.

    For Simon Willison's "Quoting" blogmarks, the cited URL is in the
    <blockquote cite="..."> attribute.  For other posts we fall back to
    the first external <a> href that isn't the feed's own domain.
    """
    if not raw_html:
        return []
    feed_host = urlparse(feed_url).netloc.lower()
    # Strip www. prefix for comparison
    feed_host = feed_host.removeprefix("www.")

    out: list[str] = []
    # 1. Quoting blogmarks: cite attribute is authoritative
    for m in _BLOCKQUOTE_CITE_RE.finditer(raw_html):
        url = m.group(1)
        canon = canonicalize_url(url)
        if canon and canon not in out:
            out.append(canon)

    # 2. If no cite found, look for first external link
    if not out:
        for m in _LINK_RE.finditer(raw_html):
            url = m.group(1)
            canon = canonicalize_url(url)
            host = urlparse(canon).netloc.lower().removeprefix("www.")
            if host and host != feed_host and canon not in out:
                out.append(canon)
                break  # only first external link

    return out


def _url_to_tastemaker_handle(feed_url: str, tastemakers: dict[str, list[str]]) -> str | None:
    """Map a feed URL to its tastemaker handle using tastemakers.json hosts."""
    feed_lower = feed_url.lower()
    for handle, hosts in tastemakers.items():
        for h in hosts:
            if h.lower() in feed_lower:
                return handle
    return None


ENDORSEMENTS_PATH = Path.home() / ".cache" / "tech-catchup" / "endorsements.json"


def _load_endorsements() -> dict[str, list[str]]:
    """Load {canonical_url: [tastemaker_handle, ...]} map."""
    if not ENDORSEMENTS_PATH.exists():
        return {}
    try:
        return json.loads(ENDORSEMENTS_PATH.read_text())
    except (json.JSONDecodeError, OSError):
        return {}


def _save_endorsements(d: dict[str, list[str]]) -> None:
    """Persist endorsements map."""
    ENDORSEMENTS_PATH.parent.mkdir(parents=True, exist_ok=True)
    ENDORSEMENTS_PATH.write_text(json.dumps(d, indent=2, sort_keys=True))


def _strip_html(text: str | None) -> str:
    if not text:
        return ""
    text = re.sub(r"<[^>]+>", " ", text)
    text = html.unescape(text)
    return re.sub(r"\s+", " ", text).strip()


_HN_ARTICLE_RE = re.compile(r"Article URL:\s*(https?://\S+)")
_REDDIT_LINK_RE = re.compile(r'<a href="(https?://[^"]+)">\s*\[link\]\s*</a>')


def _extract_primary_and_discussion(feed_url: str, raw_link: str, raw_desc_html: str) -> tuple[str, str]:
    """For HN/Reddit feeds, extract the actual article URL (primary) and keep
    the comments page as discussion. For other feeds, primary=link, discussion="".
    """
    feed_host = (feed_url or "").lower()
    if "hnrss.org" in feed_host or "ycombinator.com" in feed_host:
        m = _HN_ARTICLE_RE.search(raw_desc_html or "")
        if m:
            article = m.group(1).rstrip("./")
            return article, raw_link
        return raw_link, ""
    if "reddit.com" in feed_host:
        m = _REDDIT_LINK_RE.search(raw_desc_html or "")
        if m:
            return m.group(1), raw_link
        return raw_link, ""
    return raw_link, ""


def parse_feed(url: str, body: bytes) -> tuple[str, list[dict]]:
    """Parse RSS/Atom feed. Each item dict includes 'summary_raw' (plain text)
    and 'summary_html' (raw HTML, if available from content:encoded or atom:content).
    """
    try:
        root = ET.fromstring(body.lstrip())  # strip leading whitespace before <?xml
    except ET.ParseError as e:
        raise ValueError(f"xml parse: {e}")

    items: list[dict] = []
    feed_title = ""

    # RSS 2.0 / RDF
    RSS1 = "http://purl.org/rss/1.0/"
    RDF_NS = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    if root.tag.lower().endswith("rss") or root.tag.endswith("}RDF"):
        _ch = root.find("channel")
        if _ch is None:
            _ch = root.find("{http://purl.org/rss/1.0/}channel")
        channel = _ch if _ch is not None else root
        ft = channel.find("title")
        if ft is None:
            ft = channel.find(f"{{{RSS1}}}title")
        if ft is not None and ft.text:
            feed_title = ft.text.strip()
        for item in root.iter():
            tag = item.tag.split("}")[-1]
            if tag != "item":
                continue
            # RSS 1.0 puts title/link in the rss1.0 namespace; try both
            title = (item.findtext("title") or item.findtext(f"{{{RSS1}}}title") or "").strip()
            raw_link = (
                item.findtext("link")
                or item.findtext(f"{{{RSS1}}}link")
                or item.get(f"{{{RDF_NS}}}about")  # rdf:about as fallback
                or ""
            ).strip()
            pub = (item.findtext("pubDate")
                   or item.findtext("{http://purl.org/dc/elements/1.1/}date")
                   or "")
            # Prefer content:encoded over description for richer HTML
            desc_html = (item.findtext("{http://purl.org/rss/1.0/modules/content/}encoded")
                         or item.findtext("description")
                         or "")
            primary, discussion = _extract_primary_and_discussion(url, raw_link, desc_html)
            items.append({
                "title": title,
                "link": canonicalize_url(primary),
                "discussion": canonicalize_url(discussion),
                "published": pub,
                "summary_raw": _strip_html(desc_html),
                "summary_html": desc_html,
            })

    # Atom
    elif root.tag.endswith("}feed"):
        ft = root.find("atom:title", NS)
        if ft is not None and ft.text:
            feed_title = ft.text.strip()
        for entry in root.findall("atom:entry", NS):
            title_el = entry.find("atom:title", NS)
            title = (title_el.text or "").strip() if title_el is not None else ""
            raw_link = ""
            for l in entry.findall("atom:link", NS):
                if l.get("rel") in (None, "alternate"):
                    raw_link = l.get("href", "")
                    break
            pub = (entry.findtext("atom:published", default="", namespaces=NS)
                   or entry.findtext("atom:updated", default="", namespaces=NS))
            # Prefer atom:content over atom:summary for richer HTML
            content_html = entry.findtext("atom:content", default="", namespaces=NS)
            summary_html = entry.findtext("atom:summary", default="", namespaces=NS)
            raw_html = content_html or summary_html
            primary, discussion = _extract_primary_and_discussion(url, raw_link, raw_html)
            items.append({
                "title": title,
                "link": canonicalize_url(primary),
                "discussion": canonicalize_url(discussion),
                "published": pub,
                "summary_raw": _strip_html(raw_html),
                "summary_html": raw_html,
            })

    return feed_title, items


# ---- per-feed worker ------------------------------------------------------

def process(url: str, since: datetime, max_items: int, timeout: int,
            cache: dict, use_cache: bool, full_summaries: bool,
            tastemaker_hosts: set[str] | None = None,
            ) -> tuple[str, list[dict], str | None, str, dict | None, dict[str, list[str]]]:
    """Returns (url, items, error_msg, status, new_cache_entry, endorsements).
    status ∈ {"ok","cached","err"}.
    endorsements = {canonical_url: [tastemaker_handle, ...]}.
    """
    cache_entry = cache.get(url) if use_cache else None
    try:
        body, new_cache = fetch(url, timeout, cache_entry)
    except (HTTPError, URLError, ValueError, TimeoutError) as e:
        return url, [], f"{type(e).__name__}: {e}", "err", None, {}

    if body is None:  # 304
        return url, [], None, "cached", new_cache or cache_entry, {}

    try:
        feed_title, items = parse_feed(url, body)
    except ValueError as e:
        return url, [], f"{type(e).__name__}: {e}", "err", None, {}

    out = []
    feed_endorsements: dict[str, list[str]] = {}
    for it in items:
        dt = parse_date(it["published"])
        if dt:
            # Normalise naive datetimes (some feeds omit timezone) to UTC
            if dt.tzinfo is None:
                dt = dt.replace(tzinfo=timezone.utc)
            if dt < since:
                continue
        it["published"] = dt.isoformat() if dt else ""
        it["feed"] = url
        it["feed_title"] = feed_title
        summary = it.pop("summary_raw")
        summary_html = it.pop("summary_html", "")
        it["summary"] = summary if full_summaries else first_sentence(summary)
        out.append(it)

        # Extract endorsements from tastemaker feeds
        if tastemaker_hosts is not None:
            handle = _url_to_tastemaker_handle(url, tastemaker_hosts)
            if handle:
                endorsed = _extract_endorsements(url, it.get("title", ""), summary_html)
                for eurl in endorsed:
                    if eurl not in feed_endorsements:
                        feed_endorsements[eurl] = []
                    if handle not in feed_endorsements[eurl]:
                        feed_endorsements[eurl].append(handle)

        if len(out) >= max_items:
            break
    return url, out, None, "ok", new_cache, feed_endorsements


# ---- main -----------------------------------------------------------------

def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("urls", nargs="*")
    ap.add_argument("--feeds")
    ap.add_argument("--since", default=(datetime.now(timezone.utc) - timedelta(days=7)).strftime("%Y-%m-%d"))
    ap.add_argument("--max-per-feed", type=int, default=15)
    ap.add_argument("--timeout", type=int, default=10)
    ap.add_argument("--concurrency", type=int, default=16)
    ap.add_argument("--no-cache", action="store_true")
    ap.add_argument("--full-summaries", action="store_true")
    args = ap.parse_args()

    urls: list[str] = []
    if args.feeds:
        with open(args.feeds) as f:
            for line in f:
                s = line.strip().lstrip("!").strip()
                if s and not s.startswith("#"):
                    urls.append(s)
    if args.urls == ["-"]:
        urls += [l.strip() for l in sys.stdin if l.strip() and not l.startswith("#")]
    else:
        urls += args.urls
    urls = list(dict.fromkeys(urls))
    if not urls:
        print("no feeds given", file=sys.stderr)
        return 2

    since = parse_date(args.since) or (datetime.now(timezone.utc) - timedelta(days=7))
    if since.tzinfo is None:
        since = since.replace(tzinfo=timezone.utc)

    cache = {} if args.no_cache else _load_cache()
    new_cache = dict(cache)

    # Load tastemakers for endorsement extraction
    tastemakers = load_tastemakers(NEWS / "feeds" / "tastemakers.json")

    all_endorsements: dict[str, list[str]] = {}

    with ThreadPoolExecutor(max_workers=args.concurrency) as ex:
        futs = {
            ex.submit(process, u, since, args.max_per_feed, args.timeout,
                      cache, not args.no_cache, args.full_summaries,
                      tastemakers): u
            for u in urls
        }
        for fut in as_completed(futs):
            url, items, err, status, ce, feed_endorsements = fut.result()
            if status == "err":
                print(f"ERR {err} {url}", file=sys.stderr)
                continue
            if status == "cached":
                print(f"CACHED {url}", file=sys.stderr)
                if ce:
                    new_cache[url] = ce
                continue
            print(f"OK {len(items)} {url}", file=sys.stderr)
            if ce:
                new_cache[url] = ce
            for it in items:
                print(json.dumps(it, ensure_ascii=False))

            # Merge feed endorsements into global map
            for eurl, handles in feed_endorsements.items():
                if eurl not in all_endorsements:
                    all_endorsements[eurl] = []
                for h in handles:
                    if h not in all_endorsements[eurl]:
                        all_endorsements[eurl].append(h)

    if not args.no_cache:
        _save_cache(new_cache)

    if all_endorsements:
        _save_endorsements(all_endorsements)
        print(f"endorsements: {len(all_endorsements)} unique URLs endorsed", file=sys.stderr)

    return 0


if __name__ == "__main__":
    sys.exit(main())
