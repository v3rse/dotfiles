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

sys.path.insert(0, str(Path(__file__).resolve().parent))
from _lib import canonicalize_url, parse_date, first_sentence  # noqa: E402

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


# ---- RSS/Atom parser ------------------------------------------------------

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
    try:
        root = ET.fromstring(body)
    except ET.ParseError as e:
        raise ValueError(f"xml parse: {e}")

    items: list[dict] = []
    feed_title = ""

    # RSS 2.0 / RDF
    if root.tag.lower().endswith("rss") or root.tag.endswith("}RDF"):
        channel = root.find("channel") or root.find("{http://purl.org/rss/1.0/}channel") or root
        ft = channel.find("title")
        if ft is not None and ft.text:
            feed_title = ft.text.strip()
        for item in root.iter():
            tag = item.tag.split("}")[-1]
            if tag != "item":
                continue
            title = (item.findtext("title") or "").strip()
            raw_link = (item.findtext("link") or "").strip()
            pub = (item.findtext("pubDate")
                   or item.findtext("{http://purl.org/dc/elements/1.1/}date")
                   or "")
            desc_html = (item.findtext("description")
                         or item.findtext("{http://purl.org/rss/1.0/modules/content/}encoded")
                         or "")
            primary, discussion = _extract_primary_and_discussion(url, raw_link, desc_html)
            items.append({
                "title": title,
                "link": canonicalize_url(primary),
                "discussion": canonicalize_url(discussion),
                "published": pub,
                "summary_raw": _strip_html(desc_html),
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
            summary = (entry.findtext("atom:summary", default="", namespaces=NS)
                       or entry.findtext("atom:content", default="", namespaces=NS))
            primary, discussion = _extract_primary_and_discussion(url, raw_link, summary)
            items.append({
                "title": title,
                "link": canonicalize_url(primary),
                "discussion": canonicalize_url(discussion),
                "published": pub,
                "summary_raw": _strip_html(summary),
            })

    return feed_title, items


# ---- per-feed worker ------------------------------------------------------

def process(url: str, since: datetime, max_items: int, timeout: int,
            cache: dict, use_cache: bool, full_summaries: bool
            ) -> tuple[str, list[dict], str | None, str, dict | None]:
    """Returns (url, items, error_msg, status, new_cache_entry).
    status ∈ {"ok","cached","err"}.
    """
    cache_entry = cache.get(url) if use_cache else None
    try:
        body, new_cache = fetch(url, timeout, cache_entry)
    except (HTTPError, URLError, ValueError, TimeoutError) as e:
        return url, [], f"{type(e).__name__}: {e}", "err", None

    if body is None:  # 304
        return url, [], None, "cached", new_cache or cache_entry

    try:
        feed_title, items = parse_feed(url, body)
    except ValueError as e:
        return url, [], f"{type(e).__name__}: {e}", "err", None

    out = []
    for it in items:
        dt = parse_date(it["published"])
        if dt and dt < since:
            continue
        it["published"] = dt.isoformat() if dt else ""
        it["feed"] = url
        it["feed_title"] = feed_title
        summary = it.pop("summary_raw")
        it["summary"] = summary if full_summaries else first_sentence(summary)
        out.append(it)
        if len(out) >= max_items:
            break
    return url, out, None, "ok", new_cache


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

    with ThreadPoolExecutor(max_workers=args.concurrency) as ex:
        futs = {
            ex.submit(process, u, since, args.max_per_feed, args.timeout,
                      cache, not args.no_cache, args.full_summaries): u
            for u in urls
        }
        for fut in as_completed(futs):
            url, items, err, status, ce = fut.result()
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

    if not args.no_cache:
        _save_cache(new_cache)
    return 0


if __name__ == "__main__":
    sys.exit(main())
