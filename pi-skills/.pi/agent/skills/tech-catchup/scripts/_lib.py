"""Shared helpers for the tech-catchup skill scripts.

Pure-stdlib. Imported by fetch_feeds.py, rank.py, seen.py.
"""
from __future__ import annotations
import os, re, json
from datetime import datetime, timezone
from email.utils import parsedate_to_datetime
from pathlib import Path
from urllib.parse import urlparse, urlunparse, parse_qsl, urlencode

# Tracking-only query params we always strip
_TRACKING = {
    "utm_source", "utm_medium", "utm_campaign", "utm_term", "utm_content",
    "utm_id", "utm_name", "utm_brand", "utm_social", "utm_social-type",
    "ref", "ref_src", "ref_url", "fbclid", "gclid", "mc_cid", "mc_eid",
    "source", "amp", "_hsenc", "_hsmi", "yclid", "vero_id", "vero_conv",
}


def canonicalize_url(url: str) -> str:
    """Normalize URL for stable identity: lowercase host, strip default ports,
    drop tracking query params, strip fragment, sort remaining query keys.
    """
    if not url:
        return ""
    try:
        p = urlparse(url.strip())
    except ValueError:
        return url.strip()
    scheme = (p.scheme or "https").lower()
    host = (p.hostname or "").lower()
    if host.startswith("www."):
        host = host[4:]
    netloc = host
    if p.port and not ((scheme, p.port) in (("http", 80), ("https", 443))):
        netloc = f"{host}:{p.port}"
    path = p.path or "/"
    if path != "/" and path.endswith("/"):
        path = path[:-1]
    qs = [(k, v) for k, v in parse_qsl(p.query, keep_blank_values=True)
          if k.lower() not in _TRACKING]
    qs.sort()
    query = urlencode(qs)
    return urlunparse((scheme, netloc, path, "", query, ""))


def parse_date(s: str | None) -> datetime | None:
    if not s:
        return None
    s = s.strip()
    try:
        return parsedate_to_datetime(s)
    except (TypeError, ValueError):
        pass
    for fmt in ("%Y-%m-%dT%H:%M:%S%z", "%Y-%m-%dT%H:%M:%SZ",
                "%Y-%m-%dT%H:%M:%S.%f%z", "%Y-%m-%dT%H:%M:%S.%fZ",
                "%Y-%m-%d"):
        try:
            dt = datetime.strptime(s.replace("Z", "+0000"), fmt)
            return dt if dt.tzinfo else dt.replace(tzinfo=timezone.utc)
        except ValueError:
            continue
    return None


def first_sentence(text: str, maxlen: int = 220) -> str:
    """Return the first sentence (or first maxlen chars). Used to compact
    bloated RSS summaries before they hit agent context."""
    if not text:
        return ""
    text = text.strip()
    m = re.search(r"(?<=[.!?])\s+", text)
    if m and m.start() <= maxlen:
        return text[:m.start()].strip()
    return text[:maxlen].rstrip() + ("…" if len(text) > maxlen else "")


# ----- profile.md parsing ------------------------------------------------

def parse_profile(path: str | Path) -> dict:
    """Parse ~/org/news/profile.md into a dict. Tolerates missing file."""
    out = {
        "interests": [], "ignore": [], "tastemakers": [],
        "domains": [], "budget": "15m",
    }
    p = Path(path)
    if not p.exists():
        return out
    for line in p.read_text().splitlines():
        m = re.match(r"\s*-\s*([A-Za-z][A-Za-z _-]*?)\s*:\s*(.*)", line)
        if not m:
            continue
        key = m.group(1).strip().lower()
        val = m.group(2).strip()
        if val.startswith("<") and val.endswith(">"):
            continue  # unfilled template placeholder
        items = [x.strip() for x in re.split(r"[,;]", val) if x.strip()]
        if "interest" in key:
            out["interests"] = [i.lower() for i in items]
        elif "ignore" in key:
            out["ignore"] = [i.lower() for i in items]
        elif "tastemaker" in key:
            out["tastemakers"] = items
        elif "domain" in key:
            out["domains"] = items
        elif "budget" in key:
            out["budget"] = items[0] if items else "15m"
    return out


# ----- feeds → domain map -----------------------------------------------

def feeds_by_domain(feeds_dir: str | Path) -> dict[str, str]:
    """Return {url: domain} mapping by reading every feeds/*.txt."""
    out: dict[str, str] = {}
    for f in sorted(Path(feeds_dir).glob("*.txt")):
        for line in f.read_text().splitlines():
            line = line.strip()
            if line and not line.startswith("#"):
                # support "!url" priority prefix in future; strip leading !
                url = line.lstrip("!").strip()
                out.setdefault(url, f.stem)
    return out


def load_tastemakers(path: str | Path) -> dict[str, list[str]]:
    p = Path(path)
    if not p.exists():
        return {}
    try:
        return json.loads(p.read_text())
    except json.JSONDecodeError:
        return {}


# ----- title fingerprint for cross-feed clustering ----------------------

_STOP = {
    "the","a","an","and","or","but","of","in","on","at","to","for","with",
    "by","is","are","was","were","be","been","from","as","this","that",
    "it","its","they","we","you","your","our","i","me","my","new","how",
}


def title_fingerprint(title: str, n: int = 6) -> str:
    """Produce a stable fingerprint of a title for cross-feed merging.
    Keeps the n longest unique non-stop tokens, sorted."""
    words = re.findall(r"[a-z0-9]{3,}", (title or "").lower())
    words = [w for w in words if w not in _STOP]
    if not words:
        return ""
    seen = set()
    uniq = []
    for w in words:
        if w not in seen:
            seen.add(w)
            uniq.append(w)
    uniq.sort(key=lambda w: (-len(w), w))
    return " ".join(sorted(uniq[:n]))
