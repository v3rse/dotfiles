#!/usr/bin/env python3
"""Blindspots — surface items the tech-catchup filter quietly removed.

Reads the same fresh JSONL that rank.py reads (post-fetch, post-dedup,
pre-rank). Inverts the scoring logic so items the regular ranker would
drop or relegate to "radar" become candidates here.

Each surfaced item gets a `reason` tag explaining why your filter killed it:

  ignored        matched a keyword in profile.md `Ignore:`
  off-domain     feed lives in a domain you deprioritized (per profile)
  outside-interests
                 passed filters but didn't match any `Interests:` keyword
  no-tastemaker  high-engagement aggregator item with no tastemaker overlap

Items are scored by:
  +3 multi-source (≥2 feeds carry it)
  +2 aggregator coverage (HN/Lobsters/Techmeme)
  +1 published in last 3 days (recency)
  +1 long, substantive title (>= 8 words)

Top --top N picks are emitted, grouped by reason.

Usage:
  blindspots.py --profile ~/org/news/profile.md --top 8 \
    --deprioritized hardware,adjacent < /tmp/catchup.fresh.jsonl

Output JSONL (one cluster per line), shape mirrors rank.py output plus:
  "reason": "ignored|off-domain|outside-interests|no-tastemaker"
  "reason_detail": "matched 'crypto'" | "feed→hardware (off by default)" | ...
"""
from __future__ import annotations
import argparse, json, sys, os, re, hashlib
from datetime import datetime, timezone, timedelta
from pathlib import Path

# Reuse tech-catchup's helpers
TC_SKILL = Path.home() / ".pi" / "agent" / "skills" / "tech-catchup"
sys.path.insert(0, str(TC_SKILL / "scripts"))
from _lib import (  # type: ignore
    canonicalize_url, parse_profile, feeds_by_domain,
    load_tastemakers, title_fingerprint, parse_date, score_item,
)

NEWS = Path(os.environ.get("TECH_CATCHUP_DIR", str(Path.home() / "org" / "news")))
FEEDS_DIR = NEWS / "feeds"
TASTEMAKERS_PATH = FEEDS_DIR / "tastemakers.json"
LLM_CACHE_DIR = Path.home() / ".cache" / "tech-catchup" / "llm"

# Preserve old blindspots aggregators (excludes reddit.com) for no behavior change
BLINDSPOTS_AGGREGATORS = ("hnrss.org", "ycombinator.com", "lobste.rs", "techmeme.com")

# Fixed rationale buckets for grouping skipped items
RATIONALE_BUCKETS = {
    "off-topic": ["off-topic", "off topic", "not relevant", "unrelated"],
    "noise": ["noise", "low quality", "clickbait", "listicle", "fluff"],
    "framework-drama": ["framework", "drama", "react", "vue", "angular", "svelte"],
    "crypto": ["crypto", "web3", "blockchain", "nft", "bitcoin", "ethereum"],
    "hype": ["hype", "overhyped", "buzzword", "marketing", "pr"],
    "low-signal-aggregator": ["aggregator", "reddit", "low signal", "minimal"],
}



def kw_hit(text: str, keywords: list[str]) -> tuple[bool, str]:
    """Return (True, matched_kw) if any keyword appears as a whole word in text."""
    if not keywords:
        return False, ""
    t = text.lower()
    for kw in keywords:
        kw_low = kw.lower().strip()
        if not kw_low:
            continue
        if re.search(rf"\b{re.escape(kw_low)}\b", t):
            return True, kw
    return False, ""


def _llm_cache_path(url: str) -> Path:
    key = hashlib.sha1(url.encode()).hexdigest()
    return LLM_CACHE_DIR / f"{key}.json"


def _load_llm_cache(url: str) -> dict | None:
    """Load cached LLM result for a URL, or None if not cached."""
    p = _llm_cache_path(url)
    if not p.exists():
        return None
    try:
        data = json.loads(p.read_text())
        if isinstance(data, dict) and "section" in data:
            return data
    except (json.JSONDecodeError, OSError):
        pass
    return None


def _bucket_rationale(rationale: str) -> str:
    """Map an LLM rationale string to a fixed bucket name."""
    r_lower = rationale.lower()
    for bucket, keywords in RATIONALE_BUCKETS.items():
        for kw in keywords:
            if kw in r_lower:
                return bucket
    # Extract first noun phrase as fallback: look for first 1-3 word phrase
    words = re.findall(r"[a-z]{3,}", r_lower)
    if words:
        return words[0]
    return "other"


def _today_digest_urls() -> set[str]:
    """Extract canonical URLs from today's tech-catchup digest markdown."""
    urls: set[str] = set()
    today = datetime.now(timezone.utc).strftime("%Y-%m-%d")
    # Look for today's digest file (exact date or date-pm variant)
    candidates = [
        NEWS / f"tech-catchup-{today}.md",
        NEWS / f"tech-catchup-{today}-pm.md",
    ]
    # Also check yesterday in case it's early morning
    yesterday = (datetime.now(timezone.utc) - timedelta(days=1)).strftime("%Y-%m-%d")
    candidates.extend([
        NEWS / f"tech-catchup-{yesterday}.md",
        NEWS / f"tech-catchup-{yesterday}-pm.md",
    ])

    for path in candidates:
        if not path.exists():
            continue
        text = path.read_text()
        # Extract URLs from markdown links: [text](url)
        for match in re.finditer(r"\[([^\]]+)\]\(([^)]+)\)", text):
            url = match.group(2).strip()
            if url.startswith("http"):
                urls.add(canonicalize_url(url))
        # Also extract bare URLs
        for match in re.finditer(r"https?://[^\s\)\"\']+", text):
            urls.add(canonicalize_url(match.group(0)))
    return urls


def cluster(items: list[dict]) -> list[dict]:
    """Same clustering logic as rank.py but lighter."""
    clusters: list[dict] = []
    by_fp: dict[str, int] = {}
    by_link: dict[str, int] = {}
    for item in items:
        title = item.get("title", "") or ""
        link = canonicalize_url(item.get("link", ""))
        fp = title_fingerprint(title)
        idx = by_link.get(link) if link else None
        if idx is None and fp:
            idx = by_fp.get(fp)
        if idx is not None:
            c = clusters[idx]
            c["sources"].append({
                "feed": item.get("feed", ""),
                "feed_title": item.get("feed_title", ""),
            })
            if not c["primary"] and link:
                c["primary"] = link
            if not c["discussion"] and item.get("discussion"):
                c["discussion"] = canonicalize_url(item["discussion"])
        else:
            clusters.append({
                "title": title,
                "primary": link,
                "discussion": canonicalize_url(item.get("discussion", "")),
                "summary": item.get("summary", ""),
                "published": item.get("published", ""),
                "sources": [{
                    "feed": item.get("feed", ""),
                    "feed_title": item.get("feed_title", ""),
                }],
            })
            if link:
                by_link[link] = len(clusters) - 1
            if fp:
                by_fp[fp] = len(clusters) - 1
    return clusters


def curiosity_score(c: dict) -> int:
    """How interesting this blindspot item is, for ordering only."""
    s = 0
    feeds = {canonicalize_url(x["feed"]) for x in c["sources"]}
    if len(feeds) >= 2:
        s += 3
    if any(any(agg in (f or "").lower() for agg in BLINDSPOTS_AGGREGATORS)
           for f in feeds):
        s += 2
    pub = parse_date(c.get("published"))
    if pub and pub >= datetime.now(timezone.utc) - timedelta(days=3):
        s += 1
    if len((c.get("title") or "").split()) >= 8:
        s += 1
    return s


def classify(c: dict, profile: dict, deprioritized: set[str],
             trusted_hosts: list[tuple[str, str]],
             domain_of: dict[str, str]) -> tuple[str, str] | None:
    """Return (reason, detail) only if rank.py would have dropped this item
    OR put it in a deprioritized domain. Otherwise return None — it's not
    a blindspot, the regular ranker would have surfaced it."""
    haystack = (c["title"] + " " + c.get("summary", "")).lower()
    feeds = [s["feed"] for s in c["sources"]]

    # 1. ignored — hard drop in rank.py
    hit, kw = kw_hit(haystack, profile["ignore"])
    if hit:
        return "ignored", f"matched ignore keyword: '{kw}'"

    # Determine primary domain (first non-aggregator feed)
    domain = None
    for f in feeds:
        d = domain_of.get(f)
        if d and d != "aggregators":
            domain = d
            break

    # Preserve old blindspots aggregators (excludes reddit.com) for no behavior change
    rs, _reasons, _tastemaker_via = score_item(c, profile, trusted_hosts, BLINDSPOTS_AGGREGATORS)

    # 2. off-domain — would survive ranking but lives in a domain
    #    the user deprioritized. Worth surfacing as a blindspot regardless
    #    of score (the user said they don't want this domain by default).
    if domain and domain in deprioritized:
        return "off-domain", f"feed → {domain} (deprioritized in profile)"

    # 3 & 4: only items that would land in rank.py's 'radar' (score < 1)
    if rs >= 1:
        return None  # would surface in main digest, not a blindspot

    has_tm = any(host in (f or "").lower()
                 for _, host in trusted_hosts for f in feeds)
    is_agg = any(any(agg in (f or "").lower() for agg in BLINDSPOTS_AGGREGATORS)
                 for f in feeds)

    if is_agg and not has_tm:
        return "no-tastemaker", "on aggregator, no trusted-source overlap"

    return "outside-interests", "no interest-keyword or tastemaker hit"


def classify_llm(c: dict, digest_urls: set[str]) -> tuple[str, str, int] | None:
    """LLM-based blindspot classification.

    Returns (reason, detail, curiosity_score) if the item was skipped by LLM
    and is NOT already in today's digest. Returns None otherwise.
    """
    primary = c.get("primary", "")
    if not primary:
        return None

    # Deduplicate against digest
    canon = canonicalize_url(primary)
    if canon in digest_urls:
        return None

    cached = _load_llm_cache(primary)
    if cached is None:
        return None  # No LLM data for this item

    section = cached.get("section", "")
    if section != "skip":
        return None  # LLM didn't skip this item

    rationale = cached.get("rationale", "")
    bucket = _bucket_rationale(rationale)

    # Curiosity score: same heuristics as before, but only for skipped items
    score = curiosity_score(c)

    return bucket, rationale, score


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--profile",
                    default=str(Path.home() / "org" / "news" / "profile.md"))
    ap.add_argument("--top", type=int, default=8)
    ap.add_argument("--deprioritized", default="",
                    help="comma list of domains to treat as off-domain "
                         "(e.g. 'hardware,adjacent'). Defaults to empty.")
    ap.add_argument("--use-llm", action="store_true",
                    help="Use LLM skip signal when available (reads cached llm_rank.py results)")
    args = ap.parse_args()

    profile = parse_profile(args.profile)
    domain_of = feeds_by_domain(FEEDS_DIR)
    tastemakers = load_tastemakers(TASTEMAKERS_PATH)
    deprioritized = {d.strip() for d in args.deprioritized.split(",") if d.strip()}

    profile_handles = {h.lower().lstrip("@") for h in profile["tastemakers"]}
    trusted_hosts: list[tuple[str, str]] = []
    for handle, hosts in tastemakers.items():
        if not profile_handles or handle.lower() in profile_handles:
            for h in hosts:
                trusted_hosts.append((handle, h.lower()))

    items = []
    for line in sys.stdin:
        line = line.strip()
        if not line:
            continue
        try:
            items.append(json.loads(line))
        except json.JSONDecodeError:
            continue

    clusters = cluster(items)

    # Decide path: LLM-based or heuristic
    # If --use-llm flag is set OR any cache files exist, try LLM path first
    has_llm_cache = LLM_CACHE_DIR.exists() and any(LLM_CACHE_DIR.iterdir())
    use_llm = args.use_llm or has_llm_cache

    candidates: list[dict] = []

    if use_llm:
        digest_urls = _today_digest_urls()
        llm_candidates: list[dict] = []
        heuristic_candidates: list[dict] = []

        for c in clusters:
            # Try LLM path first
            llm_cls = classify_llm(c, digest_urls)
            if llm_cls is not None:
                reason, detail, score = llm_cls
                c["score"] = score
                c["reason"] = reason
                c["reason_detail"] = detail
                c["llm_rationale"] = detail
                llm_candidates.append(c)
                continue

            # Fall back to heuristic for this item (whether or not LLM cache exists)
            cls = classify(c, profile, deprioritized, trusted_hosts, domain_of)
            if cls is not None:
                reason, detail = cls
                c["score"] = curiosity_score(c)
                c["reason"] = reason
                c["reason_detail"] = detail
                heuristic_candidates.append(c)

        if llm_candidates:
            # Sort by curiosity score, take top N
            llm_candidates.sort(key=lambda c: (-c["score"], c.get("published") or ""))
            candidates = llm_candidates[: args.top]
            # Sort by reason bucket for stable grouping
            candidates.sort(key=lambda c: (c["reason"], -c["score"]))
            mode = "llm"
        else:
            # No LLM skips found, use heuristic candidates
            candidates = heuristic_candidates
            mode = "heuristic"
    else:
        # Pure heuristic path (no LLM cache, no --use-llm)
        for c in clusters:
            cls = classify(c, profile, deprioritized, trusted_hosts, domain_of)
            if cls is None:
                continue
            reason, detail = cls
            c["score"] = curiosity_score(c)
            c["reason"] = reason
            c["reason_detail"] = detail
            candidates.append(c)

        candidates.sort(key=lambda c: (-c["score"], c.get("published") or ""))

        # Take top N globally, then sort by reason for stable output grouping
        REASON_ORDER = ["ignored", "off-domain", "no-tastemaker", "outside-interests"]
        top = candidates[: args.top]
        top.sort(key=lambda c: (REASON_ORDER.index(c["reason"]), -c["score"]))
        candidates = top
        mode = "heuristic"

    counts: dict[str, int] = {}
    for c in candidates:
        counts[c["reason"]] = counts.get(c["reason"], 0) + 1
        out = {
            "reason": c["reason"],
            "reason_detail": c["reason_detail"],
            "score": c["score"],
            "title": c["title"],
            "primary": c["primary"],
            "discussion": c["discussion"],
            "sources": c["sources"],
            "summary": c.get("summary", ""),
            "published": c.get("published", ""),
        }
        # Preserve LLM fields if present
        if "llm_rationale" in c:
            out["llm_rationale"] = c["llm_rationale"]
        print(json.dumps(out, ensure_ascii=False))

    print(f"blindspots ({mode}): {len(candidates)} candidates from {len(clusters)} "
          f"clusters → emitted {len(candidates)} ("
          + " · ".join(f"{k}={v}" for k, v in sorted(counts.items())) + ")",
          file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
