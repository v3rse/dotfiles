#!/usr/bin/env python3
"""Cluster, score, and bucket items so the agent reads ~30 records, not 200.

Pipeline:
  fresh JSONL → cluster across feeds → score → bucket → cap → ranked JSONL

Clustering merges items with the same title fingerprint OR the same canonical
primary URL across different feeds. The cluster gets one row with merged
sources[], so multi-source corroboration is computed by the script, not the
agent reasoning over 200 lines.

Scoring (per cluster):
  +3  if ≥2 sources               → multi-source corroboration
  +2  if any source = trusted tastemaker (from profile.md + tastemakers.json)
  +2  if any source ∈ Tier-1 aggregator (HN/Lobsters/Techmeme)
  +1  if title/summary matches a profile interest keyword
  drop if title/summary matches a profile ignore keyword

Bucketing:
  score ≥ 3                  → "top"
  score 1-2 with known domain → "<domain>"   (engineering, ai-deep, ...)
  score 0 / single-source     → "radar"

Caps applied: --top N global, --per-section M per non-top bucket.

Usage:
  rank.py --profile ~/org/news/profile.md --top 5 --per-section 5 < fresh.jsonl > ranked.jsonl

Output JSONL (one cluster per line):
  {
    "bucket": "top|engineering|ai-deep|...|radar",
    "score": 5,
    "title": "...",
    "primary": "https://...",
    "discussion": "https://news.ycombinator.com/item?id=...",
    "sources": [{"feed": "...", "feed_title": "..."}],
    "tastemaker_via": ["simonw"],
    "summary": "first sentence.",
    "published": "...",
    "feeds": ["...", "..."]
  }

Stderr: one summary line.
"""
from __future__ import annotations
import argparse, json, sys, re
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from _lib import (
    canonicalize_url, parse_profile, feeds_by_domain,
    load_tastemakers, title_fingerprint,
)

NEWS = Path.home() / "org" / "news"
FEEDS_DIR = NEWS / "feeds"
TASTEMAKERS_PATH = FEEDS_DIR / "tastemakers.json"

# Tier-1 aggregator hosts (substring match against feed URL)
AGGREGATORS = ("hnrss.org", "ycombinator.com", "lobste.rs", "techmeme.com")


def kw_hit(text: str, keywords: list[str]) -> bool:
    if not keywords:
        return False
    t = text.lower()
    return any(kw and kw in t for kw in keywords)


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--profile", default=str(Path.home() / "org" / "news" / "profile.md"))
    ap.add_argument("--top", type=int, default=5)
    ap.add_argument("--per-section", type=int, default=5)
    args = ap.parse_args()

    profile = parse_profile(args.profile)
    domain_of = feeds_by_domain(FEEDS_DIR)  # {feed_url: domain}
    tastemakers = load_tastemakers(TASTEMAKERS_PATH)  # {handle: [host_substr,...]}

    # Resolve which feed-host substrings are "trusted" given the profile
    trusted_hosts: list[tuple[str, str]] = []  # [(handle, host_substr), ...]
    profile_handles = {h.lower().lstrip("@") for h in profile["tastemakers"]}
    for handle, hosts in tastemakers.items():
        if not profile_handles or handle.lower() in profile_handles:
            for h in hosts:
                trusted_hosts.append((handle, h.lower()))

    # ---- read & cluster -------------------------------------------------
    clusters: list[dict] = []
    by_fp: dict[str, int] = {}
    by_link: dict[str, int] = {}

    for line in sys.stdin:
        line = line.strip()
        if not line:
            continue
        try:
            item = json.loads(line)
        except json.JSONDecodeError:
            continue

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
            # Prefer non-discussion link as primary; merge discussion if missing
            if not c["primary"] and link:
                c["primary"] = link
            if not c["discussion"] and item.get("discussion"):
                c["discussion"] = canonicalize_url(item["discussion"])
            # Prefer earlier publish date
            if item.get("published") and (
                not c["published"] or item["published"] < c["published"]
            ):
                c["published"] = item["published"]
            if link:
                by_link.setdefault(link, idx)
            if fp:
                by_fp.setdefault(fp, idx)
        else:
            c = {
                "title": title,
                "primary": link,
                "discussion": canonicalize_url(item.get("discussion", "")),
                "summary": item.get("summary", ""),
                "published": item.get("published", ""),
                "sources": [{
                    "feed": item.get("feed", ""),
                    "feed_title": item.get("feed_title", ""),
                }],
            }
            clusters.append(c)
            i = len(clusters) - 1
            if link:
                by_link[link] = i
            if fp:
                by_fp[fp] = i

    # ---- score ----------------------------------------------------------
    scored: list[dict] = []
    for c in clusters:
        feeds = [s["feed"] for s in c["sources"]]
        # Drop if matches ignore list
        haystack = (c["title"] + " " + c.get("summary", "")).lower()
        if kw_hit(haystack, profile["ignore"]):
            continue

        score = 0
        if len({canonicalize_url(f) for f in feeds}) >= 2:
            score += 3
        if any(any(agg in (f or "").lower() for agg in AGGREGATORS) for f in feeds):
            score += 2
        tastemaker_via: list[str] = []
        for handle, host in trusted_hosts:
            if any(host in (f or "").lower() for f in feeds):
                if handle not in tastemaker_via:
                    tastemaker_via.append(handle)
        if tastemaker_via:
            score += 2
        if kw_hit(haystack, profile["interests"]):
            score += 1

        # Pick a domain — first non-aggregator source's mapped domain, else "?"
        chosen_domain = None
        for f in feeds:
            d = domain_of.get(f)
            if d and d != "aggregators":
                chosen_domain = d
                break
        if chosen_domain is None:
            chosen_domain = "engineering"  # default fallback

        c["score"] = score
        c["domain"] = chosen_domain
        c["tastemaker_via"] = tastemaker_via
        c["feeds"] = feeds
        scored.append(c)

    # ---- bucket ---------------------------------------------------------
    buckets: dict[str, list[dict]] = {"top": [], "radar": []}
    for c in scored:
        if c["score"] >= 3:
            buckets["top"].append(c)
        elif c["score"] >= 1:
            buckets.setdefault(c["domain"], []).append(c)
        else:
            buckets["radar"].append(c)

    for k in buckets:
        buckets[k].sort(key=lambda c: (-c["score"],
                                       c.get("published") or ""), reverse=False)
        # ^ score desc, then earliest pub first within score
        buckets[k].sort(key=lambda c: -c["score"])

    # ---- cap ------------------------------------------------------------
    capped: list[dict] = []
    capped += buckets["top"][: args.top]
    for name, items in buckets.items():
        if name in ("top", "radar"):
            continue
        capped += [dict(c, bucket=name) for c in items[: args.per_section]]
    for c in buckets["top"][: args.top]:
        c["bucket"] = "top"
    capped = [dict(c, bucket=c.get("bucket") or
                   ("top" if c["score"] >= 3 else c["domain"]))
              for c in capped]
    capped += [dict(c, bucket="radar") for c in buckets["radar"][: args.per_section]]

    # ---- output ---------------------------------------------------------
    bucket_counts: dict[str, int] = {}
    for c in capped:
        b = c["bucket"]
        bucket_counts[b] = bucket_counts.get(b, 0) + 1
        out = {
            "bucket": b,
            "score": c["score"],
            "title": c["title"],
            "primary": c["primary"],
            "discussion": c["discussion"],
            "sources": c["sources"],
            "tastemaker_via": c["tastemaker_via"],
            "summary": c["summary"],
            "published": c["published"],
            "feeds": c["feeds"],
        }
        print(json.dumps(out, ensure_ascii=False))

    summary = " · ".join(f"{k}={v}" for k, v in sorted(bucket_counts.items()))
    print(
        f"ranked {len(scored)} clusters from {sum(len(c['sources']) for c in scored)} "
        f"items → emitted {len(capped)} ({summary})",
        file=sys.stderr,
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
