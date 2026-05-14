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
    load_tastemakers, title_fingerprint, score_item, AGGREGATORS, kw_hit,
    classify_format,
)

# Tier-1 aggregators for "top" bucket multi-source threshold.
# Techmeme is intentionally excluded (it's an aggregator-of-aggregators).
TIER1_AGGREGATORS = ("hnrss.org", "ycombinator.com", "lobste.rs", "reddit.com")


def tier1_source_count(sources: list[dict]) -> int:
    """Count distinct Tier-1 aggregator hosts among sources.

    hnrss.org and ycombinator.com both map to HN, so they count as one.
    """
    seen: set[str] = set()
    for s in sources:
        feed = (s.get("feed") or "").lower()
        if "hnrss.org" in feed or "ycombinator.com" in feed:
            seen.add("hn")
        elif "lobste.rs" in feed:
            seen.add("lobsters")
        elif "reddit.com" in feed:
            seen.add("reddit")
    return len(seen)


NEWS = Path.home() / "org" / "news"
FEEDS_DIR = NEWS / "feeds"
TASTEMAKERS_PATH = FEEDS_DIR / "tastemakers.json"


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--profile", default=str(Path.home() / "org" / "news" / "profile.md"))
    ap.add_argument("--top", type=int, default=5)
    ap.add_argument("--per-section", type=int, default=8)
    ap.add_argument("--use-llm", action="store_true", help="Re-rank with LLM after heuristic scoring")
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

        score, _reasons, tastemaker_via = score_item(c, profile, trusted_hosts, AGGREGATORS)

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
        if c["score"] >= 4 and tier1_source_count(c["sources"]) >= 2:
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
                   ("top" if c["score"] >= 4 and tier1_source_count(c["sources"]) >= 2 else c["domain"]))
              for c in capped]
    capped += [dict(c, bucket="radar") for c in buckets["radar"][: args.per_section]]

    # ---- optional LLM re-rank ---------------------------------------------
    if args.use_llm:
        import subprocess as _sp
        script_dir = Path(__file__).resolve().parent
        llm_rank_path = script_dir / "llm_rank.py"
        if llm_rank_path.exists():
            # Serialize capped items to JSONL and pipe to llm_rank.py
            input_jsonl = "\n".join(json.dumps(c, ensure_ascii=False) for c in capped)
            try:
                result = _sp.run(
                    [sys.executable, str(llm_rank_path),
                     "--profile", args.profile],
                    input=input_jsonl,
                    capture_output=True,
                    text=True,
                    timeout=300,
                )
                if result.returncode == 0:
                    new_capped: list[dict] = []
                    for line in result.stdout.strip().splitlines():
                        if not line.strip():
                            continue
                        try:
                            new_capped.append(json.loads(line))
                        except json.JSONDecodeError:
                            continue
                    if new_capped:
                        # Override bucket with llm_section when present
                        for c in new_capped:
                            if c.get("llm_section"):
                                c["bucket"] = c["llm_section"]
                        capped = new_capped
                else:
                    print(f"llm_rank.py failed (rc={result.returncode}), using heuristic ranking", file=sys.stderr)
            except Exception as e:
                print(f"llm_rank.py error: {e}, using heuristic ranking", file=sys.stderr)
        else:
            print("llm_rank.py not found, using heuristic ranking", file=sys.stderr)

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
            "format": classify_format(c),
            "summary": c["summary"],
            "published": c["published"],
            "feeds": c["feeds"],
        }
        # Preserve LLM fields if present
        for key in ("llm_score", "llm_section", "llm_rationale"):
            if key in c:
                out[key] = c[key]
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
