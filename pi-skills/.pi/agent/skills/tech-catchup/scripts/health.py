#!/usr/bin/env python3
"""Probe every feed in feeds/*.txt and track health over time.

Runs all feeds against a 30-day window, classifies each as:
  ok       — fetched, parseable, has recent items
  empty    — fetched, parseable, but 0 items in window (author idle)
  broken   — fetch failed (HTTP / DNS / parse error)

Updates ~/org/news/.feed-health.json:
  {
    "https://...": {
       "last_check": "2026-05-07",
       "last_ok":    "2026-05-07" | null,
       "status":     "ok|empty|broken",
       "fail_streak": 0,
       "domain":     "engineering",
       "last_error": "HTTPError: 404"
    }
  }

Reports a summary to stdout. Use --report for a verbose per-feed table.
Exits 0 always (advisory tool).
"""
from __future__ import annotations
import argparse, json, os, subprocess, sys
from datetime import date, datetime, timedelta, timezone
from pathlib import Path

NEWS   = Path(os.environ.get("TECH_CATCHUP_DIR", str(Path.home() / "org" / "news")))
FEEDS  = NEWS / "feeds"
HEALTH = NEWS / ".feed-health.json"


def load() -> dict:
    if not HEALTH.exists(): return {}
    try: return json.loads(HEALTH.read_text())
    except json.JSONDecodeError: return {}


def save(d: dict) -> None:
    NEWS.mkdir(parents=True, exist_ok=True)
    HEALTH.write_text(json.dumps(d, indent=2, sort_keys=True))


def feeds_by_domain() -> dict[str, list[str]]:
    out = {}
    for f in sorted(FEEDS.glob("*.txt")):
        urls = [l.strip() for l in f.read_text().splitlines()
                if l.strip() and not l.strip().startswith("#")]
        out[f.stem] = urls
    return out


def probe_all(since: str) -> tuple[dict, dict]:
    """Run fetch_feeds.py, parse stderr to get per-feed OK/ERR + count."""
    SKILL = Path(__file__).resolve().parent.parent
    fetch = SKILL / "scripts" / "fetch_feeds.py"
    all_urls = sorted({u for urls in feeds_by_domain().values() for u in urls})

    proc = subprocess.run(
        ["python3", str(fetch), "--since", since, "--max-per-feed", "1",
         "--timeout", "8", "--concurrency", "16", *all_urls],
        capture_output=True, text=True,
    )
    status: dict[str, dict] = {}
    for line in proc.stderr.splitlines():
        parts = line.split(maxsplit=2)
        if len(parts) < 2: continue
        tag = parts[0]
        if tag == "OK":
            n, url = int(parts[1]), parts[2]
            status[url] = {"status": "ok" if n > 0 else "empty", "count": n, "error": ""}
        elif tag == "ERR":
            err = parts[1]
            url = parts[2] if len(parts) > 2 else ""
            # Lines like "ERR HTTPError: HTTP Error 404: Not Found <url>"
            # take everything after first space, peel last token as url
            tail = line[4:].rsplit(" ", 1)
            err_msg, url = (tail[0], tail[1]) if len(tail) == 2 else (line[4:], "")
            status[url] = {"status": "broken", "count": 0, "error": err_msg[:200]}
    # Anything unaccounted for = silent failure
    for u in all_urls:
        status.setdefault(u, {"status": "broken", "count": 0, "error": "no response"})
    # url -> domain
    domain_of = {}
    for d, urls in feeds_by_domain().items():
        for u in urls: domain_of.setdefault(u, d)
    return status, domain_of


def update(status: dict, domain_of: dict) -> dict:
    health = load()
    today = date.today().isoformat()
    for url, s in status.items():
        rec = health.get(url, {"fail_streak": 0, "last_ok": None})
        rec["last_check"] = today
        rec["status"]     = s["status"]
        rec["domain"]     = domain_of.get(url, "?")
        rec["last_error"] = s["error"]
        if s["status"] == "ok":
            rec["last_ok"]     = today
            rec["fail_streak"] = 0
        elif s["status"] == "broken":
            rec["fail_streak"] = rec.get("fail_streak", 0) + 1
        else:  # empty
            rec["fail_streak"] = 0  # not a failure, just quiet
        health[url] = rec
    save(health)
    return health


def summarize(health: dict, report: bool) -> None:
    today = date.today()
    buckets = {"ok": [], "empty": [], "stale": [], "broken_3plus": [], "broken": []}
    for url, r in health.items():
        s = r["status"]
        last_ok = r.get("last_ok")
        days_idle = None
        if last_ok:
            days_idle = (today - date.fromisoformat(last_ok)).days
        if s == "broken" and r.get("fail_streak", 0) >= 3:
            buckets["broken_3plus"].append((url, r))
        elif s == "broken":
            buckets["broken"].append((url, r))
        elif s == "empty" and days_idle is not None and days_idle > 60:
            buckets["stale"].append((url, r, days_idle))
        elif s == "empty":
            buckets["empty"].append((url, r))
        else:
            buckets["ok"].append((url, r))

    n = sum(len(v) for v in buckets.values())
    print(f"feed health: {n} total · ok={len(buckets['ok'])} · "
          f"quiet={len(buckets['empty'])} · stale60d+={len(buckets['stale'])} · "
          f"broken={len(buckets['broken'])} · broken3+={len(buckets['broken_3plus'])}")

    if buckets["broken_3plus"]:
        print("\n🚨 broken (3+ consecutive failures — consider removing):")
        for url, r in buckets["broken_3plus"]:
            print(f"  [{r['domain']}] {url}\n      {r['last_error']}")
    if buckets["stale"]:
        print("\n💤 stale (no posts in 60+ days):")
        for url, r, d in sorted(buckets["stale"], key=lambda x: -x[2]):
            print(f"  [{r['domain']}] {url}  ({d}d idle)")

    if report:
        print("\n--- full report ---")
        for url, r in sorted(health.items()):
            print(f"{r['status']:7} streak={r.get('fail_streak',0)} "
                  f"last_ok={r.get('last_ok','-')} [{r['domain']}] {url}")


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--since", default=(datetime.now(timezone.utc) - timedelta(days=30)).strftime("%Y-%m-%d"))
    ap.add_argument("--report", action="store_true", help="print full per-feed table")
    args = ap.parse_args()

    print(f"probing feeds (since {args.since})...", file=sys.stderr)
    status, domain_of = probe_all(args.since)
    health = update(status, domain_of)
    summarize(health, args.report)
    return 0


if __name__ == "__main__":
    sys.exit(main())
