#!/usr/bin/env python3
"""Manage ~/org/news/.seen.json for cross-session URL dedup.

Subcommands:
  filter            stdin JSONL -> stdout JSONL, drop items with seen 'link'
  mark-from-jsonl   stdin JSONL, add every 'link' to seen.json with today's date
  mark URL [URL...] add given URLs to seen.json
  trim              drop entries older than 90 days
  count             print number of seen URLs

Path: $TECH_CATCHUP_DIR/.seen.json (default ~/org/news/.seen.json).
"""
from __future__ import annotations
import json, os, sys
from datetime import date, datetime, timedelta
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from _lib import canonicalize_url

NEWS = Path(os.environ.get("TECH_CATCHUP_DIR", str(Path.home() / "org" / "news")))
SEEN = NEWS / ".seen.json"


def load() -> dict:
    if not SEEN.exists():
        return {}
    try:
        return json.loads(SEEN.read_text())
    except json.JSONDecodeError:
        return {}


def save(d: dict) -> None:
    NEWS.mkdir(parents=True, exist_ok=True)
    SEEN.write_text(json.dumps(d, indent=2, sort_keys=True))


def _link_of(obj: dict) -> str:
    """Return the canonical primary URL of an item or cluster."""
    return canonicalize_url(obj.get("primary") or obj.get("link") or "")


def cmd_filter() -> int:
    seen = load()
    for line in sys.stdin:
        line = line.strip()
        if not line:
            continue
        try:
            obj = json.loads(line)
        except json.JSONDecodeError:
            continue
        link = _link_of(obj)
        if link and link in seen:
            continue
        print(line)
    return 0


def cmd_mark_from_jsonl() -> int:
    seen = load()
    today = date.today().isoformat()
    added = 0
    for line in sys.stdin:
        line = line.strip()
        if not line:
            continue
        try:
            obj = json.loads(line)
        except json.JSONDecodeError:
            continue
        link = _link_of(obj)
        if not link or link in seen:
            continue
        seen[link] = {"first_seen": today, "title": obj.get("title", "")[:200]}
        added += 1
    save(seen)
    print(f"marked {added} new urls", file=sys.stderr)
    return 0


def cmd_mark(urls: list[str]) -> int:
    seen = load()
    today = date.today().isoformat()
    for u in urls:
        seen.setdefault(canonicalize_url(u), {"first_seen": today, "title": ""})
    save(seen)
    return 0


def cmd_trim() -> int:
    seen = load()
    cutoff = (date.today() - timedelta(days=90)).isoformat()
    before = len(seen)
    seen = {k: v for k, v in seen.items() if v.get("first_seen", "9999") >= cutoff}
    save(seen)
    print(f"trimmed {before - len(seen)} (kept {len(seen)})", file=sys.stderr)
    return 0


def cmd_count() -> int:
    print(len(load()))
    return 0


def main() -> int:
    if len(sys.argv) < 2:
        print(__doc__, file=sys.stderr)
        return 2
    cmd = sys.argv[1]
    if cmd == "filter":          return cmd_filter()
    if cmd == "mark-from-jsonl": return cmd_mark_from_jsonl()
    if cmd == "mark":            return cmd_mark(sys.argv[2:])
    if cmd == "trim":            return cmd_trim()
    if cmd == "count":           return cmd_count()
    print(f"unknown subcommand: {cmd}", file=sys.stderr)
    return 2


if __name__ == "__main__":
    sys.exit(main())
