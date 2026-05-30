#!/usr/bin/env python3
"""
CodeCrafters progress tracker.
DB: ~/.local/share/codecrafters/progress.db

Usage:
  progress.py init
  progress.py record <challenge> <slug> <pass|fail>
  progress.py status [<challenge>]
  progress.py next [<challenge>]
  progress.py dashboard
  progress.py log <challenge>
"""
import sys, os, sqlite3, json
from datetime import datetime, timezone
from pathlib import Path

DB_PATH    = Path.home() / ".local/share/codecrafters/progress.db"
STAGES_DIR = Path.home() / "src/codecrafters/stages"

CHALLENGES        = ["redis", "interpreter", "sqlite", "git", "http-server"]
RECOMMENDED_ORDER = ["http-server", "redis", "interpreter", "sqlite", "git"]

# ── DB helpers ────────────────────────────────────────────────────────────────

def get_conn():
    DB_PATH.parent.mkdir(parents=True, exist_ok=True)
    conn = sqlite3.connect(DB_PATH)
    conn.row_factory = sqlite3.Row
    return conn

def init_db(silent=False):
    conn = get_conn()
    conn.executescript("""
        CREATE TABLE IF NOT EXISTS attempts (
            id          INTEGER PRIMARY KEY AUTOINCREMENT,
            challenge   TEXT NOT NULL,
            slug        TEXT NOT NULL,
            status      TEXT NOT NULL CHECK(status IN ('pass','fail')),
            ts          TEXT NOT NULL DEFAULT (datetime('now','utc')),
            notes       TEXT
        );
        CREATE INDEX IF NOT EXISTS idx_attempts_cs ON attempts(challenge, slug);
        CREATE INDEX IF NOT EXISTS idx_attempts_ts ON attempts(ts);
    """)
    conn.commit()
    conn.close()
    if not silent:
        print(f"DB ready: {DB_PATH}")

def load_stages(challenge):
    f = STAGES_DIR / f"{challenge}.json"
    if not f.exists():
        return []
    return json.loads(f.read_text())

def slug_statuses(conn, challenge):
    """Returns dict: slug → latest status ('pass'|'fail')"""
    rows = conn.execute(
        "SELECT slug, status FROM attempts WHERE challenge=? ORDER BY ts DESC",
        (challenge,)
    ).fetchall()
    seen = {}
    for row in rows:
        if row["slug"] not in seen:
            seen[row["slug"]] = row["status"]
    return seen

# ── Commands ──────────────────────────────────────────────────────────────────

def cmd_init():
    init_db()

def cmd_record(challenge, slug, status):
    if status not in ("pass", "fail"):
        print(f"Error: status must be 'pass' or 'fail', got '{status}'")
        sys.exit(1)
    init_db(silent=True)
    ts = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")
    conn = get_conn()
    conn.execute(
        "INSERT INTO attempts (challenge, slug, status, ts) VALUES (?,?,?,?)",
        (challenge, slug, status, ts)
    )
    conn.commit()
    conn.close()

    icon  = "✓" if status == "pass" else "✗"
    color = "\033[32m" if status == "pass" else "\033[31m"
    reset = "\033[0m"
    # Get stage title if possible
    stages = load_stages(challenge)
    title  = next((s["title"] for s in stages if s["slug"] == slug), slug)
    print(f"  {color}{icon}{reset}  {challenge}/{slug} — {title}  [{status}]  {ts}")


def cmd_status(challenge=None):
    init_db(silent=True)
    conn = get_conn()
    targets = [challenge] if challenge else RECOMMENDED_ORDER

    for ch in targets:
        stages = load_stages(ch)
        if not stages:
            print(f"\n{ch}: (no stage data at {STAGES_DIR})")
            continue

        base   = [s for s in stages if s["group"] == "base"]
        smap   = slug_statuses(conn, ch)
        passed = sum(1 for s in base if smap.get(s["slug"]) == "pass")
        total  = len(base)

        print(f"\n{'─'*54}")
        print(f"  {ch.upper()}  —  {passed}/{total} base stages  {'✓ complete' if passed == total else ''}")
        print(f"{'─'*54}")

        for i, s in enumerate(base, 1):
            st    = smap.get(s["slug"])
            icon  = "✓" if st == "pass" else ("✗" if st == "fail" else "○")
            color = "\033[32m" if st == "pass" else ("\033[31m" if st == "fail" else "\033[90m")
            reset = "\033[0m"
            print(f"  {color}{icon}{reset}  {i:2}.  [{s['slug']}]  {s['title']}")

        # Show first blocked ext stage, if any
        ext = [s for s in stages if s["group"] != "base"]
        if ext:
            ext_passed = sum(1 for s in ext if smap.get(s["slug"]) == "pass")
            if ext_passed:
                print(f"\n  +{ext_passed} extension stage(s) also passed")

    conn.close()


def cmd_next(challenge=None):
    init_db(silent=True)
    conn = get_conn()

    if challenge:
        targets = [challenge]
    else:
        # Prioritise challenges already in progress (have any attempts)
        all_attempted = set(
            row[0] for row in conn.execute(
                "SELECT DISTINCT challenge FROM attempts"
            ).fetchall()
        )
        in_progress = [ch for ch in RECOMMENDED_ORDER if ch in all_attempted]
        not_started = [ch for ch in RECOMMENDED_ORDER if ch not in all_attempted]
        targets = in_progress + not_started

    found = False
    for ch in targets:
        stages = load_stages(ch)
        if not stages:
            continue

        base = [s for s in stages if s["group"] == "base"]
        smap = slug_statuses(conn, ch)

        nxt = next((s for s in base if smap.get(s["slug"]) != "pass"), None)
        if nxt:
            done  = sum(1 for s in base if smap.get(s["slug"]) == "pass")
            total = len(base)
            last_st = smap.get(nxt["slug"])
            retry   = "  (failed last attempt)" if last_st == "fail" else ""
            print(f"\n  Next: {ch}  →  [{nxt['slug']}]  {nxt['title']}{retry}")
            print(f"         Progress: {done}/{total} base stages complete")
            found = True
            break
        else:
            # All base stages done for this challenge
            print(f"  {ch}: all {len(base)} base stages complete ✓")

    if not found and not challenge:
        print("\n  All challenges complete! Consider the extension stages.")

    conn.close()


def cmd_dashboard():
    init_db(silent=True)
    conn = get_conn()

    print(f"\n  CodeCrafters — {datetime.now().strftime('%Y-%m-%d')}\n")
    bar_w = 20

    for ch in RECOMMENDED_ORDER:
        stages = load_stages(ch)
        if not stages:
            continue

        base   = [s for s in stages if s["group"] == "base"]
        total  = len(base)
        smap   = slug_statuses(conn, ch)
        passed = sum(1 for s in base if smap.get(s["slug"]) == "pass")
        failed = sum(1 for s in base if smap.get(s["slug"]) == "fail")
        pct    = int(100 * passed / total) if total else 0

        filled = int(bar_w * passed / total) if total else 0
        bar    = "█" * filled + "░" * (bar_w - filled)

        nxt      = next((s for s in base if smap.get(s["slug"]) != "pass"), None)
        nxt_info = f"→ [{nxt['slug']}] {nxt['title']}" if nxt else "✓ all base complete"

        fail_tag = f"  \033[31m{failed} failing\033[0m" if failed else ""
        print(f"  {ch:<15}  [{bar}]  {passed:2}/{total}  ({pct:3}%){fail_tag}")
        print(f"               {nxt_info}\n")

    conn.close()


def cmd_log(challenge):
    init_db(silent=True)
    conn = get_conn()

    stages     = load_stages(challenge)
    slug_title = {s["slug"]: s["title"] for s in stages}

    rows = conn.execute(
        "SELECT slug, status, ts FROM attempts WHERE challenge=? ORDER BY ts DESC LIMIT 50",
        (challenge,)
    ).fetchall()

    if not rows:
        print(f"\n  No attempts recorded for '{challenge}'")
        print(f"  Run:  python3 progress.py record {challenge} <slug> pass|fail")
    else:
        print(f"\n  Recent attempts — {challenge}\n")
        prev_date = None
        for row in rows:
            date = row["ts"][:10]
            if date != prev_date:
                print(f"  ── {date} ──")
                prev_date = date
            icon  = "✓" if row["status"] == "pass" else "✗"
            color = "\033[32m" if row["status"] == "pass" else "\033[31m"
            reset = "\033[0m"
            title = slug_title.get(row["slug"], "?")
            time  = row["ts"][11:16]
            print(f"  {color}{icon}{reset}  {time}  [{row['slug']}]  {title}")

    conn.close()

# ── Entry point ───────────────────────────────────────────────────────────────

def main():
    args = sys.argv[1:]
    if not args:
        print(__doc__)
        sys.exit(0)

    cmd = args[0]

    if cmd == "init":
        cmd_init()

    elif cmd == "record":
        if len(args) < 4:
            print("Usage: progress.py record <challenge> <slug> <pass|fail>")
            sys.exit(1)
        cmd_record(args[1], args[2], args[3])

    elif cmd == "status":
        cmd_status(args[1] if len(args) > 1 else None)

    elif cmd == "next":
        cmd_next(args[1] if len(args) > 1 else None)

    elif cmd == "dashboard":
        cmd_dashboard()

    elif cmd == "log":
        if len(args) < 2:
            print("Usage: progress.py log <challenge>")
            sys.exit(1)
        cmd_log(args[1])

    else:
        print(f"Unknown command: {cmd}")
        print(__doc__)
        sys.exit(1)

if __name__ == "__main__":
    main()
