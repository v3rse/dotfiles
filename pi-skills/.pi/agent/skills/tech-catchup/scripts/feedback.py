#!/usr/bin/env python3
"""Feedback capture CLI for the tech-catchup pipeline.

Appends feedback entries to ~/org/news/feedback.jsonl.
Usage: python3 feedback.py {up|down|save} <url> [--title <title>]

No web server or daemon — pure CLI. The feedback file is human-readable JSONL.
"""
from __future__ import annotations
import argparse, json, os, sys
from datetime import datetime, timezone
from pathlib import Path

FEEDBACK_PATH = Path.home() / "org" / "news" / "feedback.jsonl"

VALID_ACTIONS = {"up", "down", "save"}


def append_feedback(action: str, url: str, title: str = "") -> None:
    """Append a feedback entry to the JSONL file."""
    if action not in VALID_ACTIONS:
        print(f"feedback.py: invalid action '{action}'. Use: up | down | save", file=sys.stderr)
        sys.exit(1)

    if not url:
        print("feedback.py: URL is required", file=sys.stderr)
        sys.exit(1)

    entry = {
        "ts": datetime.now(timezone.utc).isoformat(),
        "action": action,
        "url": url,
        "title": title,
    }

    # Ensure parent directory exists
    FEEDBACK_PATH.parent.mkdir(parents=True, exist_ok=True)

    # Append as JSONL (one JSON object per line, human-readable)
    with FEEDBACK_PATH.open("a", encoding="utf-8") as f:
        f.write(json.dumps(entry, ensure_ascii=False) + "\n")

    print(f"feedback.py: recorded {action} for {url}")


def main() -> int:
    ap = argparse.ArgumentParser(
        description="Record feedback on a digest item. Appends to ~/org/news/feedback.jsonl"
    )
    ap.add_argument("action", choices=["up", "down", "save"], help="Feedback action")
    ap.add_argument("url", help="Item URL or identifier")
    ap.add_argument("--title", default="", help="Optional item title (for context)")
    args = ap.parse_args()

    append_feedback(args.action, args.url, args.title)
    return 0


if __name__ == "__main__":
    sys.exit(main())
