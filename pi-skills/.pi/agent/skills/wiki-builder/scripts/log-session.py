#!/usr/bin/env python3
"""Append a session entry to log.md.

Usage:
  python3 log-session.py <wiki-root> "<description>" [--created slug,slug] [--updated slug,slug]

Example:
  python3 log-session.py ~/org/wiki "Linux cryptography deep dive" \
    --created "asymmetric-cryptography-fundamentals,tls-https-internals" \
    --updated "intelligence-security-curriculum"

Appends to <wiki-root>/log.md (creates file if absent).
Each entry header starts with '## [YYYY-MM-DD]' for grep/unix tool parsing.
"""
import os, sys
from datetime import date

args = sys.argv[1:]
wiki_dir = None
description = None
created_slugs = []
updated_slugs = []

i = 0
while i < len(args):
    if args[i] == "--created":
        created_slugs = [s.strip() for s in args[i+1].split(",") if s.strip()]
        i += 2
    elif args[i] == "--updated":
        updated_slugs = [s.strip() for s in args[i+1].split(",") if s.strip()]
        i += 2
    elif wiki_dir is None:
        wiki_dir = os.path.expanduser(args[i])
        i += 1
    elif description is None:
        description = args[i]
        i += 1
    else:
        i += 1

if not wiki_dir or not description:
    print("Usage: log-session.py <wiki-root> \"<description>\" [--created slugs] [--updated slugs]")
    sys.exit(1)

today = date.today().isoformat()
log_path = os.path.join(wiki_dir, "log.md")

lines = [f"\n## [{today}] session | {description}"]
if created_slugs:
    lines.append(f"Created: {', '.join(created_slugs)}")
if updated_slugs:
    lines.append(f"Updated: {', '.join(updated_slugs)}")
if not created_slugs and not updated_slugs:
    lines.append("(no pages created or updated)")

entry = "\n".join(lines) + "\n"

if not os.path.exists(log_path):
    header = "# Wiki Session Log\n\nAppend-only record of sessions, ingests, and lint passes.\nEach entry: `## [YYYY-MM-DD] <type> | <description>`\nParse with: `grep '^## \\[' log.md | tail -10`\n"
    with open(log_path, "w") as f:
        f.write(header)

with open(log_path, "a") as f:
    f.write(entry)

print(f"Appended to {log_path}")
