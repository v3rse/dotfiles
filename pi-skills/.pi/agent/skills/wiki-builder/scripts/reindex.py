#!/usr/bin/env python3
"""Regenerate index.md for a wiki directory.

Usage: python3 reindex.py [wiki-root]
Default wiki-root: ~/org/wiki/
"""
import os, re, sys
from collections import defaultdict
from datetime import date

wiki_dir = os.path.expanduser(sys.argv[1] if len(sys.argv) > 1 else "~/org/wiki")

pages = []
for fname in sorted(os.listdir(wiki_dir)):
    if not fname.endswith(".md") or fname in ("README.md", "index.md", "log.md"):
        continue
    path = os.path.join(wiki_dir, fname)
    with open(path) as f:
        content = f.read()

    fm_match = re.match(r'^---\n(.*?)\n---', content, re.DOTALL)
    if not fm_match:
        continue
    fm = fm_match.group(1)

    title_m  = re.search(r'^title:\s*(.+)$', fm, re.MULTILINE)
    tags_m   = re.search(r'^tags:\s*\[(.+?)\]', fm, re.MULTILINE)
    status_m = re.search(r'^status:\s*(.+)$', fm, re.MULTILINE)
    lead_m   = re.search(r'^> (.+)$', content, re.MULTILINE)

    title  = title_m.group(1).strip() if title_m else fname[:-3]
    tags   = [t.strip() for t in tags_m.group(1).split(',')] if tags_m else []
    status = status_m.group(1).strip() if status_m else "unknown"
    lead   = lead_m.group(1).strip() if lead_m else ""

    pages.append({"title": title, "tags": tags, "status": status, "lead": lead, "fname": fname})

by_tag = defaultdict(list)
for p in pages:
    for tag in p["tags"]:
        by_tag[tag].append(p)

stubs = [p for p in pages if p["status"] == "stub"]

today = date.today().isoformat()
lines = [
    "# Wiki Index",
    "",
    f"Last updated: {today} · {len(pages)} pages",
    "",
    "## Quick navigation (compact)",
    "",
    "*One line per page — load this section for LLM navigation. Use tag sections below for browsing.*",
    "",
]
for p in sorted(pages, key=lambda x: x["title"]):
    status_badge = " `stub`" if p["status"] == "stub" else ""
    lines.append(f"- [{p['title']}]({p['fname']}){status_badge}")

lines += ["", "---", "", "## By tag"]
for tag in sorted(by_tag.keys()):
    lines.append(f"\n### {tag}")
    for p in sorted(by_tag[tag], key=lambda x: x["title"]):
        lines.append(f"- [{p['title']}]({p['fname']}) — {p['lead']}")

lines += ["", "## All pages (alphabetical)", ""]
for p in pages:
    lines.append(f"- [{p['title']}]({p['fname']})")

if stubs:
    lines += ["", "## Stubs (needs work)", ""]
    for p in sorted(stubs, key=lambda x: x["title"]):
        lines.append(f"- [{p['title']}]({p['fname']}) — {p['lead']}")

out = os.path.join(wiki_dir, "index.md")
with open(out, "w") as f:
    f.write("\n".join(lines) + "\n")

print(f"index.md regenerated — {len(pages)} pages ({wiki_dir})")
