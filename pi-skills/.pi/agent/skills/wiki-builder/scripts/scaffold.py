#!/usr/bin/env python3
"""Scaffold a new wiki page with correct frontmatter.

Usage: python3 scaffold.py <slug> <title> [tag,tag,...] [--wiki ~/org/wiki]

Writes <wiki-root>/<slug>.md and prints the path.
Exits 1 without writing if the file already exists.
"""
import os, re, sys
from datetime import date

args = sys.argv[1:]
wiki_dir = os.path.expanduser("~/org/wiki")

if "--wiki" in args:
    idx = args.index("--wiki")
    wiki_dir = os.path.expanduser(args[idx + 1])
    args = args[:idx] + args[idx + 2:]

if len(args) < 2:
    print("Usage: scaffold.py <slug> <title> [tag,tag,...] [--wiki PATH]")
    sys.exit(1)

slug  = args[0]
title = args[1]
tags  = [t.strip() for t in args[2].split(",")] if len(args) > 2 else ["tooling"]
today = date.today().isoformat()

out_path = os.path.join(wiki_dir, f"{slug}.md")
if os.path.exists(out_path):
    print(f"ERROR: {out_path} already exists. Edit it directly or choose a different slug.")
    sys.exit(1)

tags_inline = ", ".join(tags)
template = f"""\
---
title: {title}
slug: {slug}
tags: [{tags_inline}]
status: stub
created: {today}
updated: {today}
sources:
  - 
related:
  - 
---

# {title}

> <One-sentence lead — what an LLM sees first. Make it count.>

## Summary

<2–4 sentences. Someone reading only this section should grasp the key idea.>

## Key points

- 
- 
- 

## Details

<Longer body. Include code blocks, tables, examples. Be concrete.>

## See also

- 

## Sources

- 
"""

os.makedirs(wiki_dir, exist_ok=True)
with open(out_path, "w") as f:
    f.write(template)

print(out_path)
