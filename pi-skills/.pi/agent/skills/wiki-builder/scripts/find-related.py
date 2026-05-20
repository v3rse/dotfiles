#!/usr/bin/env python3
"""Find wiki pages related to given keywords. Replaces manual ls + rg in Steps 3 & 6.

Usage: python3 find-related.py <keyword> [keyword ...] [--wiki ~/org/wiki]

Output: matching slugs with their title, tags, and matched lines — ready to
paste into a related: block or use for cross-link decisions.
"""
import os, re, sys

args = sys.argv[1:]
wiki_dir = os.path.expanduser("~/org/wiki")

# parse --wiki flag
if "--wiki" in args:
    idx = args.index("--wiki")
    wiki_dir = os.path.expanduser(args[idx + 1])
    args = args[:idx] + args[idx + 2:]

keywords = args
if not keywords:
    print("Usage: find-related.py <keyword> [keyword ...] [--wiki PATH]")
    sys.exit(1)

pattern = re.compile("|".join(re.escape(k) for k in keywords), re.IGNORECASE)

results = []
for fname in sorted(os.listdir(wiki_dir)):
    if not fname.endswith(".md") or fname in ("README.md", "index.md"):
        continue
    path = os.path.join(wiki_dir, fname)
    with open(path) as f:
        content = f.read()

    fm_match = re.match(r'^---\n(.*?)\n---', content, re.DOTALL)
    fm = fm_match.group(1) if fm_match else ""

    title_m = re.search(r'^title:\s*(.+)$', fm, re.MULTILINE)
    tags_m  = re.search(r'^tags:\s*\[(.+?)\]', fm, re.MULTILINE)
    slug_m  = re.search(r'^slug:\s*(.+)$', fm, re.MULTILINE)

    title = title_m.group(1).strip() if title_m else fname[:-3]
    tags  = tags_m.group(1).strip() if tags_m else ""
    slug  = slug_m.group(1).strip() if slug_m else fname[:-3]

    matched_lines = []
    for i, line in enumerate(content.splitlines(), 1):
        if pattern.search(line) and not line.startswith("---"):
            matched_lines.append(f"    L{i}: {line.strip()[:100]}")

    if matched_lines:
        results.append({
            "slug": slug, "title": title, "tags": tags,
            "fname": fname, "matches": matched_lines,
        })

if not results:
    print(f"No pages matched: {', '.join(keywords)}")
    sys.exit(0)

print(f"Found {len(results)} page(s) matching: {', '.join(keywords)}\n")
for r in results:
    print(f"  slug:  {r['slug']}")
    print(f"  title: {r['title']}")
    print(f"  tags:  [{r['tags']}]")
    print(f"  file:  {r['fname']}")
    if len(r['matches']) <= 4:
        for m in r['matches']:
            print(m)
    else:
        for m in r['matches'][:3]:
            print(m)
        print(f"    ... ({len(r['matches']) - 3} more matches)")
    print()

print("# related: block (copy as needed):")
for r in results:
    print(f"  - {r['slug']}")
