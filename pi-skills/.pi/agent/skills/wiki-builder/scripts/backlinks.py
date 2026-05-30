#!/usr/bin/env python3
"""Compute backlinks, orphans, and unlinked mentions across the wiki.

Usage:
  python3 backlinks.py [wiki-root] [--orphans] [--unlinked] [--all]

Modes (default: --all):
  --orphans   Pages with zero incoming related: links
  --unlinked  Pages whose slug or title appears as plain text in another
              page's body but is NOT in that page's related: block
  --all       Run both (default when no flag given)

Output is a plain-text report suitable for pasting into a session or
for the agent to act on (add related: links, promote orphans, etc.).
"""
import os, re, sys

IGNORE = {"README.md", "index.md", "log.md"}

args = sys.argv[1:]
wiki_dir = os.path.expanduser("~/org/wiki")

show_orphans = False
show_unlinked = False

clean_args = []
for a in args:
    if a == "--orphans":
        show_orphans = True
    elif a == "--unlinked":
        show_unlinked = True
    elif a == "--all":
        show_orphans = True
        show_unlinked = True
    else:
        clean_args.append(a)

if clean_args:
    wiki_dir = os.path.expanduser(clean_args[0])

if not show_orphans and not show_unlinked:
    show_orphans = True
    show_unlinked = True

# ── load pages ────────────────────────────────────────────────────────────────

pages = {}  # slug → {title, fname, related, body}

for fname in sorted(os.listdir(wiki_dir)):
    if not fname.endswith(".md") or fname in IGNORE:
        continue
    path = os.path.join(wiki_dir, fname)
    with open(path) as f:
        content = f.read()

    slug = fname[:-3]
    fm_match = re.match(r'^---\n(.*?)\n---\s*\n', content, re.DOTALL)
    fm = fm_match.group(1) if fm_match else ""
    body = content[fm_match.end():] if fm_match else content

    title_m = re.search(r'^title:\s*(.+)$', fm, re.MULTILINE)
    related_m = re.search(r'^related:\n((?:  -.+\n)+)', fm + '\n', re.MULTILINE)

    title = title_m.group(1).strip() if title_m else slug
    related = []
    if related_m:
        for line in related_m.group(1).strip().splitlines():
            ref = line.strip().lstrip('- ').strip()
            if ref:
                related.append(ref)

    pages[slug] = {
        "title": title,
        "fname": fname,
        "related": related,
        "body": body,
    }

# ── build incoming-link map ───────────────────────────────────────────────────

incoming = {slug: [] for slug in pages}

for slug, page in pages.items():
    for ref in page["related"]:
        if ref in incoming:
            incoming[ref].append(slug)

# ── orphans ───────────────────────────────────────────────────────────────────

if show_orphans:
    orphans = [slug for slug, inc in incoming.items() if not inc]
    print(f"── ORPHANS ({len(orphans)} pages with no incoming links) " + "─" * 30)
    if orphans:
        for slug in sorted(orphans):
            print(f"  {slug}")
    else:
        print("  None — every page has at least one incoming link ✓")
    print()

# ── unlinked mentions ─────────────────────────────────────────────────────────

if show_unlinked:
    suggestions = []  # (source_slug, target_slug, matched_text, line_preview)

    for source_slug, source_page in pages.items():
        body = source_page["body"]
        existing_related = set(source_page["related"])

        for target_slug, target_page in pages.items():
            if target_slug == source_slug:
                continue
            if target_slug in existing_related:
                continue

            # Build patterns: slug (with hyphens as word boundaries) and title
            patterns = []

            # slug: match as plain text not inside []() link syntax
            slug_pat = re.compile(
                r'(?<!\[)(?<!\()' + re.escape(target_slug) + r'(?!\]|\))',
                re.IGNORECASE
            )
            # title: only if it's ≥ 4 words or clearly distinctive (skip short generic titles)
            title_words = target_page["title"].split()
            if len(title_words) >= 3:
                title_pat = re.compile(re.escape(target_page["title"]), re.IGNORECASE)
                patterns.append(("title", title_pat))
            patterns.append(("slug", slug_pat))

            for kind, pat in patterns:
                for i, line in enumerate(body.splitlines(), 1):
                    if pat.search(line):
                        # skip lines that are already markdown links to target
                        if re.search(r'\[.*?\]\(' + re.escape(target_slug) + r'\.md\)', line):
                            continue
                        suggestions.append({
                            "source": source_slug,
                            "target": target_slug,
                            "kind": kind,
                            "line": i,
                            "preview": line.strip()[:100],
                        })
                        break  # one suggestion per (source, target) pair
                else:
                    continue
                break

    # deduplicate (source, target) pairs
    seen = set()
    unique = []
    for s in suggestions:
        key = (s["source"], s["target"])
        if key not in seen:
            seen.add(key)
            unique.append(s)

    print(f"── UNLINKED MENTIONS ({len(unique)} suggestions) " + "─" * 30)
    if unique:
        # group by source page
        by_source = {}
        for s in unique:
            by_source.setdefault(s["source"], []).append(s)
        for source_slug in sorted(by_source):
            print(f"\n  {source_slug}:")
            for s in by_source[source_slug]:
                print(f"    → add related: {s['target']}")
                print(f"      ({s['kind']} match on line {s['line']}: \"{s['preview']}\")")
    else:
        print("  None — no unlinked mentions found ✓")
    print()

print(f"── {len(pages)} pages scanned ({wiki_dir})")
