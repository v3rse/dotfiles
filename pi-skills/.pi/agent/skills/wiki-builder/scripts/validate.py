#!/usr/bin/env python3
"""Validate all wiki pages against the wiki-builder checklist.

Usage: python3 validate.py [wiki-root]

Checks:
  - Required frontmatter fields present (title, slug, tags, created, updated, sources)
  - created/updated are valid ISO dates
  - At least one source
  - Every related: slug points to an existing file
  - index.md lists every page
  - Warns on pages > 400 lines (mega-page risk)

Exits 0 if clean, 1 if any errors found.
"""
import os, re, sys
from datetime import date, timedelta

STUB_AGE_DAYS = 30  # warn if stub older than this

wiki_dir = os.path.expanduser(sys.argv[1] if len(sys.argv) > 1 else "~/org/wiki")
REQUIRED_FIELDS = ["title", "slug", "tags", "status", "created", "updated", "sources"]
VALID_STATUSES = {"stub", "draft", "permanent"}
DATE_RE = re.compile(r'^\d{4}-\d{2}-\d{2}$')

errors = []
warnings = []
pages = {}

# --- load all pages ---
for fname in sorted(os.listdir(wiki_dir)):
    if not fname.endswith(".md") or fname in ("README.md", "index.md", "log.md"):
        continue
    path = os.path.join(wiki_dir, fname)
    with open(path) as f:
        content = f.read()
    slug = fname[:-3]
    pages[slug] = {"fname": fname, "path": path, "content": content}

# --- validate each page ---
for slug, page in pages.items():
    content = page["content"]
    fname = page["fname"]
    prefix = f"  {fname}"

    fm_match = re.match(r'^---\n(.*?)\n---', content, re.DOTALL)
    if not fm_match:
        errors.append(f"{prefix}: missing frontmatter")
        continue
    fm = fm_match.group(1)

    # required fields
    for field in REQUIRED_FIELDS:
        if not re.search(rf'^{field}:', fm, re.MULTILINE):
            errors.append(f"{prefix}: missing field '{field}'")

    # date validation
    for datefield in ("created", "updated"):
        m = re.search(rf'^{datefield}:\s*(.+)$', fm, re.MULTILINE)
        if m and not DATE_RE.match(m.group(1).strip()):
            errors.append(f"{prefix}: '{datefield}' is not a valid ISO date: {m.group(1).strip()}")

    # at least one source
    sources_m = re.search(r'^sources:\n((?:  -.+\n)+)', fm, re.MULTILINE)
    if not sources_m:
        errors.append(f"{prefix}: no sources listed")

    # related slugs resolve
    related_m = re.search(r'^related:\n((?:  -.+\n)+)', fm, re.MULTILINE)
    if related_m:
        for line in related_m.group(1).strip().splitlines():
            ref = line.strip().lstrip('- ').strip()
            if ref and ref not in pages:
                errors.append(f"{prefix}: related slug '{ref}' has no matching file")

    # status field validation
    status_m = re.search(r'^status:\s*(.+)$', fm, re.MULTILINE)
    if status_m:
        status_val = status_m.group(1).strip()
        if status_val not in VALID_STATUSES:
            errors.append(f"{prefix}: invalid status '{status_val}' — must be stub|draft|permanent")
        elif status_val == "stub":
            created_m = re.search(r'^created:\s*(.+)$', fm, re.MULTILINE)
            if created_m and DATE_RE.match(created_m.group(1).strip()):
                created_dt = date.fromisoformat(created_m.group(1).strip())
                age = (date.today() - created_dt).days
                if age > STUB_AGE_DAYS:
                    warnings.append(f"{prefix}: stub is {age} days old — consider fleshing out or promoting to draft")

    # mega-page warning
    line_count = content.count('\n')
    if line_count > 400:
        warnings.append(f"{prefix}: {line_count} lines — consider splitting")

# --- check index coverage ---
index_path = os.path.join(wiki_dir, "index.md")
if os.path.exists(index_path):
    with open(index_path) as f:
        index_content = f.read()
    for slug, page in pages.items():
        if page["fname"] not in index_content:
            warnings.append(f"  {page['fname']}: not listed in index.md — run reindex.py")
else:
    warnings.append("  index.md does not exist — run reindex.py")

# --- report ---
if errors:
    print(f"ERRORS ({len(errors)}):")
    for e in errors:
        print(f"  ✗ {e}")
if warnings:
    print(f"\nWARNINGS ({len(warnings)}):")
    for w in warnings:
        print(f"  ⚠ {w}")
if not errors and not warnings:
    print(f"✓ All {len(pages)} pages valid")
elif not errors:
    print(f"\n✓ No errors ({len(pages)} pages, {len(warnings)} warnings)")

sys.exit(1 if errors else 0)
