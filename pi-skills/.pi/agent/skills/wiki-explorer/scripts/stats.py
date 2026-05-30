import os
import re
from collections import Counter

IGNORE = {"index.md", "README.md", "log.md"}


def parse_frontmatter(content):
    """Parse YAML-ish frontmatter into a dict.

    Handles both inline values (tags: [a, b]) and multi-line block values
    (sources: / related: with '  - item' lines). The simple key:val split
    used previously silently dropped all multi-line values.
    """
    fm = {}
    match = re.match(r"^---\n(.*?)\n---", content, re.DOTALL)
    if not match:
        return fm
    raw = match.group(1)

    current_key = None
    current_list = None

    for line in raw.splitlines():
        # Top-level key
        kv = re.match(r"^(\w[\w-]*):\s*(.*)", line)
        if kv:
            key, val = kv.group(1), kv.group(2).strip()
            current_key = key
            if val.startswith("[") and val.endswith("]"):
                fm[key] = [t.strip().strip("\"'") for t in val[1:-1].split(",") if t.strip()]
                current_list = None
            elif val == "":
                fm[key] = []
                current_list = key
            else:
                fm[key] = val
                current_list = None
        # List item under the current key
        elif current_list and re.match(r"^\s+-\s+(.+)", line):
            item = re.match(r"^\s+-\s+(.+)", line).group(1).strip()
            fm[current_list].append(item)

    return fm


def analyze_wiki(wiki_path):
    stats = {
        "total_pages": 0,
        "tags": Counter(),
        "status": Counter(),
        "orphans": [],
        "missing_sources": [],
    }

    all_slugs = set()
    links_to = Counter()

    files = [f for f in os.listdir(wiki_path) if f.endswith(".md") and f not in IGNORE]

    for f in files:
        stats["total_pages"] += 1
        path = os.path.join(wiki_path, f)
        with open(path) as fh:
            content = fh.read()

        fm = parse_frontmatter(content)
        slug = fm.get("slug", f[:-3])
        if isinstance(slug, list):
            slug = slug[0]
        all_slugs.add(slug)

        # Tags
        tags = fm.get("tags", [])
        if isinstance(tags, str):
            tags = [t.strip() for t in tags.strip("[]").split(",") if t.strip()]
        for tag in tags:
            stats["tags"][tag] += 1

        # Status
        status = fm.get("status", "unknown")
        if isinstance(status, list):
            status = status[0]
        stats["status"][status] += 1

        # Sources — now correctly parsed as a list
        sources = fm.get("sources", [])
        if not sources:
            stats["missing_sources"].append(f)

        # Incoming links: related: block
        related = fm.get("related", [])
        if isinstance(related, str):
            related = [related]
        for rel in related:
            if rel:
                links_to[rel] += 1

        # Incoming links: body markdown links
        for link in re.findall(r"\[.*?\]\((.*?\.md)\)", content):
            links_to[link[:-3]] += 1

    for slug in all_slugs:
        if links_to[slug] == 0:
            stats["orphans"].append(slug)

    return stats


def generate_markdown(stats):
    total = stats["total_pages"]
    n_orphans = len(stats["orphans"])
    n_unsourced = len(stats["missing_sources"])
    n_stubs = stats["status"].get("stub", 0)
    n_draft = stats["status"].get("draft", 0)
    n_permanent = stats["status"].get("permanent", 0)

    md = "# Wiki Health Dashboard\n\n"
    md += "An overview of your personal knowledge base.\n\n"

    md += '<div class="grid">\n'
    md += f'  <div class="card"><span class="badge">Volume</span><h3>{total} Pages</h3><p>Total atomic concepts captured.</p></div>\n'
    md += f'  <div class="card"><span class="badge">Health</span><h3>{n_orphans} Orphans</h3><p>Pages with no incoming links.</p></div>\n'
    md += f'  <div class="card"><span class="badge">Quality</span><h3>{n_unsourced} Un-sourced</h3><p>Pages missing mandatory sources.</p></div>\n'
    md += f'  <div class="card"><span class="badge">Maturity</span><h3>{n_permanent} Permanent</h3><p>{n_draft} draft · {n_stubs} stub</p></div>\n'
    md += '</div>\n\n'

    md += "## Top Tags\n\n"
    md += '<div class="grid">\n'
    for tag, count in stats["tags"].most_common(6):
        md += f'  <div class="card"><h3>{tag}</h3><span class="badge">{count} pages</span></div>\n'
    md += '</div>\n\n'

    if n_stubs:
        md += "## Stubs (need work)\n\n"
        md += "These pages were scaffolded but not yet fleshed out.\n\n"
        md += "<ul>"
        for slug in sorted(stats.get("stubs", [])):
            md += f"<li>{slug}</li>"
        md += "</ul>\n\n"

    if stats["orphans"]:
        md += "## Orphaned Pages\n\n"
        md += "These pages have no incoming links. Consider wiring them up.\n\n"
        md += "<ul>"
        for o in stats["orphans"][:15]:
            md += f"<li>{o}</li>"
        md += "</ul>\n\n"

    if stats["missing_sources"]:
        md += "## Un-sourced Pages\n\n"
        md += "<ul>"
        for s in stats["missing_sources"][:10]:
            md += f"<li>{s}</li>"
        md += "</ul>\n\n"

    return md


if __name__ == "__main__":
    import sys
    path = sys.argv[1] if len(sys.argv) > 1 else os.path.expanduser("~/org/wiki")
    stats = analyze_wiki(path)
    print(generate_markdown(stats))
