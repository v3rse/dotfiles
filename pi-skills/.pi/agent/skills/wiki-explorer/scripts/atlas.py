import os
import re
import sys

def get_frontmatter(content):
    match = re.search(r"^---(.*?)---", content, re.DOTALL | re.MULTILINE)
    fm = {}
    if match:
        lines = match.group(1).strip().split("\n")
        for line in lines:
            if ":" in line:
                key, val = line.split(":", 1)
                key = key.strip()
                val = val.strip()
                fm[key] = val
    return fm

def build_atlas(wiki_path):
    IGNORE = {"index.md", "README.md", "log.md"}
    files = sorted([f for f in os.listdir(wiki_path) if f.endswith(".md") and f not in IGNORE])
    
    atlas_md = "# Wiki Atlas\n\n"
    atlas_md += "A complete browseable map of your atomic concepts. Use the sidebar to navigate or search.\n\n"
    
    for f in files:
        path = os.path.join(wiki_path, f)
        with open(path, "r") as file:
            raw_content = file.read()
            fm = get_frontmatter(raw_content)
            
            # Strip frontmatter for the atlas body
            body = re.sub(r"^---.*?---", "", raw_content, flags=re.DOTALL | re.MULTILINE).strip()
            
            title = fm.get("title", f.replace(".md", "").replace("-", " ").title())
            slug = fm.get("slug", f.replace(".md", ""))
            
            atlas_md += f'<a id="{slug}"></a>\n\n## {title}\n\n'
            
            # Add metadata badges
            tags = fm.get("tags", "")
            if tags:
                # Handle [tag1, tag2] format or raw string
                tags_list = tags.strip("[]").split(",")
                for t in tags_list:
                    t = t.strip().strip("\"").strip("'")
                    if t:
                        atlas_md += f'<span class="badge">{t}</span> '
                atlas_md += "\n\n"
            
            # Demote headings inside the page body so the sidebar stays clean:
            # H1 (page title) → removed (redundant with the ## section header above)
            # H2 (Summary, Details, etc.) → H3 (nested under the page in the sidebar)
            # H3 (subsections) → H4 (content only, not shown in sidebar)
            # H4+ → H5+ (keep relative depth)
            def demote_headings(text):
                lines = text.split("\n")
                out = []
                for line in lines:
                    m = re.match(r'^(#{1,6})(\s+.*)', line)
                    if m:
                        level = len(m.group(1))
                        rest = m.group(2)
                        if level == 1:
                            continue  # drop the H1 page title — already shown as ## above
                        else:
                            out.append('#' * (level + 1) + rest)
                    else:
                        out.append(line)
                return "\n".join(out)
            body = demote_headings(body)

            # Convert internal links to section links
            # [Link](target.md) -> [Link](#target)
            body = re.sub(r"\[(.*?)\]\((.*?)\.md\)", r"[\1](#\2)", body)
            
            atlas_md += body + "\n\n---\n\n"
            
    return atlas_md

if __name__ == "__main__":
    path = sys.argv[1] if len(sys.argv) > 1 else os.path.expanduser("~/org/wiki")
    print(build_atlas(path))
