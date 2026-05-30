---
name: wiki-explorer
description: Explore, visualize, and maintain your personal wiki. Use when the user wants a "health check", a visual graph of connections, a random spark of discovery, or a browseable "Atlas" of their entire knowledge base.
---

# Wiki Explorer

Discover the "shape" of your knowledge. This skill provides tools to move beyond searching for individual pages and into exploring the broader connections and health of your wiki.

## Core Workflows

## Output Directory

All generated files (intermediate `.md` and final `.html`) go to **`~/org/wiki/dist/`**. Create it if it doesn't exist:

```bash
mkdir -p ~/org/wiki/dist
```

Never write wiki explorer output to the current working directory or `/tmp`.

---

### 1. The Wiki Atlas (Browseable Wiki)
Generate a single, browseable HTML artifact containing your entire wiki.
- **Action**: Run `scripts/atlas.py`.
- **Visualize**: Pipe to `visualizer`. This creates an interactive "app" where you can search and jump between all your notes.
- **Example**:
```bash
python3 scripts/atlas.py ~/org/wiki > ~/org/wiki/dist/atlas.md && \
python3 ../visualizer/scripts/render.py ~/org/wiki/dist/atlas.md \
  --template ../visualizer/assets/base.html \
  --output ~/org/wiki/dist/wiki-atlas.html \
  --title "Wiki Atlas"
```

### 2. The Health Dashboard
Crawl the wiki to find orphaned pages, missing sources, note maturity breakdown (stub/draft/permanent), and top tags.
- **Action**: Run `scripts/stats.py`.
- **Visualize**: Pipe to `visualizer` to generate a "paper-like" dashboard.
- **Output**: `~/org/wiki/dist/wiki-health.html`
- **Note**: For actionable orphan and unlinked-mention details, also run `backlinks.py` from the `wiki-builder` skill — it gives per-page suggestions rather than just counts.

### 3. Knowledge Graph
Visualize how concepts connect using Mermaid diagrams.
- **Action**: Run `scripts/graph.py`.
- **Visualize**: Pipe to `visualizer`. The graph will be **zoomable and pannable** within the artifact.
- **Output**: `~/org/wiki/dist/wiki-graph.html`

### 4. Serendipitous Discovery
Surface a random page to spark new ideas.
- **Action**: Run `scripts/random_page.py`.

## Spatial Exploration Patterns

- **Atlas Navigation**: The `visualizer` sidebar becomes a search-enabled table of contents for your entire wiki.
- **Interactive Graphs**: Knowledge maps are zoomable. Use the "Reset" button to fit the map to the screen.
- **Deep Linking**: In the Atlas, internal wiki links are automatically converted to jump-links between sections.

## Resources

- `scripts/atlas.py`: Compiles the whole wiki into one browseable document.
- `scripts/stats.py`: Analyzes wiki volume, tags, orphans, and note maturity (stub/draft/permanent).
- `scripts/graph.py`: Generates zoomable D3 force-directed link maps.
- `scripts/random_page.py`: Picks a random concept.

## Integration with wiki-builder

`wiki-explorer` is read-only — it visualizes and reports on the wiki but never writes to it. For write operations, use the `wiki-builder` skill:

| Task | Tool |
|---|---|
| Fix orphans, add cross-links | `wiki-builder` → `backlinks.py --orphans` |
| Find unlinked mentions | `wiki-builder` → `backlinks.py --unlinked` |
| Promote stubs to draft/permanent | `wiki-builder` → edit `status:` field |
| Append session to log | `wiki-builder` → `log-session.py` |

All scripts in this skill ignore `log.md`, `index.md`, and `README.md` — these are infrastructure files, not wiki pages.
