import os
import re
import sys
import json

IGNORE = {"index.md", "README.md", "log.md"}


def get_frontmatter(content):
    """Parse frontmatter, correctly handling multi-line block values."""
    fm = {}
    match = re.match(r"^---\n(.*?)\n---", content, re.DOTALL)
    if not match:
        return fm
    raw = match.group(1)
    current_key = None
    current_list = None
    for line in raw.splitlines():
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
        elif current_list and re.match(r"^\s+-\s+(.+)", line):
            fm[current_list].append(re.match(r"^\s+-\s+(.+)", line).group(1).strip())
    return fm

def build_graph(wiki_path):
    edges = []
    nodes = {}

    files = [f for f in os.listdir(wiki_path) if f.endswith(".md") and f not in IGNORE]

    for f in files:
        path = os.path.join(wiki_path, f)
        with open(path, "r") as file:
            content = file.read()
            fm = get_frontmatter(content)

            slug = fm.get("slug", f.replace(".md", ""))
            if isinstance(slug, list): slug = slug[0]
            title = fm.get("title", slug)
            if isinstance(title, list): title = title[0]

            tags = fm.get("tags", [])
            if isinstance(tags, str):
                tags = [t.strip() for t in tags.strip("[]").split(",") if t.strip()]
            description = fm.get("description", "")
            if isinstance(description, list): description = description[0]

            nodes[slug] = {"id": slug, "title": title, "tags": tags, "description": description}

            # Links in body
            links = re.findall(r"\[.*?\]\((.*?\.md)\)", content)
            for link in links:
                target_slug = link.replace(".md", "")
                edges.append((slug, target_slug))

            # Links in related frontmatter
            related = fm.get("related", [])
            if isinstance(related, str):
                related = [r.strip() for r in related.split(",") if r.strip()]
            for rel in related:
                if rel:
                    edges.append((slug, rel))

    # Deduplicate edges, keep only those where both ends exist
    seen = set()
    clean_edges = []
    for s, t in edges:
        if s in nodes and t in nodes and (s, t) not in seen and s != t:
            seen.add((s, t))
            clean_edges.append({"source": s, "target": t})

    return list(nodes.values()), clean_edges


def generate_html(nodes, edges, title="Wiki Knowledge Graph"):
    graph_json = json.dumps({"nodes": nodes, "edges": edges}, indent=2)

    # Tag → color cluster mapping (first matching tag wins)
    tag_clusters = [
        ("ai-coding",   "#D97757"),  # clay/orange
        ("agents",      "#D97757"),
        ("security",    "#C0392B"),  # red
        ("hacking",     "#C0392B"),
        ("networking",  "#16A085"),  # teal
        ("homelab",     "#16A085"),
        ("linux",       "#2980B9"),  # blue
        ("hardware",    "#2980B9"),
        ("thinkpad",    "#2980B9"),
        ("pi",          "#27AE60"),  # green
        ("tooling",     "#27AE60"),
        ("career",      "#8E44AD"),  # purple
        ("job-search",  "#8E44AD"),
        ("llm",         "#E67E22"),  # amber
        ("workflow",    "#E67E22"),
    ]

    return f"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>{title}</title>
<script src="https://cdn.jsdelivr.net/npm/d3@7/dist/d3.min.js"></script>
<style>
  :root {{
    --bg: #F5F0E8;
    --paper: #FAFAF7;
    --border: #E2DDD4;
    --text: #2C2824;
    --g400: #A09890;
    --g500: #7A706A;
    --g700: #4A403A;
    --clay: #D97757;
    --slate: #5B7FA6;
    --shadow: 0 8px 32px rgba(0,0,0,0.08);
    --sans: "Inter", system-ui, sans-serif;
    --mono: "JetBrains Mono", "Fira Code", monospace;
  }}
  [data-theme="dark"] {{
    --bg: #1A1714;
    --paper: #221F1C;
    --border: #3A3530;
    --text: #E8E0D8;
    --g400: #6A6058;
    --g500: #8A8078;
    --g700: #C8C0B8;
  }}
  * {{ box-sizing: border-box; margin: 0; padding: 0; }}
  body {{
    font-family: var(--sans);
    background: var(--bg);
    color: var(--text);
    display: flex;
    flex-direction: column;
    height: 100vh;
    overflow: hidden;
  }}
  header {{
    display: flex;
    align-items: center;
    gap: 16px;
    padding: 14px 24px;
    border-bottom: 1.5px solid var(--border);
    background: var(--paper);
    flex-shrink: 0;
    z-index: 10;
  }}
  .gallery-link {{
    font-family: var(--mono);
    font-size: 11px;
    text-transform: uppercase;
    letter-spacing: 0.05em;
    color: var(--clay);
    text-decoration: none;
    font-weight: 600;
    flex-shrink: 0;
  }}
  .gallery-link:hover {{ opacity: 0.75; }}
  .gallery-link::before {{ content: "← "; }}
  header h1 {{
    font-size: 15px;
    font-weight: 600;
    letter-spacing: -0.01em;
    color: var(--text);
  }}
  header .subtitle {{
    font-size: 12px;
    color: var(--g500);
    font-family: var(--mono);
  }}
  .controls {{
    margin-left: auto;
    display: flex;
    align-items: center;
    gap: 10px;
  }}
  #search {{
    padding: 6px 12px;
    border: 1.5px solid var(--border);
    border-radius: 8px;
    background: var(--bg);
    color: var(--text);
    font-family: var(--sans);
    font-size: 13px;
    outline: none;
    width: 200px;
    transition: border-color 150ms;
  }}
  #search:focus {{ border-color: var(--clay); }}
  .btn {{
    padding: 6px 12px;
    border: 1.5px solid var(--border);
    border-radius: 8px;
    background: var(--paper);
    color: var(--g500);
    font-size: 12px;
    font-family: var(--mono);
    cursor: pointer;
    transition: all 150ms;
  }}
  .btn:hover {{ border-color: var(--clay); color: var(--clay); }}
  #canvas {{
    flex: 1;
    overflow: hidden;
    cursor: grab;
  }}
  #canvas:active {{ cursor: grabbing; }}
  svg {{ width: 100%; height: 100%; }}
  .node circle {{
    stroke-width: 1.5px;
    transition: opacity 200ms;
    cursor: pointer;
  }}
  .node text {{
    font-family: var(--sans);
    font-size: 11px;
    fill: var(--text);
    pointer-events: none;
    transition: opacity 200ms;
  }}
  .link {{
    stroke: var(--border);
    stroke-width: 1px;
    transition: opacity 200ms;
  }}
  .dimmed {{ opacity: 0.08 !important; }}
  .highlighted circle {{ stroke-width: 2.5px !important; filter: drop-shadow(0 0 4px currentColor); }}
  #tooltip {{
    position: fixed;
    background: var(--paper);
    border: 1.5px solid var(--border);
    border-radius: 10px;
    padding: 12px 16px;
    font-size: 13px;
    pointer-events: none;
    opacity: 0;
    transition: opacity 150ms;
    max-width: 280px;
    box-shadow: var(--shadow);
    z-index: 100;
  }}
  #tooltip.visible {{ opacity: 1; }}
  #tooltip .t-title {{ font-weight: 600; margin-bottom: 6px; color: var(--text); }}
  #tooltip .t-desc {{ font-size: 12px; color: var(--g500); line-height: 1.5; margin-bottom: 8px; }}
  #tooltip .t-tags {{ display: flex; flex-wrap: wrap; gap: 4px; }}
  #tooltip .t-tag {{
    font-family: var(--mono);
    font-size: 10px;
    padding: 2px 7px;
    border-radius: 4px;
    background: var(--bg);
    border: 1px solid var(--border);
    color: var(--g500);
  }}
  #info {{
    position: fixed;
    bottom: 20px;
    left: 50%;
    transform: translateX(-50%);
    font-size: 11px;
    font-family: var(--mono);
    color: var(--g400);
    pointer-events: none;
  }}
  .legend {{
    position: fixed;
    bottom: 20px;
    right: 20px;
    background: var(--paper);
    border: 1.5px solid var(--border);
    border-radius: 10px;
    padding: 12px 14px;
    font-size: 11px;
    font-family: var(--mono);
    box-shadow: var(--shadow);
  }}
  .legend-title {{ color: var(--g500); margin-bottom: 8px; text-transform: uppercase; letter-spacing: 0.08em; font-size: 10px; }}
  .legend-item {{ display: flex; align-items: center; gap: 8px; margin-bottom: 4px; color: var(--g700); }}
  .legend-dot {{ width: 10px; height: 10px; border-radius: 50%; flex-shrink: 0; }}
</style>
</head>
<body>
<header>
  <a class="gallery-link" href="index.html">Gallery</a>
  <div>
    <h1>{title}</h1>
    <div class="subtitle" id="stats"></div>
  </div>
  <div class="controls">
    <input type="text" id="search" placeholder="Search nodes…">
    <button class="btn" id="reset-btn">Reset</button>
    <button class="btn" id="theme-btn">Dark</button>
  </div>
</header>
<div id="canvas"></div>
<div id="tooltip"></div>
<div id="info">Drag · Scroll to zoom · Click to highlight · Double-click to open in Atlas · Esc to clear</div>

<div class="legend">
  <div class="legend-title">Clusters</div>
  <div class="legend-item"><div class="legend-dot" style="background:#D97757"></div>AI / Agents</div>
  <div class="legend-item"><div class="legend-dot" style="background:#C0392B"></div>Security</div>
  <div class="legend-item"><div class="legend-dot" style="background:#16A085"></div>Networking / Homelab</div>
  <div class="legend-item"><div class="legend-dot" style="background:#2980B9"></div>Linux / Hardware</div>
  <div class="legend-item"><div class="legend-dot" style="background:#27AE60"></div>Pi / Tooling</div>
  <div class="legend-item"><div class="legend-dot" style="background:#8E44AD"></div>Career</div>
  <div class="legend-item"><div class="legend-dot" style="background:#E67E22"></div>LLM / Workflow</div>
  <div class="legend-item"><div class="legend-dot" style="background:#A09890"></div>Other</div>
</div>

<script>
const DATA = {graph_json};

const TAG_COLORS = {json.dumps(dict(tag_clusters))};
const DEFAULT_COLOR = "#A09890";

function nodeColor(node) {{
  for (const tag of (node.tags || [])) {{
    if (TAG_COLORS[tag]) return TAG_COLORS[tag];
  }}
  return DEFAULT_COLOR;
}}

function nodeDegree(id, edges) {{
  return edges.filter(e => e.source === id || e.target === id || e.source?.id === id || e.target?.id === id).length;
}}

const nodes = DATA.nodes.map(n => ({{ ...n }}));
const edges = DATA.edges.map(e => ({{ ...e }}));

document.getElementById("stats").textContent =
  `${{nodes.length}} nodes · ${{edges.length}} edges`;

// Degree map for sizing
const degreeMap = {{}};
nodes.forEach(n => {{ degreeMap[n.id] = 0; }});
edges.forEach(e => {{
  degreeMap[e.source] = (degreeMap[e.source] || 0) + 1;
  degreeMap[e.target] = (degreeMap[e.target] || 0) + 1;
}});

const minDeg = Math.min(...Object.values(degreeMap));
const maxDeg = Math.max(...Object.values(degreeMap));
const nodeRadius = d => {{
  const t = maxDeg === minDeg ? 0.5 : (degreeMap[d.id] - minDeg) / (maxDeg - minDeg);
  return 5 + t * 14;
}};

// SVG setup
const container = document.getElementById("canvas");
const svg = d3.select(container).append("svg");
const g = svg.append("g");

const zoom = d3.zoom()
  .scaleExtent([0.1, 4])
  .on("zoom", e => g.attr("transform", e.transform));
svg.call(zoom);

function resetZoom() {{
  svg.transition().duration(400).call(zoom.transform, d3.zoomIdentity);
}}

// Force simulation
const sim = d3.forceSimulation(nodes)
  .force("link", d3.forceLink(edges).id(d => d.id).distance(80).strength(0.4))
  .force("charge", d3.forceManyBody().strength(-220))
  .force("center", d3.forceCenter(container.offsetWidth / 2, container.offsetHeight / 2))
  .force("collision", d3.forceCollide().radius(d => nodeRadius(d) + 6));

// Draw edges
const link = g.append("g").selectAll("line")
  .data(edges).join("line").attr("class", "link");

// Draw nodes
const node = g.append("g").selectAll("g")
  .data(nodes).join("g").attr("class", "node");

node.append("circle")
  .attr("r", nodeRadius)
  .attr("fill", d => nodeColor(d) + "33")
  .attr("stroke", d => nodeColor(d));

node.append("text")
  .attr("dy", d => nodeRadius(d) + 12)
  .attr("text-anchor", "middle")
  .text(d => d.title.length > 28 ? d.title.slice(0, 26) + "…" : d.title);

// Drag
node.call(d3.drag()
  .on("start", (e, d) => {{ if (!e.active) sim.alphaTarget(0.3).restart(); d.fx = d.x; d.fy = d.y; }})
  .on("drag",  (e, d) => {{ d.fx = e.x; d.fy = e.y; }})
  .on("end",   (e, d) => {{ if (!e.active) sim.alphaTarget(0); d.fx = null; d.fy = null; }}));

// Tick
sim.on("tick", () => {{
  link.attr("x1", d => d.source.x).attr("y1", d => d.source.y)
      .attr("x2", d => d.target.x).attr("y2", d => d.target.y);
  node.attr("transform", d => `translate(${{d.x}},${{d.y}})`);
}});

// Tooltip
const tooltip = document.getElementById("tooltip");
let hoveredNode = null;

node.on("mouseover", (e, d) => {{
  hoveredNode = d;
  tooltip.innerHTML = `
    <div class="t-title">${{d.title}}</div>
    ${{d.description ? `<div class="t-desc">${{d.description.slice(0, 120)}}${{d.description.length > 120 ? "…" : ""}}</div>` : ""}}
    <div class="t-tags">${{(d.tags || []).map(t => `<span class="t-tag">${{t}}</span>`).join("")}}</div>
  `;
  tooltip.classList.add("visible");
}})
.on("mousemove", e => {{
  const x = e.clientX + 14, y = e.clientY - 10;
  tooltip.style.left = (x + 280 > window.innerWidth ? e.clientX - 294 : x) + "px";
  tooltip.style.top = y + "px";
}})
.on("mouseout", () => {{
  hoveredNode = null;
  tooltip.classList.remove("visible");
}});

// Click to highlight neighbors
let activeNode = null;

function clearHighlight() {{
  activeNode = null;
  node.classed("dimmed", false).classed("highlighted", false);
  link.classed("dimmed", false);
}}

node.on("click", (e, d) => {{
  e.stopPropagation();
  if (e.metaKey || e.ctrlKey) {{
    // Cmd/Ctrl+click → open in Atlas
    window.open(`atlas.html#${{d.id}}`, "_blank");
    return;
  }}
  if (activeNode === d.id) {{ clearHighlight(); return; }}
  activeNode = d.id;

  const neighborIds = new Set([d.id]);
  edges.forEach(e => {{
    if (e.source.id === d.id) neighborIds.add(e.target.id);
    if (e.target.id === d.id) neighborIds.add(e.source.id);
  }});

  node.classed("dimmed", n => !neighborIds.has(n.id))
      .classed("highlighted", n => n.id === d.id);
  link.classed("dimmed", l => l.source.id !== d.id && l.target.id !== d.id);
}});

// Double-click → open in Atlas
node.on("dblclick", (e, d) => {{
  e.stopPropagation();
  window.open(`atlas.html#${{d.id}}`, "_blank");
}});

svg.on("click", clearHighlight);

document.addEventListener("keydown", e => {{ if (e.key === "Escape") clearHighlight(); }});

// Search
document.getElementById("search").addEventListener("input", function() {{
  const term = this.value.toLowerCase().trim();
  if (!term) {{ clearHighlight(); return; }}

  const matchIds = new Set(nodes.filter(n =>
    n.title.toLowerCase().includes(term) ||
    (n.tags || []).some(t => t.includes(term))
  ).map(n => n.id));

  node.classed("dimmed", n => !matchIds.has(n.id))
      .classed("highlighted", n => matchIds.has(n.id));
  link.classed("dimmed", true);
}});

document.getElementById("reset-btn").addEventListener("click", () => {{
  document.getElementById("search").value = "";
  clearHighlight();
  resetZoom();
}});

// Dark mode
let dark = false;
document.getElementById("theme-btn").addEventListener("click", function() {{
  dark = !dark;
  document.documentElement.setAttribute("data-theme", dark ? "dark" : "");
  this.textContent = dark ? "Light" : "Dark";
}});

// Responsive resize
new ResizeObserver(() => {{
  sim.force("center", d3.forceCenter(container.offsetWidth / 2, container.offsetHeight / 2));
  sim.alpha(0.1).restart();
}}).observe(container);
</script>
</body>
</html>"""


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("wiki_path", nargs="?", default=os.path.expanduser("~/org/wiki"))
    parser.add_argument("--output", "-o", default=None, help="Output HTML file (default: stdout as markdown)")
    args = parser.parse_args()

    nodes, edges = build_graph(args.wiki_path)

    if args.output:
        html = generate_html(nodes, edges)
        os.makedirs(os.path.dirname(args.output) or ".", exist_ok=True)
        with open(args.output, "w") as f:
            f.write(html)
        print(f"✓ Rendered: {args.output}")
    else:
        # Legacy: print markdown stub (so existing pipelines don't break)
        print("# Wiki Knowledge Graph\n")
        print("A visualization of how your atomic concepts connect.\n")
        print(f"> Run `graph.py --output graph.html` for the interactive D3 version.\n")
        print(f"**{len(nodes)} nodes · {len(edges)} edges**")
