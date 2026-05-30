---
name: visualizer
description: Transform Markdown documents into interactive, 'paper-like' HTML artifacts using purpose-built templates. Features auto-detection of document type, zoomable diagrams, searchable sidebars, dark mode, and an automatic artifact gallery.
---

# Visualizer

Transform flat Markdown into **purpose-built interactive HTML artifacts** — each one trading a document you'd skim for one you'd actually read.

This skill implements the ["HTML Effectiveness"](https://thariqs.github.io/html-effectiveness/) philosophy: give the agent a real pen (HTML/CSS/JS) instead of a single template, and let it pick the right layout for the kind of thinking the document requires.

## Philosophy

**Don't just dump text into HTML. Map the conceptual shape of the data to a spatial layout.**

A comparison document should let you scan options side-by-side. An incident report should show you the timeline at a glance. A plan should surface risks before you read the code. A kanban board should make priorities visible without scrolling.

The skill auto-detects what kind of document you have and picks the right template. You can override with `--type`.

## Template Catalog

### When to use which template

| Template | Use for | Key features | Auto-detect triggers |
|----------|---------|--------------|---------------------|
| **generic** | Universal fallback, any document | Sidebar TOC, progress bar, dark mode, scroll reveal, search highlighting, sortable tables, keyboard shortcuts, print styles | _Default when nothing else matches_ |
| **explainer** | Research digests, tech catchups, learning guides, feature deep-dives | "On this page" sidebar, TL;DR panel, collapsible steps, gotchas callout, FAQ, code copy, file-tag strip | TL;DR, "how X works", FAQ, gotchas, step numbers, catch-up/digest |
| **comparison** | Job searches, options analysis, approach comparisons | Side-by-side cards, tier filtering, sort by rank/name, score badges, recommendation panel | vs./versus, comparison, trade-off, side-by-side, job search, option N |
| **plan** | Implementation plans, roadmaps, project plans | Timeline slices with dates, mockup grid, code blocks with file labels, risk table with severity badges, open decisions | Week/Sprint/Phase N, implementation plan, risks & mitigations, decision needed, feature flag |
| **report** | Status reports, incident post-mortems, weekly updates | TL;DR panel, shipped table with risk dots, metric cards, incident timeline with color-coded events, action items, carryover grid | Status report, shipped, highlights, velocity, SEV-N, incident, post-mortem, root cause |
| **deck** | Slide presentations, meeting decks | Arrow-key navigation, touch swipe, progress dots, slide counter, full-screen sections, print as handouts | Slide, presentation, deck, keynote, talk |
| **kanban** | Triage boards, task prioritization, backlog grooming | 4-column layout (Now/Next/Later/Cut), drag-and-drop cards, priority badges (S/M/L), point estimates per column, assignee tags | Now/Next/Later headings, triage, kanban, backlog, board |

## Component Library

All templates support these embedded HTML components in your Markdown:

### Layout Components

| Component | Markdown Usage | Effect |
|-----------|---------------|--------|
| **Grid** | `<div class="grid">` or `<div class="card-grid">` | Multi-column responsive grid |
| **Card** | `<div class="card"><h3>Title</h3><p>Desc</p></div>` | Hover-elevated content card |
| **Focus Pane** | `<div class="pane">...</div>` | Highlighted box with strong border and shadow |
| **Accordion** | `<details><summary>Title</summary>Content</details>` | Collapsible section (enhanced in generic template) |

### Status & Labels

| Component | Markdown Usage | Effect |
|-----------|---------------|--------|
| **Badge** | `<span class="badge">Label</span>` | Scannable status tag |
| **Clay Badge** | `<span class="badge clay">Hot</span>` | Highlighted badge (clay color) |
| **Olive Badge** | `<span class="badge olive">Done</span>` | Success badge (olive color) |

### Action Components

| Component | Markdown Usage | Effect |
|-----------|---------------|--------|
| **Action Bridge** | `<div class="action-bridge"><span>Task</span><button class="btn-action" onclick="copyToClipboard('cmd', this)">Copy</button></div>` | Actionable row with copy button |
| **Copy Button** | _Auto-added to all `<pre>` blocks_ | Hover-reveal copy button on code blocks |

### Diagrams

| Component | Markdown Usage | Effect |
|-----------|---------------|--------|
| **Mermaid Diagram** | ` ```mermaid\ngraph TD; ...\n``` ` | Zoomable/pannable diagram with Reset and Fit buttons |

### Data Components

| Component | Markdown Usage | Effect |
|-----------|---------------|--------|
| **Sortable Table** | Any `<table>` | Click column headers to sort (generic template) |
| **Stat Cards** | `<div class="stat-grid"><div class="stat-card"><span class="stat-value">94%</span><span class="stat-label">Coverage</span></div></div>` | KPI/metric display (report template) |
| **Timeline** | `<div class="timeline"><div class="tl-event"><span class="tl-date">14:02</span>Event</div></div>` | Chronological event display (report template) |
| **Risk Table** | `<table class="risk-table">` with `<span class="risk-badge high">HIGH</span>` | Color-coded risk items (plan template) |

## Workflow

### 1. Determine the document type

The skill auto-detects based on content keywords. You can also explicitly choose:

```bash
# Auto-detect (recommended — the script is good at this)
python3 scripts/render.py doc.md --title "Title" --output dist/doc.html

# Explicit type
python3 scripts/render.py doc.md --title "Title" --output dist/doc.html --type explainer

# Use a specific template file directly (backward compat)
python3 scripts/render.py doc.md --template assets/base.html --output dist/doc.html --title "Title"
```

### 2. Author the Markdown with components

Embed HTML components where they add value. Dense data gets grids and cards. Processes get timelines. Comparisons get side-by-side cards. Plain prose sections stay as Markdown.

### 3. Render

Run the render script with the appropriate flags:

| Flag | Purpose | Example |
|------|---------|---------|
| `--output` | Output HTML path (required) | `--output dist/prd.html` |
| `--title` | Document title (required) | `--title "Project X PRD"` |
| `--type` | Template type | `--type plan` |
| `--subtitle` | Subtitle/context label | `--subtitle "Implementation Plan"` |
| `--description` | Gallery description | `--description "Q2 roadmap"` |
| `--important` | Feature in gallery | `--important` |

### 4. Deliver

Give the user the output path and mention the Gallery (`index.html` in the output directory).

## Dynamic Features (all templates)

All templates include these dynamic features:

- **Dark/Light theme toggle** — Manual override with localStorage persistence (keyboard: `t`)
- **Responsive design** — Mobile-friendly layouts
- **Print styles** — Clean paper output
- **Keyboard shortcuts** — `t` for theme, `/` for search (where applicable)
- **Progress bar** — Reading progress indicator (generic, explainer, plan)
- **Gallery link** — "← Gallery" back to artifact index

## Auto-Detection Logic

The render script scans your Markdown for keyword patterns and scores each template type. The highest-scoring type wins. When in doubt, it falls back to `generic`.

**Tips for reliable auto-detection:**
- Include clear section headings that match your document's purpose
- Use words like "comparison", "vs", "TL;DR", "Week 1", "incident", "Now/Next/Later" naturally
- For explainers, include a TL;DR at the top
- For plans, use "Week N · Day" format in headings
- For reports, use "Shipped", "Highlights", or "Carryover" section headings
- For kanban, use "## Now", "## Next", "## Later", "## Cut" headings

## The Artifact Gallery

Every output directory gets an `index.html` (Gallery) automatically maintained by the skill.

- **Back to Gallery**: Every artifact includes a link back to the index
- **Featured Artifacts**: Use `--important` to pin high-value artifacts to the "Featured" section
- **Type Filtering**: Filter by template type (Explainer, Comparison, Plan, etc.)
- **Search**: Search across all artifacts by title or description
- **Sort**: By date (newest/oldest) or alphabetically
- **View Modes**: Grid or list view
- **Dark Mode**: Toggle persisted to localStorage
- **Type Badges**: Each card shows its template type

## Resources

- `templates/` — Purpose-built HTML templates (7 types)
- `assets/gallery.html` — Gallery index template
- `assets/base.html` — Legacy base template (still works, now replaced by `templates/generic.html`)
- `scripts/render.py` — Markdown → HTML renderer with auto-detection + Gallery updater
- `scripts/manager.py` — Manages the artifact manifest and index
