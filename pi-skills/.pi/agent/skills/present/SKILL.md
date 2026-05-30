---
name: present
description: "Render agent findings, research, analysis, or plans as a rich self-contained HTML document using the house design system. Trigger: /present invocation, or when the user says 'visualise', 'make an HTML report', 'show this as HTML', 'present your findings'. Output always goes to ~/notes/<slug>.html."
argument-hint: "[slug-or-title]  e.g. /present dmarc-migration"
allowed-tools: Write Bash(mkdir -p ~/notes) Bash(date *) Read
---

# Present — HTML Findings Renderer

Turn the current conversation's findings into a navigable, self-contained HTML page and save it to `~/notes/<slug>.html`. Open it in a browser — no build step.

## Output rules

1. **Always write to `~/notes/<slug>.html`** (run `mkdir -p ~/notes` first).
2. Derive slug from `$ARGUMENTS` if given; otherwise infer from the topic (kebab-case, max 5 words).
3. Return only the full file path when done. No other prose.
4. The file must be completely self-contained — no external CSS, no external JS, no web fonts.

---

## Five output modes — pick one per document

| Mode | Use when | Key components |
|---|---|---|
| **Exploration & Planning** | Options, trade-offs, migration plans, approach comparisons | Concept cards, comparison grid, options cards, flow diagram, level blocks, recommendation |
| **Deck** | Presenting findings to a room, Friday demos, status slides | Slide shell + arrow-key JS, progress bars, sparkline SVG, inverted decision slide |
| **Research & Learning** | Explaining how a system works, feature deep-dives | Side nav, TL;DR (left-border), collapsible steps, tabs, callout, FAQ |
| **Code Review** | PR reviews, module maps, annotated diffs | Risk-map chips, diff block, bubble comments, file cards, SVG module diagram |
| **Report** | Weekly status, incident post-mortems | Stat cards + delta, SVG bar chart, incident timeline, action items checklist |

---

## Design system — shared across all modes

```css
:root {
  --ivory:  #FAF9F5;   /* page background */
  --paper:  #FFFFFF;   /* card/surface background */
  --slate:  #141413;   /* primary text, dark surfaces */
  --clay:   #D97757;   /* accent / highlight / attention */
  --clay-d: #B85C3E;   /* darker clay for hover/borders */
  --oat:    #E3DACC;   /* warm beige, hover tints */
  --olive:  #788C5D;   /* green / pass / success */
  --rust:   #B04A3F;   /* red / fail / high risk */
  --g100:   #F0EEE6;   /* lightest gray surface */
  --g200:   #E6E3DA;
  --g300:   #D1CFC5;   /* borders */
  --g500:   #87867F;   /* secondary text, muted labels */
  --g700:   #3D3D3A;   /* body text */
  --serif:  ui-serif, Georgia, "Times New Roman", Times, serif;
  --sans:   system-ui, -apple-system, "Segoe UI", Roboto, Helvetica, Arial, sans-serif;
  --mono:   ui-monospace, "SF Mono", Menlo, Monaco, Consolas, monospace;
}
```

**Typography**
- `h1`: serif, 500, `clamp(34px,4.8vw,56px)`, `letter-spacing:-0.018em`, italic `em` in clay
- `h2`: serif, 500, 22–27px, `letter-spacing:-0.012em`
- Card title: serif, 500, 18–20px
- Body: sans, 14–15px, `color:var(--g700)`, `line-height:1.6`
- Labels/eyebrows: mono, 10–12px, `letter-spacing:0.08–0.12em`, `text-transform:uppercase`, `color:var(--g500)`
- Code: mono, 11.5–13px, `background:var(--g100)`, `border:1px solid var(--g200)`, `padding:1px 5px`, `border-radius:4px`

---

## Mode 1 — Exploration & Planning

### Masthead

```html
<header class="masthead">
  <!-- eyebrow: mono 12px uppercase g500, clay 24×1.5px line via ::before -->
  <div class="eyebrow">context · sub-context</div>
  <h1>Main <em>title</em></h1><!-- em = serif italic clay -->
  <p class="intro">Summary. max-width:640px; color:var(--g700).</p>
  <nav class="toc">
    <!-- pills: border:1.5px solid g300; border-radius:999px; padding:7px 14px; hover→slate border -->
    <a href="#id">Label <span class="n">sublabel</span></a><!-- .n: mono 10px g500 → clay on hover -->
  </nav>
  <!-- optional TL;DR banner -->
  <div class="tldr"><!-- background:var(--slate); border-radius:14px; padding:24px 28px; flex + gap:20px -->
    <span class="badge">TL;DR</span><!-- mono 10px clay bg, paper text, padding:4px 10px, radius:6px -->
    <p class="text">Bottom line. <strong>Key point bold.</strong></p><!-- color:#E8E5DF -->
  </div>
</header>
```

### Section wrapper

```html
<section id="slug">
  <div class="sec-head">
    <span class="idx">01</span><!-- mono 13px clay bold, width:34px -->
    <h2>Section title</h2>
    <span class="count">N items</span><!-- mono 11px g500, g100 bg, pill -->
  </div>
  <p class="sec-intro">Framing. max-width:700px; margin-left:50px; color:var(--g700).</p>
</section>
```

### Concept cards (3-up)

```html
<div class="concept-grid"><!-- repeat(3,1fr); gap:16px; margin-left:50px -->
  <div class="concept-card"><!-- paper bg; border:1.5px g300; radius:14px; overflow:hidden -->
    <div class="c-head"><!-- padding:18px 20px 14px; border-bottom:1.5px g100; flex gap:12px -->
      <span class="c-acronym">ABC</span><!-- clay bg + paper text by default; .green=olive; .dark=slate; mono 14px 600; radius:7px; padding:4px 10px -->
      <span class="c-name">Full name</span><!-- serif 15px 500 -->
    </div>
    <div class="c-body">Explanation. font-size:13.5px; color:var(--g700).</div>
    <div class="c-fig">
      <div class="c-rule"><!-- g100 bg; border-left:3px clay; radius:8px; padding:10px 12px; .green→olive; .dark→slate -->
        <p>code example</p><!-- mono 12px g700 -->
      </div>
    </div>
  </div>
</div>
```

### Flow / step diagram

```html
<div class="flow-diagram"><!-- paper bg; border:1.5px g300; radius:14px; padding:24px -->
  <div class="flow-title">LABEL</div><!-- mono 11px uppercase g500 mb:20px -->
  <div class="flow-step">
    <!-- step::after: absolute left:14px top:32px width:1.5px bg:g300 (vertical connector) -->
    <span class="step-num">1</span><!-- 28×28px circle; border:1.5px g300; mono 11px; .active→clay+#FCF0EB; .pass→olive+#EEF2E8 -->
    <div class="step-content">
      <div class="step-label">Step name</div><!-- 13.5px 500 slate mb:3px -->
      <div class="step-sub">Detail.</div><!-- 12.5px g500 -->
      <span class="step-code">inline note</span><!-- mono 11.5px g100 bg; .clay→#FCF0EB + clay-d; .green→#EEF2E8 + olive -->
    </div>
  </div>
</div>
```

### Side-by-side comparison cards

```html
<div class="approach-grid"><!-- repeat(3,1fr) or repeat(2,1fr); gap:16px; margin-left:50px -->
  <div class="approach-card"><!-- paper; border:1.5px g300; radius:14px; overflow:hidden -->
    <!-- .recommended adds: border-color:clay; box-shadow:0 0 0 4px rgba(217,119,87,.12) -->
    <div class="a-head"><!-- padding:16px 20px; border-bottom:1.5px g100; .recommended→#FCF5F1 bg -->
      <span class="a-tag">Label</span><!-- mono 10px uppercase; g100/g200/g500; .recommended→#FCE8DE/#F4C2A8/clay-d -->
      <div class="a-title">Name</div><!-- serif 18px 500 -->
    </div>
    <div class="a-body">
      <div class="a-section-label">DNS records</div><!-- mono 10.5px uppercase g500; mt:14px mb:7px -->
      <div class="dns-records"><!-- flex col gap:5px -->
        <div class="dns-record"><!-- mono 11.5px; g100 bg; g200 border; radius:6px; padding:5px 9px; flex gap:8px -->
          <span class="rtype cname">CNAME</span><!-- 10px; radius:4px; paper text; .cname→olive; .txt→clay; .mx→slate -->
          <span class="rval">name → value</span><!-- g700; flex:1; ellipsis -->
        </div>
      </div>
      <ul class="trait-list"><!-- list-style:none -->
        <li class="pro"><span class="ico">✓</span> benefit</li>  <!-- .ico→olive -->
        <li class="con"><span class="ico">✕</span> drawback</li> <!-- .ico→clay -->
        <li class="neutral"><span class="ico">→</span> note</li> <!-- .ico→g500 -->
        <!-- li: flex gap:9px; 13px; g700; padding:5px 0; border-bottom:1px g100 -->
      </ul>
    </div>
  </div>
</div>
```

### Options cards (2-up)

```html
<div class="options-grid"><!-- repeat(2,1fr); gap:16px; margin-left:50px -->
  <div class="option-card rec"><!-- paper; border:1.5px g300; radius:14px; padding:22px 24px; .rec→clay border + #FFFAF7 bg -->
    <div class="opt-label"><!-- flex align-center gap:10px mb:12px -->
      <span class="opt-letter">A</span><!-- 28×28px circle; g100/g300/g700; .rec→clay bg + paper text -->
      <span class="opt-title">Title</span><!-- serif 17px 500 -->
    </div>
    <p class="opt-desc">Description. 13.5px g700.</p>
    <div class="trait-row"><!-- flex wrap gap:8px mt:12px -->
      <span class="trait-chip good">benefit</span>  <!-- mono 11px pill; .good→olive tones; .warn→clay tones; .neutral→g tones -->
      <span class="trait-chip warn">risk</span>
    </div>
  </div>
</div>
```

### Stat cards

```html
<div class="stats-row"><!-- repeat(3,1fr); gap:16px; margin:0 0 24px 50px -->
  <div class="stat-card"><!-- paper; border:1.5px g300; radius:14px; padding:20px 22px -->
    <div class="stat-val"><em>36</em></div><!-- serif 36px 500; em = italic clay -->
    <div class="stat-label">Description</div><!-- 13px g500 -->
  </div>
</div>
```

### Collapsible level block

```html
<div class="level-block l1 open"><!-- paper; border:1.5px g300; radius:14px; overflow:hidden; .l1→olive badge; .l2→clay badge -->
  <div class="level-head" onclick="this.parentElement.classList.toggle('open')">
    <!-- g100 bg; border-bottom:1.5px g200; padding:18px 24px; flex gap:14px; cursor:pointer -->
    <span class="level-badge">Level 1</span><!-- mono 11px 600; radius:6px; paper text -->
    <span class="level-title">Title</span><!-- serif 18px 500; flex:1 -->
    <span class="level-meta">N items</span><!-- mono 12px g500 -->
    <span class="level-chevron">›</span><!-- g500; transition:transform 200ms; .open→rotate(90deg) -->
  </div>
  <div class="level-body"><!-- display:none by default; .open→display:block; padding:20px 24px -->
    <!-- steps-list, table, code-block, etc. -->
  </div>
</div>
```

### Equation row

```html
<div class="dmarc-eq"><!-- paper; border:1.5px g300; radius:14px; padding:24px 28px; margin:24px 0 0 50px -->
  <div class="eq-label">Formula label</div><!-- mono 11px uppercase g500 mb:16px -->
  <div class="eq-row"><!-- flex align-center gap:10px flex-wrap -->
    <div class="eq-pill"><span class="dot"></span> Condition A</div><!-- mono 13px; g100 bg; g300 border; radius:999px; padding:7px 14px; .dot: 7×7px circle clay; .dot.green→olive -->
    <span class="eq-op">OR</span><!-- mono 13px g500 600 -->
    <div class="eq-result">✓ Result</div><!-- mono 13px; olive bg; paper text; 600; radius:999px; padding:7px 16px -->
  </div>
  <p class="eq-note">Footnote. 13px g500 mt:12px.</p>
</div>
```

### Recommendation card + callout grid

```html
<div class="rec-card"><!-- slate bg; paper text; radius:14px; padding:32px 36px; mb:20px -->
  <div class="rec-eyebrow">Recommended path</div><!-- mono 11px clay; flex gap:8px; ::before: 18×1.5px clay line -->
  <h3>Decision</h3><!-- serif 22px 500; letter-spacing:-0.01em; mb:14px -->
  <p>Rationale. 14.5px; color:#D0CBC3.</p>
  <p><strong>Key fact bold.</strong></p><!-- strong→paper -->
</div>
<div class="callout-grid"><!-- repeat(2,1fr); gap:14px -->
  <div class="callout"><!-- paper; border:1.5px g300; radius:12px; padding:16px 18px -->
    <div class="callout-icon">🔍</div><!-- 18px mb:8px -->
    <div class="callout-title">Heading</div><!-- 13.5px 600 slate mb:5px -->
    <div class="callout-body">Detail. 13px g700.</div>
  </div>
</div>
```

---

## Mode 2 — Deck (arrow-key slide presentation)

### Structure

```html
<body><!-- scroll-snap-type:y mandatory; overflow-x:hidden -->

<section class="slide" id="s1"><!-- 100vw × 100vh; scroll-snap-align:start; scroll-snap-stop:always; flex center; padding:8vh 6vw -->
  <!-- .invert → background:var(--slate); color:var(--ivory) -->
  <div class="slide-inner"><!-- max-width:780px; width:100% -->

    <div class="eyebrow">Section label</div><!-- mono 12px uppercase; color:var(--g500); .invert→g300 -->
    <h2>Slide heading</h2><!-- serif 500; clamp(30px,4vw,42px); letter-spacing:-0.005em; mb:44px -->
    <p class="subtitle">Supporting text. 17px; line-height:1.6; color:var(--g700); max-width:520px.</p>

    <!-- SHIPPED LIST -->
    <ul class="ship-list"><!-- flex col gap:28px -->
      <li class="ship-item"><!-- grid: 14px 1fr auto; col-gap:20px; border-bottom:1px g150; pb:24px -->
        <span class="ship-dot"></span><!-- 9×9px circle; olive bg; align-self:start; mt:8px -->
        <div>
          <div class="ship-title">Item</div><!-- serif 20px 500 mb:6px -->
          <div class="ship-desc">Description. 14px g700.</div>
        </div>
        <span class="ship-ref">#1234</span><!-- mono 11px g500; white-space:nowrap -->
      </li>
    </ul>

    <!-- PROGRESS LIST -->
    <ul class="prog-list"><!-- flex col gap:40px -->
      <li class="prog-item">
        <div class="prog-head"><!-- flex justify-between align-baseline mb:10px -->
          <span class="prog-title">Feature name</span><!-- serif 20px 500 -->
          <span class="prog-pct">~70%</span><!-- mono 12px g500 -->
        </div>
        <div class="prog-track"><!-- w:100%; h:5px; g150 bg; radius:3px; overflow:hidden; mb:10px -->
          <div class="prog-fill" style="width:70%"></div><!-- h:100%; clay bg; radius:3px -->
        </div>
        <p class="prog-note">Status detail. 13px g700.</p>
      </li>
    </ul>

    <!-- METRICS -->
    <div class="metrics"><!-- grid repeat(2,1fr); gap:56px; mb:48px -->
      <div class="metric">
        <div class="metric-label">METRIC NAME</div><!-- mono 11px uppercase letter-spacing:0.08em g500 mb:14px -->
        <div class="metric-value">184<span style="font-size:28px">ms</span></div><!-- serif 52px 500 letter-spacing:-0.01em -->
        <div class="metric-delta down">↓ 12% wk/wk</div><!-- mono 13px; .down→olive; .up→clay -->
      </div>
    </div>
    <!-- SPARKLINE -->
    <div class="sparkline-wrap"><!-- border-top:1px g300; pt:28px -->
      <svg width="100%" height="64" viewBox="0 0 780 64" preserveAspectRatio="none">
        <polyline points="0,22 110,30 220,18 330,34 440,26 550,40 660,24 780,12"
          fill="none" stroke="#788C5D" stroke-width="2" stroke-linejoin="round" stroke-linecap="round"/>
        <circle cx="780" cy="12" r="3.5" fill="#788C5D"/>
        <line x1="0" y1="63" x2="780" y2="63" stroke="#D1CFC5" stroke-width="1"/>
      </svg>
      <div class="sparkline-caption">Label — lower is better. 12px g500 mt:10px.</div>
    </div>

    <!-- DECISION CARD (use on an .invert slide) -->
    <div class="decision-card"><!-- border:1.5px clay; radius:14px; padding:36px 38px; bg:rgba(217,119,87,.06) -->
      <p class="decision-q">The question to decide. serif 24px line-height:1.4 mb:12px.</p>
      <p class="decision-context">Context. 14px g300 line-height:1.6.</p>
    </div>
    <div class="options"><!-- flex gap:14px mt:32px flex-wrap -->
      <span class="chip">B — Option</span><!-- mono 12px; border:1px g700; radius:999px; padding:10px 18px; ivory text -->
      <span class="chip lean">A — Preferred</span><!-- .lean → border:clay; color:clay -->
    </div>

    <!-- NEXT LIST -->
    <ul class="next-list"><!-- list-style:none; flex col gap:22px -->
      <li>Item. serif 21px; padding-left:36px; position:relative.
        <!-- li::before: absolute left:0 top:0.55em; 18×1.5px clay line -->
      </li>
    </ul>

  </div>
</section>

<!-- slide counter -->
<div id="counter">1 / N</div><!-- fixed bottom:22px right:28px; mono 12px g500 -->
```

### Deck JavaScript (required — copy verbatim)

```html
<script>
(function () {
  const slides = Array.from(document.querySelectorAll('.slide'));
  const counter = document.getElementById('counter');
  let current = 0;
  function go(i) {
    current = Math.max(0, Math.min(slides.length - 1, i));
    slides[current].scrollIntoView({ behavior: 'smooth' });
  }
  document.addEventListener('keydown', function (e) {
    if (e.key === 'ArrowRight' || e.key === 'ArrowDown' || e.key === ' ') { e.preventDefault(); go(current + 1); }
    if (e.key === 'ArrowLeft'  || e.key === 'ArrowUp')                     { e.preventDefault(); go(current - 1); }
  });
  const obs = new IntersectionObserver(function (entries) {
    entries.forEach(function (en) {
      if (en.isIntersecting) {
        current = slides.indexOf(en.target);
        counter.textContent = (current + 1) + ' / ' + slides.length;
      }
    });
  }, { threshold: 0.6 });
  slides.forEach(function (s) { obs.observe(s); });
}());
</script>
```

### Deck body CSS

```css
body { scroll-snap-type: y mandatory; overflow-x: hidden; }
.slide { width:100vw; height:100vh; scroll-snap-align:start; scroll-snap-stop:always; display:flex; align-items:center; justify-content:center; padding:8vh 6vw; }
.slide-inner { width:100%; max-width:780px; }
.slide.invert { background:var(--slate); color:var(--ivory); }
h2 { font-family:var(--serif); font-weight:500; font-size:clamp(30px,4vw,42px); line-height:1.15; letter-spacing:-0.005em; margin-bottom:44px; }
.ship-list { list-style:none; display:flex; flex-direction:column; gap:28px; }
.ship-item { display:grid; grid-template-columns:14px 1fr auto; column-gap:20px; align-items:baseline; padding-bottom:24px; border-bottom:1px solid var(--g100); }
.ship-item:last-child { border-bottom:none; }
.ship-dot { width:9px; height:9px; border-radius:50%; background:var(--olive); margin-top:8px; align-self:start; }
.ship-title { font-family:var(--serif); font-size:20px; font-weight:500; margin-bottom:6px; }
.ship-desc { font-size:14px; line-height:1.55; color:var(--g700); }
.ship-ref { font-family:var(--mono); font-size:11px; color:var(--g500); white-space:nowrap; padding-top:4px; }
.prog-list { list-style:none; display:flex; flex-direction:column; gap:40px; }
.prog-head { display:flex; justify-content:space-between; align-items:baseline; margin-bottom:10px; }
.prog-title { font-family:var(--serif); font-size:20px; font-weight:500; }
.prog-pct { font-family:var(--mono); font-size:12px; color:var(--g500); }
.prog-track { width:100%; height:5px; background:var(--g100); border-radius:3px; overflow:hidden; margin-bottom:10px; }
.prog-fill { height:100%; background:var(--clay); border-radius:3px; }
.prog-note { font-size:13px; line-height:1.55; color:var(--g700); }
.metrics { display:grid; grid-template-columns:1fr 1fr; gap:56px; margin-bottom:48px; }
.metric-label { font-family:var(--mono); font-size:11px; letter-spacing:0.08em; text-transform:uppercase; color:var(--g500); margin-bottom:14px; }
.metric-value { font-family:var(--serif); font-size:52px; font-weight:500; line-height:1; letter-spacing:-0.01em; }
.metric-delta { font-family:var(--mono); font-size:13px; margin-top:12px; }
.metric-delta.down { color:var(--olive); }
.metric-delta.up { color:var(--clay); }
.sparkline-wrap { border-top:1px solid var(--g300); padding-top:28px; }
.sparkline-caption { font-size:12px; color:var(--g500); margin-top:10px; }
.decision-card { border:1.5px solid var(--clay); border-radius:14px; padding:36px 38px; background:rgba(217,119,87,0.06); }
.decision-q { font-family:var(--serif); font-size:24px; line-height:1.4; margin-bottom:12px; }
.decision-context { font-size:14px; line-height:1.6; color:var(--g300); }
.options { display:flex; gap:14px; margin-top:32px; flex-wrap:wrap; }
.chip { font-family:var(--mono); font-size:12px; padding:10px 18px; border-radius:999px; border:1px solid var(--g700); color:var(--ivory); background:transparent; }
.chip.lean { border-color:var(--clay); color:var(--clay); }
.next-list { list-style:none; display:flex; flex-direction:column; gap:22px; }
.next-list li { font-family:var(--serif); font-size:21px; line-height:1.45; padding-left:36px; position:relative; }
.next-list li::before { content:""; position:absolute; left:0; top:0.55em; width:18px; height:1.5px; background:var(--clay); }
#counter { position:fixed; bottom:22px; right:28px; font-family:var(--mono); font-size:12px; color:var(--g500); user-select:none; z-index:10; }
```

---

## Mode 3 — Research & Learning

### Page layout (2-column: sticky side nav + main)

```css
body { padding: 56px 24px 120px; }
.page { max-width:1100px; margin:0 auto; display:grid; grid-template-columns:200px minmax(0,1fr); gap:48px; }
@media (max-width:920px) { .page { grid-template-columns:1fr; } nav { display:none; } }
```

### Side nav

```html
<nav><!-- sticky top:32px; align-self:start; font-size:13px -->
  <div class="label">On this page</div><!-- mono 10px uppercase g500 mb:12px -->
  <a href="#id">Section</a><!-- display:block; padding:5px 0 5px 12px; border-left:2px g300; color:g700; text-decoration:none; hover→slate+slate border -->
  <a href="#id" class="l2">Sub-section</a><!-- .l2: padding-left:24px; font-size:12.5px; color:g500 -->
  <div class="files"><!-- mt:28px; border-top:1px g300; pt:16px -->
    <div class="label">Files read</div>
    <code>path/to/file.ts</code><!-- mono 11px g500; display:block; padding:3px 0 -->
  </div>
</nav>
```

### TL;DR box (left-border variant — for Research mode)

```html
<div class="tldr">
  <!-- border:1.5px g300; border-left:3px clay; radius:10px; bg:#fff; padding:16px 18px -->
  <b>TL;DR</b> — Summary sentence. <code>key term</code> mentioned inline.
</div>
```

### Collapsible steps (`<details>`)

```html
<details open><!-- border:1.5px g300; radius:10px; bg:#fff; margin:14px 0; overflow:hidden -->
  <summary><!-- list-style:none; cursor:pointer; padding:14px 16px; serif 16px slate; flex align-baseline gap:10px -->
    <!-- summary::before: "▸" clay sans 12px; details[open]→rotate(90deg) transition:120ms -->
    1 · Step title
    <span class="where">file.ts:21</span><!-- mono 11px g500; margin-left:auto -->
  </summary>
  <div class="body"><!-- padding:0 16px 16px -->
    <p>Detail text. 14px.</p>
  </div>
</details>
```

### Tabs

```html
<div class="tabs" data-tabs><!-- border:1.5px g300; radius:10px; bg:#fff; margin:16px 0; overflow:hidden -->
  <div class="tabbar"><!-- flex; border-bottom:1px g300; bg:g150 -->
    <button class="on" data-t="0">Tab 1</button><!-- mono 12px; no border/bg; padding:10px 16px; border-right:1px g300; cursor:pointer -->
    <!-- .on: bg:#fff; color:slate; border-bottom:2px clay; margin-bottom:-1px -->
    <button data-t="1">Tab 2</button>
  </div>
  <pre class="on"><!-- display:none by default; .on→display:block; mono 12.5px; padding:16px 18px; line-height:1.6; color:slate; overflow-x:auto -->
<span class="hl">highlighted</span> normal <span class="cm"># comment</span>
  </pre><!-- .hl→clay; .cm→g500 -->
  <pre>Second tab content.</pre>
</div>

<script>
document.querySelectorAll("[data-tabs]").forEach(box => {
  const btns = box.querySelectorAll("button");
  const panes = box.querySelectorAll("pre");
  btns.forEach(b => b.addEventListener("click", () => {
    btns.forEach(x => x.classList.remove("on"));
    panes.forEach(x => x.classList.remove("on"));
    b.classList.add("on");
    panes[+b.dataset.t].classList.add("on");
  }));
});
</script>
```

### Callout box (oat tint — for tips/warnings in Research mode)

```html
<div class="callout">
  <!-- flex gap:12px; border:1.5px oat; bg:rgba(227,218,204,.35); radius:10px; padding:14px 16px; font-size:14px -->
  <span class="ico">★</span><!-- clay; font-weight:600 -->
  <div>Tip or warning text.</div>
</div>
```

### FAQ

```html
<dl class="faq"><!-- margin-top:8px -->
  <dt>Question?</dt><!-- serif 16px slate; mt:18px -->
  <dd>Answer. 14px; mt:4px; max-width:640px.</dd>
</dl>
```

---

## Mode 4 — Code Review & Understanding

### PR header

```html
<header class="pr-head"><!-- border:1.5px g300; radius:12px; padding:28px 32px; bg:#fff; mb:36px -->
  <div class="repo-line">repo/name · Pull Request #N</div><!-- mono 12.5px g500 mb:10px -->
  <h1>PR title</h1><!-- serif 500 30px slate mb:18px -->
  <div class="meta-row"><!-- flex flex-wrap gap:20px align-center -->
    <div class="author"><!-- flex gap:10px align-center -->
      <div class="avatar">AB</div><!-- 36×36px circle; oat bg; slate text; 600 13px; border:1.5px g300 -->
      <div><div class="author-name">Name</div><div class="author-sub">opened N days ago</div></div>
    </div>
    <div class="branch"><!-- mono 12.5px; g150 bg; border:1.5px g300; radius:8px; padding:6px 10px -->
      feature-branch <span class="arrow">→</span> main<!-- .arrow: g500 mx:6px -->
    </div>
    <div class="stat"><!-- mono 13px -->
      <span class="add">+142</span> / <span class="del">−38</span><!-- .add→olive 600; .del→rust 600 -->
      <span class="files">6 files</span><!-- g500 ml:10px -->
    </div>
  </div>
</header>
```

### Risk map

```html
<div class="risk-map"><!-- flex flex-wrap gap:10px -->
  <a class="chip attention" href="#file-id">
    <span class="dot"></span>filename.ts
    <!-- .chip: inline-flex gap:8px; border:1.5px g300; radius:8px; padding:8px 12px; mono 12.5px; bg:#fff -->
    <!-- .chip.safe: bg rgba(olive,.10); border rgba(olive,.45) → .dot: olive -->
    <!-- .chip.medium: bg oat → .dot: #B89B6E -->
    <!-- .chip.attention: bg rgba(clay,.12); border rgba(clay,.55) → .dot: clay -->
    <!-- .dot: 9×9px circle -->
  </a>
</div>
<div class="legend"><!-- mt:12px; 12px g500; flex gap:18px -->
  <span><span class="dot" style="background:var(--olive)"></span> safe</span>
  <span><span class="dot" style="background:#B89B6E"></span> worth a look</span>
  <span><span class="dot" style="background:var(--clay)"></span> needs attention</span>
</div>
```

### File card with diff + bubble comments

```html
<article class="file-card" id="file-id">
<!-- border:1.5px g300; radius:12px; bg:#fff; mb:24px; overflow:hidden; scroll-margin-top:20px -->
  <div class="file-head"><!-- padding:16px 20px; border-bottom:1.5px g150; flex justify-between gap:12px -->
    <div class="file-path">src/path/to/file.ts</div><!-- mono 13.5px slate -->
    <div style="display:flex;align-items:center;gap:12px">
      <span class="risk-tag attention">needs attention</span>
      <!-- .risk-tag: 11px uppercase letter-spacing:0.06em; padding:3px 8px; radius:6px; 600 -->
      <!-- .risk-tag.safe→olive tones; .risk-tag.medium→oat; .risk-tag.attention→clay tones -->
      <span class="file-delta"><span class="add">+58</span> <span class="del">−0</span></span>
    </div>
  </div>

  <!-- DIFF BLOCK -->
  <div class="diff">
  <!-- bg:slate; mono 12.5px; line-height:1.7; overflow-x:auto -->
    <div class="diff-row hunk"><span class="ln"></span><span class="mark"></span><span class="code">@@ -0,0 +1,20 @@</span></div>
    <!-- .diff-row: grid 48px 18px 1fr; padding:0 14px 0 0; white-space:pre -->
    <!-- .ln: text-align:right; pr:14px; color:g500; user-select:none -->
    <!-- .mark: text-align:center; color:g500 -->
    <!-- .code: color:#E8E6DC (on dark bg) -->
    <!-- .diff-row.ctx .code: color:#B8B6AC -->
    <!-- .diff-row.add: bg rgba(olive,.15); .mark→olive -->
    <!-- .diff-row.del: bg rgba(rust,.15); .mark→rust -->
    <!-- .diff-row.hunk: bg rgba(255,255,255,.04); code+mark→g500 -->
    <div class="diff-row add"><span class="ln">1</span><span class="mark">+</span><span class="code">new code here</span></div>
    <div class="diff-row del"><span class="ln">2</span><span class="mark">-</span><span class="code">removed code</span></div>
    <div class="diff-row ctx"><span class="ln">3</span><span class="mark"> </span><span class="code">context line</span></div>
  </div>

  <!-- BUBBLE COMMENTS -->
  <div class="comments"><!-- padding:18px 20px; flex col gap:14px; bg:g150 -->
    <div class="bubble blocking">
    <!-- bg:#fff; border:1.5px g300; border-left-width:4px; radius:8px; padding:12px 14px 12px 16px; max-width:680px -->
    <!-- .blocking: border-left-color:clay; ::before also clay -->
    <!-- .nit: border-left-color:g300 -->
    <!-- bubble::before: absolute left:-9px top:16px; 12×12px; bg:#fff; border-left+bottom from same color; rotate(45deg) — speech bubble pointer -->
      <div class="anchor">line 11</div><!-- mono 11.5px g500 mb:4px -->
      <p><span class="label">Blocking</span> Issue description with <code>inline code</code>.</p>
      <!-- .label: inline-block; 10.5px uppercase letter-spacing:0.08em; 700; mr:8px -->
      <!-- .blocking .label→clay; .nit .label→g500 -->
    </div>
  </div>
</article>

<!-- COLLAPSED FILE (safe/minor) -->
<details class="file-collapsed" id="file-safe">
<!-- border:1.5px g300; radius:12px; bg:#fff; mb:14px -->
  <summary><!-- list-style:none; cursor:pointer; padding:14px 20px; flex justify-between gap:12px -->
    <!-- summary::after "+" / "[open]→−"; mono g500 16px -->
    <span class="file-path">src/path/safe.ts</span>
    <span style="display:flex;align-items:center;gap:12px">
      <span class="risk-tag safe">safe</span>
      <span class="file-delta"><span class="add">+14</span> <span class="del">−2</span></span>
    </span>
  </summary>
  <div class="body"><!-- padding:0 20px 16px; 13.5px g700 -->
    Brief description of what changed and why it's safe.
  </div>
</details>
```

### Next-steps checklist (end of PR review)

```html
<footer class="next-steps"><!-- border:1.5px g300; radius:12px; bg:#fff; padding:24px 28px -->
  <h2>Suggested next steps</h2>
  <ul class="checklist"><!-- list-style:none -->
    <li><!-- flex align-start gap:12px; padding:8px 0 -->
      <input type="checkbox" id="s1"><!-- w:17px h:17px; accent-color:olive; cursor:pointer; mt:2px -->
      <label for="s1">Fix <code>thing</code> in <code>file.ts</code>.</label><!-- cursor:pointer; flex:1 -->
    </li>
  </ul>
</footer>
<script>
  document.querySelectorAll('.risk-map a').forEach(function (a) {
    a.addEventListener('click', function () {
      var target = document.querySelector(a.getAttribute('href'));
      if (!target) return;
      target.style.transition = 'box-shadow 180ms ease';
      target.style.boxShadow = '0 0 0 3px rgba(217,119,87,0.35)';
      setTimeout(function () { target.style.boxShadow = 'none'; }, 1400);
    });
  });
</script>
```

### SVG module map (boxes and arrows)

```html
<div class="diagram-panel"><!-- border:1.5px g300; radius:12px; bg:#fff; padding:20px; overflow-x:auto -->
  <svg class="flow" viewBox="0 0 720 280" width="720" height="280" role="img">
    <defs>
      <marker id="arr" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse">
        <path d="M 0 0 L 10 5 L 0 10 z" fill="#87867F"/>
      </marker>
    </defs>
    <!-- .box: fill:#fff; stroke:g300; stroke-width:1.5 -->
    <!-- .box.hot: fill:rgba(clay,.10); stroke:clay — for the highlighted trust boundary / hot path -->
    <rect class="box" x="30" y="40" width="150" height="64" rx="10"/>
    <rect class="box hot" x="245" y="40" width="180" height="64" rx="10"/>
    <!-- .flow text: mono 12px fill:slate -->
    <!-- .sub: mono 10px fill:g500 -->
    <text x="105" y="68" text-anchor="middle" font-family="ui-monospace,monospace" font-size="12" fill="#141413">Module A</text>
    <text x="105" y="84" text-anchor="middle" font-family="ui-monospace,monospace" font-size="10" fill="#87867F">description</text>
    <!-- arrows -->
    <line stroke="#87867F" stroke-width="1.5" fill="none" x1="180" y1="72" x2="245" y2="72" marker-end="url(#arr)"/>
    <text x="212" y="62" text-anchor="middle" font-family="ui-monospace,monospace" font-size="10" fill="#87867F">call</text>
  </svg>
</div>
```

### Sticky sidebar (for module map / code understanding)

```html
<aside><!-- position:sticky; top:24px; align-self:start -->
  <div class="panel"><!-- border:1.5px g300; radius:12px; bg:#fff; padding:18px 20px; mb:20px -->
    <h3>Key files</h3><!-- sans 11px 600 uppercase letter-spacing:0.08em g500 mb:12px -->
    <ul class="key-files"><!-- list-style:none -->
      <li style="margin-bottom:12px">
        <span class="path">src/path/file.ts</span><!-- mono 12px slate; display:block; mb:2px; word-break:break-all -->
        <span class="desc">What this file does.</span><!-- 12.5px g500 line-height:1.45 -->
      </li>
    </ul>
  </div>
  <div class="gotchas"><!-- border:1.5px clay; radius:12px; bg:rgba(clay,.06); padding:18px 20px -->
    <h3>Gotchas</h3><!-- sans 11px 700 uppercase clay mb:10px -->
    <ul style="list-style:none">
      <li style="position:relative;padding-left:16px;font-size:13px;margin-bottom:8px">
        <!-- li::before: absolute left:0 top:8px; 5×5px clay square; radius:2px -->
        Watch out for <code>this</code>.
      </li>
    </ul>
  </div>
</aside>
```

---

## Mode 5 — Reports

### Summary band (stat cards with deltas)

```html
<div class="summary-band"><!-- grid repeat(4,1fr) or repeat(2,1fr); gap:16px; mb:32px -->
  <div class="stat-card"><!-- bg:#fff; border:1.5px g300; radius:12px; padding:20px 22px -->
    <!-- .stat-card.warn: border-left:3px clay -->
    <div class="stat-val">184<span style="font-size:18px">ms</span></div><!-- serif 36px 500 slate lh:1 mb:4px -->
    <div class="stat-label">p95 latency</div><!-- mono 11px uppercase g500 mb:8px -->
    <div class="stat-delta up">↑ 3ms</div><!-- mono 12px; .up→clay; .down→olive; .flat→g500 -->
  </div>
</div>
```

### SVG bar chart

```html
<div class="chart-wrap"><!-- border:1.5px g300; radius:12px; bg:#fff; padding:20px 24px; mb:24px -->
  <div class="chart-label">CHART TITLE</div><!-- mono 11px uppercase g500 mb:16px -->
  <svg viewBox="0 0 640 180" width="100%" style="display:block">
    <!-- gridlines -->
    <line x1="0" y1="0"   x2="640" y2="0"   stroke="#D1CFC5" stroke-width="1"/>
    <line x1="0" y1="45"  x2="640" y2="45"  stroke="#F0EEE6" stroke-width="1"/>
    <line x1="0" y1="90"  x2="640" y2="90"  stroke="#F0EEE6" stroke-width="1"/>
    <line x1="0" y1="135" x2="640" y2="135" stroke="#F0EEE6" stroke-width="1"/>
    <!-- bars: normal=oat (#E3DACC), peak/highlighted=clay (#D97757) -->
    <!-- bar height unit = 40px per "unit"; bars are 56px wide with 24px gap -->
    <rect x="10"  y="60"  width="56" height="120" rx="4" fill="#E3DACC"/>
    <rect x="90"  y="20"  width="56" height="160" rx="4" fill="#D97757"/><!-- peak bar -->
    <rect x="170" y="80"  width="56" height="100" rx="4" fill="#E3DACC"/>
    <!-- x-axis labels -->
    <text x="38"  y="175" text-anchor="middle" font-family="ui-monospace,monospace" font-size="11" fill="#87867F">Mon</text>
    <text x="118" y="175" text-anchor="middle" font-family="ui-monospace,monospace" font-size="11" fill="#87867F">Tue</text>
    <!-- y-axis labels -->
    <text x="635" y="4"   text-anchor="end" font-family="ui-monospace,monospace" font-size="10" fill="#87867F">180</text>
    <text x="635" y="94"  text-anchor="end" font-family="ui-monospace,monospace" font-size="10" fill="#87867F">90</text>
  </svg>
</div>
```

### Workstream / project status table

```html
<table class="status-table"><!-- w:100%; border-collapse:collapse; font-size:13.5px -->
  <thead>
    <tr>
      <th>Stream</th><th>Owner</th><th>Status</th><th>Notes</th>
      <!-- th: mono 11px uppercase g500; pb:8px; border-bottom:1.5px g300; text-align:left; padding:8px 12px -->
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>Feature name</td>
      <td style="color:var(--g500)">@handle</td>
      <td>
        <span class="risk-dot low"></span>On track
        <!-- .risk-dot: inline-block 8×8px circle mr:6px; .low→olive; .med→clay; .high→rust -->
      </td>
      <td style="color:var(--g500)">Short note</td>
      <!-- td: padding:10px 12px; border-bottom:1px g100; color:g700; vertical-align:top -->
    </tr>
  </tbody>
</table>
```

### Incident timeline

```html
<div class="timeline">
<!-- position:relative; padding:4px 0 4px 16px -->
<!-- timeline::before: absolute left:16px top:8px bottom:8px; width:2px; bg:g300 — the vertical rail -->
  <div class="tl-entry"><!-- position:relative; padding:0 0 22px 28px -->
    <span class="tl-dot impact"></span>
    <!-- .tl-dot: absolute left:-5px top:6px; 12×12px circle; bg:g500; border:2px solid ivory -->
    <!-- .tl-dot.impact→clay; .tl-dot.mitigated→olive; .tl-dot.resolved→slate -->
    <div class="tl-time">14:23 UTC</div>
    <!-- mono 12px; inline-block; g100 bg; g300 border; radius:6px; padding:2px 8px; mb:6px -->
    <div class="tl-head">Event title</div><!-- serif 16px slate 500 mb:4px -->
    <p class="tl-body">What happened. 13.5px g700.</p>
    <!-- optional log excerpt -->
    <pre class="tl-log">ERROR: connection refused at db:5432</pre>
    <!-- mono 12px; g100 bg; g200 border; radius:6px; padding:8px 12px; mt:8px; overflow-x:auto -->
  </div>
</div>
```

### Action items (post-mortem checklist)

```html
<div class="action-items"><!-- border:1.5px g300; radius:12px; bg:#fff; overflow:hidden -->
  <div class="ai-head"><!-- padding:14px 18px; border-bottom:1.5px g100; bg:g100; mono 11px uppercase g500 -->
    Action items
  </div>
  <div class="ai-row">
  <!-- grid: 36px 36px 1fr 96px; align-center; gap:14px; padding:14px 18px; border-bottom:1px g100 -->
  <!-- .ai-row.done: text-decoration:line-through; color:g500 -->
    <div class="ai-check"></div>
    <!-- 18×18px; border:1.5px g300; radius:5px; bg:#fff -->
    <!-- .done .ai-check: bg:olive; border-color:olive — with checkmark via ::after -->
    <span class="ai-pri med">P1</span><!-- mono 10px; radius:4px; padding:2px 6px; paper text; .high→rust; .med→clay; .low→g500 -->
    <div>
      <div class="ai-title">Fix the thing</div><!-- 13.5px slate 500 mb:2px -->
      <div class="ai-owner">@handle</div><!-- mono 12px g500 -->
    </div>
    <span class="ai-due">Due May 30</span><!-- mono 11px g500 -->
  </div>
</div>
```

---

## Shared utility components (all modes)

### Data table
```html
<table class="domains-table"><!-- w:100%; border-collapse:collapse; 12.5px -->
  <thead><tr>
    <th>Col</th><!-- mono 10.5px uppercase g500; pb:6px; border-bottom:1.5px g200; bg:g100; text-align:left; padding:6px 10px -->
  </tr></thead>
  <tbody><tr>
    <td><code>value</code></td><!-- padding:8px 10px; border-bottom:1px g100; g700; vertical-align:top -->
    <td><span class="policy-tag reject">p=reject</span></td>
    <!-- .policy-tag: mono 10px; radius:4px; border:1px; padding:2px 7px -->
    <!-- .reject→clay tones; .quarantine→yellow tones; .none→gray tones -->
  </tr></tbody>
</table>
```

### Code block
```html
<div class="code-block"><!-- g100 bg; g200 border; radius:8px; padding:12px 14px; mono 12px; g700; line-height:1.6 -->
  <span class="co"># comment</span><br><!-- g500 -->
  key <span class="hi">VALUE</span><!-- clay -->
  status <span class="ok">pass</span><!-- olive -->
</div>
```

### Steps list
```html
<ol class="steps-list"><!-- list-style:none -->
  <li><!-- flex gap:12px align-start; padding:10px 0; border-bottom:1px g100; 13.5px g700 -->
    <span class="sn">1</span><!-- 22×22px circle; g100 bg; g300 border; mono 10px 600; .l1→olive border+text; .l2→clay -->
    Step description with <code>inline code</code>.
  </li>
</ol>
```

---

## Selecting the right mode

| You have | Mode |
|---|---|
| Options, trade-offs, migration plan, approach comparison | Exploration & Planning |
| Findings to walk through slide-by-slide in a meeting | Deck |
| "How does X work?" — feature/system deep-dive | Research & Learning |
| PR diff, annotated code, module call graph | Code Review |
| Weekly status, incident post-mortem, health report | Report |

---

## What to fill in from the conversation

Read back through the current conversation to extract:
- The main topic and key findings
- Any concepts that need explaining
- Any current vs. proposed state comparison
- Any options or trade-offs identified
- Any plan, migration steps, or timeline
- Any recommendation made

Map each piece to the right mode and component above. Do not invent findings — only render what was established in the conversation.

If `$ARGUMENTS` is blank, derive the slug from the topic (e.g. `auth-refactor`, `ses-migration-plan`, `pr-247-review`).

Write the file, then output only the path.
