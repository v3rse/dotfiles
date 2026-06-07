---
name: codebase-onboarding
description: Generate a personalized, richly visual HTML onboarding guide for contributing to a codebase. Use when the user asks to onboard to a repo, understand architecture, identify gaps/inadequacies, visualize internals, create a contributor guide, or model a codebase wiki/lore document. Produces ~/org/wiki/<repo>-contributor-onboarding.html by default.
---

# Codebase Onboarding

Generate a personalized contributor-onboarding artifact for the current repository. The output is a self-contained HTML document, saved outside the repo under `~/org/wiki/`, with architecture diagrams, developer workflow, codebase lore, patterns, debugging notes, contribution bar, and suggested first issues.

This skill is inspired by codebase-wiki patterns: a useful contributor guide should not only list files; it should explain history, lore, conventions, pitfalls, debugging, and the social contribution process.

## Output contract

Default output:

```text
~/org/wiki/<repo-slug>-contributor-onboarding.html
```

Rules:

- Do **not** store generated artifacts in the target repo unless the user explicitly asks.
- Create parent directories with `mkdir -p ~/notes`.
- Return a `file://` link to the artifact.
- The HTML must be self-contained: inline CSS, inline JS if needed, no external fonts/assets/CDNs.
- Use clickable local file links in responses: `file:///absolute/path`.
- If the user asks for a different destination, obey it.

## Required sources to inspect

Before generating the guide, inspect the repo and user context.

### User profile

If the user wants personalization, load only profile/career/context files the user explicitly points to, or that are configured in the current agent environment. Do not hardcode personal paths.

Do not expose private details in the artifact unless they are directly useful for onboarding and the user asked for that level of personalization. Summarize strengths/gaps as engineering-relevant traits.

### Codebase structure

Inspect:

```bash
pwd
find . -maxdepth 3 -type f \( -name 'package.json' -o -name 'Cargo.toml' -o -name 'pyproject.toml' -o -name 'go.mod' -o -name 'README*' -o -name 'CONTRIBUTING*' -o -name 'AGENTS.md' -o -name 'CLAUDE.md' \)
find . -maxdepth 3 -type d | sort | head -200
```

Then read, when present:

- root `README*`
- `CONTRIBUTING*`
- `AGENTS.md`, `CLAUDE.md`, `.github/ISSUE_TEMPLATE/*`, `.github/PULL_REQUEST_TEMPLATE*`
- package/module READMEs
- changelogs/release docs
- test scripts/configs

### Git history / lore

Collect enough history to write a meaningful lore section:

```bash
git log --format='%h %ad %s' --date=short | head -80
git log --format='%h %ad %s' --date=short | tail -40
git log --oneline | wc -l
git log --format='%ae' | sort -u | wc -l
git log --format='%ad' --date=short | sort -u | head -5
git log --format='%ad' --date=short | sort -u | tail -5
```

Find hot files:

```bash
git log --name-only --format='' | grep -E '\.(ts|tsx|js|jsx|py|go|rs|java|kt|rb|php|cs|c|cpp|h|hpp)$' | sort | uniq -c | sort -rn | head -20
```

If the repo is not a git repo, skip lore metrics and say so.

### Quantitative snapshot

Collect approximate codebase numbers. Adapt to language/tooling:

```bash
find . -type f \( -name '*.ts' -o -name '*.tsx' -o -name '*.js' -o -name '*.jsx' -o -name '*.py' -o -name '*.go' -o -name '*.rs' \) \
  ! -path '*/node_modules/*' ! -path '*/dist/*' ! -path '*/build/*' ! -path '*/target/*' | wc -l
```

For a monorepo, count source lines by package/module if useful:

```bash
for d in packages/* apps/* crates/*; do
  [ -d "$d" ] || continue
  printf '%s: ' "$d"
  find "$d" -type f \( -name '*.ts' -o -name '*.tsx' -o -name '*.js' -o -name '*.py' -o -name '*.go' -o -name '*.rs' \) \
    ! -path '*/node_modules/*' ! -path '*/dist/*' ! -path '*/build/*' ! -path '*/target/*' \
    -print0 | xargs -0 wc -l 2>/dev/null | tail -1 | awk '{print $1}'
done
```

### Issues / first tasks

Treat first issues as optional and goal-dependent.

If the user explicitly asks for an issue to start with, or asks for actionable next contribution work, inspect issues when possible:

```bash
gh issue list --limit 20
gh issue list --label good-first-issue --limit 20
gh issue view <number>
```

If suitable issues exist, pick 2–4. For each:

- explain why it matches the user's profile
- explain likely package/files to inspect
- explain root cause hypothesis if visible
- provide a concrete step-by-step implementation plan (read order, searches to run, code change, test to add, validation command, PR note)
- warn if the issue has PR history or requires maintainer confirmation

If no suitable issues exist, or issue lookup is unavailable, provide starter tasks instead:

- first reading path through important files
- safe local reproduction tasks
- characterization-test ideas
- documentation or error-message improvements
- small cleanup tasks that do not alter behavior

If the user only asks for architecture/onboarding and not next work, keep this section short or omit it entirely. Do not invent issue details.

## Document structure

Use this order by default:

1. **Profile & Gaps**
   - personalized strengths mapped to the repo
   - likely inadequacies / unfamiliar areas
   - one strongest analogy from the user's past work to this repo

2. **Contribution Rules**
   - social gate, PR expectations, commit message format
   - commands that must be run
   - commands/actions to avoid

3. **Working in this Codebase**
   - Lore
   - By the numbers
   - Dev workflow
   - Patterns & conventions
   - Debugging

4. **Package / Module Map**
   - top-level modules/packages and responsibilities
   - dependency direction
   - ownership boundaries

5. **Architecture Diagram**
   - SVG diagram showing main runtime path
   - call boundaries and data transformations

6. **Core Runtime Walkthrough**
   - main loop/request lifecycle/state machine
   - important extension points and hooks
   - where errors and retries happen

7. **Tooling / Interfaces / Persistence**
   - tools, APIs, CLI, TUI, persistence, queues, DBs, as applicable

8. **Risky Subsystems / Pitfalls**
   - compaction, concurrency, migrations, rendering, auth, provider quirks, etc.

9. **Optional: First Issues / Starter Tasks / Reading Path**
   - include only when useful for the user's goal
   - if suitable issues exist, rank recommendations and explain why each is suitable
   - include concrete next files to read
   - include a step-by-step plan for each suggested issue/task
   - if issues are unavailable or not requested, replace with a reading path or safe starter tasks

## Droid-wiki-inspired sections

### Lore

Write a narrative, not a changelog. Include:

- major eras
- the oldest/fossil code that still matters
- deprecated/superseded pieces
- growth trajectory
- historical reasons for weird shapes in the code

Tone: technical, concise, but story-shaped.

### By the numbers

Include 3–6 stat cards and 1–2 tables:

- commits
- contributors
- source files
- approximate lines
- package/module line counts
- hottest files by git edit frequency
- commit prefix/type distribution if useful

### Patterns & conventions

Document the conventions a new contributor must follow, e.g.:

- factory functions vs classes
- schema/validation patterns
- event/streaming patterns
- dependency direction
- TypeScript/Rust/Go style constraints
- testing strategy
- error handling conventions
- file naming conventions

### Debugging

Include practical debugging recipes:

- how to run a local repro
- where logs/session files are stored
- how to trace the main event path
- common error messages mapped to root causes
- test harness/fake provider/mock infrastructure

## HTML design requirements

Use a warm paper-like design system:

```css
:root {
  --ivory:  #FAF9F5;
  --paper:  #FFFFFF;
  --slate:  #141413;
  --clay:   #D97757;
  --clay-d: #B85C3E;
  --oat:    #E3DACC;
  --olive:  #788C5D;
  --rust:   #B04A3F;
  --g100:   #F0EEE6;
  --g200:   #E6E3DA;
  --g300:   #D1CFC5;
  --g500:   #87867F;
  --g700:   #3D3D3A;
  --serif:  ui-serif, Georgia, "Times New Roman", Times, serif;
  --sans:   system-ui, -apple-system, "Segoe UI", Roboto, Helvetica, Arial, sans-serif;
  --mono:   ui-monospace, "SF Mono", Menlo, Monaco, Consolas, monospace;
}
```

Required UI components:

- sticky side navigation
- TL;DR banner
- stat cards
- concept/package cards
- SVG architecture diagram
- flow/state-machine diagram
- collapsible `<details>` blocks
- issue/task cards
- code/diff blocks where useful

### Inline code styling

Avoid oversized inline code chips. Use tight inline tokens:

```css
code {
  display: inline-block;
  white-space: nowrap;
  font-family: var(--mono);
  font-size: 0.88em;
  line-height: 1;
  background: var(--g100);
  border: 0;
  padding: 0 .18em;
  border-radius: 2px;
  letter-spacing: 0;
  vertical-align: -0.04em;
  box-decoration-break: clone;
  -webkit-box-decoration-break: clone;
}
```

If inline code appears inside flex list items, wrap the text portion in a child `<div>`:

```html
<li><span class="ico-ok">1</span><div>Read <code>README.md</code> first.</div></li>
```

Do not write:

```html
<li><span class="ico-ok">1</span> Read <code>README.md</code> first.</li>
```

For dark cards, override code color/background:

```css
.dark-card code,
.tldr code,
.rec-card code {
  color: var(--ivory);
  background: rgba(255,255,255,0.12);
}
```

For sidebar key-file paths, override inline-code no-wrap rules so long paths do not overflow into the main content:

```css
.nav-files code {
  display: block !important;
  white-space: normal !important;
  word-break: break-all;
  overflow-wrap: anywhere;
  max-width: 100%;
  background: transparent;
}
```

## Workflow

1. Clarify target repo and output destination if ambiguous.
2. Load user profile if personalization is requested or implied.
3. Inspect repo structure, docs, tests, commands, git history, and issues.
4. Build a concise architecture model.
5. Identify contribution bar and local dev loop.
6. Decide whether first issues/tasks are needed for this artifact; include them only when useful.
7. Generate the HTML artifact under `~/org/wiki/`.
8. If including first issues/tasks, provide concrete step-by-step plans. If not including issues, provide a first reading path only when useful.
9. If browser/screenshot tooling is available, render-check the top and at least one lower section for layout issues.
10. Return only the file link plus a short summary of what was included.

## Quality bar

The artifact should be useful to a real contributor on day one. It must answer:

- Where do I start reading?
- How is the codebase shaped?
- What history explains the weird parts?
- What conventions will get my PR rejected if I miss them?
- How do I run checks/tests safely?
- How do I debug the main execution path?
- If I want to contribute next, what issue, starter task, or reading path should I attempt and why?

Do not produce a generic architecture summary. Use concrete file names, commands, function names, and issue numbers from the target repo.
