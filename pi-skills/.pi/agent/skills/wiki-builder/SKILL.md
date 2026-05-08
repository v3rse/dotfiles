---
name: wiki-builder
description: Build and maintain a personal LLM-readable wiki from agent session research and discussion. Use when asked to "wiki this", "wikify", "add to wiki", "save to wiki", "extract to wiki", "build wiki page", "update the wiki", "remember this in wiki", or "make a wiki entry". Produces flat markdown pages with frontmatter (tags, sources, related links) suitable for feeding back into future LLM context as a personal knowledge base (Karpathy-style LLM wiki). Targets `~/org/wiki/` for global notes or `<repo>/docs/wiki/` for project-scoped notes.
---

# Build a Personal LLM Wiki from Agent Sessions

Extract durable knowledge from a session (research findings, tool comparisons, design decisions, learned patterns, debugging conclusions) and save it as atomic markdown pages in a wiki. The wiki is the **persistent layer** above ephemeral agent chats — small enough to grep, structured enough to feed back into LLM context.

## Core principles

- **One page per concept, not per session.** A single research session typically produces 2–5 pages. A page about "git worktrees with coding agents" is reusable; a page called "session-2026-05-08-notes" is not.
- **Pages are LLM-readable units.** Each page must stand alone: a future LLM context-loaded with just this one file should understand the topic.
- **Flat structure, plain markdown.** No nested folders by default. `rg` is the index. Frontmatter carries metadata. Cross-links are relative markdown links.
- **Merge, don't duplicate.** If a relevant page already exists, update it (append new info, bump `updated`, add sources) instead of creating a near-duplicate.
- **Sources are mandatory.** Every claim should be traceable back to a URL, a session, a commit, or a file path.

## Step 1: Determine wiki scope

Decide where the wiki lives. Ask the user only if ambiguous.

| Signal | Scope | Path |
|---|---|---|
| User says "global", "personal", "my wiki" | Global | `~/org/wiki/` |
| User says "project", "in this repo", "project wiki" | Project | `<repo-root>/docs/wiki/` |
| User gives no signal but cwd is inside a git repo with existing `docs/wiki/` or `.wiki/` | Project | that path |
| User gives no signal and content is project-specific (refers to this repo's code) | Ask | confirm global vs project |
| User gives no signal and content is general (tools, patterns, external research) | Global | `~/org/wiki/` |

Resolve `<repo-root>` with `git rev-parse --show-toplevel`. Create the directory if it doesn't exist.

If creating for the first time, also seed:
- `README.md` — human entry point (see template below)
- `index.md` — auto-maintained tag/page index

## Step 2: Identify atomic concepts

Read the source material (current session, named session file, or explicit content the user pasted). Extract **atomic concepts** — discrete topics each worth their own page.

Good atomic concepts (one page each):
- "Parallel coding agent workflow"
- "Git worktrees for parallel agents"
- "tmux pipe-pane logging for session search"
- "Conductor vs Crystal vs Vibe Kanban (orchestration shapes)"

Bad atomic concepts (too narrow or too broad):
- "Tuesday's research session" (not a concept)
- "All AI coding tools" (too broad, split it)
- "That one comment Simon made" (too narrow, fold into a broader page)

For each concept, decide a **slug**: kebab-case, descriptive, stable. The slug is the filename (`<slug>.md`) and the canonical id used in `related:` links.

## Step 3: Check for existing pages

Before writing anything new:

```bash
ls <wiki-root>/*.md
rg -l "<concept-keyword>" <wiki-root>/
```

For each atomic concept:
- **Existing page on same topic** → update mode (Step 5)
- **Existing related pages** → record their slugs to add to `related:` field
- **No matches** → new page mode (Step 4)

Also scan for pages that should now link **to** the new page, and update them in Step 6.

## Step 4: Write a new page

Create `<wiki-root>/<slug>.md` using this template:

```markdown
---
title: <Human-readable Title>
slug: <slug>
tags: [tag1, tag2, tag3]
created: YYYY-MM-DD
updated: YYYY-MM-DD
sources:
  - <url, file path, or session reference>
related:
  - <other-slug>
---

# <Human-readable Title>

> <One-sentence lead. This is what an LLM sees first — make it count.>

## Summary

<2–4 sentences that fully convey the concept. Someone reading only this section should grasp the key idea.>

## Key points

- <Bullet — the most important takeaway>
- <Bullet — second key takeaway>
- <Bullet — third>

## Details

<Longer body. Include code blocks, tables, examples. Be concrete. Cite sources inline as needed.>

## See also

- [<Related Page Title>](<related-slug>.md)

## Sources

- <URL with title>
- <Session: path or id>
- <File: path:line>
```

**Tag conventions:**
- Use lowercase, kebab-case tags: `ai-coding`, `workflow`, `tooling`, `git`, `tmux`
- 2–5 tags per page; first tag is the broadest category
- Reuse existing tags — list current tags via `rg "^tags:" <wiki-root>/*.md` before inventing new ones

**Linking conventions:**
- Markdown links to relative `.md` files: `[Git Worktrees](git-worktrees.md)`
- Don't use wiki-style `[[...]]` (less portable to LLM ingestion and other tools)
- Bidirectional: if A links to B, add A to B's `related:` in Step 6

## Step 5: Update an existing page

When merging into an existing page:

1. Read the current page in full.
2. Identify what's genuinely new — don't restate what's already there.
3. Append new bullets to **Key points** or new sections to **Details**.
4. Add new entries to `sources:` (de-duplicate URLs).
5. Update `updated:` to today's date. **Never change `created:`.**
6. Add new tags only if they capture a real new dimension; don't sprawl the tag set.
7. If the new content meaningfully shifts the framing, rewrite the **Summary** to reflect the merged understanding — but keep it tight.

## Step 6: Update cross-links

After writing or updating pages, update bidirectional links:

- For each page touched, ensure every page it lists in `related:` reciprocally lists it.
- For each new page, scan existing pages for natural backlinks and add them.

```bash
# Find pages that mention the new concept by keyword
rg -l "<keyword>" <wiki-root>/*.md
```

Update only pages where the link is genuinely useful — don't link-spam.

## Step 7: Regenerate the index

Rewrite `<wiki-root>/index.md` so the wiki has one canonical, LLM-readable table of contents.

Format:

```markdown
# Wiki Index

Last updated: YYYY-MM-DD · <N> pages

## By tag

### ai-coding
- [Parallel Coding Agents](parallel-coding-agents.md) — embracing the parallel agent lifestyle
- [Tmux Workflow for Agents](tmux-workflow-for-agents.md) — tmux + worktrees for parallel sessions

### workflow
- ...

## All pages (alphabetical)

- [Git Worktrees](git-worktrees.md)
- [Parallel Coding Agents](parallel-coding-agents.md)
- ...
```

Build it by reading frontmatter from every `<wiki-root>/*.md` (excluding `README.md` and `index.md` itself). The one-line description after the `—` is the page's lead line (the `>` blockquote under the title).

## Step 8: Seed README.md (first-time only)

If `<wiki-root>/README.md` doesn't exist yet, write:

```markdown
# Wiki

Personal LLM-readable knowledge base. Plain markdown, flat structure, one page per concept.

- [Index](index.md) — all pages by tag
- Pages live as `<slug>.md` in this directory
- Each page has frontmatter: `title`, `tags`, `created`, `updated`, `sources`, `related`
- To feed into an LLM: cat the relevant pages, or load `index.md` to find what to read

Built and maintained via the `wiki-builder` skill.
```

Do not regenerate this file on subsequent runs.

## Step 9: Report

Tell the user concisely:
- Which pages were created (with paths)
- Which pages were updated (with what was added)
- Any cross-links added
- Whether the index was regenerated

Example:
```
Wiki updated at ~/org/wiki/

Created:
  - parallel-coding-agents.md (tags: ai-coding, workflow)
  - tmux-workflow-for-agents.md (tags: tmux, ai-coding, workflow)

Updated:
  - git-worktrees.md (added incident.io `w` helper, +1 source)

Cross-links: parallel-coding-agents ↔ tmux-workflow-for-agents ↔ git-worktrees
Index regenerated (12 pages total).
```

## Anti-patterns to avoid

- **Session-as-page.** Don't create `2026-05-08-research.md`. Extract the concepts.
- **Mega-pages.** If a page exceeds ~400 lines or covers >1 concept, split it.
- **Empty stubs.** Don't create a page with just frontmatter and "TODO". Either write it now or note the topic in an existing page's "See also" with a comment.
- **Tag sprawl.** Resist inventing new tags. Reuse first.
- **Lossy summaries.** The page must contain the actual knowledge, not just a pointer to "go read the session."
- **Unsourced claims.** Every page needs at least one entry in `sources:`.
- **Renaming slugs.** Slugs are stable ids. If a title needs to change, update `title:` but keep the slug (or do a proper rename: update the file, every `related:` reference, and every link in other pages' bodies).

## Validation

Before finishing, verify:

- [ ] Every new/updated page has all required frontmatter fields
- [ ] `created` and `updated` are valid ISO dates (YYYY-MM-DD)
- [ ] Every page has at least one entry in `sources:`
- [ ] Every `related:` slug points to a file that exists
- [ ] `index.md` lists every `.md` page (except `README.md` and `index.md`)
- [ ] No two pages cover the same atomic concept (would-be duplicates were merged)
