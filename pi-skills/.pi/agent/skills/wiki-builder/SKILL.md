---
name: wiki-builder
description: Build and maintain a personal LLM-readable wiki from agent session research and discussion. Use when asked to "wiki this", "wikify", "add to wiki", "save to wiki", "extract to wiki", "build wiki page", "update the wiki", "remember this in wiki", or "make a wiki entry". Produces flat markdown pages with frontmatter (tags, sources, related links) suitable for feeding back into future LLM context as a personal knowledge base (Karpathy-style LLM wiki). Targets `~/org/wiki/` for global notes or `<repo>/docs/wiki/` for project-scoped notes.
---

# Build a Personal LLM Wiki from Agent Sessions

## Scripts

All scripts live in `scripts/` alongside this file. Replace `<skill-dir>` with the absolute path to this skill's directory, and `<wiki-root>` with the wiki path (default `~/org/wiki`).

| Script | Purpose | When to use |
|---|---|---|
| `scaffold.py <slug> <title> [tags]` | Create blank page with frontmatter | Start of Step 4 |
| `find-related.py <keyword> [...]` | Find pages by keyword, output slugs | Steps 3 & 6 |
| `reindex.py` | Regenerate index.md | After every page write/update |
| `validate.py` | Check all pages against checklist | End of every session |

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
python3 <skill-dir>/scripts/find-related.py <concept-keyword> [keyword2] --wiki <wiki-root>
```

This returns matching slugs, titles, tags, and matched lines — ready to paste into `related:` fields. Fall back to `rg -l` for raw text search if needed.

For each atomic concept:
- **Existing page on same topic** → update mode (Step 5)
- **Existing related pages** → record their slugs to add to `related:` field
- **No matches** → new page mode (Step 4)

Also scan for pages that should now link **to** the new page, and update them in Step 6.

## Step 4: Write a new page

Scaffold the file first, then fill it in:

```bash
python3 <skill-dir>/scripts/scaffold.py <slug> "<Title>" tag1,tag2 --wiki <wiki-root>
```

This writes the page with correct frontmatter and today's dates. Then fill in the body. Template:

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
python3 <skill-dir>/scripts/find-related.py <slug-keyword> --wiki <wiki-root>
```

Update only pages where the link is genuinely useful — don't link-spam.

## Step 7: Regenerate the index

```bash
python3 <skill-dir>/scripts/reindex.py <wiki-root>
```

Reads all `*.md` frontmatter and lead lines, writes `index.md` grouped by tag then alphabetically. Always run this after any page is created or updated.

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

```bash
python3 <skill-dir>/scripts/validate.py <wiki-root>
```

Checks all pages for:
- Required frontmatter fields (title, slug, tags, created, updated, sources)
- Valid ISO dates
- At least one source
- All `related:` slugs resolve to existing files
- Index coverage (warns on unlisted pages)
- Mega-pages (>400 lines)

Exits 0 if clean. Fix any errors before reporting done.

Still check manually: no two pages cover the same atomic concept (would-be duplicates were merged).
