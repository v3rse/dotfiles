# DRI Kickoff Playbook

Trigger: `/dri kickoff <name>` (or "<name> --sync" if state exists).

Goal: get the project from "vague intent" to "scope-clear, owners-named, channels-live" with as little overhead as possible.

## Steps

1. **Slug + bootstrap.** Compute slug from name (lowercase, hyphenated). Create `~/notes/dri/<slug>/` with empty `decisions.md`, `status-log.md`, `risks.md`, `dependencies.md`. If the dir already exists, switch to gap-fill mode (only fill what's missing).

2. **Pull from the roadmap first.** Most projects come from the current quarter's roadmap (see SKILL.md "Where projects come from"). Search Confluence for the project name; if found, fetch the entry and pre-fill: problem, scope, success criteria, ETC, dependencies, owning team. Show the user what was inherited and ask them to confirm/edit.

3. **Gather missing facts via AskUserQuestion** (one batched question, max 4 sub-questions). Only ask what wasn't already in the roadmap entry or prompt:
   - Problem statement & success criteria (1 sentence each)
   - ETC (Estimated Time of Completion) — date, range, or "End of Q<N>"
   - Team Lead and DACI involvement (who's Accountable, who's Contributor, who's Informed)
   - Existing Slack channel? Jira epic? Confluence poster? (or create them?)

4. **Identify dependencies explicitly.** This is the kickoff step engineers most often under-do. For each milestone, ask:
   - Which other teams must do something for this to succeed?
   - For each: a one-line ask, an owner contact (or "TBD"), and a `need by` date.
   - Seed the dependency table in `dependencies.md` (state: `identified`).
   - Recommend running `/dri deps <slug>` next to draft the initial commitment requests — concrete asks beat "we'll loop them in later".

3. **Draft the project poster.** Use the template below. Output as markdown the user can paste into Confluence. Do NOT publish unless user passes `--publish` or confirms. If publishing is asked, use `mcp__MCP_DOCKER__confluence_create_page` under the appropriate space (ask for space key if unknown).

4. **Propose Jira epic structure.** A single epic with 3–6 milestone tickets is the default. Output as a checklist for user confirmation; only call `jira_create_issue` after they approve. Map each milestone to a named work stream owner.

5. **Slack channel.** Suggest `#proj-<slug>` if not already set. Output the suggested channel topic + an opening-message draft. Don't post — user does.

6. **Write `project.md`** with the gathered frontmatter + narrative sections:
   - Problem
   - Success criteria
   - Scope (in / out / non-goals)
   - Milestones
   - Stakeholders
   - Risks (initial)
   - Communication plan (channel, cadence, audiences)

7. **Kickoff checklist** — output to user with status (✅ done, ⬜ todo) for each item:
   - [ ] Project pulled from / linked to roadmap entry
   - [ ] Problem & success criteria written
   - [ ] Scope confirmed with product/business (in / out / non-goals)
   - [ ] Involved Teams populated (DACI matrix)
   - [ ] Dependencies surfaced — every dep has a team, owner, ask, and need-by date
   - [ ] Initial dependency asks sent (or scheduled via `/dri deps <slug>`)
   - [ ] Team Lead aligned on timeline
   - [ ] Slack `#proj-<full-name>` channel created + topic set + poster pinned
   - [ ] Jira epic + milestone tickets created, owners assigned
   - [ ] Project poster published (Confluence)
   - [ ] First "kickoff" status update posted to `#proj-*`

8. **Next action.** End with the single most important next step. Usually it's *not* "post the status update" — it's `/dri deps <slug>` to send the initial commitment asks to dependent teams. Dependencies left to slip turn into rollout-time crises; ask early.

## Project poster template (SoSafe MSG / PEN convention)

This is the team's canonical structure — match it. Reference examples: Internal SMTP migration (page `3674210462`), Lingo.dev Adoption Project Poster (page `3711270913`).

```markdown
# <Project name>

## Overview

|  |  |
|---|---|
| **Status** | in progress / on hold / done — colour: 🟢 / 🟡 / 🔴 |
| **DRI** | @<handle> |
| **Estimated Time of Completion** | <e.g. "End of Q2", "May 22, 2026"> |
| **Resources** | • [Epics](<jira link to epic / label filter>)<br>• [<Vision & Strategy / related Confluence>]()<br>• [#proj-<channel>](<slack link>) |

## Problem Statement

<2–4 sentences. What's wrong / what opportunity, who is affected, why now. End with the cost of *not* doing this.>

## Scope

### What's in scope?

- <Concrete deliverable 1>
- <Concrete deliverable 2>

### What's out of scope?

- <Explicit non-goal 1>
- <Explicit non-goal 2>

## Success Criteria

| **Criteria / Metrics / KPI** | **Expected Outcome** |
|---|---|
| <metric> | <measurable target> |
| <metric> | <measurable target> |

## Involved Teams

Use **DACI** (Driver, Accountable, Contributor, Informed) — not RACI.

| **Team** | **DACI** | **Owner** | **Why** |
|---|---|---|---|
| <DRI's team> | Driver | @<DRI> | <responsibilities> |
| <accountable team> | Accountable | @<TL or sponsor> | <why> |
| <contributing team> | Contributor | @<owner> | <what they contribute> |
| <dependent / affected team> | Informed | TL or @<handle> | <why they need to know> |

### Feedback Loops

| **Stakeholder** | **Review Cadence** | **Feedback Notes & Actions** |
|---|---|---|
| <name / team> | <weekly / bi-weekly / ongoing via #proj-*> | <what they're tracking> |

## Milestones

| **Milestone Summary** | **Details** | **Dependencies** | **ETC** |
|---|---|---|---|
| <name> | <linked Jira epic/ticket> | <other team or none> | <date or range> |

## Risks & Mitigation

| **Risk** | **Level of Impact** | **Mitigation Plan** |
|---|---|---|
| <risk> | HIGH 🔴 / MEDIUM 🟡 / LOW 🟢 | <plan — usually "rollback path" or "feature flag" or "exclude from migration"> |
```

### Notes on the SoSafe convention

- **DACI, not RACI.** SoSafe uses Driver/Accountable/Contributor/Informed.
- **ETC is the term.** Not "target date". Acceptable values include "End of Q2", a date, or a range.
- **Status uses Confluence status macros with colour** when published — `in progress` (yellow), `done` (green), `on hold` (red). In the markdown draft, use 🟢/🟡/🔴 emoji and the team will swap to macros at publish time.
- **DRI handle goes in Overview, not as a header field.** Use `@<handle>` (Atlassian user mention).
- **Resources block is the single source of links** — Jira epic(s), Vision & Strategy doc, Slack channel. Not split across sections.
- **Slack channel naming** is `#proj-<full-kebab-case-name>`, not the abbreviated slug. Example: `#proj-migrate-internal-smtp-traffic` (not `#proj-msg-internal-smtp`).
- **Project catalog & roadmap.** Before writing the poster, check whether the project is already in:
  - [INTERNAL] Messaging Platform Project Candidates (page `3234103381`) — the team's catalog
  - Current quarter roadmap (e.g. "Roadmap - Messaging Platform Q2 2026", page `3651109029`)
  - If it's there, link to it from the poster Resources block; if not, propose adding it.

## Anti-patterns to flag during kickoff

- Vague success criteria ("improve performance") → push for measurable.
- No named owners on milestones → don't proceed without them.
- "We'll figure stakeholders out later" → no, surface them now even if incomplete.
- Skipping Confluence poster because "the team knows" → poster is for stakeholders 6 weeks from now who don't.

## On `--publish`

If user explicitly asks to publish the poster:

1. Confirm space key. For Messaging Platform / Platform Engineering work the default is `PEN`.
2. Confirm parent page — usually under the team's quarterly roadmap or a "Projects" parent.
3. Call `mcp__MCP_DOCKER__confluence_create_page` with title `<Project name>` (no "Project Poster" suffix — see existing examples like "Migrate Internal SMTP Traffic to New Mail Platform", "Lingo.dev Adoption Project Poster").
4. Save returned page id/url to `confluence_poster` in `project.md` frontmatter.
5. Show the live URL.

## Bootstrapping from existing artifacts

If the user references an existing roadmap entry, problem-statement register, or vision doc, fetch and read them first via `mcp__MCP_DOCKER__confluence_get_page` and pre-fill what's already known. Don't re-ask the user for facts that exist. Surface what was inherited so they can confirm.
