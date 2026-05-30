# DRI Decision Playbook

Trigger: `/dri decision <slug>` — when the project is stuck on a choice and the DRI needs to call it.

Doctrine: the DRI owns the call when (a) multiple valid options exist, (b) consensus can't be reached, or (c) a trade-off must be made between speed/quality/scope. Document the rationale.

## Steps

1. **Read state.** `project.md` and current `risks.md`. If `--sync` or stale, refresh.

2. **Frame the decision via AskUserQuestion** (single batched question):
   - What's the question? (1 sentence)
   - What options are on the table? (2–4)
   - Who has weighed in, and what did they prefer?
   - What's the constraint forcing a decision now? (deadline / blocker / cost)

3. **Sanity check first — should this be escalated, not decided?**

   Push to escalation if any are true:
   - Decision has company-wide impact (security, compliance, public API contract)
   - Resource / capacity is the constraint, not the technical choice
   - Cross-team conflict where the DRI lacks authority
   - You don't have enough information and no one knows where to get it

   If yes → tell the user to run `/dri escalate <slug>` instead, with a draft pointing to this decision.

4. **Walk the options.** For each option, output:
   - Pros (1–3 bullets)
   - Cons (1–3 bullets)
   - Reversibility (cheap to undo / expensive / one-way door)
   - Who it makes happy / unhappy

5. **Make the call.** Don't punt. Pick one option, state the reason in 1–2 sentences, and name the trade-off explicitly: "We're choosing B. We accept slower initial throughput in exchange for a one-way migration we can do in a single weekend."

6. **Output the decision record** (ADR-lite). Append to `decisions.md`:

```markdown
## 2026-05-08 — <decision title>

**Status:** Decided
**Decided by:** <DRI name> (DRI), with input from <names>
**Context:** <2–4 sentences on what we're solving and why now>
**Options considered:**
- A — <one-line summary>
- B — <one-line summary>
- C — <one-line summary>

**Decision:** <Option chosen + 1-sentence why>

**Trade-offs accepted:** <what we give up>

**Reversibility:** <cheap / costly / one-way>

**Revisit if:** <new info that would re-open this>

**Linked:** <ADR/TDD url, jira ticket, slack thread>
```

7. **Communication.** Output a 3-line Slack message draft for `#proj-<slug>` announcing the decision:

```
🧭 Decision: <title>
We discussed A, B, C and are going with **B** — <1-sentence reason>.
Trade-off: <what we accept>. Full rationale: <link or thread>.
```

8. **Next action.** Usually: "Post the decision to `#proj-<slug>`. If it should live somewhere durable, ask whether to link it from the Confluence poster (I can update it on confirm)."

## When DRIs hesitate

Common patterns and how to push back:

- "I want everyone to agree first." → That's the consensus trap. Decisions can be wrong; indecision is always wrong if the project is stalled. Pick, document, revisit.
- "I don't have authority." → If it's project-scoped, you do. If it's truly cross-cutting, escalate — but say so explicitly, don't just delay.
- "I need more data." → Name the data, name who can produce it, name the deadline. If those don't exist, you have enough.
- "What if I'm wrong?" → Note reversibility. For two-way doors, decide fast and adjust.

## On weight + format

- Lightweight reversible call → a Slack message + appended decisions.md entry is enough.
- Architectural / long-lived → ask the user if a full ADR or TDD belongs in the repo / Confluence. Offer to draft it.
- Compliance-touching → flag for Team Lead review before publishing.
