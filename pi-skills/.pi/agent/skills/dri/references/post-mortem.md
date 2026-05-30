# DRI Post-Mortem Playbook

Trigger: `/dri post-mortem <slug>` — when a rollout (or other project event) causes a customer-visible incident or significant internal disruption.

Reference: SoSafe Internal SMTP post-mortem (page `3822289020`), MSG-625 post-mortem (page `3810983954`).

Goal: a structured, factual write-up that drives prevention, not blame.

## When to write one

- Customer-visible regression caused by a release / migration / rollout step
- Internal incident with > 15-min impact on a critical path
- Near-miss that would've been a P1 if the flag had been at 100%
- Any rollback that was triggered by a real-world failure (not just a precaution)

If unsure → write it. Cheap to do, expensive to skip.

## Steps

1. **Read state.** `project.md`, `decisions.md`, `status-log.md`. If the incident has a Jira `IM-*` or `CSP-*` ticket, fetch it via `mcp__MCP_DOCKER__jira_get_issue` for the timeline.

2. **Gather facts via AskUserQuestion** (one batch):
   - Date, severity, current status (mitigated / fixed / monitoring)
   - One-sentence summary of customer-visible impact
   - Numbers: how many customers / sends / requests / users affected
   - Timeline highlights (when started, when detected, when mitigated, when fixed)
   - Detection channel (customer ticket / Slack / monitor / ?)
   - Mitigation taken (rollback / flag flip / hot fix / per-customer exclusion)
   - Suspected or confirmed root cause

3. **Draft using the SoSafe structure** (mirror the Internal SMTP post-mortem):

```markdown
# Post-Mortem: <Project name> — <one-line headline>

**Date:** <YYYY-MM-DD>
**Severity:** <Low / Medium / High / Critical>
**Status:** <Mitigated / Fixed / Monitoring / Closed>
**Author:** <DRI name>

## Summary

<3–6 sentences. What broke, who was affected, how big, what was bounded. End with the mitigation status.>

## Quick reference

| Question | Answer |
|---|---|
| How many customers / events affected? | <number + per-customer / per-campaign breakdown link> |
| Were other paths / channels affected? | <yes/no + why> |
| Levels of customer-visible impact? | <e.g. delivered with wrong content / bounced / quarantined> |
| Why did it happen on the new path? | <one-sentence root cause> |
| What's missing? | <missing field / check / test> |
| How do we validate this in the future? | <gating tests / monitors> |
| What type of change was this? | <progressive flag rollout / direct deploy / config change> |

## Impact Summary

- Total affected: <number>
- Total customers / users: <number>
- Breakdown: <delivered / bounced / failed / etc.>

### What was affected

<List: which feature / template / endpoint / customer segment.>

### What was NOT affected

<Equally important — bounds the blast radius. Other adapters / channels / customer types that were safe.>

## Timeline

All times <TZ>. Per-customer or per-component timelines as sub-sections if needed.

| Date / Time | Event |
|---|---|
| <YYYY-MM-DD HH:MM> | <event> |

## Detection

- **Channels:** <how it surfaced — ticket, Slack, monitor>
- **Confirming signal:** <what made it certain>
- **Why no automated alert fired:** <if applicable>
- **Why human triage took N hours/days:** <process gap, no blame>

## Mitigation

- <First-line fix: e.g. flag to 0%, per-customer exclusion>
- <Why this works>

## Proposed fix

<Code-level fix. If multiple components must change together, list them as a coordinated set — "all three must land together".>

## Verification (pre-resume checklist)

Before re-enabling / re-rolling out:

1. <Contract test on the missing field>
2. <Parity test old-vs-new>
3. <Receiver / consumer-side smoke test>
4. <Staging end-to-end with a previously-affected case>

## Prevention measures

1. <Test that would've caught it — usually a parity or contract test>
2. <Monitoring / alerting gap to close>
3. <Rollout-process change — gating, triage rule, register>
4. <Org / routing change — auto-routing tickets to active-rollout owners, etc.>

## Lessons learned

- <One short paragraph per lesson. Frame as "principles for the next migration", not "we should have…".>
- <Generalisable lessons travel further than incident-specific ones.>
```

4. **Append a record to `decisions.md`** with type `post-mortem`, linking the published page or this draft.

5. **Update `risks.md`** — close the risk that fired, capture any *new* risks the incident surfaced.

6. **Set `project.md` `status:` to `paused`** if the rollout is on hold pending the fix; back to `rollout` once the verification checklist passes.

7. **Next action.** Usually: "Share the draft with TL + dependent-team owners for review before publishing. Once published, post a 1-paragraph summary to `#proj-<slug>` linking the doc."

## Tone

- **Blameless on people, specific on systems.** "The contract didn't carry envelope.from" is fine; "Bob forgot to add envelope.from" is not.
- **Numbers over adjectives.** "88,039 sends across 928 customers" beats "many".
- **What was NOT affected** is as important as what was. Bounds the scope explicitly so readers don't over-react.
- **Lessons should generalise.** Not "next time check envelope.from" but "map every implicit behaviour of an old path to an explicit field on the new contract".
- **No confidence theatre.** If detection took 4 days, say so and explain why. The prevention measures are the answer to "how do we make this faster next time".

## On `--publish`

If user explicitly asks to publish:

1. Confirm space (default `PEN`).
2. Confirm parent — usually under the project poster page or the team's Post-Mortems parent.
3. Title format: `Post-Mortem: <Project name> — <one-line headline>` (mirrors existing examples).
4. After publishing, link the page from the project poster's Resources block (offer to update the poster).
