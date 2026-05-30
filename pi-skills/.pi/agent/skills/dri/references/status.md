# DRI Status Update Playbook

Trigger: `/dri status <slug>`.

Goal: produce a Slack-ready bi-weekly status update that the engineer pastes into `#proj-<slug>`.

## Steps

1. **Read state.** `project.md`, last 3 entries of `status-log.md`, current `risks.md`.

2. **If `last_synced` > 7 days** or user passed `--sync`, refresh Jira epic state via `mcp__MCP_DOCKER__jira_get_issue` and `jira_get_project_issues` to capture new tickets / status changes. Don't sync silently — tell the user what changed.

3. **Detect status colour.** Use the SoSafe scheme:
   - 🟢 On Track — no risks, ahead of or on milestones
   - 🟢🟡 On Track (risk identified) — minor risk, mitigations in flight
   - 🟡 At Risk — timeline / scope drift likely; mitigation needed
   - 🔴 Blocked — work cannot progress without external action

   Don't downgrade reality. If two updates ago was 🟡 and nothing changed, it's still 🟡 — say so.

4. **Compose using the SoSafe template** (verbatim structure):

```
🟢 🟡 🔴 Status: On Track / At Risk/Delayed / Blocked/Major Issue
<Upon status change, describe the reason in a few words>
⏳ Progress
- <what's happened since last update>
➡️ Next steps
- <what happens next>
⚠️ Risks / Blockers
- <what's blocking or risky, if any>
🙋 Asks / Escalations
- <help needed or decision required, if any>
```

Drop sections that genuinely don't apply (no risks → omit risks line). Keep it tight: 5–10 bullets total across the whole update.

5. **Output the draft** in a code block ready to paste. Above it, give a 1-line summary: "Drafted update for #proj-<slug>; status is 🟡 (target slipped 1w due to dep X)."

6. **Append to `status-log.md`** with today's date as a heading, the full draft below. Format:

```markdown
## 2026-05-08

🟡 Status: At Risk
...
```

7. **Update `project.md`** if status colour changed (set `status:` field accordingly: kickoff/execution/rollout etc., and add a `current_health: green|yellow|red` field if not present).

8. **Next action.** Suggest: "Paste into `#proj-<slug>`. If 🟡 or 🔴 and no recent escalation, run `/dri escalate <slug>` next."

## Cadence rules (from SoSafe doctrine)

- Bi-weekly minimum. Weekly when at risk or blocked.
- Even "no progress" deserves an update — silence implies "everything is fine".
- Don't wait until you have good news. Stakeholders prefer slow truth to late surprise.

## Examples (concise reference)

**On track:**

```
🟢 Status: On Track
⏳ Progress
- Upgraded cluster rds-pl-production with Learning team support.
➡️ Next steps
- Move to rds-simulation-production. Cut-over aligned with SIM team for 2026-05-12.
```

**At risk:**

```
🟡 Status: At Risk
<Dependency X unclear, slipping ~1w>
⏳ Progress
- Upgrade work paused pending dep clarification.
➡️ Next steps
- Talking to teams A, B, C this week to resolve.
⚠️ Risks / Blockers
- Dep X owner TBD.
🙋 Asks / Escalations
- Will escalate to TL if unresolved by Wed.
```

**Blocked, no progress:**

```
🔴 Status: Blocked
⏳ Progress
- Still blocked on dep X. All relevant teams aware, working solution.
🙋 Asks / Escalations
- Stakeholders informed of further delay; will share new ETA once unblocked.
```

## When NOT to use this subcommand

- Right after kickoff with nothing to report yet → use `/dri kickoff` to draft the kickoff post instead.
- Mid-rollout going-live announcement → use `/dri rollout` for the launch comms.
