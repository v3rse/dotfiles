# DRI Rollout / Finalisation Playbook

Trigger: `/dri rollout <slug>` — when approaching go-live or actively rolling out.

Doctrine (rollout phase):
- Rollout/rollback plan in place
- Go-live readiness confirmed
- Operational ownership clear
- Documentation exists and is discoverable
- Follow-ups / tech debt captured and visible
- Learnings collected
- Org knows what changed

## Steps

1. **Read state.** `project.md` (esp. milestones + target_date), `risks.md`, latest `status-log.md`. Refresh Jira epic if stale.

2. **Run the readiness checklist.** Output ✅ / ⚠️ / ❌ with a reason for each.

   **Rollout plan**
   - [ ] Sequence written down (steps + order)
   - [ ] Rollback plan exists, has been mentally walked through
   - [ ] Cut-over window agreed with dependent teams
   - [ ] Communication plan for D-day (who announces what, where)

   **Go-live readiness**
   - [ ] All P0 acceptance criteria met
   - [ ] Monitoring / alerts in place for the new path
   - [ ] On-call / oncall rotation knows about the change
   - [ ] Feature flag (or equivalent kill switch) exists if rollback isn't trivial

   **Operational ownership**
   - [ ] Long-term owning team confirmed (and they know)
   - [ ] Runbook delta written or pointer exists
   - [ ] Pager routing updated if needed

   **Documentation**
   - [ ] User-facing docs updated (if applicable)
   - [ ] Internal architecture docs / Confluence updated
   - [ ] ADR / TDD landed for any architectural decisions

   **Cleanup tracking**
   - [ ] Tech debt + follow-ups captured as Jira tickets, linked to epic or backlog
   - [ ] Old code / config deletion tracked (with date) if applicable

3. **Identify gaps.** For each ❌ or ⚠️, propose the smallest concrete action that moves it to ✅.

4. **Draft the go-live announcement** (Slack `#proj-<slug>` + wider channels):

```
🎉 <Project name> — going live <date> at <time TZ>

**What's changing:** <1–2 sentences>

**Who's affected:** <users / teams / systems>

**Watch for:** <expected impact, any temporary side effects>

**Owning team going forward:** <team name + slack channel>

**Rollback plan:** <link or 1-line summary>

**Docs:** <link>

Ping <DRI handle> in this thread with anything weird.
```

5. **Draft the post-launch comms.** Two artifacts:

   **a. Internal "what happened" post (24–72h after launch):**

   ```
   📦 <Project name> — shipped <date>

   **Status:** stable / monitoring / minor issues
   **Metrics:** <numbers if available — usage, latency, error rate>
   **Wins:** <what went well>
   **Surprises:** <what we learned>
   **Follow-ups:** <linked tickets>

   Big thanks to <names>.
   ```

   **b. Closing entry in `status-log.md`** (status = done):

   ```markdown
   ## 2026-05-08 — Closeout

   🟢 Status: Done
   ⏳ Progress
   - Shipped <date>. Migration of <scope> complete.
   ➡️ Next steps
   - Owning team: <team>. Tech debt tracked: <link>.
   - Retrospective scheduled: <date or "async in #proj-*">.
   ```

6. **Retrospective prompt.** Async-friendly template, posted to `#proj-<slug>`:

```
🪞 <Project name> — async retro

What went well? (one-liners welcome)
What didn't? (be specific, no blame)
What would we do differently next time?
What follow-ups should we capture as tickets?

Reply in thread by <date>; I'll summarise.
```

7. **Update state.**
   - Set `status: rollout` in `project.md` frontmatter when starting rollout, `status: done` once shipped + retro complete.
   - Append a "Closeout" decision to `decisions.md` summarising what shipped, key trade-offs, who owns it now.

8. **Next action.** Usually: "Post the go-live announcement once the readiness gaps are closed. After launch, run `/dri rollout <slug>` again to draft the post-launch comms + retro."

## Anti-patterns at rollout

- **"We'll write the runbook later"** → no. The launch is the moment ownership transfers; if the receiving team can't operate it, you haven't shipped, you've punted.
- **Silent ship** → big launches deserve announcements. Small ones still deserve a closing status update so stakeholders know it's done.
- **Skipping retro because "it went fine"** → the easy ones teach the most. 15 mins async is enough.
- **Leaving cleanup unticketed** → if it's not in Jira, it doesn't exist; old code rots and the next DRI inherits the mess.
- **DRI staying on as permanent owner by default** → name the long-term owner explicitly. DRI role ends; ownership transfers.

---

## SoSafe MSG rollout patterns

These are the team's actual practices (reference: Internal SMTP migration, page `3674210462`; post-mortem page `3822289020`).

### Progressive feature-flag rollout

Default rollout shape for any platform-level migration:

| Step | % | Gate before next step |
|---|---|---|
| Phase 1 — Pilot | 1% | 24h clean: no error-rate change, parity tests still passing, no customer reports |
| Phase 2a | 5% | Same gates + 24h |
| Phase 2b | 25% | Same gates + bounce-rate / receiver-side checks (if applicable) |
| Phase 2c | 50% | Same gates — non-determinism in bug reports starts mattering at 50%, treat any new ticket as a same-day cross-check |
| Phase 3 | 100% | All prior gates + on-call awareness |
| Phase 4 — Cleanup | n/a | Remove flag and legacy code path |

**Conventions:**

- Flag name pattern: `be-<scope>-<feature>` (e.g. `be-internal-smtp-mail-client-sdk`).
- Per-customer / per-tenant exclusions are the first mitigation lever before global rollback.
- Rollback = flag to 0%. Always faster than a code revert; should be the first response.
- A *new customer report during an in-flight rollout* deserves a same-day cross-check against that rollout, regardless of how few reports have come in. Triage rule.

### Test plan artifact

For migrations touching N call sites, write a Test Plan as a sibling Confluence page. Reference: "Internal SMTP Adapter Test Plan" (page `3720020072`), "External SMTP Adapter Test Plan" (page `3450699882`).

Format — one row per call site, with environment progression checkboxes:

```markdown
## <Service or component name>

### Summary

Status: <Done 🟢 / In Progress 🔵 / Blocked 🔴>

- Total `<function>` call sites in code: <N>
- SDK feature-flagged (new path present): <count>
- Legacy-only (no feature flag): <count>
- Ignored places: <count + reason>

### Details

| # | Location | Mail/event type | Short description | Migration Status | DEV | STAGE | PROD | Test Guide | Tested By |
|---|---|---|---|---|---|---|---|---|---|
| 1 | `src/foo.ts:42` | <type> | <what it does> | Migrated / Pending | ✅/⬜ | ✅/⬜ | ✅/⬜ | <link or note> | @<handle> |
```

### "Affected things" doc

For migrations that touch many use cases (templates, customers, services), maintain a separate Confluence page enumerating *every* affected case with its migration status. Reference: "Affected Email Types of Custom SMTP Configuration Migration" (page `3380084737`). Link from the poster Resources block.

### Post-mortem (when something goes wrong during rollout)

If the rollout produces a customer-visible incident, write a post-mortem. See `references/post-mortem.md` for the SoSafe structure.

### Rollout/rollback section in the poster

The poster Risks & Mitigation table should always include the rollback lever for the migration:

| Risk | Impact | Mitigation |
|---|---|---|
| <delivery / behaviour regression> | HIGH 🔴 | Feature flag `<name>` to 0% reverts to legacy path. Per-customer exclusions for partial rollback. |
