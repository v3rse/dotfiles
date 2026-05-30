# DRI Review Playbook

Trigger: `/dri review <slug>` — health check + anti-pattern audit on a project.

Goal: surface concrete gaps the DRI can fix this week. Not a vibes report.

## Steps

1. **Read state.** `project.md`, `decisions.md`, `status-log.md`, `risks.md`. Refresh from Confluence/Jira if `last_synced` > 7d or `--sync`.

2. **Run the health checklist.** Output as ✅ / ⚠️ / ❌ per item with a short reason.

   **Setup**
   - [ ] Project poster exists and is current (in-scope/non-goals defined, success criteria measurable)
   - [ ] Slack `#proj-*` channel exists, used regularly
   - [ ] Jira epic exists, has linked tickets following the team's Definition of Ready
   - [ ] All milestones have a named owner

   **Execution**
   - [ ] Last status update ≤ 14 days ago
   - [ ] Status colour matches reality (no green-washing of a yellow project)
   - [ ] All open risks have a named owner and a next action
   - [ ] Decisions in last 30 days are documented (Slack thread, ADR, or `decisions.md`)

   **Stakeholders**
   - [ ] Stakeholders list current; all dependent teams know they're on the hook
   - [ ] Team Lead has visibility into current health (recent 1:1 mention or `#proj-*` membership)

   **Trajectory**
   - [ ] Target date still credible given current pace
   - [ ] Critical-path items identified and tracked

3. **Run the 5 anti-pattern checks.** For each, look at evidence and call ✅ healthy / ⚠️ at-risk / ❌ active.

   **Passive coordinator**
   - Evidence: meetings without decisions, status updates without next steps, owner = "team" instead of a name.
   - Healthy when: every status update has clear "next steps" with owners; decisions are visible.

   **Consensus trap**
   - Evidence: an item appearing in `risks.md` or status updates 3+ times without resolution; "let's discuss next week" multiple times.
   - Healthy when: when consensus stalls, DRI calls it within a sprint and documents.

   **Silent DRI**
   - Evidence: gap > 14 days in `status-log.md`; stakeholders pinging asking for status.
   - Healthy when: bi-weekly cadence held even during boring stretches.

   **Scope absorber**
   - Evidence: milestones added without dates moving; growing `risks.md` without escalations; engineer mentioning new asks they accepted alone.
   - Healthy when: scope changes show up in `decisions.md` or escalations, not silently.

   **Escalation avoider**
   - Evidence: 🟡/🔴 status for > 1 cycle without an entry in `decisions.md` of type `escalation`.
   - Healthy when: any prolonged at-risk/blocked has a corresponding TL conversation logged.

4. **Output a punch list of fixes.** Most-impactful first, with the slash command to run:

   ```
   ⚠️ Silent DRI — last update 17d ago
   → /dri status <slug>

   ⚠️ Consensus trap — "DB choice" open in risks.md since 2026-04-22
   → /dri decision <slug>

   ⚠️ Escalation avoider — 🟡 for 3 weeks, no TL conversation logged
   → /dri escalate <slug>

   ✅ Setup is solid (poster, epic, channel, owners).
   ```

5. **Optional: commit the review.** If the user passes `--log`, append a dated review entry to `decisions.md` (type: review) summarising findings. Otherwise, the review is ephemeral.

6. **Next action.** Usually: "Pick the top item from the punch list and run that subcommand now."

## Tone

- Not pearl-clutching. A 🟡 project is normal. An undocumented 🟡 project is a problem.
- Don't pretend everything is fine to be polite. Don't manufacture problems either.
- If the project is genuinely healthy, say so in 2 lines and stop. Don't pad.
