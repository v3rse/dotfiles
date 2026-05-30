# DRI Escalation Playbook

Trigger: `/dri escalate <slug>`.

Doctrine: **early escalation is expected, late escalation is a problem**. Escalation is part of the role, not a sign of failure.

## When to escalate (any of these)

- **Scope creep** — work is being added without alignment
- **Repeated missed commitments** — pattern, not a one-off
- **Unexpected delivery risks** — timeline / quality drift
- **Dependencies blocking progress** — and your peer-level pings haven't moved it
- **Capacity issues** — you can't deliver with the people you have
- **Decisions with company-wide impact** — security, compliance, public API
- **Cross-team conflict** requiring authority you don't have
- **Team member performance** affecting delivery (TL territory, not yours)

If none of these → it's a `/dri decision`, not an escalation.

## Steps

1. **Read state.** `project.md`, `risks.md`, recent `status-log.md` and `decisions.md` entries.

2. **Identify the audience via AskUserQuestion if unclear:**
   - Team Lead — default for most escalations
   - TL + dependent team's TL — for cross-team conflict
   - Sponsor / product — for scope conflict
   - Department lead / staff eng — for company-wide impact

3. **Compose the escalation message** using the structure below. Tone: factual, no blame, action-oriented. Don't catastrophise; don't undersell.

```
**Escalation: <project name> — <one-line summary of the problem>**

**What's happening:** <2–3 sentences of factual context>

**Why I'm raising it now:** <the specific trigger — missed commitment, blocked dep, etc. — and why I can't resolve it at my level>

**Impact if unaddressed:** <timeline slip, scope cut, quality risk, dep on another team's roadmap>

**Options I see:**
- A — <option + cost>
- B — <option + cost>
- (or: I don't see a clean option, which is why I'm escalating)

**What I need from you:** <decision / authority / connection / capacity — be specific>

**Decision deadline:** <date — when the lack of decision becomes its own cost>
```

4. **Output channel suggestion.** Default: 1:1 with TL (DM or 1:1 doc), then summarised in `#proj-<slug>` once direction is set. For cross-team, suggest a 30-min sync with TLs of both teams.

5. **Append to `decisions.md`** as type `escalation`:

```markdown
## 2026-05-08 — Escalation: <title>

**Type:** Escalation
**Raised to:** <names>
**Trigger:** <what made me escalate now>
**Ask:** <what I need>
**Status:** Open / Resolved / Acknowledged

(full draft message below)

---

<draft>
```

6. **Update `risks.md`** — mark the relevant risk row as `escalated: <date>` so the next `/dri review` and `/dri` next-action recommender don't flag it again immediately.

7. **Next action.** Usually: "Send to <TL name> directly. After their response, summarise direction in `#proj-<slug>` (run `/dri status <slug>` if it changes project health)."

## Anti-patterns to flag

- **"I'll wait one more week"** → escalation avoider. The cost of asking is small; the cost of waiting often isn't.
- **Dumping the problem without options** → "I don't know what to do" is fine if true, but try to surface 1–2 options first. Easier for the TL to react.
- **Escalating in `#proj-*` first** → start in DM with TL. Public escalation can be necessary later but isn't the opening move.
- **Blame language** → describe the situation, not the people. The TL hears "Team X hasn't replied in 2 weeks despite 3 follow-ups", not "Team X is unresponsive".
- **Vague ask** → "what should I do?" is too broad. "I need a decision on whether we cut M3 or slip 2 weeks" is actionable.

## Examples

### Dependency block

```
**Escalation: SSO Rollout — auth-vendor reply blocking M2 by 9 days**

**What's happening:** We need vendor confirmation of SAML metadata changes to proceed with M2 (staging cutover). Engineering reached out 2026-04-29; followed up 2026-05-02 and 2026-05-05. No reply.

**Why now:** M2 is on the critical path for our 2026-05-22 target. Slip > 2 weeks puts our quarterly commitment at risk.

**Impact:** 2-week target slip; downstream FE work paused.

**Options:**
- A — Continue waiting; risk full slip.
- B — Build a fallback path with self-issued metadata; ~3 days work, throwaway.

**Ask:** Can you reach out to vendor account exec, or authorise option B?

**Decision deadline:** 2026-05-12 (after which option B no longer saves us time).
```

### Capacity

```
**Escalation: Reporting v2 — capacity gap for M3**

**What's happening:** M3 needs ~2 weeks of FE work I don't have on the team. Current FE is fully on M2.

**Why now:** Without resourcing decision by 2026-05-15, M3 slips into Q3.

**Impact:** Either slip target by 4 weeks or cut M3 from initial release.

**Options:**
- A — Borrow FE from <team>; need TL-to-TL conversation.
- B — Cut M3 from v1; ship as fast-follow.
- C — Slip target.

**Ask:** Conversation with <other TL>, or alignment on B/C.

**Decision deadline:** 2026-05-15.
```
