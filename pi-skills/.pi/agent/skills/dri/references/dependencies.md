# DRI Dependencies Playbook

Trigger: `/dri deps <slug>` — manage the cross-team commitments your project needs.

This is the hardest part of being a DRI for most engineers: getting other teams to *commit*, not just acknowledge. This playbook makes it mechanical.

## Mental model

A dependency has three states beyond "exists":

1. **Identified** — you know you need them, you haven't asked yet.
2. **Asked** — you sent a concrete request. Awaiting a response.
3. **Committed** — they've said yes to specific work, with a rough date. Or said no, with reasoning.

"They acknowledged it" is not committed. "They're aware" is not committed. "I'll mention it next sprint" is not committed. **Concrete = a person + a deliverable + a rough timeline.**

If you can't write the dependency as `<Team> will do <X> by <date> (owner: @<handle>)`, it isn't a dependency yet — it's a hope.

## State

Per-project file: `~/notes/dri/<slug>/dependencies.md`. Append-only log of asks/responses, plus a current state table at the top.

```markdown
# Dependencies — <project name>

## Current state (last refreshed YYYY-MM-DD)

| # | Dep | Team | Owner contact | Need by | State | Last action | Next action |
|---|---|---|---|---|---|---|---|
| 1 | <one-line ask> | <team> | @<handle> | <date> | identified / asked / committed / declined / blocked | <YYYY-MM-DD: what happened> | <YYYY-MM-DD: what to do> |

## Log

### YYYY-MM-DD — <dep title>

- Action: <asked / pinged / escalated / committed>
- Channel: <DM / #proj-* / ticket>
- Outcome: <reply or none>
- Next: <when to act again>
```

## Steps

1. **Read state.** `project.md`, `dependencies.md` if it exists. If not, bootstrap.

2. **Identify mode** — what does the user need right now?

   - "I have a new dependency" → add to the table, draft initial ask
   - "I haven't heard back" → draft a follow-up or recommend escalation
   - "They said something vague" → help convert to a concrete commitment
   - "Audit my dependencies" → status-walk through each one

   If unclear, ask via AskUserQuestion (one batched question).

3. **For an identified-but-not-yet-asked dep:** draft the initial ask (template below).

4. **For an asked-but-no-reply dep:**
   - **0–2 days:** wait. They're busy. Don't double-ping.
   - **3–4 days:** draft a polite follow-up.
   - **5–7 days:** draft a TL-loop-in message ("@<their TL>, can you help me get traction on this?").
   - **8+ days:** this is an escalation now. Suggest `/dri escalate <slug>` with the specific dep as the trigger.

5. **For a vague response** (e.g. "we'll try", "next sprint maybe", "I'll mention it"): draft a clarifying reply that asks for the three concrete pieces — *who*, *what*, *when*.

6. **For a committed dep:** record the commitment. Set `state: committed` with the agreed deliverable + date in the table. Move on.

7. **For a declined dep:** record it. Trigger a `/dri decision` or `/dri escalate` — your project plan needs to react.

8. **Update the state table.** Always show current snapshot at the top of `dependencies.md`. Append the action to the log section below.

9. **Update `risks.md`** if any dep is now in "asked > 7 days" or "declined" — those are risks until resolved.

10. **Next action.** Always end with one specific message-to-send or call-to-make.

## Templates

### 1. Identifying who to ask

If the user doesn't know who to contact, work it out:

- Check the project poster's Involved Teams table.
- For SoSafe MSG: common dependencies map to teams in the [INTERNAL] Messaging Platform Project Candidates page (`3234103381`) and the current quarter roadmap. Use `mcp__MCP_DOCKER__confluence_search` for `<topic>` to find their TL or recent DRIs.
- Use `mcp__MCP_DOCKER__jira_search` for recent epics in the candidate team's project to identify their active DRI.
- Default fallback: ping the Team Lead. They route.

Output: "You probably want @<TL handle> from <Team>. They were DRI on <recent project> which touched the same area. Confirm?"

### 2. Initial ask — concrete commitment template

Send via DM (most cases) or in `#proj-<slug>` if it's a public/well-scoped ask.

```
Hi <name>, I'm DRI for <project name> (poster: <link>).

I'd like to confirm <Team> as a dependency. Specifically I need:

**What:** <concrete deliverable in 1 line — what they have to produce or change>
**Why:** <1 sentence on why it matters / what it unblocks>
**By when:** <date / range — be specific, even if it's an ask>
**Owner from your side:** <if you have a guess, propose a name; otherwise ask them to assign>

Effort I'm guessing: <T-shirt size or "X engineering days" — anchor the conversation>.

Could you confirm if this is feasible, and who'd be the contact? Happy to jump on a 15-min call if it's easier than async.

Slack channel for the project: #proj-<slug>.
```

**Why this format works:**

- Concrete *what* — they can say yes/no without a meeting.
- Anchored timeline — easier to say "no, but X works" than to invent one.
- Owner ask — forces them to name a person, which is half of commitment.
- Effort estimate — gives them a starting point to push back on, beats "could you take a look at this".

### 3. Follow-up (no response, day 3–4)

```
Hi <name>, following up on <ask> for <project>. Could you let me know if this is something <Team> can take on by <date>? Even a "no" or "later" works — I need to know so I can plan around it.

Original message: <link to thread>.
```

Keep it shorter than the original. The point is to surface the silence, not to re-pitch.

### 4. TL loop-in (day 5–7)

DM to *their* TL (or yours if they're outside your immediate org):

```
Hi <TL>, I've been trying to get a commitment from <Team> on <ask> for <project name>. Reached out to <name> on <date>, followed up on <date>, no reply yet.

This blocks <milestone> by <date>. Could you help me get traction, either by routing me to the right person or flagging it on your side?

Original ask: <link>.
```

### 5. Vague-to-concrete reply

When they say "we'll try" or "yeah I think we can":

```
Thanks <name>! To make sure I plan correctly: can we firm this up?

- Owner from your side: <name?>
- Realistic delivery window: <can you do <X-Y> dates?>
- Anything from us to unblock you?

If <date> isn't feasible, knowing that now is way more useful than finding out at the deadline 🙂
```

### 6. Recording a commitment

After a verbal commitment (call / Slack message / huddle):

```
Just to capture for my notes: <Team> commits to <deliverable> by <date>, with <name> as the owner. Confirming on Slack so we both have the record.

Anything I'm missing — happy to adjust.
```

This is critical: the *written* commitment is the dependency. Verbal-only commitments evaporate.

### 7. Declined dep — your reply

```
Got it, thanks for the clear answer. I'll factor that into the plan — likely <Plan B summary>. If anything changes on your side, ping me anytime.
```

Then trigger `/dri decision <slug>` or `/dri escalate <slug>` — your timeline / scope needs to react.

## Anti-patterns to flag

- **Mega-ask in one message.** "Can your team look at A, B, C, D…?" → split into separate, individually-actionable asks. Each gets its own row in the table.
- **Asking via #proj-* channel only.** Public channels get scrolled past. Always DM the contact for the initial ask, then summarise the commitment in the channel for the audit trail.
- **Re-pinging in <48h.** Looks anxious; trains the recipient to ignore you. Wait the 3 days unless it's a true emergency.
- **Accepting "yes" as commitment.** "Yes" without a name + date isn't a commitment. Push for the specifics — politely, but always.
- **Hiding declines.** A "no" is data. Flag it visibly on the project; don't quietly absorb the work.
- **Going silent yourself.** If a dependency replied and you went quiet for a week, you've lost goodwill. Acknowledge within 1 business day, even if your real reply takes longer.

## Watch list — when to escalate

Run this check during `/dri review <slug>`:

- Any dep in `asked` state > 7 days → escalation candidate
- Any dep in `committed` state where the agreed date is < 7 days away and you have no visible progress signal → ping for status
- Any `declined` dep that hasn't triggered a `/dri decision` entry → your plan is out of date with reality
- More than 2 deps in `identified` (not yet asked) for > 3 days → you're in delay-mode; just send the asks today

## On the human discomfort

The asking *is* the job. It feels like nagging because you're tracking what other teams should know on their own — but they don't, because they have their own DRIs and their own asks. Treat this as a service to them: clearer asks → easier yes/no → fewer mid-quarter surprises on their side too.

The DRI doctrine is explicit: "*Drive momentum, force clarity.*" Asking firmly for a commitment is not pushiness. Letting a dep drift while quietly hoping is the actual anti-pattern (consensus trap + silent DRI).
