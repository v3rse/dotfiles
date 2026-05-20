---
name: interview-prep
description: Generate personalized interview prep checklists for specific companies and roles. Use when asked to "prep me for an interview", "interview prep", "prepare for company X", "what to expect at company Y", "interview checklist", "mock interview", "practice for interview", or "interview readiness check". Loads the user's career profile from ~/org/career/, looks up known interview patterns for the target company, and outputs a tailored prep plan with stack gaps, STAR story mapping, system design prompts, and a progress tracker.
allowed-tools: Read, Bash, web_search, web_fetch
---

Generate a personalized interview prep checklist for a specific company and role, loading the user's profile from ~/org/career/ and known interview patterns from references.

## Step 1: Capture Target

Ask the user: **"Which company and role are you interviewing for?"**

Examples:
- "Grafana Labs — Senior Backend Engineer (Mimir)"
- "Stream — Lead Engineer, Platform & Infrastructure"
- "Ashby — Staff Product Engineer"

If the user provides only a company name, infer the most relevant open role from their profile.

## Step 2: Load Profile

Read these files from ~/org/career/:

| File | Purpose |
|------|---------|
| `external-positioning.org` | Core narrative, target role types, value pillars |
| `cv-entry.org` | CV summary, skills stack, compact bullets |
| `star-stories.org` | STAR stories for behavioral prep |
| `target-companies.org` | Tiered target list — check if company is already listed |

## Step 3: Load Company Pattern

Read `${CLAUDE_SKILL_ROOT}/references/company-patterns.md` to look up the target company's known interview format.

If the company is not in the reference file, run `web_search` to find current interview process details:
- `"<company>" interview process "<role>" 2026`
- `"<company>" Glassdoor interview questions 2026`

## Step 4: Generate Prep Checklist

Produce a structured markdown checklist with these sections:

```markdown
# Interview Prep — <Company> · <Role>

## Your Fit Summary

| Aspect | Your Evidence |
|--------|-------------|
| **Role level** | <Senior/Staff/etc.> |
| **Stack match** | <TypeScript/Node.js = exact match / Go = gap to close / Python = moderate gap> |
| **Domain fit** | <Platform/reliability/distributed systems = strong> |
| **Blue Card** | <Safe / Verify / Risky> |

## Interview Rounds (Expected)

- [ ] **Round 1: <Name>** — <focus>. <duration>. <prep action>
- [ ] **Round 2: <Name>** — <focus>. <duration>. <prep action>
- [ ] **Round 3: <Name>** — <focus>. <duration>. <prep action>
- [ ] **Round 4: <Name>** — <focus>. <duration>. <prep action>

## Stack Gaps to Close

| Gap | Urgency | Action | Time |
|-----|---------|--------|------|
| <e.g. Go basics> | High | A Tour of Go + 2 small projects | 3–4 hrs |
| <e.g. Python brush-up> | Medium | Django basics review | 2 hrs |

## STAR Stories to Prepare

| Likely Question | Best Story | Key Detail to Emphasize |
|-----------------|----------|------------------------|
| "Tell me about a system you designed from scratch" | <best story from star-stories.org> | <key technical detail> |
| "How do you handle production incidents?" | <best story from star-stories.org> | <key detail: rollback, post-mortem, remediation> |
| "Give me an example of cross-functional delivery" | <best story from star-stories.org> | <key detail: stakeholders, pivots, outcomes> |
| "How do you make technical decisions with uncertainty?" | <best story from star-stories.org> | <key detail: structured decision process> |
| "Tell me about a project without a PM or deadline" | <best story from star-stories.org> | <key detail: ownership, initiative, impact> |

## System Design Prompts to Practice

1. **<Company-specific prompt>** — <context from company pattern>
2. **<General platform prompt>** — "Design a system that ingests millions of events per second and allows sub-second querying"

## Practical Coding Prep

- [ ] Rate limiter design
- [ ] LRU cache implementation
- [ ] Task queue with retries
- [ ] Pub/sub system
- [ ] <Company-specific practical problem if known>

## Company-Specific Angles

- <What to emphasize for this company's culture>
- <Questions to ask the interviewer>
- <Red flags to watch for in the process>

## Questions to Ask Them

1. <Question about team structure or scope>
2. <Question about on-call or operational ownership>
3. <Question about growth path to Staff/Principal>

## 1-Week Sprint Plan

| Day | Focus | Time |
|-----|-------|------|
| Mon | Story rehearsal (all 5, out loud, 3 min each) | 30 min |
| Tue | Stack gap brush-up | 45 min |
| Wed | System design practice #1 | 60 min |
| Thu | Coding practice (practical problems) | 45 min |
| Fri | Mock behavioral with a friend or recording | 45 min |
| Sat | System design practice #2 + take-home simulation | 90 min |
| Sun | Rest + light review | 30 min |

## Progress Tracker

- [ ] Profile loaded and fit assessed
- [ ] Company research complete
- [ ] Stack gaps identified and plan made
- [ ] STAR stories rehearsed out loud
- [ ] System design practiced (2 sessions)
- [ ] Coding problems practiced (3+ practical)
- [ ] Mock behavioral completed
- [ ] Questions for interviewer prepared
```

## Step 5: Offer Deep Dive

After delivering the checklist, ask:

> "Want me to drill deeper on any section? I can generate a full system design walkthrough, mock behavioral Q&A, or a take-home simulation for this company."

## Step 6: Track Progress

If the user updates you on their prep progress, update the checklist by checking off completed items and adding notes. Save the updated checklist to `~/org/career/interview-prep-<company-slug>.md` for future reference.
