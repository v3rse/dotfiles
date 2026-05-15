---
name: job-search
description: Run a targeted job search for Senior/Staff backend and platform engineering roles. Use when asked to "search for jobs", "find me work", "job search", "look for roles", "find backend jobs", "search remote Germany", "find work", "job hunt", "apply for jobs", or "find engineering roles". Loads the user's career profile from ~/org/career/, searches live listings, filters for English-speaking roles in Germany or remote-from-Germany, scores fit against the profile, and outputs a tiered report with Blue Card safety notes.
allowed-tools: Read, Bash, web_search, web_fetch
---

Run a targeted job search pipeline for Senior/Staff backend and platform engineering roles, loading the user's career profile from ~/org/career/ and outputting a structured, tiered report.

## Step 1: Load Profile

Read these files from ~/org/career/ to build the user's profile context:

| File | What It Provides |
|------|-----------------|
| `external-positioning-2026.org` | Core narrative, target role types, value pillars |
| `target-companies-2026.org` | Tiered target company list with career page links |
| `career-cv-entry.org` | CV summary, skills stack, compact bullets |
| `career-next-steps.org` | Market context, must-have filters, red flags |

If any file is missing, proceed with what is available and note the gap.

## Step 2: Capture Search Criteria

Ask the user for search parameters. Use these defaults from the profile if not specified:

| Parameter | Default |
|-----------|---------|
| Role level | Senior / Staff Engineer |
| Domain | Backend / Platform / Distributed Systems |
| Location | Germany — remote or Cologne |
| Language | English-speaking |
| Blue Card | Must keep — prioritize German payroll |

Confirm with the user before proceeding.

## Step 3: Execute Search

Run 4–6 parallel `web_search` calls covering:

1. **Tier 1 targets** — Search the user's Tier 1 companies from `target-companies-2026.org` for open roles matching the criteria. Example queries:
   - `"<company>" "senior backend engineer" Germany remote 2026`
   - `site:greenhouse.io OR site:jobs.ashbyhq.com OR site:workable.com "<company>" backend engineer remote Germany`

2. **Broad market** — Search general job boards and HN hiring threads:
   - `"senior backend engineer" OR "staff engineer" remote Germany English TypeScript Node.js AWS 2026`
   - `site:hnhiring.com backend engineer remote Europe Germany May 2026`

3. **Specific stack** — Search for the user's core technologies:
   - `"platform engineer" OR "reliability engineer" Germany remote Kubernetes AWS 2026`

Use `maxResults: 10` per search. Prefer specific queries over broad ones.

## Step 4: Fetch & Verify

For each promising result:

1. `web_fetch` the job page to extract role details, requirements, location policy, and comp if listed.
2. Check if the posting is current — skip listings older than 3 months unless verified active.
3. Note the application URL (Greenhouse, Ashby, Workable, Lever, company careers page).

## Step 5: Score & Filter

Score each role against the user's profile using these criteria:

| Score | Criteria |
|-------|----------|
| **Strong fit** | JD mentions ≥3 of: distributed systems, event-driven architecture, platform/infrastructure ownership, reliability/SRE accountability, AWS/Kubernetes, design doc/ADR culture. Stack includes TypeScript/Node.js or adjacent. |
| **Moderate fit** | JD mentions 2 of the above, or stack is close (Go/Python with platform focus). |
| **Weak fit** | Generic "full stack" with unclear ownership, no operational accountability, or stack mismatch. |

**Blue Card safety filter:**

| Safety | Criteria |
|--------|----------|
| **Safe** | German company with German office, or explicitly employs remote workers in Germany on German payroll. |
| **Verify** | Remote-friendly foreign company — ask recruiter about German payroll entity. |
| **Risky** | US-only remote, no European entity, or explicit "no sponsorship" policy. |

## Step 6: Output Report

Produce a structured markdown report with this format:

```markdown
# Job Search Results — <date>

## 🔥 Tier 1 — Apply This Week

### 1. <Company> — <Role Title>
- **Comp:** <salary range if listed>
- **Location:** <remote policy / office>
- **Stack:** <technologies>
- **Fit:** <strong/moderate/weak> — <one-line reason>
- **Blue Card:** <safe/verify/risky> — <note>
- **Link:** <application URL>

## ✅ Tier 2 — Strong Fit, Verify Current

<same format>

## 🎯 Tier 3 — Worth Monitoring

<same format>

## ⚠️ Blue Card Critical Notes

<Safety guidance specific to this search batch>

## 📋 Next Steps

1. Apply this week: <list Tier 1>
2. Verify + apply: <list Tier 2>
3. Set alerts: <list Tier 3 and any target companies with no current listings>
```

## Step 7: Offer Tailoring

After delivering the report, ask the user:

> "Want me to tailor your CV and draft a cover letter for any of these? Paste the job description and I'll customize using your work-history doc."

If the user provides a JD, read `career-work-history.org` and `career-star-stories.org` from ~/org/career/, then:

1. Rewrite the CV entry bullets to match the JD's stated needs.
2. Draft a cover letter connecting specific job requirements to specific past wins.
3. Suggest 2–3 STAR stories from `career-star-stories.org` that fit the role.
