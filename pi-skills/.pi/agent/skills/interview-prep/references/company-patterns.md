# Company Interview Patterns

Known interview formats for companies relevant to the user's job search. Loaded conditionally by the interview-prep skill.

## Grafana Labs

**Role:** Senior Backend Engineer (Mimir OSS, k6)
**Level:** Senior
**Location:** Remote (Germany, Spain, UK, Sweden)
**Stack:** Go (primary), accepts Python/C/C++/Rust
**Format:** Recruiter screen (30 min) → Technical (45-60 min, coding + system design) → Behavioral (45 min)
**Coding:** Go or your lang + concurrency, data structures, error handling
**System design:** Metrics ingestion/storage at massive scale, observability pipelines
**Behavioral:** Ownership, on-call, open-source contribution mindset
**Company angle:** Mention using Grafana/Prometheus operationally. Emphasize structured decision-making (ADRs).
**Comp:** €97,034 – €116,441 + RSUs (Germany)

## Stream

**Role:** Lead Engineer (Staff/Principal), Platform & Infrastructure
**Level:** Staff/Principal — reports to CTO
**Location:** Amsterdam/Skopje, remote possible, visa sponsorship
**Stack:** Go/Python, AWS/GCP, Kubernetes, PostgreSQL, CockroachDB, Redis, Terraform
**Format:** Recruiter screen → Staff behavioral → System design → Leadership/CTO conversation → Coding
**Coding:** Go or Python — practical distributed systems problems
**System design:** Platform reliability, SLOs, cloud cost optimization, architecture decisions
**Behavioral:** Cross-org influence, technical strategy, mentorship, leading without authority
**Company angle:** Position as "Senior with Staff-level scope." Demonstrate: architecture across teams, multiplied productivity, sustained ownership without oversight.
**Comp:** Not public; Series B+ scale

## Ashby

**Role:** Product Engineer (Senior/Staff)
**Level:** Senior or Staff
**Location:** 100% remote, async, Europe distribution
**Stack:** TypeScript, React, GraphQL, Node.js, Postgres, Redis — exact match
**Format:** Recruiter → Product + technical → Coding → System design → Behavioral
**Coding:** TypeScript/Node.js — practical, real-world
**System design:** Product architecture — data models, API design, user workflows
**Behavioral:** Ownership, async collaboration, decision-making, written communication
**Company angle:** Product engineering culture — understand user problem before writing code. Emphasize end-to-end ownership and async discipline.
**Comp:** Sr $153k-$220k, Staff $190k-$275k (public calculator)

## PostHog

**Role:** Backend Engineer
**Level:** Engineer to Principal
**Location:** Fully distributed, 30+ countries
**Stack:** Python (Django backend), TypeScript (React frontend)
**Format:** Recruiter → Take-home (4-6 hrs, build a feature) → Take-home review (90 min) → 1:1 with teammates → Founder chat
**Coding:** Take-home is primary signal — practical, not LeetCode
**System design:** Event ingestion (Kafka → ClickHouse), session replay storage, feature flags at edge
**Behavioral:** Written communication, autonomy, OSS instincts
**Company angle:** Async, written-first, OSS-first. ADR and documentation habit is direct fit.
**Comp:** Engineer $170K–$230K, Senior $230K–$310K, Staff $310K–$420K, Principal $420K–$550K (public bands)

## n8n

**Role:** Backend Engineer (TypeScript)
**Level:** Senior
**Location:** Remote within Europe or Berlin office
**Stack:** Node.js, TypeScript — exact match
**Format:** Recruiter → Technical (coding + system design) → Take-home (2-4 hrs) → Final
**Coding:** TypeScript + async patterns, event-driven architecture
**System design:** Workflow execution engine, queue-based dispatch
**Behavioral:** Team fit, async communication, ownership
**Company angle:** Workflow automation. Your SQS → processor architecture maps directly to their domain.
**Comp:** Not public; Series A ($14M raised)

## Synthesia

**Role:** Software Engineering (platform/infra/full-stack/ML)
**Level:** Senior+
**Location:** Munich office or remote (DE/UK/CH/ES/IE/SI/DK/CZ/BE)
**Stack:** TypeScript, Python (for ML roles)
**Format:** Phone screen (15-30 min) → Technical (30-60 min) → Take-home (2-4 hrs) → Final (30-60 min)
**Coding:** Technical + take-home
**System design:** AI infrastructure, video pipelines, model serving
**Behavioral:** Problem-solving, communication, teamwork, AI/ML interest
**Company angle:** AI video platform. Add interest in AI infrastructure: model serving, GPU scheduling, real-time video.
**Comp:** Not public; Series E

## NetBird

**Role:** Senior Backend Engineer
**Level:** Senior
**Location:** Berlin, Germany (some roles remote)
**Stack:** Go, networking, distributed systems
**Format:** Recruiter/CEO screen → Technical (Go + networking) → System design → Final
**Coding:** Go + network protocols (WireGuard, NAT traversal)
**System design:** Secure mesh network, NAT traversal, peer-to-peer routing
**Behavioral:** Team fit, Berlin/remote logistics
**Company angle:** Open-source secure networking. Need Go + networking depth.
**Comp:** Not public; Series A

## DuckDuckGo

**Role:** Senior Backend Engineer
**Level:** Senior
**Location:** Remote
**Stack:** Not specified in job posting; likely varies by team
**Format:** Recruiter → Heavy behavioral (73+ behavioral questions) → Light technical → Final
**Coding:** Minimal; behavioral is the main signal
**System design:** Unlikely at Senior level given behavioral focus
**Behavioral:** Disagreement, risk-taking, performance ratings, strengths, long-term goals
**Company angle:** Privacy-focused. Security SaaS background is relevant.
**Comp:** $178,500 + equity (public)

## Brandfetch

**Role:** Senior Backend Engineer
**Level:** Senior
**Location:** Remote (UTC+0 to UTC+9)
**Stack:** Node.js, TypeScript, Python, AWS (Lambda, DynamoDB, CloudFront, S3)
**Format:** Screen → Technical (Node.js + system design) → Take-home or live coding → Final
**Coding:** API design, CDN, caching, vector search
**System design:** Brand asset delivery at scale, CDN architecture
**Behavioral:** Ownership, async communication, growth mindset
**Company angle:** 52M+ brand repos, APIs for Google/Canva/Salesforce. S3/CloudFront + API design maps directly.
**Comp:** Competitive + equity; profitable company

## General Patterns

### DevTools/Platform Startup Interview Archetypes

| Archetype | Coding | System Design | Behavioral | Examples |
|-----------|--------|---------------|------------|----------|
| **Take-home heavy** | 4-6 hr practical build | Deep discussion of take-home | Light | PostHog |
| **Live coding + design** | 45-60 min practical | 60 min architecture | Medium | Grafana, Stream |
| **Behavioral heavy** | Minimal | Minimal | 45-60 min deep | DuckDuckGo |
| **Product engineering** | Practical feature | Product architecture | Medium | Ashby, n8n |

### Stack Gap Severity

| Language | Companies | Gap Severity | Quick Prep |
|----------|-----------|--------------|------------|
| TypeScript/Node.js | Ashby, n8n, Synthesia, Brandfetch | None | Your daily language |
| Go | Grafana, Stream, NetBird | Medium | A Tour of Go (2-3 hrs) |
| Python | PostHog | Medium | Django + async Python review |

### Blue Card Safety by Company

| Company | Safety | Note |
|---------|--------|------|
| n8n | Safe | German company (Berlin HQ) |
| NetBird | Safe | German company (Berlin) |
| Synthesia | Safe | Munich office exists |
| eventfirst | Safe | Berlin-based |
| Grafana | Verify | Remote from Germany OK, but verify German payroll entity |
| Stream | Verify | Remote possible, ask about German entity |
| Ashby | Verify | 100% remote, verify payroll |
| PostHog | Verify | Fully distributed, verify payroll |
| Brandfetch | Verify | Remote, verify payroll |
| DuckDuckGo | Risky | US company, likely US-only remote |
