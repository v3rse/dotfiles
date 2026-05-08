---
name: tech-catchup
description: Catch the user up on what's happening in software engineering, AI, hardware, industry, security, policy, and adjacent tech so they don't miss important news, releases, and discussions. Use when the user says "catch me up", "what's new in tech", "what did I miss", "tech news", "weekly catchup", "what's happening in AI/frontend/devtools/chips/security", "stay in the loop", or asks for a digest of recent developments. Pulls from a curated, RSS-first list of sources that top engineers and tastemakers (Simon Willison, Gergely Orosz, swyx, Andrej Karpathy, Ben Thompson, Dylan Patel, Casey Newton, etc.) actually read. Personalizes via `~/org/news/profile.md`, dedupes via `~/org/news/.seen.json`, and defaults to "since last catchup".
---

# Tech Catchup

Goal: produce a high-signal, low-fluff briefing of what a working software engineer should know about across the broader tech space — grounded in **sources real tastemakers use**, personalized to the user, incremental across runs, and **easy to skim**.

## How this skill is laid out

```
tech-catchup/
├── SKILL.md          ← this file (strategy + output format)
├── scripts/
│   ├── init.sh           — scaffold ~/org/news/{profile.md,.seen.json,threads/}
│   ├── since_last.sh     — echo afterDate from newest digest, else 7d ago
│   ├── fetch_feeds.sh    — wrapper: --domains X,Y --since DATE --budget 5m|15m|30m
│   ├── fetch_feeds.py    — parallel RSS/Atom fetch & parse → JSONL
│   ├── seen.py           — filter / mark-from-jsonl / mark / trim / count
│   └── health.py         — probe all feeds, track rot in .feed-health.json
└── feeds/
    ├── aggregators.txt   (always included)
    ├── engineering.txt
    ├── ai-deep.txt
    ├── industry.txt
    ├── hardware.txt
    ├── security.txt
    ├── policy.txt
    └── adjacent.txt
```

**Don't reinvent these.** Use them. They handle parsing, parallel fetch, dedup, and date filtering.

To curate sources over time, edit `feeds/<domain>.txt` (one URL per line, `#` comments). No code changes needed.

## Workflow

### 1. Init & profile

Run once at the start (idempotent):

```bash
SKILL=$HOME/.pi/agent/skills/tech-catchup
"$SKILL/scripts/init.sh"
cat ~/org/news/profile.md
```

Read `profile.md` to get user's interests, ignored topics, default domains, default budget, trusted tastemakers. Use these to weight scoring and skip clarifying questions.

If `profile.md` is the unmodified template (still has `<...>` placeholders), ask the user to fill it in OR ask one compact question covering domains + budget.

### 2. Determine scope

Defaults — only ask if profile is empty AND user gave no hints:

| Dial | Default |
|---|---|
| Timeframe | `./scripts/since_last.sh` (newest digest date, else 7d ago) |
| Domains | from profile, else `everything` |
| Budget | from profile, else `15m` |
| Depth | `briefing` (skim-friendly, see Step 4) |

User overrides win. `"catch me up on AI today"` → domain=ai-deep, timeframe=today.

### 3. Fetch → dedup → rank

Three commands. Run them in order. **Do not** write your own RSS parser, scoring loop, or clustering logic.

```bash
SKILL=$HOME/.pi/agent/skills/tech-catchup

# (a) Fetch — conditional GET, parallel, retries, HN/Reddit URL extraction
"$SKILL/scripts/fetch_feeds.sh" \
  --domains <comma-list> \
  --since   <YYYY-MM-DD> \
  --budget  <5m|15m|30m> \
  > /tmp/catchup.jsonl 2> /tmp/catchup.log

# (b) Drop already-seen items (canonical URL match)
python3 "$SKILL/scripts/seen.py" filter \
  < /tmp/catchup.jsonl > /tmp/catchup.fresh.jsonl

# (c) Cluster across feeds, score, bucket, cap to ~30 records
python3 "$SKILL/scripts/rank.py" \
  --profile ~/org/news/profile.md \
  --top 5 --per-section 5 \
  < /tmp/catchup.fresh.jsonl > /tmp/catchup.ranked.jsonl 2>> /tmp/catchup.log
```

**Read `/tmp/catchup.ranked.jsonl`, not the raw output.** Each line is one *cluster* (one story, possibly with multiple sources merged):

```json
{
  "bucket": "top|engineering|ai-deep|industry|hardware|security|policy|adjacent|radar",
  "score": 5,
  "title": "...",
  "primary": "https://canonical-article-url",
  "discussion": "https://news.ycombinator.com/item?id=...",
  "sources": [{"feed": "...", "feed_title": "..."}],
  "tastemaker_via": ["simonw"],
  "summary": "first sentence.",
  "published": "2026-05-07T19:57:06+00:00",
  "feeds": ["...", "..."]
}
```

The **scoring, multi-source corroboration, tastemaker attribution, dedup, and bucket assignment are all done by `rank.py`.** Your job is judgment: pick which clusters to feature, write the one-sentence "why it matters" lines, and group them into the output template.

#### About the cache

`fetch_feeds.py` writes `~/org/news/.feed-cache.json` with ETag/Last-Modified per feed and sends conditional GETs on subsequent runs. **Caveat**: if you re-run with a *broader* `--since` than last time, cached feeds may suppress items in the wider window. If the user explicitly asks for older content, pass `--no-cache` to `fetch_feeds.py` directly or delete `~/org/news/.feed-cache.json`.

#### Surfacing fetch failures

If `/tmp/catchup.log` shows lots of `ERR` lines (≥3 different feeds), surface a one-liner at the end of the digest:
> *Heads up: 4 feeds failed this run. Run `scripts/health.py` to investigate, or `scripts/health.py --report` for a full table.*

`CACHED` lines are **not failures** — they mean the feed has no new items since last run.

Don't surface single transient failures.

### Feed maintenance (when to run)

Feeds rot over time — sites move RSS URLs, change format, or shut down. The skill's source list will become stale unless tended.

**Run `scripts/health.py` when:**
- Multiple feeds failed in a single fetch (≥3 ERR lines)
- The user asks "are my sources still good?" / "check feed health"
- It's been ≥30 days since the last health check (compare `mtime` of `.feed-health.json`)
- After editing `feeds/*.txt` to validate new entries

The script updates `~/org/news/.feed-health.json` with per-feed status, last-success date, and a fail-streak counter. Surface findings to the user:
- **`broken3+`** (≥3 consecutive failures) → recommend commenting out the line in `feeds/<domain>.txt` and offer to find a replacement URL via `web_search`.
- **`stale60d+`** (no posts in 60+ days) → ask whether the user still wants it; some blogs are rare-but-worth-it.
- **`broken`** (1-2 fails) → mention but don't act; could be transient.

Never silently delete a feed — always confirm with the user first.

### 4. Score & prioritize — *handled by `rank.py`*

Don't re-implement scoring in your head. `rank.py` already applies these rules; you just read the output:

| Signal | Weight |
|---|---|
| ≥2 sources for one story | +3 → Top stories candidate |
| Tastemaker (from `feeds/tastemakers.json` filtered by profile) | +2 |
| Tier-1 aggregator source (HN/Lobsters/Techmeme) | +2 |
| Matches profile's `Interests:` | +1 |
| Matches profile's `Ignore:` | dropped |

**Top stories** = `bucket: "top"` (score ≥ 3). **Domain sections** = `bucket: "engineering"|"ai-deep"|…`. **On the radar** = `bucket: "radar"`.

**Verification gate**: if a top-story claim originated from HN/Lobsters and asserts something concrete ("X acquired Y", "Z shipped W"), `web_fetch` the `primary` URL before stating it as fact. If you can't verify, frame as "rumor on HN".

### 5. Synthesize — skim-friendly output

The user finds walls of text overwhelming. **Optimize for visual breathing room.**

#### Hard rules

- **TL;DR first** — 3-5 single-line bullets at the top. Reading just these = caught up.
- **One story = one line.** Bold headline + one short sentence + a `↳` meta line for links/attribution.
- **Cap each section at 5 items.** Overflow → single `…+N more` line + follow-up offer.
- **Horizontal rules between sections.** Whitespace > density.
- **Drop empty sections silently.** Never write "Nothing notable in X."
- **Max 4 sections visible.** TL;DR + Top + 2-3 most-relevant domain sections. Everything else folded into a `<details>` block at the bottom.

#### Template

````markdown
# 📰 Tech Catchup — <timeframe>

*<domains> · <N> sources · <X> items · ~<Y> min read*

---

## ⚡ TL;DR

- 🔥 **<one-liner>** — [link](url)
- 🤖 **<one-liner>** — [link](url)
- 🛠️ **<one-liner>** — [link](url)
- 💬 **<one-liner>** — [link](url)
- 👀 **<one-liner>** — [link](url)

---

## 🔥 Top stories

**<Headline in bold>**
<One-sentence why-it-matters.>
↳ [primary](url) · [💬 discussion](hn-url) · *via @simonw*

**<Headline>**
<One sentence.>
↳ [primary](url) · [💬 hot thread](hn-url)

*…+3 more — ask for a deep dive on any.*

---

## 🤖 AI / ML

| | Story | Source |
|---|---|---|
| 🆕 | **<headline>** — <one-clause why> | [link](url) |
| 📈 | **<headline>** — <one-clause why> | [link](url) |
| 🧵 | **<headline>** — <one-clause why> | [link](url) |

---

## 🛠️ Devtools & Infra

*(same table format)*

---

<details>
<summary>📂 More (Hardware · Industry · Security · Policy · Adjacent · Releases · Ongoing · On the radar)</summary>

*(remaining sections in same compact table format)*

</details>

---

*<N> new URLs added to `.seen.json`. Saved to `~/org/news/tech-catchup-<date>.md`.*
````

#### Visual conventions

- **Emoji as section anchors only** — one per heading, never decorating bullets.
- **Inline status emoji in tables**: 🆕 new · 📈 trending · 🧵 ongoing saga · 🚨 incident · 💬 hot thread · 🚀 release · 📉 controversy.
- **Bold the noun** (the thing/company/release); everything else plain.
- **Italic for attribution and meta** (*via @simonw*, *~5 min read*).
- **Markdown tables for ≥3 similar items** — scan faster than bullet lists.
- **`↳` arrow** to demote link/meta below headline so the eye lands on the headline first.
- **Max 2 levels of nesting.** No bullet-of-bullets-of-bullets.
- **≤100 chars per line** — prevents awkward wraps.

#### Content rules

- **Dedupe** — one story, one row, best primary link + discussion link if hot.
- **Engineering/decision angle**, not press-release angle.
- **Cite every claim** with a markdown link.
- **Surface tastemaker endorsements** as `*via @handle*`, not as a sentence.
- **Mention X/YouTube as italic pointers**, not scraped quotes: *via @karpathy on X*.
- **If you can't say it in one line**, drop to "On the radar" with a follow-up offer.

### 6. Persist & follow up

```bash
SKILL=$HOME/.pi/agent/skills/tech-catchup

# Mark every cited URL as seen (canonical, cross-session dedup)
python3 "$SKILL/scripts/seen.py" mark-from-jsonl < /tmp/catchup.ranked.jsonl

# Trim seen.json occasionally (drops entries >90 days old)
python3 "$SKILL/scripts/seen.py" trim
```

`mark-from-jsonl` understands both raw items (`link` field) and ranked clusters (`primary` field).

Save the digest:

```bash
cat > ~/org/news/tech-catchup-$(date +%Y-%m-%d).md  # paste digest
```

Append to running threads if applicable: `~/org/news/threads/<slug>.md` gets a dated bullet for ongoing sagas (GitHub reliability, OpenAI governance, etc.). Surface the file as a one-line `🧵 ongoing` reference in the digest, don't re-summarize.

End with 2-3 concrete follow-ups:
- "Deep-read the [X] HN thread and summarize the debate?"
- "Pull recent posts from @karpathy via chrome-cdp?"
- "Open a new running thread on [Y]?"

## Always-cite, never-fetch (X / YouTube)

Mention these by handle when relevant; don't try to scrape:

- **X**: `@simonw`, `@karpathy`, `@GergelyOrosz`, `@swyx`, `@dhh`, `@patio11`, `@kentcdodds`, `@dan_abramov`, `@kelseyhightower`, `@ID_AA_Carmack`, `@mitchellh`, `@thorstenball`, `@b0rk`, `@danluu`, `@dylan522p`, `@benthompson`, `@CaseyNewton`
- **YouTube**: Fireship, ThePrimeTime, Theo - t3.gg, Asianometry, Acquired, Dwarkesh Patel, MKBHD

If `chrome-cdp` or `agent-browser` skills are active, optionally offer to pull recent posts from a specific handle.

## Guardrails

- **Use the scripts.** Don't write parallel fetch loops or RSS parsers inline.
- **Default to `since-last`** — don't make the user pick a timeframe every time.
- **Run `health.py`** when fetch failures spike or monthly. Don't let the source list rot silently.
- **Respect `Ignore:` strictly.**
- **Respect budget.** 5m = 8 feeds, 15m = 20, 30m = all.
- **Don't fabricate.** If a feed returns nothing, say nothing.
- **Verify hot rumors** before asserting.
- **Fail soft** on missing files — `init.sh` is idempotent; defaults always work.
- **Don't surface fetch errors** unless persistent.
- **Skim-friendly output, always.**
