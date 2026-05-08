---
name: blindspots
description: Surface news items that the user's tech-catchup profile filtered out — grouped by reason (ignored keyword, off-domain, no tastemaker, outside interests). Use when the user says "show my blindspots", "what am I missing", "what did I filter out", "broaden me", "what's outside my filter", "noise check", or wants a periodic peek at content their profile rejects. Complements the `tech-catchup` skill — reuses its fetch data, no new HTTP cost.
---

# Blindspots

Goal: produce a small, honest "what your filter quietly skipped" digest. The `tech-catchup` profile is aggressive by design — it drops `Ignore:` matches, deprioritizes off-domains, and pushes no-tastemaker items to `radar`. This skill inverts that lens for ~3 minutes of curiosity-reading.

**Don't reinvent fetching or feed parsing.** This skill operates entirely on the JSONL output that `tech-catchup`'s `fetch_feeds.sh` already produced. If `/tmp/catchup.fresh.jsonl` is missing or stale, run `tech-catchup` first.

## Layout

```
blindspots/
├── SKILL.md
└── scripts/
    └── blindspots.py    — classifier + ranker over fresh.jsonl
```

## Workflow

### 1. Check (or refresh) the source data

```bash
TC=$HOME/.pi/agent/skills/tech-catchup
ls -la /tmp/catchup.fresh.jsonl 2>/dev/null
```

If missing or older than the timeframe you want to inspect, re-run the same fetch the user ran for tech-catchup:

```bash
SINCE=$("$TC/scripts/since_last.sh")
"$TC/scripts/fetch_feeds.sh" --domains everything --since "$SINCE" --budget 15m \
  > /tmp/catchup.jsonl 2> /tmp/catchup.log
python3 "$TC/scripts/seen.py" filter < /tmp/catchup.jsonl > /tmp/catchup.fresh.jsonl
```

**Important**: blindspots intentionally runs *before* `seen.py mark-from-jsonl` is called for the day, so the user can see items even if they were already shown in their main digest. If the fresh file is post-mark, just re-fetch.

### 2. Read the profile to determine deprioritized domains

Inspect `~/org/news/profile.md` for any "off by default", "low signal", or "only when X" qualifications under the user's `Domains I care about:` section. Pass those as `--deprioritized`.

Example: if the profile says "hardware (off by default)" and "adjacent (fun-only)", pass `--deprioritized hardware,adjacent`. If unclear, ask once or default to empty.

### 3. Run the classifier

```bash
SKILL=$HOME/.pi/agent/skills/blindspots
python3 "$SKILL/scripts/blindspots.py" \
  --profile ~/org/news/profile.md \
  --top 8 \
  --deprioritized <comma-list-or-empty> \
  < /tmp/catchup.fresh.jsonl > /tmp/blindspots.jsonl 2>> /tmp/catchup.log
```

Read `/tmp/blindspots.jsonl`. Each line is one cluster with a `reason` field:

| Reason | Why it was filtered |
|---|---|
| `ignored` | Matched a keyword in profile `Ignore:` (strictest filter) |
| `off-domain` | Feed lives in a deprioritized domain |
| `no-tastemaker` | Trending on HN/Lobsters/Techmeme but no trusted-source overlap |
| `outside-interests` | Passed filters but didn't match any `Interests:` keyword |

### 4. Render — keep it short

This is a curiosity layer, not a primary digest. **Cap output at ~8 items total, group by reason, one line per item.**

```markdown
# 🔭 Blindspots — what your filter quietly skipped (<timeframe>)

*Items your profile rejected — grouped by why. Skim, don't deep-read.*

## 🚫 Hit your Ignore: list (<N>)

- **<headline>** — *<reason_detail>* · [link](url)

## 🌑 Off-domain (<N>)

- **<headline>** — *feed → <domain>* · [link](url)

## 🌊 What aggregators cared about that your tastemakers didn't (<N>)

- **<headline>** · [link](url) · *<aggregator name>*

## 🔭 Outside your interests (<N>)

- **<headline>** — *no interest-keyword hit* · [link](url)

---

*Profile filtering hid <total> items this week. <N> shown above (top by aggregator coverage + recency). Want to broaden any of these into the main digest?*
```

#### Visual rules
- **One line per item** — no expansion, no "why it matters" paragraphs. The point is breadth, not depth.
- **Italic the filter reason** so the eye lands on the headline.
- **Drop empty sections silently** — if `ignored` has zero hits, don't write that section.
- **Don't re-cite items already in the user's main digest.** Cross-check primary URL against today's `~/org/news/tech-catchup-*.md`.
- **Never recommend overriding `Ignore:`** — those are a hard line. Just surface them as awareness.

### 5. Follow up

End with one or two concrete offers:
- "Want me to remove `<keyword>` from your Ignore list? I'm seeing <N> related stories this week."
- "Promote `<domain>` from off-default to full coverage for next catchup?"
- "Open a running thread on the `<topic>` cluster I'm seeing?"

**Do not auto-modify `profile.md`.** Always confirm with the user first.

## Periodic mode

If the user asks for "weekly blindspots check" or "monthly noise audit", this skill works as-is — just re-fetch a wider window before classifying. Suggested cadence: weekly (lighter) or monthly (deeper, with a `--top 15`).

## Guardrails

- **Don't bypass `Ignore:` silently.** Surface ignored items, but always frame them as "your filter caught this — flagged for awareness".
- **Don't refetch unnecessarily.** If `/tmp/catchup.fresh.jsonl` exists and is recent, reuse it.
- **Don't claim something is a blindspot if it's already in the main digest.** That's just duplication.
- **Tone:** curious, brief, non-defensive. Filters exist for good reasons; this is just a peek over the wall.
- **No follow-up cycles.** Blindspots is a single-pass digest; if the user wants to dig in, that's a separate `tech-catchup` deep-dive on a specific topic.
