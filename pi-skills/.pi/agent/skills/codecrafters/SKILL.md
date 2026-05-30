---
name: codecrafters
description: Local CodeCrafters challenge runner at ~/src/codecrafters/. Tracks stage progress in SQLite, gives AI hints when stuck, browses GitHub solutions after passing, and shows a dashboard across all 5 challenges (redis, interpreter, sqlite, git, http-server). Use when: working on a coding challenge, "what's my next stage", "I'm stuck on stage X", "hint please", "show solutions for Y", "my progress", "start redis/git/sqlite/interpreter/http-server", "test stage X", "how am I doing", "continue", "codecrafters".
---

# CodeCrafters Local Platform

`<skill-dir>` = the directory containing this file (e.g. `~/.pi/agent/skills/codecrafters`).

## Platform layout

| Path | Purpose |
|---|---|
| `~/src/codecrafters/cc` | The test runner (official CodeCrafters tester binaries) |
| `~/src/codecrafters/challenges/<name>/src/main.rs` | Your Rust code |
| `~/src/codecrafters/challenges/<name>/your_program.sh` | Entry point the tester calls |
| `~/src/codecrafters/descriptions/<name>/` | Stage description `.md` files (one per stage) |
| `~/src/codecrafters/stages/<name>.json` | Ordered stage list with slug, group, title |
| `~/src/codecrafters/bin/<name>-tester` | Official CodeCrafters test binaries |
| `~/.local/share/codecrafters/progress.db` | SQLite progress database (auto-created) |

## Runner (`cc`) commands

```bash
cd ~/src/codecrafters

./cc <challenge> list              # list all stages with group/number/slug/title
./cc <challenge> desc <slug>       # print stage description markdown
./cc <challenge> test              # run all base stages (stops at first failure)
./cc <challenge> test <slug>       # run one stage by slug (e.g. jm1)
./cc <challenge> test <N>          # run stage N by number
./cc <challenge> test <N-M>        # run stages N through M
./cc <challenge> test <group>      # run all stages in a group (e.g. replication)
./cc <challenge> test all          # run every stage
```

Challenges: `redis`, `interpreter`, `sqlite`, `git`, `http-server`

## Progress tracker commands

Script: `<skill-dir>/scripts/progress.py`  
DB: `~/.local/share/codecrafters/progress.db`

```bash
python3 <skill-dir>/scripts/progress.py init                              # create DB (idempotent)
python3 <skill-dir>/scripts/progress.py record <challenge> <slug> pass|fail  # log a result
python3 <skill-dir>/scripts/progress.py status [<challenge>]              # stage-by-stage status
python3 <skill-dir>/scripts/progress.py next [<challenge>]                # what to work on next
python3 <skill-dir>/scripts/progress.py dashboard                         # overview of all challenges
python3 <skill-dir>/scripts/progress.py log <challenge>                   # recent attempt history
```

Always call `init` once before the first `record` — it's idempotent, safe to call every time.

---

## Workflow 1: "Work on a challenge" (the standard session)

Use when the user says "continue redis", "start http-server", "what's next on interpreter", etc.

### Step 1 — Determine next stage

```bash
python3 <skill-dir>/scripts/progress.py next [<challenge>]
```

If no challenge specified and no progress exists, recommend starting with `http-server` (warmup).
Recommended order: `http-server → redis → interpreter → sqlite → git`

### Step 2 — Show the stage description

```bash
cd ~/src/codecrafters && ./cc <challenge> desc <slug>
```

Read the description fully. Pay special attention to any `{{#reader_is_bot}}` block — these are
hints written specifically for AI assistants and contain reduced-scope hints or important constraints.
Summarize the stage goal for the user in 2-3 sentences.

### Step 3 — Let the user code

Tell them the file to edit:
```
~/src/codecrafters/challenges/<challenge>/src/main.rs
```

Wait for them to implement, or help them think through the design if asked.

### Step 4 — Run the test

```bash
cd ~/src/codecrafters && ./cc <challenge> test <slug>
```

Capture both stdout and the exit code. Exit 0 = pass, non-zero = fail.

### Step 5 — Record the result

```bash
python3 <skill-dir>/scripts/progress.py init
python3 <skill-dir>/scripts/progress.py record <challenge> <slug> pass   # or fail
```

### Step 6 — React

**If pass:**
- Congratulate briefly
- Show updated progress: `python3 <skill-dir>/scripts/progress.py status <challenge>`
- Offer to continue to the next stage

**If fail:**
- Show the relevant part of the test output
- Ask: "Want a hint, or would you like to take another look?"
- If hint requested → [Workflow 2: Hint](#workflow-2-hint)

---

## Workflow 2: Hint

Use when stuck. **Never give a direct solution** — give the minimal concept nudge that unlocks the next step.

### How to generate a hint

1. **Read the stage description:**
   ```bash
   cd ~/src/codecrafters && ./cc <challenge> desc <slug>
   ```

2. **Read their current code:**
   ```
   ~/src/codecrafters/challenges/<challenge>/src/main.rs
   ```

3. **Compare**: what does the stage require vs what the code does or is missing?

4. **Identify the gap** — usually one of:
   - Wrong Rust abstraction (e.g. using sync instead of async)
   - Missing protocol detail (e.g. RESP framing, HTTP line endings)
   - Ownership/lifetime pattern they haven't seen yet
   - Algorithm or data structure choice

5. **Write the hint**: name the concept, point at the right part of the docs, explain the *why*. No code.

### Hint format

> **Hint for [challenge]/[slug]: [stage name]**
>
> The key insight: [one sentence on what's missing conceptually].
>
> Look at: [specific Rust concept, crate, or docs section].
>
> Constraint to keep in mind: [any tricky protocol or language constraint they may be missing].

**Never**: paste working code, give step-by-step implementation, solve the problem for them.

**Level up on second hint request**: if they ask again after the first hint, you can show the
skeleton/structure (function signatures, module layout) but not the implementation bodies.

---

## Workflow 3: Solution review (after passing)

Use only **after the user has already passed a stage**. Verify via:
```bash
python3 <skill-dir>/scripts/progress.py log <challenge>
```

If the stage shows `pass` → proceed. If not → offer a hint instead.

### How to browse solutions

Search GitHub for real Rust implementations:

```
site:github.com "codecrafters" rust <stage title or slug>
```

Or: `"build-your-own-redis" language:Rust` on GitHub search.

Look for repos with multiple stages complete (more complete = more interesting patterns).

### What to present

1. **2-3 different approaches** — not just the first result
2. **Key patterns** each uses: e.g. "uses `BufReader` + manual RESP parser", "splits on `\r\n`"
3. **Idiomatic Rust observations**: what's clean, what's verbose, what's clever
4. **Compare to their solution**: what's similar, what's different, what they might want to steal

Do not paste entire files. Show relevant snippets (10-20 lines) with commentary.

---

## Workflow 4: Dashboard / progress check

Use when the user asks "how am I doing", "show my progress", "codecrafters stats".

```bash
python3 <skill-dir>/scripts/progress.py dashboard
python3 <skill-dir>/scripts/progress.py next
```

Interpret the output: mention what challenge to focus on next, estimate how much work remains
on the current challenge, note any long gaps in the attempt log.

---

## Challenge sequence (recommended)

| Order | Challenge | Base stages | Key Rust concepts forced |
|---|---|---|---|
| 1 | `http-server` | 8 | Tokio `TcpListener`, basic parsing, concurrent connections |
| 2 | `redis` | 7 | Async I/O, RESP binary protocol, `Arc<Mutex<_>>`, `tokio::spawn` |
| 3 | `interpreter` | 15 | Recursive data structures, enums, borrow checker stress test |
| 4 | `sqlite` | 9 | Binary file parsing, B-tree concepts, `byteorder` crate |
| 5 | `git` | 7 | SHA1, zlib, content-addressable storage, hex encoding |

Extension stages (beyond base) are unlocked on the real platform. Locally they're all available —
do them if you want depth, but the base stages cover the core learning arc.

---

## Notes on description files

- Files are named `<group>-<order>-<slug>.md` in `~/src/codecrafters/descriptions/<challenge>/`
- `{{#reader_is_bot}}` blocks contain hints for AI assistants — always read and use them
- Some descriptions are sparse; supplement with the stage title from `stages/<challenge>.json`

## Notes on `your_program.sh`

The tester calls `./your_program.sh` to start the program. The starter template's script does
`cargo build --release && exec ./target/release/<binary>`. First run is slow (compiles); subsequent
runs are fast. If tests time out on first run, that's expected — rerun after the binary is built.

## Troubleshooting

| Symptom | Fix |
|---|---|
| `Tester not found` | Re-run `~/src/codecrafters/setup.sh` |
| Port already in use | `lsof -i :6379` and kill the process |
| Slow first test run | First `cargo build --release` takes ~30-60s; rerun the test |
| DB not found | Run `python3 <skill-dir>/scripts/progress.py init` |
