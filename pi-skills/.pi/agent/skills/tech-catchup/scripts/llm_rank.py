#!/usr/bin/env python3
"""LLM-based re-ranker for the tech-catchup pipeline.

Reads ranked items from stdin (JSONL), calls the claude CLI to re-score
them against the user's profile, and emits items with llm_score,
llm_section, and llm_rationale fields added.

Fail-safe: if claude CLI is missing, errors, or times out (>90s), pass
through heuristic ranking unchanged.

Cache: results keyed by item URL in ~/.cache/tech-catchup/llm/<sha1>.json
"""
from __future__ import annotations
import argparse, hashlib, json, os, subprocess, sys, tempfile, time
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from _lib import parse_profile

CACHE_DIR = Path.home() / ".cache" / "tech-catchup" / "llm"
BATCH_SIZE = 25
TIMEOUT = 90
MODEL = "claude-haiku-4-5"

# Valid sections the LLM can return
VALID_SECTIONS = {"top", "engineering", "security", "ai", "radar", "skip"}


def _cache_path(url: str) -> Path:
    key = hashlib.sha1(url.encode()).hexdigest()
    return CACHE_DIR / f"{key}.json"


def _load_cache(url: str) -> dict | None:
    p = _cache_path(url)
    if p.exists():
        try:
            return json.loads(p.read_text())
        except (json.JSONDecodeError, OSError):
            pass
    return None


def _save_cache(url: str, result: dict) -> None:
    CACHE_DIR.mkdir(parents=True, exist_ok=True)
    p = _cache_path(url)
    try:
        p.write_text(json.dumps(result, ensure_ascii=False))
    except OSError:
        pass


def _build_prompt(items: list[dict], profile_md: str) -> str:
    """Build the prompt for a single batch of items."""
    # Build a compact JSON array with just id, title, summary, source
    batch = []
    for item in items:
        batch.append({
            "id": item["_id"],
            "title": item.get("title", ""),
            "summary": item.get("summary", "")[:200],
            "source": item.get("primary", ""),
        })

    return (
        f"You score tech news items for a senior engineer with this profile:\n"
        f"<profile>\n{profile_md}\n</profile>\n\n"
        f"For each item, return JSON: "
        f'{{"id": <int>, "score": 0-10, "section": "top|engineering|security|ai|radar|skip", "rationale": "<20 words"}}.\n'
        f"Score 10 = must-read for this person. Score 0 = skip entirely (off-topic or ignored).\n"
        f'Section "skip" overrides score.\n\n'
        f"Items:\n{json.dumps(batch, ensure_ascii=False)}\n\n"
        f"Return JSON array, one object per item, same order."
    )


def _call_claude(prompt: str) -> list[dict] | None:
    """Call the claude CLI, return parsed JSON array or None on failure."""
    try:
        result = subprocess.run(
            ["claude", "--print", "--model", MODEL],
            input=prompt,
            capture_output=True,
            text=True,
            timeout=TIMEOUT,
        )
    except (subprocess.TimeoutExpired, FileNotFoundError, OSError):
        return None

    if result.returncode != 0:
        return None

    text = result.stdout.strip()
    if not text:
        return None

    # Try to extract JSON from the response (might be wrapped in markdown)
    # First, try direct parse
    try:
        return json.loads(text)
    except json.JSONDecodeError:
        pass

    # Try to find a JSON array in the text
    start = text.find("[")
    end = text.rfind("]")
    if start != -1 and end != -1 and end > start:
        try:
            return json.loads(text[start:end + 1])
        except json.JSONDecodeError:
            pass

    # Try to find a JSON object and wrap it
    start = text.find("{")
    end = text.rfind("}")
    if start != -1 and end != -1 and end > start:
        try:
            obj = json.loads(text[start:end + 1])
            if isinstance(obj, dict):
                return [obj]
        except json.JSONDecodeError:
            pass

    return None


def _merge_results(items: list[dict], llm_results: list[dict]) -> list[dict]:
    """Merge LLM results back into items, with safe defaults on mismatch."""
    out = []
    for i, item in enumerate(items):
        result = llm_results[i] if i < len(llm_results) else {}
        score = result.get("score")
        section = result.get("section", "")
        rationale = result.get("rationale", "")

        # Validate score
        try:
            score = int(score)
            if not 0 <= score <= 10:
                score = None
        except (TypeError, ValueError):
            score = None

        # Validate section
        if section not in VALID_SECTIONS:
            section = None

        out.append(dict(
            item,
            llm_score=score if score is not None else item.get("score", 0),
            llm_section=section if section is not None else item.get("bucket", "radar"),
            llm_rationale=str(rationale)[:120] if rationale else "",
        ))
    return out


def _batch_items(items: list[dict]) -> list[list[dict]]:
    """Split items into batches of BATCH_SIZE."""
    return [items[i:i + BATCH_SIZE] for i in range(0, len(items), BATCH_SIZE)]


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--profile", default=str(Path.home() / "org" / "news" / "profile.md"))
    ap.add_argument("--verbose", action="store_true")
    args = ap.parse_args()

    profile = parse_profile(args.profile)
    profile_md = Path(args.profile).read_text() if Path(args.profile).exists() else ""

    # Read all items from stdin
    items: list[dict] = []
    for line in sys.stdin:
        line = line.strip()
        if not line:
            continue
        try:
            item = json.loads(line)
            items.append(item)
        except json.JSONDecodeError:
            continue

    if not items:
        return 0

    # Assign internal IDs for batch tracking
    for i, item in enumerate(items):
        item["_id"] = i

    # Check cache for each item
    uncached: list[dict] = []
    cached_results: dict[int, dict] = {}

    for item in items:
        url = item.get("primary", "") or item.get("link", "")
        cached = _load_cache(url)
        if cached is not None:
            cached_results[item["_id"]] = cached
        else:
            uncached.append(item)

    # Call LLM for uncached items in batches
    llm_results: dict[int, dict] = {}
    if uncached:
        batches = _batch_items(uncached)
        if args.verbose:
            print(f"llm_rank: {len(uncached)} uncached items in {len(batches)} batch(es)", file=sys.stderr)

        for batch in batches:
            prompt = _build_prompt(batch, profile_md)
            raw = _call_claude(prompt)

            if raw is None:
                if args.verbose:
                    print("llm_rank: LLM call failed for batch, using fallback", file=sys.stderr)
                # Fallback: use heuristic values
                for item in batch:
                    llm_results[item["_id"]] = {
                        "id": item["_id"],
                        "score": item.get("score", 0),
                        "section": item.get("bucket", "radar"),
                        "rationale": "",
                    }
                continue

            # Map results by id
            for r in raw:
                if isinstance(r, dict) and "id" in r:
                    try:
                        idx = int(r["id"])
                        llm_results[idx] = r
                    except (ValueError, TypeError):
                        pass

            # Cache each result
            for item in batch:
                idx = item["_id"]
                result = llm_results.get(idx, {
                    "id": idx,
                    "score": item.get("score", 0),
                    "section": item.get("bucket", "radar"),
                    "rationale": "",
                })
                url = item.get("primary", "") or item.get("link", "")
                _save_cache(url, result)

    # Merge all results
    all_results: list[dict] = []
    for item in items:
        idx = item["_id"]
        if idx in cached_results:
            all_results.append(cached_results[idx])
        elif idx in llm_results:
            all_results.append(llm_results[idx])
        else:
            all_results.append({
                "id": idx,
                "score": item.get("score", 0),
                "section": item.get("bucket", "radar"),
                "rationale": "",
            })

    merged = _merge_results(items, all_results)

    # Clean up internal _id and emit
    for item in merged:
        out = {k: v for k, v in item.items() if k != "_id"}
        print(json.dumps(out, ensure_ascii=False))

    if args.verbose:
        llm_count = sum(1 for r in all_results if r.get("rationale"))
        print(f"llm_rank: processed {len(items)} items, {llm_count} with LLM rationale", file=sys.stderr)

    return 0


if __name__ == "__main__":
    sys.exit(main())
