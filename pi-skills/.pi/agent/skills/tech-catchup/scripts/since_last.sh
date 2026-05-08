#!/usr/bin/env bash
# Echo afterDate (YYYY-MM-DD) for "since last catchup".
# Logic: newest ~/org/news/tech-catchup-*.md filename date, else 7 days ago.
set -euo pipefail

NEWS="${TECH_CATCHUP_DIR:-$HOME/org/news}"

newest=""
if [[ -d "$NEWS" ]]; then
  newest=$(ls -1 "$NEWS"/tech-catchup-*.md 2>/dev/null \
    | sed -E 's|.*tech-catchup-([0-9]{4}-[0-9]{2}-[0-9]{2})\.md$|\1|' \
    | sort -r | head -n1)
fi

if [[ -n "$newest" ]]; then
  echo "$newest"
else
  date -u -d '7 days ago' +%Y-%m-%d 2>/dev/null \
    || date -u -v-7d +%Y-%m-%d  # BSD/macOS fallback
fi
