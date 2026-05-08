#!/usr/bin/env bash
# Fetch feeds for one or more domains.
# Usage: fetch_feeds.sh --domains engineering,ai-deep [--since YYYY-MM-DD] [--budget 5m|15m|30m]
#
# Concatenates feeds/<domain>.txt files (always includes aggregators.txt),
# then pipes to fetch_feeds.py. Output: JSONL on stdout, log on stderr.
set -euo pipefail

DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
NEWS="${TECH_CATCHUP_DIR:-$HOME/org/news}"
FEEDS_DIR="$NEWS/feeds"

domains="everything"
since=""
budget="15m"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --domains) domains="$2"; shift 2 ;;
    --since)   since="$2"; shift 2 ;;
    --budget)  budget="$2"; shift 2 ;;
    *) echo "unknown arg: $1" >&2; exit 2 ;;
  esac
done

[[ -z "$since" ]] && since="$("$DIR/since_last.sh")"

case "$budget" in
  5m)  max_per_feed=5;  cap=8  ;;
  15m) max_per_feed=10; cap=20 ;;
  30m) max_per_feed=20; cap=999 ;;
  *)   max_per_feed=10; cap=20 ;;
esac

# Resolve domain list
if [[ "$domains" == "everything" ]]; then
  files=("$FEEDS_DIR"/*.txt)
else
  files=("$FEEDS_DIR/aggregators.txt")
  IFS=',' read -ra ds <<< "$domains"
  for d in "${ds[@]}"; do
    f="$FEEDS_DIR/${d// /}.txt"
    [[ -f "$f" ]] && files+=("$f") || echo "warn: no feed list for domain '$d'" >&2
  done
fi

# Concat, strip comments/blanks, dedupe, cap
tmp="$(mktemp)"
trap 'rm -f "$tmp"' EXIT
grep -hEv '^\s*(#|$)' "${files[@]}" | awk '!seen[$0]++' | head -n "$cap" > "$tmp"

n=$(wc -l < "$tmp" | tr -d ' ')
echo "fetching $n feeds, since=$since, budget=$budget" >&2

python3 "$DIR/fetch_feeds.py" --feeds "$tmp" --since "$since" --max-per-feed "$max_per_feed"
