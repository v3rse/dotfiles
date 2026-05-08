#!/usr/bin/env bash
# Scaffold ~/org/news/ on first run. Idempotent.
set -euo pipefail

NEWS="${TECH_CATCHUP_DIR:-$HOME/org/news}"
SKILL="${TECH_CATCHUP_SKILL:-$HOME/.pi/agent/skills/tech-catchup}"
mkdir -p "$NEWS/threads" "$NEWS/feeds"

# Copy default feed configs from skill dir if missing
if [[ ! -f "$NEWS/feeds/aggregators.txt" ]] && [[ -d "$SKILL/feeds" ]]; then
  cp "$SKILL/feeds/"*.txt "$SKILL/feeds/"*.json "$NEWS/feeds/" 2>/dev/null || true
  echo "copied default feeds to $NEWS/feeds/" >&2
fi

if [[ ! -f "$NEWS/profile.md" ]]; then
  cat > "$NEWS/profile.md" <<'EOF'
# Catchup profile

- Interests: <e.g. Rust, local-first, distributed systems, AI infra, devtools>
- Domains: engineering, ai-deep, industry
- Ignore: <e.g. crypto, generic VC fundraising, AI ethics philosophy>
- Default budget: 15m
- Trusted tastemakers: simonw, GergelyOrosz, patio11
EOF
  echo "created $NEWS/profile.md (template — edit me)" >&2
fi

[[ -f "$NEWS/.seen.json" ]] || echo '{}' > "$NEWS/.seen.json"

echo "ready: $NEWS"
