#!/usr/bin/env bash
# check-substitute-health.sh — is the SystoleOS build hub alive and useful?
#
# Public copy of OUH-MESHLab/ivs-infrastructure scripts/check-substitute-health.sh
# (the build hub's own repository, which also publishes the beacon); keep
# the two in sync.  Run daily by .github/workflows/substitute-health.yml.
#
# Reads the public health beacon relay-push publishes with every sync
# (<url>/ci-status.txt) and fails loudly when:
#   * the beacon is missing or older than MAX_AGE_HOURS (relay stopped —
#     the 2026-08-12 failure that went unnoticed for three months);
#   * the last coverage-probe line is an ERROR or below COVERAGE_MIN %;
#   * the last relay-realize line is an ERROR.
# HTTP only: runs anywhere (operator box, a scheduled CI job, a monitor).
#
#   scripts/check-substitute-health.sh [URL]
#   COVERAGE_MIN=90 MAX_AGE_HOURS=36 scripts/check-substitute-health.sh
#
# Exit 0 = healthy, 1 = unhealthy (reasons on stdout), 2 = unreachable.
set -uo pipefail
URL="${1:-${PUBLIC_SUBSTITUTE_URL:-https://substitutes.systoleos.io}}"
MAX_AGE_HOURS="${MAX_AGE_HOURS:-36}"
COVERAGE_MIN="${COVERAGE_MIN:-90}"

status=$(curl -fsS --max-time 30 "$URL/ci-status.txt") || {
  echo "UNREACHABLE: $URL/ci-status.txt"; exit 2; }
field() { sed -n "s/^$1=//p" <<<"$status" | head -n 1; }

bad=0
fail() { echo "FAIL  $*"; bad=1; }

gen=$(field generated)
if ! gen_epoch=$(date -d "$gen" +%s 2>/dev/null); then
  fail "no parsable generated= timestamp"
else
  age_h=$(( ($(date +%s) - gen_epoch) / 3600 ))
  if (( age_h >= MAX_AGE_HOURS )); then
    fail "beacon is ${age_h}h old (>= ${MAX_AGE_HOURS}h): relay-push stopped"
  else
    echo "ok    beacon ${age_h}h old"
  fi
fi

cov=$(field coverage)
pct=$(grep -oE 'coverage=[0-9]+(\.[0-9]+)?%' <<<"$cov" | tail -n 1 | tr -dc '0-9.')
if [[ -z "$cov" || "$cov" == *" ERROR "* ]]; then
  fail "coverage probe: ${cov:-no line}"
elif [[ -z "$pct" ]]; then
  fail "coverage probe line has no coverage=: $cov"
elif (( ${pct%%.*} < COVERAGE_MIN )); then
  fail "coverage ${pct}% < ${COVERAGE_MIN}%: $cov"
else
  echo "ok    coverage ${pct}%"
fi

rea=$(field realize)
if [[ -z "$rea" || "$rea" == *" ERROR "* ]]; then
  fail "relay-realize: ${rea:-no line}"
else
  echo "ok    realize: ${rea#* }"
fi

echo "      push: $(field push)"
exit "$bad"
