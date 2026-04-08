#!/usr/bin/env bash
# End-to-end ROS 2 Jazzy smoke test for the systole channel.
#
# Must be run inside a `guix shell -L systole --no-grafts ros-jazzy
# gcc-toolchain cmake` environment (see README.md).  Exits 0 on success,
# non-zero on failure.

set -euo pipefail

: "${RMW_IMPLEMENTATION:=rmw_cyclonedds_cpp}"
export RMW_IMPLEMENTATION

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BUILD="$(mktemp -d -t systole-smoke.XXXXXX)"
trap 'rm -rf "$BUILD"' EXIT

echo "[smoke] RMW_IMPLEMENTATION=${RMW_IMPLEMENTATION}"
echo "[smoke] build dir: ${BUILD}"

echo "[smoke] configuring..."
cmake -S "$HERE" -B "$BUILD" -DCMAKE_BUILD_TYPE=Release >/dev/null

echo "[smoke] building talker..."
cmake --build "$BUILD" --parallel 2 >/dev/null

TALKER="$BUILD/systole_smoke_talker"
test -x "$TALKER" || { echo "[smoke] talker binary missing"; exit 1; }

echo "[smoke] starting talker..."
"$TALKER" &
TALKER_PID=$!
trap 'kill "$TALKER_PID" 2>/dev/null || true; rm -rf "$BUILD"' EXIT

echo "[smoke] running listener..."
if python3 "$HERE/listener.py"; then
  echo "[smoke] SUCCESS"
  kill "$TALKER_PID" 2>/dev/null || true
  wait "$TALKER_PID" 2>/dev/null || true
  exit 0
else
  echo "[smoke] FAILURE"
  kill "$TALKER_PID" 2>/dev/null || true
  wait "$TALKER_PID" 2>/dev/null || true
  exit 1
fi
