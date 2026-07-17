# Manual tests

Tests that need resources CI does not have (QEMU VMs, long builds) or
operator interaction.  They are NOT picked up by scripts/run-tests.sh;
run them explicitly:

- `test-config-generation-offline.sh` — offline check that the
  installer's config generation injects the SSH deploy key correctly.
- `test-deployment-workflow.sh` — end-to-end deploy workflow against a
  local QEMU VM (builds an ISO, boots it, runs guix deploy).

For the automated equivalents see tests/installer/ (module checks) and
tests/installer/installer.scm (Marionette VM system tests, run via
scripts/run-vm-tests.sh).
