# Hardware CI via Treadmill

Tock currently relies primarily on static checks and a limited number of
emulated targets for CI and tests. This document describes an architecture to
extend Tock's automated test coverage towards real physical targets and boards.

This architecture centers around Treadmill, a distributed hardware testbed
platform that is being built for over two years. It focuses on how we can
integrate the primitives that Treadmill provides into workflows that allow
Tock's contributors and tools to automatically carry out a diverse array of
hardware-in-the-loop tests, while maintaining security, without oversubscribing
the limited hardware resources we have available as part of this platform, and
keeping CI reliable.

Table of Contents:

- [What a Hardware Test Looks Like](#what-a-hardware-test-looks-like)
- [Goals](#goals)
- [Workflows](#workflows)
- [Technical Details](#technical-details)

## What a Hardware Test Looks Like

*(Placeholder — to be filled in.)*

Tests are Rust modules/functions. Each test gets access to a small API to:

- flash a kernel and one or more apps onto a board
- interact with the board over its console
- control and read GPIO pins (and later, other host-provided I/O)

TBD: the exact API, where test code lives, and how a test declares which
board/DUT capabilities it needs.

## Goals

*(Placeholder — to be filled in.)*

Roughly: catch hardware-facing regressions before merge, per subsystem (BLE,
Ethernet, GPIO, ...) and per board, without slowing down every PR.

## Workflows

### Contributor View

On every push to a PR, an automated workflow computes a test matrix. This is
done by analyzing the code changed in the pull request. This can be on the
granularity of crates, or individual files changed.

In the future, this analysis could also take into account dependency graphs
(e.g., a change to the 15.4 radio should trigger tests of all subsystems on the
OpenThread path), or classify certain types of changes as "non-breaking" (e.g.,
comment-only changes).

This analysis phase runs in the context of a hosted GitHub Actions job. It
requires no special tokens or privileges, and therefore runs immediately, for
all PRs, even for untrusted external contributors (subject to the general GitHub
Actions rules as for other checks today).

After this analysis, with the test matrix computed, the PR is annotated with a
special GitHub actions check named "Treadmill". The initial status of this check
is "neutral" / "in-progress". It includes a "Details" page that shows the table
of planned tests (board, subsystem, status, ...). When each one of these tests
is actually run depends on how trusted the contributor is (see [Maintainer
view](#maintainer-view)). Test rows stay labeled as "pending" until they are
run, the overall status of the "Treadmill" check stays "neutral".

As jobs are run on the actual hardware boards, the table is progressively
updated. Each row links to the Treadmill console for that job, where you can see
the actual console/serial output and test logs.

The "Treadmill" check finishes:

- **success** if every test of the plan succeeded, or there are no tests
- **failure** if any test failed
- **neutral** if any tests are still in progress / pending

### Requesting and Retrying Tests

Trusted users can interact with the system through GitHub comments (similar to
bors on Nixpkgs or the Rust repo):

- **(re-)run the automatically determined test matrix**

  ```
  @treadmill-tb run
  ```

  This command is also used to request tests for a PR from an untrusted
  contributor.

- **(re-)run a specific board/subsystem/test**

  ```
  @treadmill-tb test nrf52840dk, apollo3/ble
  ```

The bot reacts with an emoji to acknowledge, and possibly a comment reply.
Re-running tests supersedes the current run for that commit. To be able to
enqueue a PR from an untrusted contributor to the merge queue, GitHub will
require a trusted user to request running the checks manually. Possibly, this
command can be extended to automatically enqueue a PR into the merge queue
following successful checks, or automatically running checks when attaching a
`last-call` label.

### Debugging a Failure

On failure, the Treadmill job (holding a lease over the DUT / target board) is
kept alive for about 10 minutes before its lease is released. The check output
prints a ready-to-use command sequence for trusted users to reproduce the
failure and debug it:

```
tml job set-active $jobid
tml job lease extend 30m
tml job ssh
tock-harness run --prebuilt-artifacts /opt/path-to-unpacked-artifacts \
    --board nrf52840dk --test blink-and-buttons
```

Alternatively, if those 10 minutes have passed, a contributor can request
another job with the exact same board and configuration:

```
tml job create --restart $jobid --same-host --wait
tml job ssh
tock-harness run \
    --board nrf52840dk --test blink-and-buttons
# This will re-build the kernel and userspace, and then run the test
```

### Writing a New Test

To write a new test from scratch, a trusted user can request an ordinary
interactive Treadmill job. The `tock-harness` will include a Makefile or other
script to launch a Treadmill job, upload all required dependencies, and run it
on the remote host. This emulates the exact environment that this check would
run in when executing in CI.

### Maintainer view

- PRs from trusted users run tests automatically, other PRs only compute the
  test matrix and can then be run after approval.
- Once a PR enters the merge queue, hardware tests always run. The fact that a
  PR was enqueued means that a trusted user has approved it to be merged, and
  its contents are deemed trusted.

### Trust & permissions reference

There are two different groups of users:

- The Tock repository will contain a `treadmill.yml` (or similar) file with a
  list of trusted users (file is always read from `master` and not the PR
  branch).
  
  This list governs whose contributions may be automatically tested, and who can
  request checks to be run on other PRs.

- The Treadmill `tock` group (auto-synced from GitHub "tock" org membership).

  This group "owns" every Treadmill job, can cancel or restart them if they get
  stuck, or SSH into the jobs to debug them.

## Technical details

### Architecture

Services:

- GitHub: PR events, comments, checks.

- "ci-bridge":

  This service bridges the Treadmill testbed with the Tock repository on GitHub.
  It receives events as GitHub webhooks, dispatches GitHub actions jobs
  (specifically to evaluate the test matrix and pre-build the test kernels and
  userspace apps), manages Treadmill jobs, and updates the GitHub status checks.

- GHA plan+build workflow:

  This CI job builds the `tock-harness` binary, which contains the logic for
  determining the CI job matrix (`tock-harness plan`), and can pre-build the
  necessary test kernels and userspace apps (`tock-harness prebuild`).
  
  It then uploads the plan and pre-built artifacts as a GitHub actions artifact,
  and reports this status back to the "ci-bridge".

- Treadmill switchboard: schedules jobs onto hosts.

- Treadmill host:

  Connected to the DUT / target board, and runs a special Tock CI image
  (`probe-rs`, `openocd`, J-Link, etc. pre-installed).
  
  This fetches the pre-built `tock-harness` and its artifacts from "ci-bridge",
  then runs tests (flashing the boards, interacting with them through UART,
  GPIO, etc.), and reports results back.

### Events & Flow

1. PR event webhook to "ci-bridge"
2. "ci-bridge" dispatches plan+build on GH Actions
3. plan+build reports matrix and artifact reference back to "ci-bridge"
4. (if trusted) "ci-bridge" requests Treadmill jobs
5. host pulls the artifact via "ci-bridge"
6. host runs `tock-harness` to actually execute the tests
7. `tock-harness` reports `task_exit_status` and results back to switchboard
8. "ci-bridge" listens to switchboard events
9. "ci-bridge" updates the GitHub check.
