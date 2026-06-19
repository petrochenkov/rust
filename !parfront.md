Stabilization strategy for rustc parallel frontend

## Problem statement

Rust compiler has a mode that can parallelize some of the work happening in its frontend
(the part running before code generation).
That mode can speedup compilation significantly, and is usable in majority of user scenarios.
However, it is only available on nightly (through `-Zthreads`), but not on stable releases.

## End goal

Make the parallel frontend available on stable releases, so users can benefit from the speedups,
if their scenarios allow it.

## Obstacles (high-level)

### Reproducibility

The parallel frontend mode has fundamental issues with reproducibility of the compiler output,
both binaries and diagnostics.
Some scenarios require such reproducibility, but the majority of them do not and should be
able to benefit from the parallel frontend speedups.
Ensuring the reproducibility in all cases may take years, and may be not a reasonable thing to do
(e.g. for diagnostics).

We need to ensure that all the necessary opt-ins or opt-outs are available for people that need
reproducibility.

### Lack of confidence

During previous stabilization discussions a number of people expressed concerns about the parallel
frontend correctness and reliability.

We need to provide the necessary tests and CI runs to improve confidence in the system, especially
among the compiler team, who is responsible for the decision of making the parallel frontend stable.

## The current state (issues)

All open issues currently filed on the parallel frontend can be found here (20 open issues):
- https://github.com/rust-lang/rust/issues?q=state%3Aopen%20label%3AA-parallel-compiler

### Reproducibility issues

Open reproducibility issues with parallel frontend (11 issues):
- https://github.com/rust-lang/rust/issues?q=state%3Aopen%20label%3AA-parallel-compiler%20label%3AA-reproducibility

5 of the issues are about the produced binaries, and 6 are about the text output (diagnostics or MIR dumps).

The differences in diagnostics are represented quite well by the UI test suite, when it is run in
parallel mode (see later).
Issue https://github.com/rust-lang/rust/issues/154314 lists examples of diagnostic differences and
their reasons.

- The most common issue is that diagnostics are simply reported in different order.
  At least 5-10% of tests fail for this reason in parallel mode
  (https://rust-lang.zulipchat.com/#narrow/channel/187679-t-compiler.2Fparallel-rustc/topic/Add.20the.20parallel.20front-end.20test.20suite/near/578775786)
  Parallel test suite resolves this issue by enabling test output normalization called `//@ compare-output-by-lines`,
  this leaves us with a much smaller number of less trivial issues.

- Another issues is various global IDs leaking into user-visible diagnostics.
  Allocation IDs during const evaluation are the most notable example
  (https://github.com/rust-lang/rust/issues/157665).
  Allocation IDs can also be easily normalized away for testing
  (https://github.com/rust-lang/rust/pull/156716).

- The third issue happens when some extra note can be attached to the "first reported diagnostic of
  this kind", and the exact formatting of the note depends on the first diagnostic's message text.
  These formatting differences are also currently normalized away during testing
  (https://github.com/rust-lang/rust/pull/157299).

- The last notable case is about diagnostics reported during resolution of query cycles.
  The way query cycles are broken depends on the exact state of the query execution graph at the
  moment when the cycle is detected. Making the behavior here reproducible is hard, and some query
  cycle tests are currently disabled during parallel testing.

### Confidence issues

Open issues about rustc crashing when parallel frontend is enabled (4 issues).
There are no issues about rustc compiling something incorrectly in paralle mode.
- https://github.com/rust-lang/rust/issues?q=is%3Aissue%20state%3Aopen%20label%3AA-parallel-compiler%20label%3AI-ICE

- https://github.com/rust-lang/rust/issues/154560
  - Only happens after a query cycle error
- https://github.com/rust-lang/rust/issues/154056
  - Only happens after a query cycle error
- https://github.com/rust-lang/rust/issues/152662
  - Likely fixed by https://github.com/rust-lang/rust/pull/153472, needs to be tested, but the reproduction is heavy
- https://github.com/rust-lang/rust/issues/90227
  - Does not currently happen due to a workaround (`overflow-checks = false` in `rustc_thread_pool`), but may still potentially exist

### Performance issues

3 issues report the parallel frontend not being as fast as it could potentially be:
- https://github.com/rust-lang/rust/issues?q=is%3Aissue%20state%3Aopen%20label%3AA-parallel-compiler%20label%3AI-compiletime

Definitely not a blocker, things can always be optimized more, both in parallel and single-threaded mode.

### Other issues

2 umbrella issues for tracking work.

- https://github.com/rust-lang/rust/issues/113349 tracking issue
- https://github.com/rust-lang/rust/issues/118698 another tracking issue

## The current state (other activities)

UI test suite now supports running in parallel mode (https://github.com/rust-lang/rust/pull/153801).
Making it enabled on CI is in progress (https://github.com/rust-lang/rust/pull/157705).

Adding a benchmark suite for the parallel frontend to rustc-perf is in progress
(https://github.com/rust-lang/rustc-perf/pull/2421).

## The current state (recent past activities)

Last remaining widespread crashes and deadlocks were recently fixed, improving the parallel
frontend correctness to an acceptable label.

Most notable examples:
- https://github.com/rust-lang/rust/pull/151509 fixed crashes
- https://github.com/rust-lang/rust/pull/143035 fixed deadlocks
- more fixes can be found in https://github.com/rust-lang/rust/pulls?q=is%3Apr+is%3Amerged+author%3Azoxc+label%3AA-query-system

## The plan

To achieve the end goal we need an organizational push.
We currently have a 2026 project goal for stabilizing the parallell frontend
(https://rust-lang.github.io/rust-project-goals/2026/parallel-front-end.html),
but it lacks more specific steps for achieving the goal, people responsible for executing the steps,
and tentative timeline.
This MCP aims to provide all these details, and propose a stabilization strategy for the parallel frontend.

TODO fill with details

- Publish this MCP, publish updates to the project goal now, and regularly in the future
- Enable parallel ui test suite on CI in blocking mode
- Implement option `rustc -j` controlling parallel job limit for the whole rustc
- Implement option for preserving determinism when running with -j
- rustc-perf benchmarks for parallel frontend - iterate on the feedback, merge
- Enable parallel frontend on nightly by default, collect feedback for at least 3 months
- Address feedback and issues collected during 3 months or running on nightly
- Stabilize -j and the determinism-preserving option, decide whether they should be enabled
  by default in rustc (probably not), decide on integration with cargo

TODO

TODO
