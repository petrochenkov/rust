Stabilization strategy for rustc parallel frontend

## Problem statement

Rust compiler has a mode that can parallelize some of the work happening in its frontend
(the part running before code generation).
That mode may speedup compilation significantly, and is usable in majority of user scenarios.
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
among the compiler team members, who are responsible for the decision of making the parallel
frontend stable.

## The current state (issues)

All open issues currently filed on the parallel frontend can be found here (20 open issues):
- https://github.com/rust-lang/rust/issues?q=state%3Aopen%20label%3AA-parallel-compiler

### Reproducibility issues

Open reproducibility issues with parallel frontend (11 issues):
- https://github.com/rust-lang/rust/issues?q=state%3Aopen%20label%3AA-parallel-compiler%20label%3AA-reproducibility

5 of the issues are about the produced binaries, and 6 are about the text output (diagnostics or MIR dumps).

The differences in diagnostics are represented quite well by the UI test suite, when it is run in
parallel mode (see later).
Issue [#154314](https://github.com/rust-lang/rust/issues/154314) lists examples of diagnostic differences and
their reasons.

- The most common issue is that diagnostics are simply reported in different order.
  At least [5-10%](https://rust-lang.zulipchat.com/#narrow/channel/187679-t-compiler.2Fparallel-rustc/topic/Add.20the.20parallel.20front-end.20test.20suite/near/578775786) of tests fail for this reason in parallel mode.
  Parallel test suite resolves this issue by enabling test output normalization called `//@ compare-output-by-lines`,
  this leaves us with a much smaller number of less trivial issues.

- Another issues is various global IDs leaking into user-visible diagnostics.
  Allocation IDs during const evaluation are the most [notable example](https://github.com/rust-lang/rust/issues/157665).
  Allocation IDs can also be easily [normalized away](https://github.com/rust-lang/rust/pull/156716) for testing.

- The third issue happens when some extra note can be attached to the "first reported diagnostic of
  this kind", and the exact formatting of the note depends on the first diagnostic's message text.
  These formatting differences are also currently [normalized away](https://github.com/rust-lang/rust/pull/157299)
  during testing.

- The last notable case is about diagnostics reported during resolution of query cycles.
  The way query cycles are broken depends on the exact state of the query execution graph at the
  moment when the cycle is detected. Making the behavior here reproducible is hard, and some query
  cycle tests are currently disabled during parallel testing.

### Confidence issues

Open issues about rustc crashing when parallel frontend is enabled (4 issues).
There are no issues about rustc compiling something incorrectly in paralle mode.
- https://github.com/rust-lang/rust/issues?q=is%3Aissue%20state%3Aopen%20label%3AA-parallel-compiler%20label%3AI-ICE

- ["[ICE]: parallel: variances.get(i).unwrap() None"](https://github.com/rust-lang/rust/issues/154560)
  - Only happens after a query cycle error
- ["[ICE]: cycle detected when computing function signature"](https://github.com/rust-lang/rust/issues/154056)
  - Only happens after a query cycle error
- ["[ICE]: stealing value which is locked"](https://github.com/rust-lang/rust/issues/152662)
  - Likely fixed by [#153472](https://github.com/rust-lang/rust/pull/153472), needs to be tested, but the reproduction is heavy
- ["Non-deterministic overflow in rustc_thread_pool causing CI failures"](https://github.com/rust-lang/rust/issues/90227)
  - Does not currently happen due to a workaround (`overflow-checks = false` in `rustc_thread_pool`), but may still potentially exist

### Performance issues

3 issues report the parallel frontend not being as fast as it could potentially be:
- https://github.com/rust-lang/rust/issues?q=is%3Aissue%20state%3Aopen%20label%3AA-parallel-compiler%20label%3AI-compiletime

Definitely not a blocker, things can always be optimized more, both in parallel and single-threaded mode.

### Other issues

2 umbrella issues for tracking work.

- ["Tracking Issue for Parallel Rustc Front-end"](https://github.com/rust-lang/rust/issues/113349)
- ["Add parallel front-end test suite to compiletest"](https://github.com/rust-lang/rust/issues/118698)

## The current state (other activities)

UI test suite now supports [running in parallel mode](https://github.com/rust-lang/rust/pull/153801).
Making it enabled on CI is [in progress](https://github.com/rust-lang/rust/pull/157705).

Adding a benchmark suite for the parallel frontend to rustc-perf is also [in progress](https://github.com/rust-lang/rustc-perf/pull/2421).

## The current state (recent past activities)

Last remaining widespread crashes and deadlocks were recently fixed, improving the parallel
frontend correctness to an acceptable label.

Most notable examples:
- ["Handle race when coloring nodes concurrently as both green and red"](https://github.com/rust-lang/rust/pull/151509) fixed crashes
- ["Only work-steal in the main loop for rustc_thread_pool"](https://github.com/rust-lang/rust/pull/143035) fixed deadlocks
- More fixes can be found among [these PRs](https://github.com/rust-lang/rust/pulls?q=is%3Apr+is%3Amerged+author%3Azoxc+label%3AA-query-system)

More of the not-so-recent history can be found in the [tracking issue](https://github.com/rust-lang/rust/issues/113349)
and also in the [old tracking issue](https://github.com/rust-lang/rust/issues/48685).

## The plan

To achieve the end goal we need an organizational push.
We currently have a 2026 [project goal](https://rust-lang.github.io/rust-project-goals/2026/parallel-front-end.html)
for stabilizing the parallell frontend, but it lacks more specific steps for achieving the goal,
people responsible for executing the steps, and tentative timeline.
This MCP aims to provide all these details, and propose a stabilization strategy for the parallel frontend.

The next steps will happen more or less in order.

### MCP and the project goal

This MCP is published.
Recent updates are published to the project goal [tracking issue](https://github.com/rust-lang/rust-project-goals/issues/121),
now and regularly in the future.

Responsible people: @petrochenkov.

### Enabling parallel UI test suite on CI

UI test suite is a collection of 20+ thousands of small programs testing all kinds of compiler behavior,
both in successful and failing compilation scenarios.

UI test suite now supports [running in parallel mode](https://github.com/rust-lang/rust/pull/153801).
- `--parallel-frontend-threads=N` sets the number of threads used by parallel frontend.
- `--iteration-count=M` sets the number of times each test is run.

The large values of `N` and `M` may give better reproduction for potential issues, infrastructure
team will determine which values we can afford in practice.
We recommend starting with `N=4` and `M=1`, there are a lot of CI runs and the issues will reproduce
sooner or later even with small `M`.

There's also a subset of the test suite (`tests/ui/parallel-rustc`) that contains reproducers for
issues encountered in parallel frontend specifically.
For this subset it may be desirable to set a larger number of `M`.

When the parallel test suite is enabled in blocking mode, there should be an announcement,
and the project contributors should be aware that it is entirely ok and is a recommended course of action
to immediately disable these tests with `//@ ignore-parallel-frontend triage`.
The responsible people will then triage the found tests, create corresponding issues,
and try to debug and eventually fix them.

Before enabling the test suite in blocking mode it can be optionally enabled in non-blocking mode first.

Implementation for this step is [in progress](https://github.com/rust-lang/rust/pull/157705).
Responsible people: CI - @heinwol, reviews - infra team, debugging found issues - @zetanumbers, backup - @petrochenkov.
Required time: 1 month.

### Enable parallel benchmark suite in rustc-perf

We need a benchmark suite that can detect nontrivial changes in performance of the parallel compiler.
It will likely have to rely on wall times, so the measurements won't be very precise.

There's an [in progress implementation](https://github.com/rust-lang/rustc-perf/pull/2421) for this,
and the PR thread has discussion about what exactly needs to be measured.
There's some existing feedback from rust-perf maintainers that needs to be addressed,
and the work needs to be finished in general.

The progress will happen in parallel with the stabilization, but the stabilization probably shouldn't be blocked on this.

Responsible people: addressing feedback / finishing work - @heinwol, reviews - @Kobzol, backup - @petrochenkov.
Required time: unclear.

### Stable option for controlling parallelism in `rustc`

`rustc` will have a single stable option called `-j` (short) or `--jobs` (long),
taking a number that will limit the amount of parallel jobs that the compiler can use.

The option name is borrowed from existing tools like [cargo](https://doc.rust-lang.org/cargo/commands/cargo-build.html#option-cargo-build---jobs),
or [make](https://linux.die.net/man/1/make), or [cmake](https://cmake.org/cmake/help/latest/manual/cmake.1.html#cmdoption-cmake-build-j).

This limit will affect all kinds of parallelism used internally by the compiler - frontend
parallelism (parallel queries), backend parallelism (parallel codegen for multiple codegen units),
linker parallelism (if supported).
If necessary, the compiler can use *less* parallelism in some of its parts if it knows that using
more will cause performance issues.

This limit only gives a static upper bound for the parallelism, the actual usable resources will
also be controlled dynamically by [jobserver](https://doc.rust-lang.org/rustc/jobserver.html).
So if cargo is run with `-jN`, it will pass to rustc both `-jN` option for the statically known limit,
and the jobserver for dynamic adaptability.

Why one option - the user typically doesn't need to be aware of the compiler's internal details
to limit resources available to it.
Other options like `--jobs-frontend`, `--jobs-backend` or `--jobs-linker` can be additionally
implemented for benchmarking and advanced use cases, but don't need to be stable or block
stabilization of the single common `--jobs`.

If `-j` is not passed to rustc, we maintain the current default behavior for compatibility,
i.e. 1) no paralllel frontend, 2) parallel codegen, and 3) parallel linking when the used linker
does it by default.

Responsible people: implementation - @zetanumbers, reviews - @bjorn3 or @Zoxc,
cargo implementation - @Bryanskiy, cargo reviews - cargo maintainers, backup - @petrochenkov.
Required time: 1 month.

### Preserving deterministic compilation

By deterministic compilation (reproducible builds) here we'll understand obtaining identical
binaries (or other compiler outputs) when rustc is run multiple times on the same source code
with the same options and environment.

The reproducibility may be needed in a number of scenarios:
- When compiler outputs are compared to some existing snapshots for testing (rustc's own UI test suite)
- When compiler outputs are [cached](https://rust-lang.zulipchat.com/#narrow/channel/187679-t-compiler.2Fparallel-rustc/topic/Adding.20a.20new.20option.20to.20parallel.20front-end/near/599644554) between multiple compilations using tools like sccache.
  In this case different outputs will break the caching even if they are functionally equivalent.
- [Other scenarios](https://reproducible-builds.org/docs/why/), including security-oriented.

However, for the majority of compiler uses the reproducibility is not an issue, for example:
- Building and running the project to check whether the tests pass, on CI or locally.
- Compiling the project with `cargo check` to see and fix the reported errors.

Parallel frontend has fundamental issues with deterministic compilation (see "Reproducibility issues" above).
- Some issues may be fixable, but simply require a lot of resources to fix.
- Some fixes may make compilation slower.
- In some cases it's [not clear](https://github.com/rust-lang/rust/issues/49737#issuecomment-2833364108)
  whether the fix is worth it, e.g. if it requires delaying diagnostic reporting until the compilation is finished.

So we propose not blocking stabilization of the parallel frontend on resolving the determinism issues,
but instead provide an option that allows to request determinism (or non-determinism) explicitly.

Tentative option name - `--deterministic`.
The option either doesn't take any arguments, or can take boolean arguments like `yes`/`no`.
Eventually the option may allow more fine-grained control with something like `--deterministic=binary`
or `--deterministic=diagnostics`, but this MCP doesn't propose it.

The initial implementation of this option in rustc will simply turn off the frontend parallelism,
while keeping the default codegen/linking parallelism.
Perhaps later we'll be able to turn it off only partially.

It's not entirely clear how the defaults for this option should work.
For backward compatibility we need determinism to be enabled by default,
but we can disable it by default if `-j` is passed.
But then if cargo always passes through `-j` to rustc, then for compatibility it will also need
to pass `--deterministic=true` together with `-j`.
This option will need to be supported on cargo command line and in cargo config files.

Responsible people: implementation - @zetanumbers, reviews - anyone?, cargo implementation - @Bryanskiy,
cargo reviews - cargo maintainers, backup - @petrochenkov.
Required time: 2 weeks.

### Enabling on nightly

After all the options are implemented, the parallel frontend needs to be enabled by default
on nightly by default, and we'll need to collect user feedback for it for at least ~3 months.
A blog post announcement will need to be prepared.

It makes sense to limit the parallelism to `-j2`, this way we may get fewer reported bugs and
reproducibility issues, but we'll at least get less reports related to resource exhaustion issues like OOMs.

What kind of feedback we expect:
- Crashes, incorrect compilation, any similar issues.
- More use cases requiring enabling `--deterministic`.
- Potential performance and memory use issues.

Nightly users encountering issues of any kind will be able to mitigate them by passing
`--deterministic` to rustc, or even `-j1` in case of resource exhaustion, but that will be slower
(and less resource hungry) than what we have on nightly now.

The feedback will be collected, issues will be created and debugged,
starting from crashes and incorrect compilations.
We are ready to spend another 3 months on that.

Responsible people: implementation / blog post - @petrochenkov, debugging - @zetanumbers and optionally @Zoxc,
backup - @petrochenkov, something in cargo? - @Bryanskiy and cargo maintainers
Required time: 3-6 months.

### Stabilization

Stabilize `-j` and `--deterministic`, finalize the decisions on defaults and integration with cargo,
write an announcement post.

Stabilize the corresponding cargo options simultaneously, or some time later.

Responsible people: implementation / blog post - @petrochenkov,
something in cargo? - @Bryanskiy and cargo maintainers
