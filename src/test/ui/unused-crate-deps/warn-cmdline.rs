// Check for unused crate dep, no path

// edition:2018
// check-pass
// compile-flags: -Wunused-crate-deps
// aux-crate:bar=bar.rs
// no-prefer-dynamic
// ignore-tidy-linelength

fn main() {}
//~^ WARNING External crate `bar` unused in `warn_cmdline`. Remove the dependency or add `use bar as _;`.
