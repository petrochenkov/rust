// Check for unused crate dep, no path

// edition:2018
// check-pass
// aux-crate:bar=bar.rs
// ignore-tidy-linelength

#![warn(unused_crate_deps)]
//~^ WARNING External crate `bar` unused in `warn_attr_dylib`. Remove the dependency or add `use bar as _;`.

fn main() {}
