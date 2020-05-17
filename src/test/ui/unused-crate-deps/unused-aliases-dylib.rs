// Warn about unused aliased for the crate

// edition:2018
// check-pass
// aux-crate:bar=bar.rs
// aux-crate:barbar=bar.rs
// ignore-tidy-linelength

#![warn(unused_crate_deps)]
//~^ WARNING External crate `barbar` unused in `unused_aliases_dylib`. Remove the dependency or add `use barbar as _;`.

use bar as _;

fn main() {}
