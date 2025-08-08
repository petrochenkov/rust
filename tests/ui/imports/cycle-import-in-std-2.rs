//@ edition: 2018

// https://github.com/rust-lang/rust/issues/125013

use ops::{self as std};
//~^ ERROR: unresolved import `ops`
//~| ERROR cannot determine resolution for the import
use std::ops::Deref::{self as ops};

fn main() {}
