#![crate_type = "lib"]

pub mod f {}
pub use unresolved::f;
//~^ ERROR unresolved import `unresolved`

/// [g]
pub use f as g;
//~^ ERROR cannot determine resolution for the import
//~| ERROR cannot determine resolution for the import

fn main() {}
