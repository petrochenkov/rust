use not_existing_crate::*; //~ ERROR unresolved import `not_existing_crate
use std as foo;
//~^ ERROR cannot determine resolution for the import
//~| ERROR cannot determine resolution for the import

fn main() {}
