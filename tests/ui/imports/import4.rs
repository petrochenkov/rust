mod a { pub use crate::b::foo; } //~ ERROR unresolved import `crate::b::foo`
                                 //~| ERROR cannot determine resolution for the import
mod b { pub use crate::a::foo; } //~ ERROR cannot determine resolution for the import

fn main() { println!("loop"); }
