// https://github.com/rust-lang/rust/pull/124840#issuecomment-2098148587

mod a {
    pub(crate) use crate::S;
    //~^ ERROR unresolved imports `crate::S`, `self::a::S`
    //~| ERROR cannot determine resolution for the import
    //~| ERROR cannot determine resolution for the import
    //~| ERROR cannot determine resolution for the import
}
mod b {
    pub struct S;
}
use self::a::S; //~ ERROR cannot determine resolution for the import
use self::b::*;

fn main() {}
