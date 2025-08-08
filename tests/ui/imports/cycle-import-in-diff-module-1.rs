// similar `cycle-import-in-diff-module-0.rs`

mod a {
    pub(crate) use crate::s;
    //~^ ERROR unresolved imports `crate::s`, `self::a::s`
    //~| ERROR cannot determine resolution for the import
    //~| ERROR cannot determine resolution for the import
}
mod b {
    pub mod s {}
}
use self::b::*;
use self::a::s; //~ ERROR cannot determine resolution for the import

fn main() {}
