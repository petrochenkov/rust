pub type T = ();
mod foo { pub use super::T; }
//~^ ERROR unresolved imports `super::T`, `super::T`, `super::T`, `super::foo::S`
//~| ERROR cannot determine resolution for the import
//~| ERROR cannot determine resolution for the import
mod bar { pub use super::T; }
//~^ ERROR cannot determine resolution for the import
//~| ERROR cannot determine resolution for the import

pub use foo::*;
pub use bar::*;

mod baz {
    pub type T = ();
    mod foo { pub use super::T as S; }
    //~^ ERROR cannot determine resolution for the import
    //~| ERROR cannot determine resolution for the import
    mod bar { pub use super::foo::S as T; }
    //~^ ERROR cannot determine resolution for the import
    pub use self::bar::*;
}

fn main() {}
