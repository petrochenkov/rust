pub mod foo {
    pub type X = u8;
}

pub mod bar {
    pub type X = u8;
}

pub use foo::*;
//~^ ERROR ambiguous glob re-exports
pub use bar::*;

pub fn main() {}
