mod a {
    pub use crate::b::x; //~ ERROR unresolved import `crate::b::x`
    //~^ ERROR cannot determine resolution for the import
}

mod b {
    pub use crate::a::x; //~ ERROR cannot determine resolution for the import

    fn main() { let y = x; }
}

fn main() {}
