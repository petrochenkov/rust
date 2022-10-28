// edition:2021
//! Missing docs lint warns about undocumented exported items.
//! Use the lint to additionally verify that items are reachable
//! but not exported.
#![allow(non_camel_case_types)]
#![deny(missing_docs)]
#![feature(rustc_attrs)]

mod hidden {
    pub struct s;
    // #[rustc_effective_visibility]
    pub enum e {
        // #[rustc_effective_visibility]
        x,
        y,
        z,
    }
    // #[rustc_effective_visibility]
    pub use e::*;
    impl s {
        pub fn f(&self) {}
    }
    impl e {
        pub fn g(&self) {}
    }
}
// Hide all type definitions while reexporting their constructors:
mod e {}
mod x {}
mod y {}
mod z {}
mod s {}
// #[rustc_effective_visibility]
pub use hidden::*;
