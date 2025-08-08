//@ edition:2018

// Built-in attribute
use inline as imported_inline;
mod builtin {
    pub use inline as imported_inline;
}

// Tool module
use rustfmt as imported_rustfmt;
mod tool_mod {
    pub use rustfmt as imported_rustfmt;
}

#[imported_inline]
#[builtin::imported_inline] //~ ERROR cannot determine resolution for the attribute macro
#[imported_rustfmt::skip] //~ ERROR cannot use a tool module through an import
#[tool_mod::imported_rustfmt::skip] //~ ERROR cannot use a tool module through an import
fn main() {}
