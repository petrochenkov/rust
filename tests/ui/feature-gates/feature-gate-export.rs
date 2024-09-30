#![crate_type="lib"]

#[export]
//~^ ERROR the `#[export]` attribute is an experimental feature
pub mod a {}
