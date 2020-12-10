#![feature(const_trait_impl)]
#![allow(incomplete_features)]

trait Foo {}

const impl Foo for i32 {} //~ ERROR

trait Bar {}

const impl<T: Foo> Bar for T {} //~ ERROR

fn main() {}
