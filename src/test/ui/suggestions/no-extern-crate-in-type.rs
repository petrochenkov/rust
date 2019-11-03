// aux-build:foo.rs

extern crate foo;

type Output = Option<Foo>; //~ ERROR cannot find value `Foo`

fn main() {}
