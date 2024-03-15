//@ check-pass

#![feature(fn_delegation)]
#![allow(incomplete_features)]

trait Trait {
    fn foo(&self) -> u8 { 0 }
    fn bar(&self) -> u8 { 1 }
}

impl Trait for u8 {}

struct S(u8);
struct Z(u8);

impl Trait for S {
    reuse Trait::{foo, bar} { &self.0 }
}

impl Trait for Z {
    // reuse <S as Trait>::{foo, bar} { &self.0 }
}

fn main() {
    let s = S(2);
    s.foo();
    s.bar();
}
