// run-pass

mod to_reuse {
    use crate::S;

    pub fn foo<'a>(#[cfg(FALSE)] a: u8, _b: &'a S) -> u32 {
        1
    }
}

reuse to_reuse::foo;

trait Trait {
    fn foo(&self) -> u32 { 0 }
}

struct F;
impl Trait for F {}

struct S(F);

impl Trait for S {
    reuse to_reuse::foo { self }
}

fn main() {
    let s = S(F);
    assert_eq!(1, foo(&s));
    assert_eq!(1, s.foo());
}
