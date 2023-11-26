macro_rules! reuse { {} => {} }

mod reuse {
    use crate::reuse;

    pub fn to_unsafe(x: i32) -> i32 { x + 1 }
    pub fn to_pub() {}

    #[allow(unused, non_camel_case_types)]
    struct reuse {
        a: i32,
        b: i32,
        c: i32,
    }

    impl reuse {
        reuse!();
    }

    #[allow(unused)]
    fn baz() {
        let (a, b, c) = (0, 0, 0);
        reuse {a, b, c};
    }
}

reuse!();

unsafe reuse reuse::to_unsafe;
//~^ ERROR expected item, found keyword `unsafe`
#[inline]
pub reuse reuse::to_pub;

fn main() {}
