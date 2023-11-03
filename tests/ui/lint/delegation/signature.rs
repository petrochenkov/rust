#![deny(delegations_detailed, delegations_per_parent_stats)]

use std::hash::{Hash, Hasher};
use std::ops::Add;

trait Foo {
    fn foo(&mut self) -> Self;
    fn bar(&self) -> i32;
}

impl Foo for u32 {
    fn foo(&mut self) -> Self { 1 as u32 }
    fn bar(&self) -> i32 { 1 }
}

struct NonZeroU32(u32);

impl NonZeroU32 {
    fn foo(&mut self) -> Self {
        Self(self.0.foo())
        //~^ ERROR dstats. parent: InherentImpl, stmts: ZeroWithTail, delegate_to: Other, args_match: Same, args_preproc: false, ret_match: Same, callee_has_self: false, caller_has_self: true, same_name: false, ret_postproc: false.
        //~| ERROR dstats. parent: InherentImpl, stmts: ZeroWithTail, delegate_to: Field, args_match: Same, args_preproc: false, ret_match: SameUpToSelfType, callee_has_self: true, caller_has_self: true, same_name: true, ret_postproc: true.
    }

    fn bar(&self) -> i32 {
        self.0.bar()
        //~^ ERROR dstats. parent: InherentImpl, stmts: ZeroWithTail, delegate_to: Field, args_match: Same, args_preproc: false, ret_match: Same, callee_has_self: true, caller_has_self: true, same_name: true, ret_postproc: false.
    }
}

impl Hash for NonZeroU32 {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
        //~^ ERROR dstats. parent: TraitImpl, stmts: OneWithoutTail, delegate_to: Field, args_match: Same, args_preproc: false, ret_match: Same, callee_has_self: true, caller_has_self: true, same_name: true, ret_postproc: false.
    }
}

impl Add for NonZeroU32 {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self (self.0.add(other.0))
        //~^ ERROR dstats. parent: TraitImpl, stmts: ZeroWithTail, delegate_to: Other, args_match: Different, args_preproc: false, ret_match: Same, callee_has_self: false, caller_has_self: true, same_name: false, ret_postproc: false.
        //~| ERROR dstats. parent: TraitImpl, stmts: ZeroWithTail, delegate_to: Field, args_match: SameUpToSelfType, args_preproc: true, ret_match: SameUpToSelfType, callee_has_self: true, caller_has_self: true, same_name: true, ret_postproc: true.
    }
}

mod iter {
    trait Iter {
        type Item;

        fn next(&self) -> Option<Self::Item>;
        fn last(&self) -> Option<Self::Item>;
    }

    impl<T: Iter> Iter for &mut T {
        type Item = T::Item;
        #[inline]
        fn next(&self) -> Option<T::Item> {
            (**self).next()
            //~^ ERROR dstats. parent: TraitImpl, stmts: ZeroWithTail, delegate_to: Other, args_match: Same, args_preproc: false, ret_match: Same, callee_has_self: true, caller_has_self: true, same_name: true, ret_postproc: false.
        }
        fn last(&self) -> Option<Self::Item> {
            (**self).last()
            //~^ ERROR dstats. parent: TraitImpl, stmts: ZeroWithTail, delegate_to: Other, args_match: Same, args_preproc: false, ret_match: Same, callee_has_self: true, caller_has_self: true, same_name: true, ret_postproc: false.
        }
    }
}

mod anon1 {
    trait Trait {}
    struct Struct;

    fn bar() -> Struct { Struct }

    impl Trait for Struct {}

    impl Struct {
        pub fn new() -> Box<dyn Trait> {
            Box::new(Struct)
            //~^ ERROR dstats. parent: InherentImpl, stmts: ZeroWithTail, delegate_to: Other, args_match: Different, args_preproc: true, ret_match: Coerced, callee_has_self: false, caller_has_self: false, same_name: true, ret_postproc: false.
        }

        pub fn bar() -> impl Trait {
            bar()
            //~^ ERROR dstats. parent: InherentImpl, stmts: ZeroWithTail, delegate_to: Other, args_match: Same, args_preproc: false, ret_match: Same, callee_has_self: false, caller_has_self: false, same_name: true, ret_postproc: false.
        }
    }
}

mod anon2 {
    use std::fmt::Display;
    struct F<'a>(&'a u32);
    impl<'a> F<'a> {
        fn foo(&self) -> &'a u32 {
            self.0
        }
    }

    fn foo<'a>(f: &F<'a>) -> &'a dyn Display {
        f.foo()
        //~^ ERROR dstats. parent: Other, stmts: ZeroWithTail, delegate_to: Other, args_match: Same, args_preproc: false, ret_match: Coerced, callee_has_self: true, caller_has_self: false, same_name: true, ret_postproc: false.
    }
}

mod coerce {
    use std::fmt::Display;

    struct F;
    impl F {
        fn foo(&self, str: &str) {}
        fn bar(x: &u32) -> &u32 { x }
    }

    struct S(F);

    impl S {
        fn foo(&self, str: &mut str) {
            self.0.foo(str)
            //~^ ERROR dstats. parent: InherentImpl, stmts: ZeroWithTail, delegate_to: Field, args_match: Coerced, args_preproc: false, ret_match: Same, callee_has_self: true, caller_has_self: true, same_name: true, ret_postproc: false.
        }

        fn bar(x: &u32) -> &dyn Display {
            F::bar(x)
            //~^ ERROR dstats. parent: InherentImpl, stmts: ZeroWithTail, delegate_to: Other, args_match: Same, args_preproc: false, ret_match: Coerced, callee_has_self: false, caller_has_self: false, same_name: true, ret_postproc: false.
        }
    }
}

fn main() {}
