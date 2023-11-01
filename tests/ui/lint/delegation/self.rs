#![deny(delegations_detailed, delegations_per_parent_stats)]

struct F;

impl F {
    fn bar<T>(x: T) -> T { x }
    pub fn foo<T>(self, x: T) -> T { x }
    fn get_self() -> F { F }
}

struct S {
    field: F,
}

struct Stuple(F);

impl S {
    fn bar<T>(x: T) -> T {
        F::bar::<T>(x)
        //~^ ERROR dstats. parent: InherentImpl, stmts: ZeroWithTail, args_match: Same, args_preproc: false, ret_match: Same, callee_has_self: false, caller_has_self: false, same_name: true, ret_postproc: false.
    }

    fn foo<T>(self, x: T) -> T {
        self.field.foo::<T>(x)
        //~^ ERROR dstats. parent: InherentImpl, stmts: ZeroWithTail, args_match: Same, args_preproc: false, ret_match: Same, callee_has_self: true, caller_has_self: true, same_name: true, ret_postproc: false.
    }
}

fn foo<T>(x: T) -> T {
    F::bar::<T>(x)
    //~^ ERROR dstats. parent: Other, stmts: ZeroWithTail, args_match: Same, args_preproc: false, ret_match: Same, callee_has_self: false, caller_has_self: false, same_name: false, ret_postproc: false.
}

fn bar(x: i32) {
    F::get_self().foo::<i32>(x);
    //~^ ERROR dstats. parent: Other, stmts: OneWithoutTail, args_match: Different, args_preproc: true, ret_match: Different, callee_has_self: true, caller_has_self: false, same_name: false, ret_postproc: true.
    //~| ERROR dstats. parent: Other, stmts: OneWithoutTail, args_match: Different, args_preproc: false, ret_match: Different, callee_has_self: false, caller_has_self: false, same_name: false, ret_postproc: true.
}

trait Trait {
    fn foo<T>(x: T) -> T {
        foo(x)
        //~^ ERROR dstats. parent: Trait, stmts: ZeroWithTail, args_match: Same, args_preproc: false, ret_match: Same, callee_has_self: false, caller_has_self: false, same_name: true, ret_postproc: false.
    }
}

fn main() {}
