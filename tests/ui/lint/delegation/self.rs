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
        //~^ ERROR caller_parent: InherentImpl, stmts_before: false, arg0_match: Same, arg0_preproc: No, args_match: Same, args_preproc: No, ret_match: Same, has_self: false, caller_has_self: false, same_name: true, ret_postproc: false
    }

    fn foo<T>(self, x: T) -> T {
        self.field.foo::<T>(x)
        //~^ ERROR caller_parent: InherentImpl, stmts_before: false, arg0_match: Different, arg0_preproc: Field, args_match: Same, args_preproc: No, ret_match: Same, has_self: true, caller_has_self: true, same_name: true, ret_postproc: false
    }
}

fn foo<T>(x: T) -> T {
    F::bar::<T>(x)
    //~^ ERROR caller_parent: Other, stmts_before: false, arg0_match: Same, arg0_preproc: No, args_match: Same, args_preproc: No, ret_match: Same, has_self: false, caller_has_self: false, same_name: false, ret_postproc: false
}

fn bar(x: i32) {
    F::get_self().foo::<i32>(x);
    //~^ ERROR caller_parent: Other, stmts_before: false, arg0_match: Different, arg0_preproc: Other, args_match: DifferentCount, args_preproc: Other, ret_match: Different, has_self: true, caller_has_self: false, same_name: false, ret_postproc: true
    //~| ERROR caller_parent: Other, stmts_before: false, arg0_match: Different, arg0_preproc: Other, args_match: DifferentCount, args_preproc: Other, ret_match: Different, has_self: false, caller_has_self: false, same_name: false, ret_postproc: true
}

trait Trait {
    fn foo<T>(x: T) -> T {
        foo(x)
        //~^ ERROR caller_parent: Trait, stmts_before: false, arg0_match: Same, arg0_preproc: No, args_match: Same, args_preproc: No, ret_match: Same, has_self: false, caller_has_self: false, same_name: true, ret_postproc: false
    }
}

fn main() {}
