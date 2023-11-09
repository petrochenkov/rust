#![deny(delegations_detailed, delegations_per_parent_stats)]

struct A;

impl A {
    fn foo(&self) -> i32 { 0 }
    fn bar(&self) -> Self { A }
}

struct B(A);

impl B {
    fn foo(&self) {
        self.0.foo().clone() as i32;
        //~^ ERROR caller_parent: InherentImpl, stmts_before: false, arg0_match: Different, arg0_preproc: Other, args_match: Same, args_preproc: No, ret_match: Different, has_self: true, caller_has_self: true, same_name: false, ret_postproc: true
        //~| ERROR caller_parent: InherentImpl, stmts_before: false, arg0_match: Different, arg0_preproc: Field, args_match: Same, args_preproc: No, ret_match: Different, has_self: true, caller_has_self: true, same_name: true, ret_postproc: true
    }

    fn bar(&self) -> Self {
        Self(self.0.bar())
        //~^ ERROR caller_parent: InherentImpl, stmts_before: false, arg0_match: Different, arg0_preproc: Other, args_match: Same, args_preproc: No, ret_match: Same, has_self: false, caller_has_self: true, same_name: false, ret_postproc: false
        //~| ERROR caller_parent: InherentImpl, stmts_before: false, arg0_match: Different, arg0_preproc: Field, args_match: Same, args_preproc: No, ret_match: SameUpToSelfType, has_self: true, caller_has_self: true, same_name: true, ret_postproc: true
    }
}

fn main() {}
