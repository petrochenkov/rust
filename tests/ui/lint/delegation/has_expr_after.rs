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
        //~^ ERROR dstats. parent: InherentImpl, stmts: OneWithoutTail, delegate_to: Other, args_match: Same, args_preproc: false, ret_match: Different, callee_has_self: true, caller_has_self: true, same_name: false, ret_postproc: true.
        //~| ERROR dstats. parent: InherentImpl, stmts: OneWithoutTail, delegate_to: Field, args_match: Same, args_preproc: false, ret_match: Different, callee_has_self: true, caller_has_self: true, same_name: true, ret_postproc: true.
    }

    fn bar(&self) -> Self {
        Self(self.0.bar())
        //~^ ERROR dstats. parent: InherentImpl, stmts: ZeroWithTail, delegate_to: Other, args_match: Same, args_preproc: false, ret_match: Same, callee_has_self: false, caller_has_self: true, same_name: false, ret_postproc: false.
        //~| ERROR dstats. parent: InherentImpl, stmts: ZeroWithTail, delegate_to: Field, args_match: Same, args_preproc: false, ret_match: SameUpToSelfType, callee_has_self: true, caller_has_self: true, same_name: true, ret_postproc: true.
    }
}

fn main() {}
