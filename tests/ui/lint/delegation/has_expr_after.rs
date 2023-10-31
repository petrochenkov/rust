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
        //~^ ERROR dstats. stmts: OneWithoutTail, args_match: Same, ret_match: Different, callee_has_self: true, caller_has_self: true, same_name: false, has_expr_after: true.
        //~| ERROR dstats. stmts: OneWithoutTail, args_match: Same, ret_match: Different, callee_has_self: true, caller_has_self: true, same_name: true, has_expr_after: true.
    }

    fn bar(&self) -> Self {
        Self(self.0.bar())
        //~^ ERROR dstats. stmts: ZeroWithTail, args_match: Same, ret_match: Same, callee_has_self: false, caller_has_self: true, same_name: false, has_expr_after: false.
        //~| ERROR dstats. stmts: ZeroWithTail, args_match: Same, ret_match: SameUpToSelfType, callee_has_self: true, caller_has_self: true, same_name: true, has_expr_after: true.
    }
}

fn main() {}
