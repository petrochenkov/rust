#![deny(delegations_per_parent_stats, delegation_pattern)]

struct A;

impl A {
    fn foo(&self) -> i32 { 0 }
    fn bar(&self) -> Self { A }
}

struct B(A);

impl B {
    fn foo(&self) {
        self.0.foo().clone() as i32;
        //~^ ERROR dstats. stmts: OneWithoutTail, args_match: Same, ret_match: Different, self_arg: Value, same_name: false, has_expr_after: true.
        //~| ERROR dstats. stmts: OneWithoutTail, args_match: Same, ret_match: Different, self_arg: Value, same_name: true, has_expr_after: true.
    }

    fn bar(&self) -> Self {
        Self(self.0.bar())
        //~^ ERROR dstats. stmts: ZeroWithTail, args_match: Different, ret_match: Same, self_arg: Type, same_name: false, has_expr_after: false.
        //~| ERROR dstats. stmts: ZeroWithTail, args_match: Same, ret_match: SameUpToSelfType, self_arg: Value, same_name: true, has_expr_after: true.
    }
}

fn main() {}
