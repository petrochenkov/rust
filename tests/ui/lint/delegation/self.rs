#![deny(delegated_methods, delegation_pattern)]

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
        //~^ ERROR dstats. stmts: ZeroWithTail, args_match: Same, ret_match: Same, self_arg: Type, same_name: true, has_expr_after: false.
    }

    fn foo<T>(self, x: T) -> T {
        self.field.foo::<T>(x)
        //~^ ERROR dstats. stmts: ZeroWithTail, args_match: Same, ret_match: Same, self_arg: Value, same_name: true, has_expr_after: false.
    }
}

fn foo<T>(x: T) -> T {
    F::bar::<T>(x)
    //~^ ERROR dstats. stmts: ZeroWithTail, args_match: Same, ret_match: Same, self_arg: Other, same_name: false, has_expr_after: false.
}

fn bar(x: i32) {
    F::get_self().foo::<i32>(x);
    //~^ ERROR dstats. stmts: OneWithoutTail, args_match: Same, ret_match: Different, self_arg: Value, same_name: false, has_expr_after: false.
    //~| ERROR dstats. stmts: OneWithoutTail, args_match: Different, ret_match: Different, self_arg: Other, same_name: false, has_expr_after: true.
}

trait Trait {
    fn foo<T>(x: T) -> T {
        foo(x)
        //~^ ERROR dstats. stmts: ZeroWithTail, args_match: Same, ret_match: Same, self_arg: Type, same_name: true, has_expr_after: false.
    }
}

fn main() {}
