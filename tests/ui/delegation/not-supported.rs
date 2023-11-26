#![feature(c_variadic)]

mod generics {
    trait GenericTrait<T> {
        fn foo(&self, x: T) -> T { x }
        fn foo1() {}
    }
    trait Trait {
        fn foo(&self, x: i32) -> i32 { x }
        fn foo1<'a>(&self, x: &'a i32) -> &'a i32 { x }
        fn foo2<T>(&self, x: T) -> T { x }
        fn foo3<'a: 'a>(_: &'a u32) {}
    }

    struct F;
    impl Trait for F {}
    impl<T> GenericTrait<T> for F {}

    struct S(F);

    impl<T> GenericTrait<T> for S {
        reuse <F as GenericTrait<T>>::foo { &self.0 }
        //~^ ERROR delegation with early bound generics is not supported yet
        reuse GenericTrait::<T>::foo1;
        //~^ ERROR delegation with early bound generics is not supported yet
    }

    impl Trait for S {
        reuse Trait::foo1 { &self.0 }
        reuse <F as Trait>::foo2 { &self.0 }
        //~^ ERROR delegation with early bound generics is not supported yet
        reuse <F as Trait>::foo3 { &self.0 }
        //~^ ERROR delegation with early bound generics is not supported yet
    }

    struct GenericS<T> {
        i32: T
    }

    impl<T: Trait> Trait for GenericS<T> {
        reuse <T as Trait>::foo { &self.0 }
        //~^ ERROR delegation with early bound generics is not supported yet
    }
}

mod opaque {
    trait Trait {}

    mod to_reuse {
        use super::Trait;

        pub fn opaque_arg(_: impl Trait) -> i32 { 0 }
        pub fn opaque_ret() -> impl Trait { unimplemented!() }
    }
    reuse to_reuse::opaque_arg;
    //~^ ERROR delegation with early bound generics is not supported yet
    reuse to_reuse::opaque_ret;
    //~^ ERROR delegation to a function with opaque type is not supported yet
}

mod fn_header {
    mod to_reuse {
        pub unsafe fn unsafe_fn() {}
        pub extern "C" fn extern_fn() {}
        pub unsafe extern "C" fn variadic_fn(n: usize, mut args: ...) {}
        pub const fn const_fn() {}
    }

    reuse to_reuse::unsafe_fn;
    //~^ ERROR delegation to unsafe functions is not supported yet
    reuse to_reuse::extern_fn;
    //~^ ERROR delegation to non Rust ABI functions is not supported yet
    reuse to_reuse::variadic_fn;
    //~^ ERROR delegation to variadic functions is not supported yet
    reuse to_reuse::const_fn;
    //~^ ERROR delegation to const functions is not supported yet
}

mod recursive {
    mod to_reuse1 {
        pub mod to_reuse2 {
            pub fn foo() {}
        }

        pub reuse to_reuse2::foo;
    }

    reuse to_reuse1::foo;
    //~^ ERROR recursive delegation is not supported yet
}

fn main() {}
