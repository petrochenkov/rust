struct SemiPriv;
//@ check-pass
mod m1 {
    struct Priv;
    impl crate::SemiPriv {
        pub fn f(_: Priv) {} //~ WARN type `m1::Priv` is more private than the item `m1::<impl SemiPriv>::f`
    }

    impl Priv {
        pub fn f(_: Priv) {} // ok
    }
}

mod m2 {
    struct Priv;
    impl std::ops::Deref for crate::SemiPriv {
        type Target = Priv; //~ WARN type `m2::Priv` is more private than the item `m2::<impl Deref for SemiPriv>::Target`
        fn deref(&self) -> &Self::Target { unimplemented!() }
    }

    impl std::ops::Deref for Priv {
        type Target = Priv; // ok
        fn deref(&self) -> &Self::Target { unimplemented!() }
    }
}

trait SemiPrivTrait {
    type Assoc;
}

mod m3 {
    struct Priv;
    impl crate::SemiPrivTrait for () {
        type Assoc = Priv; //~ WARN type `m3::Priv` is more private than the item `m3::<impl SemiPrivTrait for ()>::Assoc`
    }
}

fn main() {}
