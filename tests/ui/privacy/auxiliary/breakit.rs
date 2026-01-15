#![feature(rustc_attrs)]

// -------------------------------------------

pub trait PubTr {
    type Assoc;
}

struct Priv;

// Through this impl we can name private `Priv` from different crates.
impl PubTr for u8 {
    type Assoc = Priv;
}

// -------------------------------------------

pub trait GetUnreachable {
    type Assoc;
}

mod m {
    pub struct Unreachable;

    impl Unreachable {
        // #[rustc_effective_visibility] reports: Direct: pub(crate), Reexported: pub(crate),
        // Reachable: pub(crate), ReachableThroughImplTrait: pub(crate)
        // So the function is unreachable, and its MIR shouldn't be encoded.
        pub fn generic<T>() {}
    }

    // Does not make `Unreachable` reachable because `Priv` is private.
    impl crate::GetUnreachable for crate::Priv {
        type Assoc = Unreachable;
    }
}
