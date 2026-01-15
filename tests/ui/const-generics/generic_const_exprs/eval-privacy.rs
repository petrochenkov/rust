#![crate_type = "lib"]
#![feature(generic_const_exprs)]
#![allow(incomplete_features)]
//@ check-pass
pub struct Const<const U: u8>;

pub trait Trait {
    type AssocTy;
    fn assoc_fn() -> Self::AssocTy;
}

impl<const U: u8> Trait for Const<U>
where
    // OK, trait impl predicates
    Const<{ my_const_fn(U) }>:,
{
    type AssocTy = Const<{ my_const_fn(U) }>;
    //~^ WARN type `fn(u8) -> u8 {my_const_fn}` is more private than the item `<Const<U> as Trait>::AssocTy`
    //~| WARN type `fn(u8) -> u8 {my_const_fn}` is more private than the item `<Const<U> as Trait>::AssocTy`
    fn assoc_fn() -> Self::AssocTy {
        Const
    }
}

const fn my_const_fn(val: u8) -> u8 {
    // body of this function doesn't matter
    val
}
