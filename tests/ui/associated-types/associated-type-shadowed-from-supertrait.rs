// Test Setting the value of an associated type
// that is shadowed from a supertrait

pub trait Super {
    type X;
}

pub trait Sub: Super {
    type X;
}

impl<T> Clone for Box<dyn Sub<X = T>> {
    //~^ ERROR E0191
    fn clone(&self) -> Self {
        unimplemented!();
    }
}

pub fn main() {}
