// Make sure that users can construct structs through associated types

// check-pass
fn main() {
    let _ = <Bar as A>::Assoc { br: 2 };
}

struct Foo {
    br: i8,
}

struct Bar;


trait A {
    type Assoc;
}

impl A for Bar {
    type Assoc = Foo;
}