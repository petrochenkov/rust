// Make sure that users can construct structs through associated types
// in both expressions and patterns

// check-pass
fn main() {
    let <Bar as A>::Assoc { br: _br } = <Bar as A>::Assoc { br: 2 };
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