// Make sure that users can construct structs through associated types
// in both expressions and patterns

// check-pass
fn main() {
    let <Foo as A>::Assoc { br: br } = <Foo as A>::Assoc { br: 2 };
    let <Bar as A>::Assoc(n) = <Bar as A>::Assoc(2);
    assert!(br == 2);
    assert!(n == 2);
}

struct StructStruct {
    br: i8,
}

struct TupleStruct(i8);

struct Foo;
struct Bar;


trait A {
    type Assoc;
}

impl A for Foo {
    type Assoc = StructStruct;
}

impl A for Bar {
    type Assoc = TupleStruct;
}
