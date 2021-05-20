// Make sure that users can construct structs through associated types
// in both expressions and patterns

#![feature(qualified_path_in_patterns)]

// check-pass
fn main() {
    let <Foo as A>::Assoc { br } = <Foo as A>::Assoc { br: 2 };
    assert!(br == 2);
}

struct StructStruct {
    br: i8,
}

struct Foo;

trait A {
    type Assoc;
}

impl A for Foo {
    type Assoc = StructStruct;
}
