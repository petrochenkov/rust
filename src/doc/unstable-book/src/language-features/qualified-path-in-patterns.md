# `qualified_path_patterns`

The `qualified_path_in_patterns` feature can be used in order to enable the
use of qualified paths in patterns.

## Example

```rust
#![feature(qualified_path_in_patterns)]

fn main() {
    // destructure through a qualified path
    let <Foo as A>::Assoc { br } = StructStruct { br: 2 };
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
```
