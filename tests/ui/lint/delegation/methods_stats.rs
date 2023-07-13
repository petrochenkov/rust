#![deny(delegated_methods, delegation_pattern)]

struct F;

impl F {
    fn foo1(&self) {}
    fn foo2(&self) {}
    fn foo3(&self) {}
    fn foo4(&self) {}
}

struct S1 {
    field: F,
}

impl S1 {
    fn foo1(&self) { self.field.foo1();} //~ ERROR
}

struct S2 {
    field: F,
}

impl S2 {
    fn foo1(&self) {self.field.foo1();} //~ ERROR
    fn foo2(&self) {self.field.foo2();} //~ ERROR
}

struct S3 {
    field: F,
}

impl S3 {
    fn foo1(&self) {self.field.foo1();} //~ ERROR
    fn foo2(&self) {self.field.foo2();} //~ ERROR
    fn foo3(&self) {self.field.foo3();} //~ ERROR
}

struct S4 {
    field: F,
}

impl S4 {
    fn foo1(&self) {self.field.foo1();} //~ ERROR
    fn foo2(&self) {self.field.foo2();} //~ ERROR
    fn foo3(&self) {self.field.foo3();} //~ ERROR
    fn foo4(&self) {self.field.foo4();} //~ ERROR
}

fn main() {}
