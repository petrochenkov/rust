//@ edition:2018

use derive as my_derive;

#[my_derive(Debug)] //~ ERROR cannot determine resolution for the attribute macro
struct S;

fn main() {
    println!("{:?}", S); //~ ERROR `S` doesn't implement `Debug`
}
