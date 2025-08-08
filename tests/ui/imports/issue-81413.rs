pub const ITEM: Item = Item;

pub struct Item;

pub fn item() {}

pub use doesnt_exist::*;
//~^ ERROR unresolved import `doesnt_exist`
mod a {
    use crate::{item, Item, ITEM};
    //~^ ERROR cannot determine resolution for the import
    //~| ERROR cannot determine resolution for the import
    //~| ERROR cannot determine resolution for the import
    //~| ERROR cannot determine resolution for the import
    //~| ERROR cannot determine resolution for the import
    //~| ERROR cannot determine resolution for the import
    //~| ERROR cannot determine resolution for the import
}

mod b {
    use crate::item;
    //~^ ERROR cannot determine resolution for the import
    //~| ERROR cannot determine resolution for the import
    use crate::Item;
    //~^ ERROR cannot determine resolution for the import
    //~| ERROR cannot determine resolution for the import
    //~| ERROR cannot determine resolution for the import
    use crate::ITEM;
    //~^ ERROR cannot determine resolution for the import
    //~| ERROR cannot determine resolution for the import
}

mod c {
    use crate::item;
    //~^ ERROR cannot determine resolution for the import
    //~| ERROR cannot determine resolution for the import
}

fn main() {}
