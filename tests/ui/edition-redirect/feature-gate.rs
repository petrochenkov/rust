// gate-test-edition_redirect

pub struct Old;

#[rustc_edition_redirect = "2024"]
//~^ ERROR use of an internal attribute
pub use Old as Current;

pub struct Current;

fn main() {}
