fn main() {
    let y = #[repr(uwu(4))]
    //~^ ERROR malformed `repr` attribute input
    (&id(5)); //~ ERROR: cannot find function `id` in this scope
}
