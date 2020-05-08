fn main() {
    let a: i8 *= 1; //~ ERROR blah
    let _ = a;
    let b += 1; //~ ERROR blah
    let _ = b;
    let c *= 1; //~ ERROR blah
    let _ = c;
}
