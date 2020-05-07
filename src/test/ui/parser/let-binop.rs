fn main() {
    let a: i8 *= 1; //~ ERROR expected one of `!`, `(`, `+`, `::`, `;`, `<`, or `=`, found `*=`
    let _ = a;
    let b += 1;
    let _ = b;
    let c *= 1;
    let _ = c;
}
