// check-pass
// aux-crate:bar=bar.rs
// compile-flags:--crate-type lib -Wunused-crate-deps

pub fn fib(n: u32) -> Vec<u32> {
//~^ External crate `bar` unused in `libfib`. Remove the dependency or add `use bar as _;`.
let mut prev = 0;
    let mut cur = 1;
    let mut v = vec![];

    for _ in 0..n {
        v.push(prev);
        let n = prev + cur;
        prev = cur;
        cur = n;
    }

    v
}
