//@ check-pass

fn main() {
    let _ = #[deny(warnings)] if true {
    } else if false {
    } else {
    };
}
