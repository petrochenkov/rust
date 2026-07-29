//@ check-pass

const X: i32 = #[allow(dead_code)] 8;

const Y: i32 =
    /// foo
    8;

const Z: i32 = {
    //! foo
    8
};

fn main() {}
