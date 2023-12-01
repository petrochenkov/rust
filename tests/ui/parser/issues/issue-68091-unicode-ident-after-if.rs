macro_rules! x {
    ($($c:tt)*) => {
        $($c)ö* {}
    };
}

fn main() {
    x!(if); //~ ERROR missing condition for `if` expression
}
