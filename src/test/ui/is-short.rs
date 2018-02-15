// Copyright 2017 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#![feature(rustc_attrs)]
#![rustc_alternative_is_bindings_scope]

#![warn(unused)]

fn main() {
    let my_opt = Some(15);
    if my_opt is Some(x) &&
       x > 10 {
        println!("{:?}", x);
    }

    let mut iter = my_opt.into_iter();
    while iter.next() is Some(mut y) &&
          y > 10 {
        println!("{:?}", y);
    }

    my_opt is Some(z);

    my_opt is Some(unused); // `unused` warining are reported later

    let d = '6';
    let is_digit = d is '0' ... '9' && d > '5';
    if is_digit {
        println!("{:?}", d);
    }

    // Variables bound in `is` are in scope only for their full expression
    let _x = x; //~ ERROR cannot find value `x` in this scope
    let _y = y; //~ ERROR cannot find value `y` in this scope
    let _z = z; //~ ERROR cannot find value `z` in this scope
}
