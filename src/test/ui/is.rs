// Copyright 2017 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

fn main() {
    let my_opt = Some(15);
    if my_opt is Some(x) && //~ ERROR type annotations needed
       x > 10 {
        println!("{:?}", x);
    }

    let iter = my_opt.into_iter();
    while iter.next() is Some(y) &&
          y > 10 {
        println!("{:?}", y);
    }

    let z = '6';
    let is_digit = z is '0' ... '9' && z > '5';
    if is_digit {
        println!("{:?}", z);
    }

    let _ = x;
    let _ = y;
    let _ = z;
}
