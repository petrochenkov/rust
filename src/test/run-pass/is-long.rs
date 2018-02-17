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
    if my_opt is Some(x) &&
       x > 10 {
        assert_eq!(x, 15);
    } else {
        panic!();
    }

    let mut count = 0;
    let mut iter = my_opt.into_iter();
    while iter.next() is Some(mut y) &&
          y > 10 {
        assert_eq!(y, 15);
        count += 1;
    }
    assert_eq!(count, 1);

    let b = my_opt is Some(z);
    assert_eq!(b, true);

    if Ok(10) is Ok(u) | Err(u) {
        assert_eq!(u, 10);
    }

    let d = '6';
    let is_digit = d is '0' ... '9' && d > '5';
    if is_digit {
        // OK
    } else {
        panic!();
    }
}
