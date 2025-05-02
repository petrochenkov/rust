//@ build-fail
//@ dont-check-compiler-stderr
//@ aux-build:panic-runtime-unwind.rs
//@ aux-build:panic-runtime-unwind2.rs
//@ aux-build:panic-runtime-lang-items.rs

#![no_std]
#![no_main]

extern crate panic_runtime_unwind;
extern crate panic_runtime_unwind2;
extern crate panic_runtime_lang_items;

fn main() {}

//~? ERROR cannot link together two panic runtimes: panic_runtime_unwind and panic_runtime_unwind2
//~? ERROR? the linked panic runtime `panic_runtime_unwind2` is not compiled with this crate's panic strategy `abort`
//~? ERROR? the crate `panic_runtime_unwind` requires panic strategy `unwind` which is incompatible with this crate's strategy of `abort`
