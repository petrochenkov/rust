//! Some stuff used by rustc that doesn't have many dependencies
//!
//! Originally extracted from rustc::back, which was nominally the
//! compiler 'backend', though LLVM is rustc's backend, so rustc_target
//! is really just odds-and-ends relating to code gen and linking.
//! This crate mostly exists to make rustc smaller, so we might put
//! more 'stuff' here in the future. It does not have a dependency on
//! LLVM.

#![doc(html_root_url = "https://doc.rust-lang.org/nightly/nightly-rustc/")]
#![feature(bool_to_option)]
#![feature(const_fn)]
#![feature(const_panic)]
#![feature(nll)]
#![feature(never_type)]
#![feature(associated_type_bounds)]
#![feature(exhaustive_patterns)]

#[macro_use]
extern crate rustc_macros;

#[macro_use]
extern crate tracing;

pub mod abi;
pub mod asm;
pub mod spec;

/// Requirements for a `StableHashingContext` to be used in this crate.
/// This is a hack to allow using the `HashStable_Generic` derive macro
/// instead of implementing everything in librustc_middle.
pub trait HashStableContext {}

pub fn host_triple() -> &'static str {
    // Get the host triple out of the build environment. This ensures that our
    // idea of the host triple is the same as for the set of libraries we've
    // actually built.  We can't just take LLVM's host triple because they
    // normalize all ix86 architectures to i386.
    //
    // Instead of grabbing the host triple (for the current host), we grab (at
    // compile time) the target triple that this rustc is built with and
    // calling that (at runtime) the host triple.
    (option_env!("CFG_COMPILER_HOST_TRIPLE")).expect("CFG_COMPILER_HOST_TRIPLE")
}
