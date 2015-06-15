#![cfg_attr(not(feature = "with-syntex"), feature(plugin))]
#![cfg_attr(not(feature = "with-syntex"), plugin(rustlex))]

#[cfg(feature = "with-syntex")] extern crate rustlex_codegen as rustlex;
#[cfg(feature = "with-syntex")] include!(concat!(env!("OUT_DIR"), "/test.rs"));

#[cfg(not(feature = "with-syntex"))] #[allow(plugin_as_library)] extern crate rustlex;
#[cfg(not(feature = "with-syntex"))] include!("test.in.rs");

pub fn main () {
    test_simple();
    test_other();
}
