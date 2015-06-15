#![feature(plugin_registrar, rustc_private)]

extern crate rustlex_codegen;
extern crate rustc;

pub use rustlex_codegen::rt;

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut rustc::plugin::Registry) {
    rustlex_codegen::plugin_registrar(reg);
}
