#![feature(plugin_registrar, rustc_private)]

extern crate rustlex_codegen;
extern crate rustc_plugin;

pub use rustlex_codegen::rt;

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut rustc_plugin::Registry) {
    rustlex_codegen::plugin_registrar(reg);
}
