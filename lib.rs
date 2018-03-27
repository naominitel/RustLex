#![feature(proc_macro)]

extern crate rustlex_codegen;
extern crate proc_macro;

#[proc_macro]
pub fn foo(inp: proc_macro::TokenStream) -> proc_macro::TokenStream {
    panic!()
}
