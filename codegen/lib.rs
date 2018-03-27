#![feature(proc_macro)]

extern crate proc_macro;
extern crate proc_macro2;
#[macro_use] extern crate log;
#[macro_use] extern crate quote;
extern crate bit_set;
extern crate fsa;

mod analysis;
// // mod codegen;
mod lexer;
mod parser;
mod regex;
mod unicode;
// // pub mod rt;

// the main rustlex macro
#[proc_macro]
pub fn rustlex(inp: proc_macro::TokenStream) -> proc_macro::TokenStream {
     let def = parser::parse(inp);
     // let lex = lexer::Lexer::new(def, cx);
    // lex.gen_code(cx, sp)
    panic!()
}

// pub fn plugin_registrar(reg: &mut rustc_plugin::Registry) {
//     reg.register_syntax_extension(
//         syntax::symbol::Symbol::intern("rustlex"),
//         syntax::ext::base::IdentTT(Box::new(rustlex), None, false)
//     );
// }
