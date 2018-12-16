#![feature(proc_macro)]
#![recursion_limit="256"]

extern crate proc_macro;
#[macro_use] extern crate log;
extern crate bit_set;
extern crate fsa;
#[macro_use] extern crate quote;
#[macro_use] extern crate proc_macro2;

mod analysis;
mod codegen;
mod lexer;
mod parser;
mod regex;
mod unicode;

fn main(inp: proc_macro::TokenStream) -> Result<proc_macro::TokenStream, ()> {
    let def = try!(parser::parse(inp.into()).map_err(|diag| diag.emit()));
    let lex = try!(lexer::Lexer::new(def));
    Ok(lex.gen_code())
}

// the main rustlex macro
#[proc_macro]
pub fn rustlex(inp: proc_macro::TokenStream) -> proc_macro::TokenStream {
    main(inp).unwrap_or(proc_macro::TokenStream::empty())
}
