#![feature(plugin_registrar,rustc_private,quote)]

extern crate syntax;
extern crate rustc_plugin;
extern crate rustc_errors;

#[macro_use] extern crate log;
extern crate bit_set;
extern crate fsa;

use syntax::ast::Ident;
use syntax::codemap::Span;
use syntax::ext::base::{ExtCtxt, MacResult};
use syntax::tokenstream::TokenTree;

mod analysis;
mod codegen;
mod lexer;
mod parser;
mod regex;
mod unicode;
pub mod rt;

// the main rustlex macro
pub fn rustlex<'a>(cx: &'a mut ExtCtxt, sp: Span, ident:Ident, args: Vec<TokenTree>)
        -> Box<MacResult+'a> {
    let mut p = ::syntax::parse::new_parser_from_tts(
        cx.parse_sess,
        args
    );

    let def = parser::parse(ident, &mut p)
        .unwrap_or_else(|mut e| {
            e.emit();
            cx.span_fatal(sp, "error while parsing lexer");
        });
    let lex = lexer::Lexer::new(def, cx);
    lex.gen_code(cx, sp)
}

pub fn plugin_registrar(reg: &mut rustc_plugin::Registry) {
    reg.register_syntax_extension(
        syntax::symbol::Symbol::intern("rustlex"),
        syntax::ext::base::IdentTT(Box::new(rustlex), None, false)
    );
}
