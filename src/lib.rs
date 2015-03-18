#![feature(plugin_registrar,rustc_private,collections,core)]
#![feature(quote)]

#![crate_type="dylib"]
#![crate_name="rustlex"]

extern crate collections;
extern crate syntax;
extern crate rustc;

#[macro_use] extern crate log;

use syntax::ast::{Ident, TokenTree};
use syntax::codemap::Span;
use syntax::ext::base::{ExtCtxt, MacResult, IdentTT};
use syntax::parse::token;
use rustc::plugin::Registry;

mod dfa;
mod lexer;
mod nfa;
mod parser;
mod regex;
mod util;
pub mod rt;

// the main rustlex macro
pub fn rustlex<'a>(cx: &'a mut ExtCtxt, sp: Span, ident:Ident, args: Vec<TokenTree>)
        -> Box<MacResult+'a> {
    let mut p = ::syntax::parse::new_parser_from_tts(
        cx.parse_sess,
        cx.cfg.clone(),
        args
    );

    let def = Box::new(parser::parse(ident, &mut p));
    let lex = lexer::Lexer::new(def, cx);
    lex.gen_code(cx, sp)
}

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_syntax_extension(
        token::intern("rustlex"),
        IdentTT(Box::new(rustlex), None, false)
    );
}
