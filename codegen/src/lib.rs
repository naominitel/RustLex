#![cfg_attr(not(feature = "with-syntex"), feature(plugin_registrar,rustc_private,quote))]

#[cfg(feature = "with-syntex")] extern crate quasi;
#[cfg(feature = "with-syntex")] extern crate syntex;
#[cfg(feature = "with-syntex")] extern crate syntex_syntax as syntax;

#[cfg(not(feature = "with-syntex"))] extern crate syntax;
#[cfg(not(feature = "with-syntex"))] extern crate rustc;

#[macro_use] extern crate log;

use syntax::ast::{Ident, TokenTree};
use syntax::codemap::Span;
use syntax::ext::base::{ExtCtxt, MacResult};

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

    let def = Box::new(parser::parse(ident, &mut p)
        .unwrap_or_else( |_| panic!("error while parsing lexer")));
    let lex = lexer::Lexer::new(def, cx);
    lex.gen_code(cx, sp)
}

#[cfg(feature = "with-syntex")]
pub fn plugin_registrar(reg: &mut syntex::Registry) {
    reg.add_ident_macro("rustlex", rustlex);
}

#[cfg(not(feature = "with-syntex"))]
pub fn plugin_registrar(reg: &mut rustc::plugin::Registry) {
    reg.register_syntax_extension(
        syntax::parse::token::intern("rustlex"),
        syntax::ext::base::IdentTT(Box::new(rustlex), None, false)
    );
}
