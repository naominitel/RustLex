#![feature(plugin_registrar)]
//#![feature(macro_rules)]
#![feature(quote)]
#![feature(macro_rules)]

#![crate_type="dylib"]
#![crate_name="rustlex"]

extern crate collections;
extern crate syntax;
extern crate rustc;

use syntax::ast::TokenTree;
use syntax::codemap::Span;
use syntax::ext::base::ExtCtxt;
use syntax::ext::base::MacResult;
use rustc::plugin::Registry;

mod dfa;
mod lexer;
mod nfa;
mod parser;
mod regex;
mod util;

// the main rustlex macro
pub fn rustlex(cx: &mut ExtCtxt, sp: Span, args: &[TokenTree])
        -> Box<MacResult+'static> {
    let mut p = ::syntax::parse::new_parser_from_tts(
        cx.parse_sess,
        cx.cfg.clone(),
        args.to_vec()
    );

    let def = box parser::parse(&mut p);
    let lex = lexer::Lexer::new(def, cx);
    lex.gen_code(cx, sp)
}

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_macro("rustlex", rustlex);
}

/*
#[macro_registrar]
pub fn registrar(register: |Name, base::SyntaxExtension|) {
    register(
        syntax::parse::token::intern("rustlex"),
        base::NormalTT(box base::TTMacroExpander {
            expander: rustlex,
            span: None
        }, None)
    );
}
*/
