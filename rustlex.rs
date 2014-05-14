#![feature(macro_registrar)]
#![feature(macro_rules)]
#![feature(managed_boxes)]
#![feature(quote)]

#![crate_type="dylib"]
#![crate_id="rustlex#0.1"]

extern crate collections;
extern crate syntax;

use syntax::ast::Name;
use syntax::ast::TokenTree;
use syntax::codemap::Span;
use syntax::ext::base;
use syntax::ext::base::ExtCtxt;
use syntax::ext::base::MacResult;

mod dfa;
mod lexer;
mod nfa;
mod parser;
mod regex;
mod util;

// the main rustlex macro
pub fn rustlex(cx: &mut ExtCtxt, sp: Span, args: &[TokenTree]) -> Box<MacResult> {
    let mut p = ::syntax::parse::new_parser_from_tts(
        cx.parse_sess,
        cx.cfg.clone(),
        Vec::from_slice(args)
    );

    let def = box parser::parse(&mut p);
    let lex = lexer::Lexer::new(def, cx);
    lex.genCode(cx, sp)
}

#[macro_registrar]
pub fn registrar(register: |Name, base::SyntaxExtension|) {
    register(
        syntax::parse::token::intern("rustlex"),
        base::NormalTT(box base::BasicMacroExpander {
            expander: rustlex,
            span: None
        }, None)
    );
}
