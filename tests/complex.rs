#![feature(rustc_private,plugin,collections)]
#![plugin(rustlex)]

#[allow(plugin_as_library)]
extern crate rustlex;

#[macro_use] extern crate log;

use std::io::BufReader;

// The Token type is returned by the lexer function on
// each call and must be declared in the same module
// as where the rustlex! macro is invoked
use self::Token::{TokInt, TokFloat, TokId, TokString};
#[derive(PartialEq,Debug)]
pub enum Token {
    TokInt(u32),
    TokFloat(f32),
    TokId(String),
    TokString(String)
}

rustlex! ComplexLexer {
    // define some regular expressions that matches
    // float and int constants allowed in C
    // definitions are of the form
    //    let name = regex;
    // a complete description of the regex syntax
    // is available in the manual
    let INT = ['0'-'9']+["uUlL"]?;
    let HEX = '0'["xX"]['a'-'f''A'-'F''0'-'9']+["uUlL"]?;
    let FLOAT = (['0'-'9']+'.'|['0'-'9']*'.'['0'-'9']+)(["eE"]["+-"]?['0'-'9']+)?["fFlL"]?;
    let DEC_FLOAT = ['0'-'9']+(["eE"]["+-"]?['0'-'9']+)["fFlL"]?;
    let HEX_FLOAT = '0'["xX"]['a'-'f''A'-'F''0'-'9']*'.'['a'-'f''A'-'F''0'-'9']*(["pP"]['0'-'9']+)?["fFlL"]?;
    let INTCONST = (INT|HEX);
    let FLTCONST = (FLOAT|HEX_FLOAT|DEC_FLOAT);
    let STR = '"' ([^'\\''"']|'\\'.)* '"' ;
    let ID = ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''_''0'-'9']*;

    // define rules that associate a pattern
    // to a Rust snippet to be executed when a
    // token matching the pattern is encountered
    // each rule is of the form
    //    regex => action
    // action can be a block or a single statement
    INT => |lexer:&mut ComplexLexer<R>| Some(TokInt(lexer.yystr()[..].parse().unwrap()))
    HEX => |lexer:&mut ComplexLexer<R>| {
        let s = lexer.yystr();
        let i:u32 = u32::from_str_radix(&s[2 .. s.len()-1], 16).unwrap();
        Some(TokInt(i))
    }
    FLTCONST => |lexer:&mut ComplexLexer<R>| Some(TokFloat(lexer.yystr()[..].parse().unwrap()))
    ID => |lexer:&mut ComplexLexer<R>| Some(TokId(lexer.yystr()))
    STR => |lexer:&mut ComplexLexer<R>| Some(TokString(lexer.yystr()))
}

#[test]
fn test_complex() {
    let expected = vec!(
        TokId(String::from_str("foo")),
        TokId(String::from_str("bar")),
        TokId(String::from_str("baz")),
        TokFloat(0.1),
        TokInt(212u32),
        TokString(String::from_str("\"a\"")),
        TokInt(0x121u32),
        TokId(String::from_str("baz")),
        TokInt(123),
        TokId(String::from_str("foo")));
    let str = "foo bar baz 0.10 212 \"a\" 0x121u baz 123foo ";
    let inp = BufReader::new(str.as_bytes());
    let lexer = ComplexLexer::new(inp);
    let mut iter = expected.iter();
    for tok in lexer {
        let expect = iter.next().unwrap();
        assert!(expect == &tok);
    }
    assert!(iter.next() == None);
}
