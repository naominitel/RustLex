#![feature(phase)]
#![feature(globs)]
#[phase(plugin, link)] extern crate rustlex;
#[phase(plugin, link)] extern crate log;

use std::io::BufReader;

use self::Token::*;

#[deriving(PartialEq,Show)]
enum Token {
    TokA(String),
    TokB(String)
}

rustlex! SimpleLexer {
    let A = 'a';
    A => |lexer:&mut SimpleLexer<R>| Some(TokA ( lexer.yystr() ))
}

#[test]
fn test_simple() {
    let expected = vec!(TokA(String::from_str("a")), TokA(String::from_str("a")));
    let str = "aa";
    let inp = BufReader::new(str.as_bytes());
    let mut lexer = SimpleLexer::new(inp);
    let mut iter = expected.iter();
    for tok in *lexer {
        assert!(iter.next().unwrap() == &tok);
    }
    assert!(iter.next() == None);
}

rustlex! OtherLexer {
    let B = 'b';
    B => |lexer:&mut OtherLexer<R>| Some(TokB ( lexer.yystr() ))
}

#[test]
fn test_other() {
    let expected = vec!(TokB(String::from_str("b")), TokB(String::from_str("b")));
    let str = "bb";
    let inp = BufReader::new(str.as_bytes());
    let mut lexer = OtherLexer::new(inp);
    let mut iter = expected.iter();
    for tok in *lexer {
        assert!(iter.next().unwrap() == &tok);
    }
    assert!(iter.next() == None);
}
