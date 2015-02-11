#![feature(plugin,io,collections)]
#![plugin(rustlex)]

extern crate rustlex;

#[macro_use] extern crate log;

use std::old_io::BufReader;

use self::Token::TokA;
use self::TokenB::TokB;

#[derive(PartialEq,Debug)]
enum Token {
    TokA(String),
}

rustlex! SimpleLexer {
    let A = 'a';
    A => |&: lexer:&mut SimpleLexer<R>| Some(TokA ( lexer.yystr() ))
}

#[test]
fn test_simple() {
    let expected = vec!(TokA(String::from_str("a")), TokA(String::from_str("a")));
    let str = "aa";
    let inp = BufReader::new(str.as_bytes());
    let lexer = SimpleLexer::new(inp);
    let mut iter = expected.iter();
    for tok in lexer {
        assert!(iter.next().unwrap() == &tok);
    }
    assert!(iter.next() == None);
}

#[derive(PartialEq,Debug)]
enum TokenB {
    TokB(String)
}

rustlex! OtherLexer {
    token TokenB;
    let B = 'b';
    B => |&: lexer:&mut OtherLexer<R>| Some(TokB ( lexer.yystr() ))
}

#[test]
fn test_other() {
    let expected = vec!(TokB(String::from_str("b")), TokB(String::from_str("b")));
    let str = "bb";
    let inp = BufReader::new(str.as_bytes());
    let lexer = OtherLexer::new(inp);
    let mut iter = expected.iter();
    for tok in lexer {
        assert!(iter.next().unwrap() == &tok);
    }
    assert!(iter.next() == None);
}
