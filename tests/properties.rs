#![feature(plugin)]
#[plugin] extern crate rustlex;
#[macro_use] extern crate log;

use std::io::BufReader;

use self::Token::{Open,Close};

#[derive(PartialEq,Show)]
enum Token {
    Open,
    Close
}

rustlex! PropertiesLexer {
    property depth:int = 0;
    let OPEN = '(';
    let CLOSE = ')';
    OPEN => |lexer:&mut PropertiesLexer<R>| { lexer.depth += 1; Some(Open) }
    CLOSE => |lexer:&mut PropertiesLexer<R>| {
        lexer.depth -= 1;
        if lexer.depth<0 { panic!("invalid parens nesting") };
        Some(Close) }
}

#[test]
fn test_ok() {
    let inp = BufReader::new("((()()))".as_bytes());
    let lexer = PropertiesLexer::new(inp);
    let result:Vec<Token> = lexer.collect();
    assert_eq!(8, result.len());
}

#[test]
fn test_not_closed() {
    let inp = BufReader::new("(((()))".as_bytes());
    let mut lexer = PropertiesLexer::new(inp);
    while lexer.next().is_some() {}
    assert_eq!(1, lexer.depth);
}

#[test]
#[should_fail]
fn test_not_open() {
    let inp = BufReader::new("(()))".as_bytes());
    let mut lexer = PropertiesLexer::new(inp);
    while lexer.next().is_some() {}
}
