#![feature(phase)]
#[phase(plugin)] extern crate rustlex;
#[phase(plugin, link)] extern crate log;

use std::io::BufReader;

#[deriving(PartialEq,Show)]
enum Token {
    TokA(String)
}

rustlex!(
    let A = 'a';

    A => |yy| { Some(TokA ( yy )) }
)

#[test]
fn test_simple() {
    let expected = vec!(TokA(String::from_str("a")), TokA(String::from_str("a")));
    let str = "aa";
    let inp = BufReader::new(str.as_bytes());
    let mut lexer = Lexer::new(inp);
    let mut iter = expected.iter();
    for tok in *lexer {
        assert!(iter.next().unwrap() == &tok);
    }
    assert!(iter.next() == None);
}
