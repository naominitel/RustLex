#![feature(phase)]
#![feature(trace_macros)]
#[phase(plugin)]
extern crate rustlex;

#[path="common/strreader.rs"]
mod strreader;

#[deriving(PartialEq)]
enum Token {
    TokA
}

rustlex!(
    let A = 'a';

    A => return Some(TokA)
)

#[test]
fn test_simple() {
    let expected = vec!(TokA, TokA);
    let str = "aa";

    let inp = strreader::reader(str) as Box<::std::io::Reader>;
    let mut lexer = Lexer::new(inp);
    let mut iter = expected.iter();
    for tok in *lexer {
        assert!(iter.next().unwrap() == &tok);
    }
    assert!(iter.next() == None);
}
