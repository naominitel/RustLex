#![feature(phase)]
#[phase(plugin, link)] extern crate rustlex;
#[phase(plugin, link)] extern crate log;

use std::io::BufReader;

use self::Token::{TokOuterStuff, TokInnerStuff};

#[deriving(PartialEq,Show)]
enum Token {
    TokOuterStuff(String),
    TokInnerStuff(String)
}

rustlex! ConditionLexer {
    let OPEN = '{';
    let CLOSE = '}';
    let STUFF = [^'{''}']*;
    INITIAL {
        STUFF => |lexer: &mut ConditionLexer<R>|
            Some(TokOuterStuff(lexer.yystr().trim().to_string()))
        OPEN => |lexer: &mut ConditionLexer<R>| {
            lexer.INNER();
            None
        }
    }
    INNER {
        STUFF => |lexer: &mut ConditionLexer<R>|
            Some(TokInnerStuff(lexer.yystr().trim().to_string()))
        CLOSE => |lexer: &mut ConditionLexer<R>| {
            lexer.INITIAL();
            None
        }
    }
}

#[test]
fn test_conditions() {
    let expected = vec!(TokOuterStuff("outer".to_string()),
                        TokInnerStuff("inner".to_string()));
    let str = "outer { inner }";
    let inp = BufReader::new(str.as_bytes());
    let mut lexer = ConditionLexer::new(inp);
    let mut iter = expected.iter();
    for tok in *lexer {
        assert_eq!(iter.next().unwrap(), &tok);
    }
    assert_eq!(iter.next(), None);
}
