use std::io::BufReader;

use self::Token::TokA;
use self::TokenB::TokB;

#[derive(PartialEq,Debug)]
pub enum Token {
    TokA(String),
}

rustlex! SimpleLexer {
    let A = 'a';
    A => |lexer:&mut SimpleLexer<R>| Some(TokA ( lexer.yystr() ))
}

fn test_simple() {
    let expected = vec!(TokA("a".to_string()), TokA("a".to_string()));
    let str = "aa";
    let inp = BufReader::new(str.as_bytes());
    let lexer = SimpleLexer::new(inp);
    let mut expected_iter = expected.iter();
    for tok in lexer {
        assert!(expected_iter.next().unwrap() == &tok);
    }
    assert!(expected_iter.next() == None);
}

#[derive(PartialEq,Debug)]
pub enum TokenB {
    TokB(String)
}

rustlex! OtherLexer {
    token TokenB;
    let B = 'b';
    B => |lexer:&mut OtherLexer<R>| Some(TokB ( lexer.yystr() ))
}

fn test_other() {
    let expected = vec!(TokB("b".to_string()), TokB("b".to_string()));
    let str = "bb";
    let inp = BufReader::new(str.as_bytes());
    let lexer = OtherLexer::new(inp);
    let mut expected_iter = expected.iter();
    for tok in lexer {
        assert!(expected_iter.next().unwrap() == &tok);
    }
    assert!(expected_iter.next() == None);
}
