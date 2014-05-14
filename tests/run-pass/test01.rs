#![feature(phase)]
#[phase(syntax)]
extern crate rustlex;

#[path="../common/strreader.rs"]
mod strreader;

// The Token type is returned by the lexer function on
// each call and must be declared in the same module
// as where the rustlex! macro is invoked
#[deriving(Eq)]
enum Token {
    TokInt,
    TokFloat,
    TokId
}

rustlex!(
    // define some regular expressions that matches
    // float and int constants allowed in C
    // definitions are of the form
    //    let name = regex;
    // a complete description of the regex syntax
    // is available in the manual
    let INT = ['0'-'9']+["uUlL"]%;
    let HEX = '0'["xX"]['a'-'f''A'-'F''0'-'9']+["uUlL"]%;
    let FLOAT = (['0'-'9']+'.'|['0'-'9']*'.'['0'-'9']+)(["eE"]["+-"]%['0'-'9']+)%["fFlL"]%;
    let DEC_FLOAT = ['0'-'9']+(["eE"]["+-"]%['0'-'9']+)["fFlL"]%;
    let HEX_FLOAT = '0'["xX"]['a'-'f''A'-'F''0'-'9']*'.'['a'-'f''A'-'F''0'-'9']*(["pP"]['0'-'9']+)%["fFlL"]%;
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
    INTCONST => return Some(TokInt)
    FLTCONST => return Some(TokFloat)
    ID => return Some(TokId)
    STR => println!("Saw a str: {:s}", _yystr)
    . => println!("Unknown token: {:s}", _yystr)
)

fn main() {
    let expected = vec!(TokId, TokId, TokId, TokFloat, TokId, TokInt, TokId, TokInt, TokId);
    let str = "foo bar baz 0.10 ii212\"aa\\\"aa\\.a\"\"a\" 0x121u baz 123foo ";
    let inp = strreader::reader(str) as Box<::std::io::Reader>;
    let mut lexer = Lexer::new(inp);
    let mut iter = expected.iter();
    for tok in lexer {
        assert!(iter.next().unwrap() == &tok);
    }
    assert!(iter.next() == None);
}
