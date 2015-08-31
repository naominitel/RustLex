#![feature(rustc_private,plugin)]
#![plugin(rustlex)]

#[allow(plugin_as_library)]
extern crate rustlex;

#[macro_use] extern crate log;

mod l {
    use self::Token::TokA;

    #[derive(PartialEq,Debug)]
    pub enum Token {
        TokA(String),
    }

    rustlex! SimpleLexer {
        let A = 'a';
        A => |lexer:&mut SimpleLexer<R>| Some(TokA ( lexer.yystr() ))
    }
}

mod t {

    use ::l::Token::TokA;
    use std::io::BufReader;

    #[test]
    fn test_simple() {
        let expected = vec!(TokA("a".to_string()), TokA("a".to_string()));
        let str = "aa";
        let inp = BufReader::new(str.as_bytes());
        let lexer = ::l::SimpleLexer::new(inp);
        let mut iter = expected.iter();
        for tok in lexer {
            assert!(iter.next().unwrap() == &tok);
        }
        assert!(iter.next() == None);
    }

}
