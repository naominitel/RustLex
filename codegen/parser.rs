// parser for the lexer description
// invoked by Rustc when compiling a lexer file
// uses libsyntax to parse what Rustc gives us

use std::collections::hash_map::HashMap;
use std::iter::{self, FromIterator};
use proc_macro2::{TokenNode, TokenStream, TokenTree, TokenTreeIter};
use proc_macro2::{Delimiter, Spacing, Span, Term};
use proc_macro::{Diagnostic};
use lexer::{Condition, LexerDef, Rule};
use regex;
use regex::Regex;

pub type PResult<T> = Result<T, Diagnostic>;

trait Tokenizer {
    // // returns the current token, without consuming it
    // fn token(&self) -> &token::Token;

    // consumes the current token
    fn bump(&mut self) -> ();

    // consumes the current token and return it
    // equivalent to token() followed by bump()
    fn bump_and_get(&mut self) -> TokenNode;

    // consumes the current token and return true if
    // it corresponds to the given tok, or false otherwise
    fn eat_op(&mut self, op: char, spc: Spacing) -> bool;
    fn eat_keyword(&mut self, kwd: &str) -> bool;

    // returns true if the next token is the given
    // token or keyword, without consuming it
    fn check_op(&mut self, op: char, spc: Spacing) -> bool;
    fn check_keyword(&mut self, kwd: &str) -> bool;

    // expects the following token to be the given operator,
    // followed by spacing or not, then consumes it
    fn expect_op(&mut self, op: char, spc: Spacing) -> PResult<()>;

    // expect the next token to be an ident and consumes it
    fn parse_ident(&mut self) -> PResult<Term>;

    // // returns the span of the previous token
    fn last_span(&self) -> Span;

    // various functions to abort parsing
    fn span_error(&self, m: &str) -> Diagnostic;
    fn unexpected(&self) -> Diagnostic;

    fn unexpected_last(&mut self) -> Diagnostic;
}

// // the "lexical" environment of regular expression definitions
type Env = HashMap<String, usize>;

fn get_tokens(parser: &mut Parser) -> PResult<Term> {
    match parser.node {
        TokenNode::Term(t) if t.as_str() == "token" => {
            parser.bump();
            let token = try!(parser.parse_ident());
            try!(parser.expect_op(';', Spacing::Alone));
            Ok(token)
        }
        _ => Ok(Term::intern("Token"))
    }
}

fn get_properties(parser: &mut Parser)
        -> PResult<Vec<(Term, TokenTree, TokenTree)>> {
    let mut ret = Vec::new();
    loop {
        match parser.node {
            TokenNode::Term(t) if t.as_str() == "property" => {
                parser.bump();
                let name = try!(parser.parse_ident());
                try!(parser.expect_op(':', Spacing::Alone));
                let ty = try!(parser.parse_ty());
                try!(parser.expect_op('=', Spacing::Alone));
                let expr = try!(parser.parse_expr());
                try!(parser.expect_op(';', Spacing::Alone));
                ret.push((name, ty, expr));
            }

            _ => break
        }
    }

    Ok(ret)
}

// // the functions below migth read their tokens either from the libsyntax
// // parser type or from our tokenizer that reads tokens from a raw string,
// // hence the type parameter

// recursively parses a character class, e.g. ['a'-'z''0'-'9''_']
// basically creates an or-expression per character in the class
fn get_char_class<T: Tokenizer>(parser: &mut T)
                                -> PResult<regex::CharSet<char>> {
    let mut ret = regex::CharSet::new();
    loop {
        let tok = parser.bump_and_get();
        match tok {
            // eof
            TokenNode::Op(' ', Spacing::Joint) => break,

            // TokenNode::Literal(token::Lit::Char(i), _) => {
            //     let ch = parse::char_lit(&*i.as_str(), None).0;

            //     match *parser.token() {
            //         token::BinOp(token::Minus) => {
            //             // a char seq, e.g. 'a' - 'Z'
            //             parser.bump();
            //             let ch2 = match parser.bump_and_get() {
            //                 token::Literal(token::Lit::Char(ch), _) =>
            //                     parse::char_lit(&*ch.as_str(), None).0,
            //                 _ => return Err(parser.unexpected())
            //             };
            //             if ch >= ch2 {
            //                 let last_span = parser.last_span();
            //                 return Err(parser.span_fatal(last_span,
            //                     "invalid character range"))
            //             }
            //             ret.push(ch .. ch2);
            //         }

            //         _ => { ret.push(ch .. ch); }
            //     }
            // }

            // token::Literal(token::Lit::Str_(id),_) => {
            //     if id.as_str().len() == 0 {
            //         let last_span = parser.last_span();
            //         return Err(parser.span_fatal(last_span,
            //             "bad string constant in character class"))
            //     }
            //     for b in id.as_str().chars() {
            //         ret.push(b .. b);
            //     }
            // }

            _ => return Err(parser.unexpected_last())
        }
    }
    Ok(ret)
}

// parses a "constant" in an regular expression, i.e. either
// - a literal character
// - a character class
// - an identifier refering to another expression
// parenthesized subexpressions are also parsed here since the have
// the same operator precedence as the constants
fn get_const<T: Tokenizer>(parser: &mut T, env: &Env) -> PResult<Regex> {
    let tok = parser.bump_and_get();
    // here we expect either
    // the start of a character-class, '['
    // the start of a parenthesized expression, '('
    // a literal char constant, 'a'
    match tok {
        TokenNode::Op('.', _) => Ok(Box::new(regex::Literal(regex::Any))),
        TokenNode::Group(Delimiter::Parenthesis, toks) => {
            let mut sub = Parser::new(toks);
            get_regex(&mut sub, ' ', Spacing::Joint, env)
        }
        TokenNode::Group(Delimiter::Bracket, toks) => {
            let mut sub = Parser::new(toks);
            if parser.eat_op('^', Spacing::Alone) {
                Ok(Box::new(regex::Literal(
                    regex::NotClass(try!(get_char_class(&mut sub)))
                )))
            } else {
                Ok(Box::new(regex::Literal(
                    regex::Class(try!(get_char_class(&mut sub)))
                )))
            }
        }
        // FIXME
        // TokenNode::Literal(token::Lit::Char(ch), _) =>
        //     Ok(Box::new(regex::Literal(
        //         regex::Char(parse::char_lit(&*ch.as_str(), None).0)
        //     ))),
        // TokenNode::Literal(token::Lit::Str_(id), _) =>
        //     match regex::string(&*id.as_str()) {
        //         Some(reg) => Ok(reg),
        //         None => {
        //             let last_span = parser.last_span();
        //             Err(parser.span_fatal(last_span,
        //                 "bad string constant in regular expression"))
        //         }
        //     },
        TokenNode::Term(id) => match env.get(id.as_str()).cloned() {
            Some(value) => Ok(Box::new(regex::Var(value))),
            None => {
                let last_span = parser.last_span();
                Err(Diagnostic::spanned(
                    ::proc_macro::Span::(parser.last_span()),
                    ::proc_macro::Level::Error,
                    format!("unknown identifier: {}", id.as_str())
                ))
            }
        },
        _ => Err(parser.unexpected_last())
    }
}

// a "closure" in a regular expression, i.e. expr*
// the * operator has lower precedence that concatenation
fn get_closure<T: Tokenizer>(parser: &mut T, env: &Env) -> PResult<Regex> {
    let reg = try!(get_const(parser, env));
    if parser.eat_op('*', Spacing::Alone) ||
        parser.eat_op('*', Spacing::Joint) {
        Ok(Box::new(regex::Closure(reg)))
    } else if parser.eat_op('+', Spacing::Alone) ||
        parser.eat_op('+', Spacing::Joint) {
        Ok(Box::new(regex::Cat(reg.clone(), Box::new(regex::Closure(reg)))))
    } else if parser.eat_op('?', Spacing::Alone) ||
        parser.eat_op('?', Spacing::Joint) {
        Ok(Box::new(regex::Maybe(reg))) }
    else {
        Ok(reg)
    }
}

// recursively parses a sequence of concatenations
// continues until it reaches the end of the current subexpr,
// indicated by the end parameter or an or operator, which has
// higher precedence. Concatenation is left-assoc
fn get_concat<T: Tokenizer>(parser: &mut T, end: char, spc: Spacing, env: &Env)
                            -> PResult<Regex> {
    let opl = try!(get_closure(parser, env));
    if parser.check_op(end, spc) ||
        parser.check_keyword("as") ||
        parser.check_op('|', Spacing::Alone) {
        Ok(opl)
    } else {
        let opr = try!(get_concat(parser, end, spc, env));
        Ok(Box::new(regex::Cat(opl, opr)))
    }
}

// parse a binding of a regexp to an identifier, i.e. expr as id
fn get_binding<T: Tokenizer>(parser: &mut T, end: char, spc: Spacing, env: &Env)
                             -> PResult<Regex> {
    let expr = try!(get_concat(parser, end, spc, env));
    if parser.eat_keyword("as") {
        let name = try!(parser.parse_ident());
        Ok(Box::new(regex::Bind(name, expr)))
    } else { Ok(expr) }
}

// entry point of the regex parser, parses an or-expression
// tries to parse a concat expression as the left operation, and then
// if we are not at the end of the current subexpression as indicated by
// the end parameter, we try to read a | operator followed by another
// expression which is parsed recursively (or is left-assoc)
fn get_regex<T: Tokenizer>(parser: &mut T, end: char, spc: Spacing, env: &Env)
                           -> PResult<Regex> {
    if parser.eat_op(end, spc) {
        return Err(parser.unexpected());
    }
    let left = try!(get_binding(parser, end, spc, env));
    if parser.eat_op(end, spc) {
        Ok(left)
    } else {
        try!(parser.expect_op('|', Spacing::Alone));
        let right = try!(get_regex(parser, end, spc, env));
        Ok(Box::new(regex::Or(left, right)))
    }
}

// a pattern is an association of a name to a regular expression
// this function expects the next tokens to be id = reg, with id
// being a non-keyword identifier and reg a literal constant
fn get_pattern(parser: &mut Parser, env: &Env) -> PResult<(Term, Regex)> {
    let name = try!(parser.parse_ident());
    try!(parser.expect_op('=', Spacing::Alone));
    let reg = try!(get_regex(parser, ';', Spacing::Alone, env));
    Ok((name, reg))
}

// a definition is of the form let pattern; see the function above
// for a description of pattern. This function just tries to parse
// as much definitions as possible, until it sees something that
// does not match. returns an error if it encounters a malformed
// definition, otherwise return the "environment" containing named
// regular expression definitions
fn get_definitions(parser: &mut Parser)
        -> PResult<(Env, Vec<Regex>)> {
    let mut env = HashMap::new();
    let mut defs = Vec::new();
    while parser.eat_keyword("let") {
        let (id, pat) = try!(get_pattern(parser, &env));
        env.insert(id.as_str().to_string(), defs.len());
        defs.push(pat);
    }
    Ok((env, defs))
}

// parses the contents of a "condition" body, i.e. simply a
// list of rules of the form regex => action
// stops as soon as we encounter a closing brace } which
// indicates the end of the condition body
fn get_condition(parser: &mut Parser, env: &Env) -> PResult<Vec<Rule>> {
    let mut ret = Vec::new();
    loop {
        // eof?
        if let TokenNode::Op(' ', Spacing::Joint) = parser.node { break };
        let pattern = try!(get_regex(parser, '=', Spacing::Joint, env));
        try!(parser.expect_op('>', Spacing::Alone));
        let action = try!(parser.parse_expr());
        // optionnal comma for disambiguation
        parser.eat_op(',', Spacing::Alone);
        ret.push(Rule { pattern:pattern, action:action });
    }
    Ok(ret)
}

// parses the main body of the lexer description
// entries here may be either rules of the gorm regex => action
// or "conditions" of the form condition { ... } that contains rules
// rules outside conditions implicitely belong to the "INITIAL" condition
fn get_conditions(parser: &mut Parser, env: &Env) -> PResult<Vec<Condition>> {
    // remember the names of the conditions we already
    // encountered and where we stored their rules in
    // the conditions array
    let mut cond_names: HashMap<String, usize> = HashMap::new();
    let mut ret = Vec::new();
    let initial = Condition {
        name: Term::intern("INITIAL"),
        span: parser.span,
        rules: Vec::new()
    };

    cond_names.insert(initial.name.as_str().to_string(), 0);
    ret.push(initial);

    loop {
        match parser.node {
            // eof
            TokenNode::Op(' ', Spacing::Joint) => break,

            // here we can expect either a condition declaration
            // or simply a rule, which is then implicitly in the
            // "Initial" condition
            // in any case, we expect an ident or a regex first
            TokenNode::Term(id) => {
                // this may be either the start of a regexp followed
                // by an arrow and an action or a condition followed
                // by an opening brace.
                // if we see an opening brace '{' here then it's a
                // condition whose name is the id we just parsed

                let la = ::std::mem::replace(&mut parser.lookahead.kind, EOF);
                if let TokenNode::Group(Delimiter::Brace, gr) = la {
                    // ok it's a condition
                    // bump 2 times: the identifier and the lbrace
                    let sp = parser.span;
                    parser.bump();
                    parser.bump();

                    // parse the condition body
                    let mut sub = Parser::new(gr);
                    let rules = try!(get_condition(&mut sub, env));

                    // have we seen this condition before ?
                    match cond_names.get(id.as_str()).cloned() {
                        Some(i) => {
                            ret[i].rules.extend(rules.into_iter());
                            continue
                        }

                        None => ()
                    }

                    // nope, create it
                    ret.push(Condition { rules: rules, span: sp, name: id });
                    cond_names.insert(id.as_str().to_string(), ret.len() - 1);
                } else {
                    // ok, it's not a condition, so it's a rule of the form
                    // regex => action, with regex beginning by an identifier
                    parser.lookahead.kind = la;
                    let reg = try!(get_regex(parser, '=', Spacing::Joint, env));
                    try!(parser.expect_op('>', Spacing::Alone));
                    let expr = try!(parser.parse_expr());
                    parser.eat_op(',', Spacing::Alone);
                    ret[0].rules.push(Rule { pattern: reg, action: expr });
                }
            }

            _ => {
                // it's not an ident, but it may still be the
                // beginning of a regular expression
                let reg = try!(get_regex(parser, '=', Spacing::Joint, env));
                try!(parser.expect_op('>', Spacing::Alone));
                let expr = try!(parser.parse_expr());
                parser.eat_op(',', Spacing::Alone);
                ret[0].rules.push(Rule { pattern: reg, action: expr });
            }
        }
    }

    Ok(ret)
}

// an endless stream of TokenTree
// returns tts from the original stream until it reaches the end, and then
// endlessly returns eof (represented as an op with a value of space, which
// should never appear in the original stream) with the span of the last
// token.
struct EndlessTokStream {
    stream: TokenTreeIter,
    span: Span
}

const EOF: TokenNode = TokenNode::Op(' ', Spacing::Joint);

impl EndlessTokStream {
    fn next(&mut self) -> TokenTree {
        match self.stream.next() {
            Some(tt) => {
                self.span = tt.span;
                tt
            }
            None => TokenTree {
                span: self.span,
                kind: EOF
            }
        }
    }
}

struct Parser {
    node: TokenNode,
    span: Span,
    lookahead: TokenTree,
    prev_span: Span,
    stream: EndlessTokStream
}

impl Parser {
    fn new(tokens: TokenStream) -> Parser {
        let mut stream = EndlessTokStream {
            stream: tokens.into_iter(),
            // will only be used if we don't
            // have any tokens in the stream
            span: Span::call_site()
        };
        let tt = stream.next();
        let la = stream.next();
        Parser {
            node: tt.kind,
            span: tt.span,
            lookahead: la,
            prev_span: tt.span,
            stream: stream
        }
    }

    // parse a Rust ‶type″.
    // proc_macro has no knowledge of types so for the moment our definition
    // what is a type is a single tt, like a term or a parenthesized more
    // complex expression.
    // the downside is that we are no longer able to parse something
    // like Vec<T> without parens but that will do for now.
    fn parse_ty(&mut self) -> PResult<TokenTree> {
        match self.node {
            TokenNode::Term(_) |
            TokenNode::Group(Delimiter::Parenthesis, _) |
            TokenNode::Group(Delimiter::Bracket, _) => {
                let span = self.span;
                let tt = self.bump_and_get();
                Ok(TokenTree { kind: tt, span: span })
            }
            _ => Err(self.span_error("expected a type"))
        }
    }

    // parse a Rust ‶expr″.
    // same as type, expect we also allow literals and {}-delimited blocks here
    fn parse_expr(&mut self) -> PResult<TokenTree> {
        match self.node {
            TokenNode::Term(_) |
            TokenNode::Literal(_) |
            TokenNode::Group(_, _) => {
                let span = self.span;
                let tt = self.bump_and_get();
                Ok(TokenTree { kind: tt, span: span })
            }
            _ => Err(self.span_error("expected an expression"))
        }
    }
}

impl Tokenizer for Parser {
    // fn token(&self) -> &token::Token { &self.token }

    fn bump(&mut self) {
        self.prev_span = self.span;
        self.node = ::std::mem::replace(&mut self.lookahead.kind, EOF);
        self.span = self.lookahead.span;
        self.lookahead = self.stream.next();
    }

    fn bump_and_get(&mut self) -> TokenNode {
        let token = ::std::mem::replace(&mut self.node, EOF);
        self.bump();
        token
    }

    fn eat_op(&mut self, op: char, spacing: Spacing) -> bool {
        if self.check_op(op, spacing) {
            self.bump();
            true
        } else {
            false
        }
    }

    fn eat_keyword(&mut self, kwd: &str) -> bool {
        if self.check_keyword(kwd) {
            self.bump();
            true
        } else {
            false
        }
    }

    fn check_op(&mut self, op: char, spacing: Spacing) -> bool {
        match self.node {
            TokenNode::Op(ch, spc) if ch == op && spc == spacing => true,
            _ => false
        }
    }

    fn check_keyword(&mut self, kwd: &str) -> bool {
        match self.node {
            TokenNode::Term(t) if t.as_str() == kwd => true,
            _ => false
        }
    }

    fn expect_op(&mut self, op: char, spacing: Spacing) -> PResult<()> {
        if self.check_op(op, spacing) { Ok(self.bump()) }
        else { Err(self.span_error(&format!("expected {}", op))) }
    }

    fn parse_ident(&mut self) -> PResult<Term> {
        match self.node {
            TokenNode::Term(t) => {
                self.bump();
                Ok(t)
            }

            // TODO: errors
            _ => Err(self.span_error("expected an identifier"))
        }
    }

    fn last_span(&self) -> Span {
        self.prev_span
    }

    fn span_error(&self, m: &str) -> Diagnostic {
        Diagnostic::spanned(self.span.into(), ::proc_macro::Level::Error, m)
    }

    fn unexpected(&self) -> Diagnostic {
        self.span_error("unexpected token")
    }

    fn unexpected_last(&mut self) -> Diagnostic {
        Diagnostic::spanned(self.last_span().into(),
                            ::proc_macro::Level::Error,
                            "unexpected token")
    }
}

// runs the parsing of the full analyser description
// - first gets an environment of the regular expression definitions
// - then parses the definitions of the rules and the conditions
pub fn parse<'a>(inp: TokenStream) -> PResult<LexerDef> {
    let mut parser = Parser::new(inp);
    let name = try!(parser.parse_ident());
    try!(parser.expect_op(':', Spacing::Alone));
    let tokens = try!(get_tokens(&mut parser));
    let props = try!(get_properties(&mut parser));
    let (env, defs) = try!(get_definitions(&mut parser));
    let conditions = try!(get_conditions(&mut parser, &env));
    Ok(LexerDef {
        ident: name, tokens: tokens, properties: props,
        defs: defs, conditions: conditions
    })
}
