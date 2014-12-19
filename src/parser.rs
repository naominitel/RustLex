// parser for the lexer description
// invoked by Rustc when compiling a lexer file
// uses libsyntax to parse what Rustc gives us

use std::collections::hash_map::HashMap;
use lexer::Condition;
use lexer::LexerDef;
use lexer::Rule;
use regex;
use regex::Regex;
use std::rc::Rc;
use syntax::ast::Expr;
use syntax::ast::Ident;
use syntax::ast::Name;
use syntax::ast::Ty;
use syntax::codemap::Span;
use syntax::parse;
use syntax::parse::token;
use syntax::parse::token::keywords;
use syntax::parse::parser::Parser;
use syntax::ptr::P;
use util::BinSetu8;

trait Tokenizer {
    // returns the current token, without consuming it
    fn token<'a>(&'a self) -> &'a token::Token;

    // consumes the current token
    fn bump(&mut self);

    // consumes the current token and return it
    // equivalent to token() followed by bump()
    fn bump_and_get(&mut self) -> token::Token;

    // consumes the current token and return true if
    // it corresponds to tok, or false otherwise
    fn eat(&mut self, tok: &token::Token) -> bool;

    // expects the following token to be the
    // same as the given token, then consumes it
    fn expect(&mut self, tok: &token::Token);

    // returns the span of the previous token
    fn last_span(&self) -> Span;

    // various functions to abort parsing
    fn span_fatal(&mut self, sp: Span, m: &str) -> !;
    fn unexpected(&mut self) -> !;
    fn unexpected_last(&mut self, tok: &token::Token) -> !;
}

impl<'a> Tokenizer for Parser<'a> {
    fn token<'b>(&'b self) -> &'b token::Token { &self.token }
    fn bump(&mut self) { self.bump() }
    fn bump_and_get(&mut self) -> token::Token { self.bump_and_get() }
    fn eat(&mut self, tok: &token::Token) -> bool { self.eat(tok) }
    fn expect(&mut self, tok: &token::Token) { self.expect(tok) }
    fn last_span(&self) -> Span { self.last_span }
    fn span_fatal(&mut self, sp: Span, m: &str) -> ! { self.span_fatal(sp, m) }
    fn unexpected(&mut self) -> ! { self.unexpected() }
    fn unexpected_last(&mut self, tok: &token::Token) -> ! { self.unexpected_last(tok) }
}

// the "lexical" environment of regular expression definitions
type Env = HashMap<Name, Rc<Regex>>;

fn get_tokens<'a>(parser: &mut Parser) -> Option<Ident> {
    let tokens = token::intern("token");
    match parser.token {
        token::Ident(id, _) if id.name == tokens => {
            parser.bump();
            let token = Some(parser.parse_ident());
            parser.expect(&token::Semi);
            token
        }
        _ => None
    }
}

fn get_properties<'a>(parser: &mut Parser) -> Vec<(Name, P<Ty>, P<Expr>)> {
    let mut ret = Vec::new();
    let prop = token::intern("property");
    loop {
        match parser.token {
            token::Ident(id, _) if id.name == prop => {
                parser.bump();
                let name = parser.parse_ident();
                parser.expect(&token::Colon);
                let ty = parser.parse_ty();
                parser.expect(&token::Eq);
                let expr = parser.parse_expr();
                parser.expect(&token::Semi);
                ret.push((name.name, ty, expr));
            }

            _ => break
        }
    }

    ret
}

// the functions below migth read their tokens either from the libsyntax
// parser type or from our tokenizer that reads tokens from a raw string,
// hence the type parameter

// recursively parses a character class, e.g. ['a'-'z''0'-'9''_']
// basically creates an or-expression per character in the class
fn get_char_class<T: Tokenizer>(parser: &mut T) -> Box<BinSetu8> {
    let mut ret = box BinSetu8::new(256);
    loop {
        let tok = parser.bump_and_get();
        match tok {
            token::CloseDelim(token::Bracket) => {
                break
            }

            token::Literal(token::Lit::Char(i), _) => {
                let mut ch = parse::char_lit(i.as_str()).val0() as u8;

                match *parser.token() {
                    token::BinOp(token::Minus) => {
                        // a char seq, e.g. 'a' - 'Z'
                        parser.bump();
                        let ch2 = match parser.bump_and_get() {
                            token::Literal(token::Lit::Char(ch), _) =>
                                parse::char_lit(ch.as_str()).val0() as u8,
                            _ => parser.unexpected()
                        };
                        if ch >= ch2 {
                            let last_span = parser.last_span();
                            parser.span_fatal(last_span,
                                "invalid character range")
                        }
                        while ch <= ch2 {
                            ret.insert(ch);
                            ch += 1;
                        }
                    }

                    _ => ret.insert(ch)
                }
            }

            token::Literal(token::Lit::Str_(id),_) => {
                let s = token::get_name(id);
                let s = s.get();
                if s.len() == 0 {
                    let last_span = parser.last_span();
                    parser.span_fatal(last_span,
                        "bad string constant in character class")
                }
                for b in s.bytes() {
                    ret.insert(b);
                }
            }

            _ => parser.unexpected_last(&tok)
        }
    }
    ret
}

// parses a "constant" in an regular expression, i.e. either
// - a literal character
// - a character class
// - an identifier refering to another expression
// parenthesized subexpressions are also parsed here since the have
// the same operator precedence as the constants
fn get_const<T: Tokenizer>(parser: &mut T, env: &Env) -> Box<Regex> {
    let tok = parser.bump_and_get();
    // here we expect either
    // the start of a character-class, '['
    // the start of a parenthesized expression, '('
    // a literal char constant, 'a'
    match tok {
        token::Dot => box regex::Any,
        token::OpenDelim(token::Paren) => get_regex(parser,
            &token::CloseDelim(token::Paren), env),
        token::OpenDelim(token::Bracket) => {
            if parser.eat(&token::BinOp(token::Caret)) {
                box regex::NotClass(get_char_class(parser))
            } else {
                box regex::Class(get_char_class(parser))
            }
        }
        token::Literal(token::Lit::Char(ch), _) =>
            box regex::Char(parse::char_lit(ch.as_str()).val0() as u8),
        token::Literal(token::Lit::Str_(id), _) =>
            match regex::string(token::get_name(id).get()) {
                Some(reg) => reg,
                None => {
                    let last_span = parser.last_span();
                    parser.span_fatal(last_span,
                        "bad string constant in regular expression")
                }
            },
        token::Ident(id, _) => match env.get(&id.name).cloned() {
            Some(value) => box regex::Var(value),
            None => {
                let last_span = parser.last_span();
                parser.span_fatal(last_span,
                format!("unknown identifier: {}", 
                    token::get_name(id.name).get()).as_slice())
            }
        },
        _ => parser.unexpected_last(&tok)
    }
}

// a "closure" in a regular expression, i.e. expr*
// the * operator has lower precedence that concatenation
fn get_closure<T: Tokenizer>(parser: &mut T, env: &Env) -> Box<Regex> {
    let reg = get_const(parser, env);
    if parser.eat(&token::BinOp(token::Star)) { box regex::Closure(reg) }
    else if parser.eat(&token::BinOp(token::Plus)) {
        box regex::Cat(reg.clone(), box regex::Closure(reg))
    }
    else if parser.eat(&token::Question) { box regex::Maybe(reg) }
    else { reg }
}

// recursively parses a sequence of concatenations
// continues until it reaches the end of the current subexpr,
// indicated by the end parameter or an or operator, which has
// higher precedence. Concatenation is left-assoc
fn get_concat<T: Tokenizer>(parser: &mut T, end: &token::Token, env: &Env)
    -> Box<Regex> {
    let opl = get_closure(parser, env);
    if parser.token() == end ||
        *parser.token() == token::BinOp(token::Or) {
        opl
    } else {
        let opr = get_concat(parser, end, env);
        box regex::Cat(opl, opr)
    }
}

// entry point of the regex parser, parses an or-expression
// tries to parse a concat expression as the left operation, and then
// if we are not at the end of the current subexpression as indicated by
// the end parameter, we try to read a | operator followed by another
// expression which is parsed recursively (or is left-assoc)
fn get_regex<T: Tokenizer>(parser: &mut T, end: &token::Token, env: &Env)
    -> Box<Regex> {
    if parser.eat(end) {
        parser.unexpected();
    }
    let left = get_concat(parser, end, env);
    if parser.eat(end) { left }
    else {
        parser.expect(&token::BinOp(token::Or));
        let right = get_regex(parser, end, env);
        box regex::Or(left, right)
    }
}

// a pattern is an association of a name to a regular expression
// this function expects the next tokens to be id = reg, with id
// being a non-keyword identifier and reg a literal constant
fn get_pattern(parser: &mut Parser, env: &Env) -> (Ident, Box<Regex>) {
    let name = parser.parse_ident();
    parser.expect(&token::Eq);
    let reg = get_regex(parser, &token::Semi, env);
    (name, reg)
}

// a definition is of the form let pattern; see the function above
// for a description of pattern. This function just tries to parse
// as much definitions as possible, until it sees something that
// does not match. returns an error if it encounters a malformed
// definition, otherwise return the "environment" containing named
// regular expression definitions
fn get_definitions(parser: &mut Parser) -> Box<Env> {
    let mut ret = box HashMap::new();
    while parser.eat_keyword(keywords::Let) {
        let (id, pat) = get_pattern(parser, &*ret);
        ret.insert(id.name, Rc::new(*pat));
    }
    ret
}

// parses the contents of a "condition" body, i.e. simply a
// list of rules of the form regex => action
// stops as soon as we encounter a closing brace } which
// indicates the end of the condition body
fn get_condition(parser: &mut Parser, env: &Env) -> Vec<Rule> {
    let mut ret = Vec::new();
    while !parser.eat(&token::CloseDelim(token::Brace)) {
        let pattern = get_regex(parser, &token::FatArrow, env);
        let action = parser.parse_expr();
        // optionnal comma for disambiguation
        parser.eat(&token::Comma);
        ret.push(Rule { pattern:pattern, action:action });
    }
    ret
}

// parses the main body of the lexer description
// entries here may be either rules of the gorm regex => action
// or "conditions" of the form condition { ... } that contains rules
// rules outside conditions implicitely belong to the "INITIAL" condition
fn get_conditions(parser: &mut Parser, env: &Env) -> Vec<Condition> {
    // remember the names of the conditions we already
    // encountered and where we stored their rules in
    // the conditions array
    let mut cond_names: HashMap<Name, uint> = HashMap::new();
    let mut ret = Vec::new();
    let initial = Condition {
        name: token::intern("INITIAL"),
        rules: Vec::new()
    };

    cond_names.insert(initial.name, 0);
    ret.push(initial);

    while parser.token != token::Eof {
        // here we can expect either a condition declaration
        // or simply a rule, which is then implicitly in the
        // "Initial" condition
        // in any case, we expect an ident or a regex first
        match parser.token {
            token::Ident(id, _) => {
                // this may be either the start of a regexp followed
                // by an arrow and an action or a condition followed
                // by an opening brace.
                // if we see an opening brace '{' here then it's a
                // condition whose name is the id we just parsed
                if parser.look_ahead(1, |tok| tok == &token::OpenDelim(token::Brace)) {
                    // ok it's a condition
                    // bump 2 times: the identifier and the lbrace
                    parser.bump();
                    parser.bump();

                    // parse the condition body
                    let rules = get_condition(parser, env);

                    // have we seen this condition before ?
                    match cond_names.get(&id.name).cloned() {
                        Some(i) => {
                            ret[i].rules.extend(rules.into_iter());
                            continue
                        }

                        None => ()
                    }

                    // nope, create it
                    ret.push(Condition { rules: rules, name: id.name });
                    cond_names.insert(id.name, ret.len() - 1);
                } else {
                    // ok, it's not a condition, so it's a rule of the form
                    // regex => action, with regex beginning by an identifier
                    let reg = get_regex(parser, &token::FatArrow, env);
                    let expr = parser.parse_expr();
                    ret[0].rules.push(Rule { pattern: reg, action: expr });
                }
            }

            _ => {
                // it's not an ident, but it may still be the
                // beginning of a regular expression
                let reg = get_regex(parser, &token::FatArrow, env);
                let expr = parser.parse_expr();
                ret[0].rules.push(Rule { pattern: reg, action: expr });
            }
        }
    }

    ret
}

// runs the parsing of the full analyser description
// - first gets an environment of the regular expression definitions
// - then parses the definitions of the rules and the conditions
pub fn parse(ident:Ident, parser: &mut Parser) -> LexerDef {
    let tokens = get_tokens(parser);
    let props = get_properties(parser);
    let defs = get_definitions(parser);
    let conditions = get_conditions(parser, &*defs);
    LexerDef { ident:ident, tokens:tokens, properties: props, conditions: conditions }
}
