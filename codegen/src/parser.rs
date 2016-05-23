// parser for the lexer description
// invoked by Rustc when compiling a lexer file
// uses libsyntax to parse what Rustc gives us

use std::collections::hash_map::HashMap;
use lexer::Condition;
use lexer::LexerDef;
use lexer::Rule;
use regex;
use regex::Regex;
use syntax::ast::Expr;
use syntax::ast::Ident;
use syntax::ast::Name;
use syntax::ast::Ty;
use syntax::codemap::Span;
use syntax::parse;
use syntax::parse::token;
use syntax::parse::token::keywords;
use syntax::parse::parser::Parser;
use syntax::errors::DiagnosticBuilder;
use syntax::ptr::P;


trait Tokenizer<'a> {
    // returns the current token, without consuming it
    fn token(&self) -> &token::Token;

    // consumes the current token
    fn bump(&mut self) -> ();

    // consumes the current token and return it
    // equivalent to token() followed by bump()
    fn bump_and_get(&mut self) -> token::Token;

    // consumes the current token and return true if
    // it corresponds to tok, or false otherwise
    fn eat(&mut self, tok: &token::Token) -> bool;
    fn eat_keyword(&mut self, kwd: token::keywords::Keyword) -> bool;

    // returns true if the next token is the given
    // token or keyword, without consuming it
    fn check(&mut self, tok: &token::Token) -> bool;
    fn check_keyword(&mut self, kwd: token::keywords::Keyword) -> bool;

    // expects the following token to be the
    // same as the given token, then consumes it
    fn expect(&mut self, tok: &token::Token) -> Result<(),DiagnosticBuilder<'a>>;

    // expect the next token to be an ident and consumes it
    fn parse_ident(&mut self) -> Result<Ident, DiagnosticBuilder<'a>>;

    // returns the span of the previous token
    fn last_span(&self) -> Span;

    // various functions to abort parsing
    fn span_fatal(&mut self, sp: Span, m: &str) -> DiagnosticBuilder<'a>;
    fn unexpected(&mut self) -> DiagnosticBuilder<'a>;
    fn unexpected_last(&mut self, tok: &token::Token) -> DiagnosticBuilder<'a>;
}

impl<'a> Tokenizer<'a> for Parser<'a> {
    fn token(&self) -> &token::Token { &self.token }
    fn bump(&mut self) { self.bump() }
    fn bump_and_get(&mut self) -> token::Token {
        self.bump_and_get()
    }
    fn eat(&mut self, tok: &token::Token) -> bool {
        self.eat(tok)
    }
    fn eat_keyword(&mut self, kwd: token::keywords::Keyword) -> bool {
        self.eat_keyword(kwd)
    }
    fn check(&mut self, tok: &token::Token) -> bool {
        self.check(tok)
    }
    fn check_keyword(&mut self, kwd: token::keywords::Keyword) -> bool {
        self.check_keyword(kwd)
    }
    fn expect(&mut self, tok: &token::Token) -> Result<(),DiagnosticBuilder<'a>> {
        self.expect(tok)
    }
    fn parse_ident(&mut self) -> Result<Ident, DiagnosticBuilder<'a>> { self.parse_ident() }
    fn last_span(&self) -> Span { self.last_span }
    fn span_fatal(&mut self, sp: Span, m: &str) -> DiagnosticBuilder<'a> {
        Parser::span_fatal(self, sp, m)
    }
    fn unexpected(&mut self) -> DiagnosticBuilder<'a> { Parser::unexpected::<()>(self).unwrap_err() }
    fn unexpected_last(&mut self, tok: &token::Token) -> DiagnosticBuilder<'a> {
        Parser::unexpected_last::<()>(self, tok).unwrap_err()
    }
}

// the "lexical" environment of regular expression definitions
type Env = HashMap<Name, usize>;

fn get_tokens<'a>(parser: &mut Parser<'a>) -> Result<Ident,DiagnosticBuilder<'a>> {
    let token = token::intern("token");
    match parser.token {
        token::Ident(id, _) if id.name == token => {
            parser.bump();
            let token = try!(parser.parse_ident());
            try!(parser.expect(&token::Semi));
            Ok(token)
        }
        _ => Ok(Ident::with_empty_ctxt(token::intern("Token")))
    }
}

fn get_properties<'a>(parser: &mut Parser<'a>)
        -> Result<Vec<(Name, P<Ty>, P<Expr>)>,DiagnosticBuilder<'a>> {
    let mut ret = Vec::new();
    let prop = token::intern("property");
    loop {
        match parser.token {
            token::Ident(id, _) if id.name == prop => {
                parser.bump();
                let name = try!(parser.parse_ident());
                try!(parser.expect(&token::Colon));
                let ty = try!(parser.parse_ty());
                try!(parser.expect(&token::Eq));
                let expr = try!(parser.parse_expr());
                try!(parser.expect(&token::Semi));
                ret.push((name.name, ty, expr));
            }

            _ => break
        }
    }

    Ok(ret)
}

// the functions below migth read their tokens either from the libsyntax
// parser type or from our tokenizer that reads tokens from a raw string,
// hence the type parameter

// recursively parses a character class, e.g. ['a'-'z''0'-'9''_']
// basically creates an or-expression per character in the class
fn get_char_class<'a, T: Tokenizer<'a>>(parser: &mut T)
        -> Result<regex::CharSet<char>, DiagnosticBuilder<'a>> {
    let mut ret = regex::CharSet::new();
    loop {
        let tok = parser.bump_and_get();
        match tok {
            token::CloseDelim(token::Bracket) => {
                break
            }

            token::Literal(token::Lit::Char(i), _) => {
                let ch = parse::char_lit(&*i.as_str()).0;

                match *parser.token() {
                    token::BinOp(token::Minus) => {
                        // a char seq, e.g. 'a' - 'Z'
                        parser.bump();
                        let ch2 = match parser.bump_and_get() {
                            token::Literal(token::Lit::Char(ch), _) =>
                                parse::char_lit(&*ch.as_str()).0,
                            _ => return Err(parser.unexpected())
                        };
                        if ch >= ch2 {
                            let last_span = parser.last_span();
                            return Err(parser.span_fatal(last_span,
                                "invalid character range"))
                        }
                        ret.push(ch .. ch2);
                    }

                    _ => { ret.push(ch .. ch); }
                }
            }

            token::Literal(token::Lit::Str_(id),_) => {
                if id.as_str().len() == 0 {
                    let last_span = parser.last_span();
                    return Err(parser.span_fatal(last_span,
                        "bad string constant in character class"))
                }
                for b in id.as_str().chars() {
                    ret.push(b .. b);
                }
            }

            _ => return Err(parser.unexpected_last(&tok))
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
fn get_const<'a, T: Tokenizer<'a>>(parser: &mut T, env: &Env)
        -> Result<Regex,DiagnosticBuilder<'a>> {
    let tok = parser.bump_and_get();
    // here we expect either
    // the start of a character-class, '['
    // the start of a parenthesized expression, '('
    // a literal char constant, 'a'
    match tok {
        token::Dot => Ok(Box::new(regex::Literal(regex::Any))),
        token::OpenDelim(token::Paren) => get_regex(parser,
            &token::CloseDelim(token::Paren), env),
        token::OpenDelim(token::Bracket) => {
            if parser.eat(&token::BinOp(token::Caret)) {
                Ok(Box::new(regex::Literal(
                    regex::NotClass(try!(get_char_class(parser)))
                )))
            } else {
                Ok(Box::new(regex::Literal(
                    regex::Class(try!(get_char_class(parser)))
                )))
            }
        }
        token::Literal(token::Lit::Char(ch), _) =>
            Ok(Box::new(regex::Literal(
                regex::Char(parse::char_lit(&*ch.as_str()).0)
            ))),
        token::Literal(token::Lit::Str_(id), _) =>
            match regex::string(&*id.as_str()) {
                Some(reg) => Ok(reg),
                None => {
                    let last_span = parser.last_span();
                    Err(parser.span_fatal(last_span,
                        "bad string constant in regular expression"))
                }
            },
        token::Ident(id, _) => match env.get(&id.name).cloned() {
            Some(value) => Ok(Box::new(regex::Var(value))),
            None => {
                let last_span = parser.last_span();
                Err(parser.span_fatal(last_span,
                &format!("unknown identifier: {}", id.name.as_str())))
            }
        },
        _ => Err(parser.unexpected_last(&tok))
    }
}

// a "closure" in a regular expression, i.e. expr*
// the * operator has lower precedence that concatenation
fn get_closure<'a, T: Tokenizer<'a>>(parser: &mut T, env: &Env)
        -> Result<Regex,DiagnosticBuilder<'a>> {
    let reg = try!(get_const(parser, env));
    if parser.eat(&token::BinOp(token::Star)) {
        Ok(Box::new(regex::Closure(reg)))
    } else if parser.eat(&token::BinOp(token::Plus)) {
        Ok(Box::new(regex::Cat(reg.clone(), Box::new(regex::Closure(reg)))))
    } else if parser.eat(&token::Question) {
        Ok(Box::new(regex::Maybe(reg))) }
    else {
        Ok(reg)
    }
}

// recursively parses a sequence of concatenations
// continues until it reaches the end of the current subexpr,
// indicated by the end parameter or an or operator, which has
// higher precedence. Concatenation is left-assoc
fn get_concat<'a, T: Tokenizer<'a>>(parser: &mut T, end: &token::Token, env: &Env)
    -> Result<Regex,DiagnosticBuilder<'a>> {
    let opl = try!(get_closure(parser, env));
    if parser.check(end) ||
        parser.check_keyword(token::keywords::As) ||
        parser.check(&token::BinOp(token::Or)) {
        Ok(opl)
    } else {
        let opr = try!(get_concat(parser, end, env));
        Ok(Box::new(regex::Cat(opl, opr)))
    }
}

// parse a binding of a regexp to an identifier, i.e. expr as id
fn get_binding<'a, T: Tokenizer<'a>>(parser: &mut T, end: &token::Token, env: &Env)
        -> Result<Regex, DiagnosticBuilder<'a>> {
    let expr = try!(get_concat(parser, end, env));
    if parser.eat_keyword(token::keywords::As) {
        let name = try!(parser.parse_ident());
        Ok(Box::new(regex::Bind(name, expr)))
    } else { Ok(expr) }
}

// entry point of the regex parser, parses an or-expression
// tries to parse a concat expression as the left operation, and then
// if we are not at the end of the current subexpression as indicated by
// the end parameter, we try to read a | operator followed by another
// expression which is parsed recursively (or is left-assoc)
fn get_regex<'a, T: Tokenizer<'a>>(parser: &mut T, end: &token::Token, env: &Env)
    -> Result<Regex,DiagnosticBuilder<'a>> {
    if parser.eat(end) {
        return Err(parser.unexpected());
    }
    let left = try!(get_binding(parser, end, env));
    if parser.eat(end) {
        Ok(left)
    } else {
        try!(parser.expect(&token::BinOp(token::Or)));
        let right = try!(get_regex(parser, end, env));
        Ok(Box::new(regex::Or(left, right)))
    }
}

// a pattern is an association of a name to a regular expression
// this function expects the next tokens to be id = reg, with id
// being a non-keyword identifier and reg a literal constant
fn get_pattern<'a>(parser: &mut Parser<'a>, env: &Env)
        -> Result<(Ident, Regex),DiagnosticBuilder<'a>> {
    let name = try!(parser.parse_ident());
    try!(parser.expect(&token::Eq));
    let reg = try!(get_regex(parser, &token::Semi, env));
    Ok((name, reg))
}

// a definition is of the form let pattern; see the function above
// for a description of pattern. This function just tries to parse
// as much definitions as possible, until it sees something that
// does not match. returns an error if it encounters a malformed
// definition, otherwise return the "environment" containing named
// regular expression definitions
fn get_definitions<'a>(parser: &mut Parser<'a>)
        -> Result<(Env, Vec<Regex>), DiagnosticBuilder<'a>> {
    let mut env = HashMap::new();
    let mut defs = Vec::new();
    while parser.eat_keyword(keywords::Let) {
        let (id, pat) = try!(get_pattern(parser, &env));
        env.insert(id.name, defs.len());
        defs.push(pat);
    }
    Ok((env, defs))
}

// parses the contents of a "condition" body, i.e. simply a
// list of rules of the form regex => action
// stops as soon as we encounter a closing brace } which
// indicates the end of the condition body
fn get_condition<'a>(parser: &mut Parser<'a>, env: &Env)
        -> Result<Vec<Rule>,DiagnosticBuilder<'a>> {
    let mut ret = Vec::new();
    while !parser.eat(&token::CloseDelim(token::Brace)) {
        let pattern = try!(get_regex(parser, &token::FatArrow, env));
        let action = try!(parser.parse_expr());
        // optionnal comma for disambiguation
        parser.eat(&token::Comma);
        ret.push(Rule { pattern:pattern, action:action });
    }
    Ok(ret)
}

// parses the main body of the lexer description
// entries here may be either rules of the gorm regex => action
// or "conditions" of the form condition { ... } that contains rules
// rules outside conditions implicitely belong to the "INITIAL" condition
fn get_conditions<'a>(parser: &mut Parser<'a>, env: &Env)
        -> Result<Vec<Condition>,DiagnosticBuilder<'a>> {
    // remember the names of the conditions we already
    // encountered and where we stored their rules in
    // the conditions array
    let mut cond_names: HashMap<Name, usize> = HashMap::new();
    let mut ret = Vec::new();
    let initial = Condition {
        name: token::intern("INITIAL"),
        span: parser.span,
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
                    let sp = parser.span;
                    parser.bump();
                    parser.bump();

                    // parse the condition body
                    let rules = try!(get_condition(parser, env));

                    // have we seen this condition before ?
                    match cond_names.get(&id.name).cloned() {
                        Some(i) => {
                            ret[i].rules.extend(rules.into_iter());
                            continue
                        }

                        None => ()
                    }

                    // nope, create it
                    ret.push(Condition { rules: rules, span: sp, name: id.name });
                    cond_names.insert(id.name, ret.len() - 1);
                } else {
                    // ok, it's not a condition, so it's a rule of the form
                    // regex => action, with regex beginning by an identifier
                    let reg = try!(get_regex(parser, &token::FatArrow, env));
                    let expr = try!(parser.parse_expr());
                    ret[0].rules.push(Rule { pattern: reg, action: expr });
                }
            }

            _ => {
                // it's not an ident, but it may still be the
                // beginning of a regular expression
                let reg = try!(get_regex(parser, &token::FatArrow, env));
                let expr = try!(parser.parse_expr());
                ret[0].rules.push(Rule { pattern: reg, action: expr });
            }
        }
    }

    Ok(ret)
}

// runs the parsing of the full analyser description
// - first gets an environment of the regular expression definitions
// - then parses the definitions of the rules and the conditions
pub fn parse<'a>(ident:Ident, parser: &mut Parser<'a>) ->
        Result<LexerDef,DiagnosticBuilder<'a>> {
    let tokens = try!(get_tokens(parser));
    let props = try!(get_properties(parser));
    let (env, defs) = try!(get_definitions(parser));
    let conditions = try!(get_conditions(parser, &env));
    Ok(LexerDef { ident:ident, tokens:tokens, properties: props, defs: defs,
                  conditions: conditions })
}
