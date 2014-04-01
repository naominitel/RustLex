// parser for the lexer description
// invoked by Rustc when compiling a lexer file
// uses libsyntax to parse what Rustc gives us

use collections::hashmap::HashMap;
use lexer::Condition;
use lexer::LexerDef;
use lexer::Rule;
use regex;
use regex::Regex;
use regex::{Cat, Char, Closure, Or, Maybe, Var};
use std::rc::Rc;
use syntax::ast::Ident;
use syntax::ast::Name;
use syntax::parse::token;
use syntax::parse::token::keywords;
use syntax::parse::parser::Parser;

// the "lexical" environment of regular expression definitions
type Env = HashMap<Name, Rc<Regex>>;

// recursively parses a character class, e.g. ['a'-'z''0'-'9''_']
// basically creates an or-expression per character in the class
fn getCharClass(parser: &mut Parser) -> ~Regex {
    let reg = match parser.bump_and_get() {
        token::LIT_CHAR(ch) => {
            let ch = ch as u8;

            match parser.token {
                token::BINOP(token::MINUS) => {
                    // a char seq, e.g. 'a' - 'Z'
                    parser.bump();
                    let ch2 = match parser.bump_and_get() {
                        token::LIT_CHAR(ch) => ch as u8,
                        _ => parser.unexpected()
                    };
                    match regex::seq(ch, ch2) {
                        Some(r) => r,
                        None => parser.span_fatal(parser.last_span,
                            "invalid character range")
                    }
                }

                _ => ~Char(ch)
            }
        }

        token::LIT_STR(id) => {
            match regex::class(token::get_name(id.name).get()) {
                Some(reg) => reg,
                None => parser.span_fatal(parser.last_span,
                    "bad string constant in character class")
            }
        }

        _ => parser.unexpected()
    };

    match parser.token {
        token::RBRACKET => {
            parser.bump();
            reg
        }

        _ => ~Or(reg, getCharClass(parser))
    }
}

// parses a "constant" in an regular expression, i.e. either
// - a literal character
// - a character class
// - an identifier refering to another expression
// parenthesized subexpressions are also parsed here since the have
// the same operator precedence as the constants
fn getConst(parser: &mut Parser, env: &Env) -> ~Regex {
    let tok = parser.bump_and_get();
    // here we expect either
    // the start of a character-class, '['
    // the start of a parenthesized expression, '('
    // a literal char constant, 'a'
    match tok {
        token::LPAREN => getRegex(parser, &token::RPAREN, env),
        token::LBRACKET => getCharClass(parser),
        token::LIT_CHAR(ch) => ~Char(ch as u8),
        token::LIT_STR(id) => match regex::string(token::get_name(id.name).get()) {
            Some(reg) => reg,
            None => parser.span_fatal(parser.last_span,
                "bad string constant in regular expression")
        },
        token::IDENT(id, _) => match env.find_copy(&id.name) {
            Some(value) => ~Var(value),
            None => parser.span_fatal(parser.last_span,
                format!("unknown identifier: {:s}", 
                    token::get_name(id.name).get()))
        },
        _ => parser.unexpected_last(&tok)
    }
}

// a "closure" in a regular expression, i.e. expr*
// the * operator has lower precedence that concatenation
fn getClosure(parser: &mut Parser, env: &Env) -> ~Regex {
    let reg = getConst(parser, env);
    if parser.eat(&token::BINOP(token::STAR)) { ~Closure(reg) }
    else if parser.eat(&token::BINOP(token::PLUS)) {
        ~Cat(reg.clone(), ~Closure(reg))
    }
    else if parser.eat(&token::BINOP(token::PERCENT)) { ~Maybe(reg) }
    else { reg }
}

// recursively parses a sequence of concatenations
// continues until it reaches the end of the current subexpr,
// indicated by the end parameter or an or operator, which has
// higher precedence. Concatenation is left-assoc
fn getConcat(parser: &mut Parser, end: &token::Token, env: &Env) -> ~Regex {
    let opl = getClosure(parser, env);
    if &parser.token == end ||
        parser.token == token::BINOP(token::OR) {
        opl
    } else {
        let opr = getConcat(parser, end, env);
        ~Cat(opl, opr)
    }
} 

// entry point of the regex parser, parses an or-expression
// tries to parse a concat expression as the left operation, and then
// if we are not at the end of the current subexpression as indicated by
// the end parameter, we try to read a | operator followed by another
// expression which is parsed recursively (or is left-assoc)
fn getRegex(parser: &mut Parser, end: &token::Token, env: &Env) -> ~Regex {
    if parser.eat(end) {
        parser.unexpected();
    }
    let left = getConcat(parser, end, env);
    if parser.eat(end) { left }
    else {
        parser.expect(&token::BINOP(token::OR));
        let right = getRegex(parser, end, env);
        ~Or(left, right)
    }
}

// a pattern is an association of a name to a regular expression
// this function expects the next tokens to be id = reg, with id
// being a non-keyword identifier and reg a literal constant
fn getPattern(parser: &mut Parser, env: &Env) -> (Ident, ~Regex) {
    let name = parser.parse_ident();
    parser.expect(&token::EQ);
    let reg = getRegex(parser, &token::SEMI, env);
    (name, reg)
}
                        
// a definition is of the form let pattern; see the function above
// for a description of pattern. This function just tries to parse
// as much definitions as possible, until it sees something that
// does not match. returns an error if it encounters a malformed
// definition, otherwise return the "environment" containing named
// regular expression definitions
fn getDefinitions(parser: &mut Parser) -> ~Env {
    let mut ret = ~HashMap::new();
    while parser.eat_keyword(keywords::Let) {
        let (id, pat) = getPattern(parser, ret);
        ret.insert(id.name, Rc::new(*pat));
    }
    ret
}

// parses the contents of a "condition" body, i.e. simply a
// list of rules of the form regex => action
// stops as soon as we encounter a closing brace } which
// indicates the end of the condition body
fn getCondition(parser: &mut Parser, env: &Env) -> ~[Rule] {
    let mut ret = ~[];
    while !parser.eat(&token::RBRACE) {
        let reg = getRegex(parser, &token::FAT_ARROW, env);
        let stmt = parser.parse_stmt(Vec::new());
        ret.push(Rule { pattern: reg, action: stmt });
    }
    ret
}

// parses the main body of the lexer description
// entries here may be either rules of the gorm regex => action
// or "conditions" of the form condition { ... } that contains rules
// rules outside conditions implicitely belong to the "INITIAL" condition
fn getConditions(parser: &mut Parser, env: &Env) -> ~[Condition] {
    // remember the names of the conditions we already
    // encountered and where we stored their rules in
    // the conditions array
    let mut cond_names: HashMap<Name, uint> = HashMap::new();
    let mut ret = ~[];
    let initial = Condition {
        name: token::intern("INITIAL"),
        rules: ~[]
    };

    cond_names.insert(initial.name, 0);
    ret.push(initial);

    while parser.token != token::EOF {
        // here we can expect either a condition declaration
        // or simply a rule, which is then implicitly in the
        // "Initial" condition
        // in any case, we expect an ident or a regex first
        match parser.token {
            token::IDENT(id, _) => {
                // this may be either the start of a regexp followed
                // by an arrow and an action or a condition followed
                // by an opening brace.
                // if we see an opening brace '{' here then it's a
                // condition whose name is the id we just parsed
                if parser.look_ahead(1, |tok| tok == &token::LBRACE) {
                    // ok it's a condition
                    // bump 2 times: the identifier and the lbrace
                    parser.bump();
                    parser.bump();

                    // parse the condition body
                    let rules = getCondition(parser, env);

                    // have we seen this condition before ?
                    match cond_names.find_copy(&id.name) {
                        Some(i) => {
                            ret[i].rules.push_all_move(rules);
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
                    let reg = getRegex(parser, &token::FAT_ARROW, env);
                    let stmt = parser.parse_stmt(Vec::new());
                    ret[0].rules.push(Rule { pattern: reg, action: stmt });
                }
            }

            _ => {
                // it's not an ident, but it may still be the
                // beginning of a regular expression
                let reg = getRegex(parser, &token::FAT_ARROW, env);
                let stmt = parser.parse_stmt(Vec::new());
                ret[0].rules.push(Rule { pattern: reg, action: stmt });
            }
        }
    }

    ret
}

// runs the parsing of the full analyser description
// - first gets an environment of the regular expression definitions
// - then parses the definitions of the rules and the conditions
pub fn parse(parser: &mut Parser) -> LexerDef {
    let defs = getDefinitions(parser);
    let conditions = getConditions(parser, defs);
    LexerDef { conditions: conditions }
}
