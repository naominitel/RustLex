# RustLex: lexical analysers generator for Rust

*Warning: RustLex 0.4.0 and higher works only for the nightly channel of Rust.*

*If you want to use RustLex with the nightly channel of Rust instead, use
version [0.3.4] instead. Note though that syntex is no longer actively maintained
and, while this version is provided for compatibility, its use is discouraged.*

[0.3.4]: https://crates.io/crates/rustlex/0.3.4

[![Build Status](https://travis-ci.org/naominitel/RustLex.svg)](https://travis-ci.org/naominitel/RustLex)

RustLex is a lexical analysers generator, i.e. a program that generate [lexical
analysers](http://en.wikipedia.org/wiki/Lexical_analysis) for use in compiler
from a description of the language using regular expressions. It is similar to
the well-known [Lex](http://en.wikipedia.org/wiki/Lex_(software)) but is written
in Rust and outputs Rust code as the analyser.  It differs from Lex by using
Rust's new [syntax extensions]() system as the interface for defining lexical
analysers. The description of the analyser thus can be directly embedded into a
Rust source file, and the generator code will be called by `rustc` at the
macro-expansion phase.

## RustLex availability and Rust compatibility

RustLex using syntax extensions, it has to deal with rustc `libsyntax`.
`libsyntax` is more or less the compiler guts, and it has been explicitely
excluded from the Rust 1.0 roadmap. Bottom line is, RustLex inline syntax
generation is not usable with Rust stable.

Using Rust nightly, just indicate a dependency to `rustlex` in your Cargo.toml
and add the following lines at the top of your crate:

```rust
#![feature(plugin)]
#![plugin(rustlex)]
#[allow(plugin_as_library)] extern crate rustlex;
```

This will make `rustc` load the RustLex plugin which contains everything that
is needed to generate the code.

## Defining a lexer

You can then invoke the `rustlex!` macro anywhere. The macro will expand into a
single lexer structure and implementation describing the lexical analyser.

The `rustlex!` macro takes as argument the name of the structure and the
 description of the lexical analyser. The description consists of two parts:
* definitions of regular expressions
* definitions of rules

A minimum lexer will look like:

```rust
rustlex! SimpleLexer {
    // expression definitions
    let A = 'a';

    // then rules
    A => |lexer:&mut SimpleLexer<R>| Some(TokA ( lexer.yystr() ))
}
```

More complex regular expression definition examples can be found in [a more
 complex example](tests/complex.rs). It is worth noting that:
* characters (standalone or in character class) and strings have to be quoted as
  in Rust or C (simple quote for character, double quote for strings)
* an expression definition can be "called" by its identifier in another
  expression

## Using a lexer

The lexer will read characters from a standard Rust `Reader` and implement a
`Token` `iterator`.

```rust
let inp = BufReader::new("aa".as_bytes());
let mut lexer = SimpleLexer::new(inp);
for tok in lexer {
    ...
}
```

## Advanced lexer features

### Token enumeration

By default, `rustlex!` assumes the existence of a token enumeration named
`Token` in the same module, but this name can be overriden when needed as is the
case for the `OtherLexer` from [this example](tests/simple.rs).

### Conditions

As in flex, conditions can be defined to have the lexer switch from one mode to
another.

Check out [this example](tests/condition.rs).

### Arbitrary lexer properties and methods

It is possible to add specific fields to the lexer structure using the
`property` keyword as shown [there](tests/properties.rs).

Lexer methods (to be called from action code) can also be defined by a normal
`impl` section.
