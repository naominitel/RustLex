### RustLex: lexical analysers generator for Rust

RustLex is a lexical analysers generator, i.e. a program that generate [lexical
analysers](http://en.wikipedia.org/wiki/Lexical_analysis) for use in compiler
from a description of the language using regular expressions. It is similar to
the well-known [Lex](http://en.wikipedia.org/wiki/Lex_(software)) but is written
in Rust and outputs Rust code as the analyser.  It differs from Lex by using
Rust's new [syntax extensions]() system as the interface for defining lexical
analysers. The description of the analyser thus can be directly embedded into a
Rust source file, and the generator code will be called by Rustc at the
 macro-expansion phase.

#### Rustlex availibility and rust compatibility

Rustlex using syntax extensions, it has to deal with rustc `libsyntax`.
`libsyntax` is more or less the compiler guts, and it has been explicitely
excluded from the Rust 1.0 roadmap. Bottom line is, RustLex inline syntax
generation will not be usable with Rust 1.0.

The way of using RustLex depends on the version of Rust you are using to build
your project.

#### Nightly

This is the easy way. Just indicate a dependency to `rustlex` in your Cargo.toml
and add the following lines at the top of your crate:

```rust
#![feature(plugin)]
#![plugin(rustlex)]
#[allow(plugin_as_library)] extern crate rustlex;
#[macro_use] extern crate log;
```

This will make `rustc` load the RustLex plugin which contains everything that
is needed to generate the code.

#### Stable

On the stable channel, you have to use [syntex]
(https://github.com/erickt/rust-syntex) to first perform code generation and then
 `include!()` the produced code into your project.

Your `Cargo.toml` should look like that:

```toml
[package]
name = "your package"
version = "0.0.0"
build = "build.rs"

[build-dependencies]
rustlex_codegen = { version = "*", features = ["with-syntex"] }
syntex          = { version = "*", optional = true }

[dependencies]
rustlex_codegen = { version = "*", features = ["with-syntex"] }
```

You will need to write a `build.rs` file. This file is automatically called by
`cargo` before the build (according to the `build` variable in the `Cargo.toml`
file). In our case, it will use syntex to process your code and call RustLex's
code generation:

```rust
pub fn main() {
    extern crate syntex;
    extern crate rustlex_codegen;
    use std::env;
    use std::path::Path;

    let mut registry = syntex::Registry::new();
    rustlex_codegen::plugin_registrar(&mut registry);
    let src = Path::new("src/foo.in.rs");
    let dst = Path::new(&env::var_os("OUT_DIR").unwrap()).join("foo.rs");
    registry.expand("", &src, &dst).unwrap();
}
```

Replace `foo.in.rs` by the name of the file in which you will use RustLex. Note
that all its submodules are processed with it, so you don't need to add them as
well even if they also contain calls to the RustLex macro.

This will generate a file called `foo.rs` (or however you named it) in Cargo's
`OUT_DIR`. To use this file, add something like this somewhere in your project
(for example in a `foo.rs` file placed alongside `foo.in.rs`):

If you want to build a project using RustLex that can be built using either
stable or nightly, you can write a portable `Cargo.toml` file using features,
and use the `#[cfg()]` attribute in your `build.rs` and the rest of your code to
make it build using both versions. You can check out the [test project]
(http://github.com/naominitel/RustLex/tree/master/codegen/tests/Cargo.toml/) for
`rustlex_codegen` for an example.

#### Defining a lexer

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
  in rust or C (simple quote for character, double quote for strings)
* an expression definition can be "called" by its identifier in another
  expression

#### Using a lexer

The lexer will read characters from a standard rust `Reader` and implement a
`Token` `iterator`.

```rust
let inp = BufReader::new("aa".as_bytes());
let mut lexer = SimpleLexer::new(inp);
for tok in lexer {
    ...
}
```

#### Advanced lexer features

##### Token enumeration

By default, `rustlex!` assumes the existence of a token enumeration named
`Token` in the same module, but this name can be overriden when needed as is the
case for the `OtherLexer` from [this example](tests/simple.rs).

##### Conditions

As in flex, conditions can be defined to have the lexer switch from one mode to
another.

Check out [this example](tests/condition.rs).

##### Arbitrary lexer properties and methods

It is possible to add specific fields to the lexer structure using the
`property` keyword as shown [there](tests/properties.rs).

Lexer methods (to be called from action code) can also be defined by a normal
`impl` section.
