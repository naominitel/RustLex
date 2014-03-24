### RustLex: lexical analysers generator for Rust

RustLex is a lexical analysers generator, i.e. a program that generate [lexical analysers](http://en.wikipedia.org/wiki/Lexical_analysis) for use in compiler from a description of the language using regular expressions. It is similar to the well-known [Lex](http://en.wikipedia.org/wiki/Lex_(software)) but is written in Rust and outputs Rust code as the analyser.
It differs from Lex by using Rust's new [syntax extensions]() system as the interface for defining lexical analysers. The description of the analyser thus can be directly embedded into a Rust source file, and the generator code will be called by Rustc at the macro-expansion phase.

#### Building

RustLex can be very easily compiled into a dynamically library that will be loaded by Rustc when expanding syntax extensions:

```
rustc rustlex.rs
```

#### Usage

Defining a lexical analyser is done using the `rustlex!` syntax extension. To made this syntax extension available, you first need to tell Rustc to load the RustLex library by adding the following at the top of your crate:

```
#[feature(phase)];
#[phase(syntax)]
extern crate rustlex;
```

You can then invoke the `rustlex!` macro anywhere. The macro will expand into several structures and functions describing the lexical analyser. Putting the macro invokation in a module will cause there structures and functions to be available inside this module. You can thus have several lexical analysers in the same crate provided they are in different modules to avoid naming conflicts.

The `rustlex!` macro takes as argument the description of the lexical analyser. The description consists of two parts:
* definitions of regular expressions
* definitions of rules

Here's a simple example that matches integers, floats and identifiers in a file, using the C language syntax for literals and identifiers:

```rust
#[feature(phase)];
#[phase(syntax)]
extern crate rustlex;

// The Token type is returned by the lexer function on
// each call and must be declared in the same module
// as where the rustlex! macro is invoked
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
    let INT = ['0'-'9']+['u''U''l''L']%;
    let HEX = '0'['x''X']['a'-'f''A'-'F''0'-'9']+['u''U''l''L']%;
    let FLOAT = (['0'-'9']+'.'|['0'-'9']*'.'['0'-'9']+)(['e''E']['+''-']%['0'-'9']+)%['f''F''l''L']%;
    let DEC_FLOAT = ['0'-'9']+(['e''E']['+''-']%['0'-'9']+)['f''F''l''L']%;
    let HEX_FLOAT = '0'['x''X']['a'-'f''A'-'F''0'-'9']*'.'['a'-'f''A'-'F''0'-'9']*(['p''P']['0'-'9']+)%['f''F''l''L']%;
    let INTCONST = (INT|HEX);
    let FLTCONST = (FLOAT|HEX_FLOAT|DEC_FLOAT);
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
)

fn main() {
    let pth = Path::new("input");
    let inp = ~::std::io::File::open(&pth).unwrap() as ~::std::io::Reader;
    
    // instantiating the lexer struct takes a ~Reader as argument
    let mut lexer = Lexer::new(inp);

    // you can now use the lexer as an iterator
    // each call to next() will return Some(tok),
    // with tok being the next available token,
    // or None when reaching eof
    for tok in lexer {
        match tok {
            TokInt => println!("Saw an int"),
            TokFloat => println!("Saw a float"),
            TokId => println!("Saw an ID"),
        }
    }
}
```

Just compile the above example using:

```
rustc example.rs -L.
```

Don't forget to add the directory containing the RustLex library to the linker path of Rustc, with the `-L` option. In my case, it's the current directory (`.`).
