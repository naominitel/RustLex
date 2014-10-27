#![feature(phase)]

#[phase(plugin)]
extern crate rustlex;

#[path="../common/strreader.rs"]
mod strreader;

type Token = ();

rustlex!(
    "sizeof" => println!("Saw token sizeof") //~ ERROR unreachable pattern
    //~^ NOTE included in another pattern
    ['a'-'z']+ => println!("Saw an identifier")
)

fn main() {
    let str = "foo ";
    let inp = strreader::reader(str) as Box<::std::io::Reader>;
    let mut lexer = Lexer::new(inp);

    for tok in lexer {
    }
}
