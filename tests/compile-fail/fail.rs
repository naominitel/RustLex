#![feature(plugin)]
#![plugin(rustlex)]

#[allow(plugin_as_library)] extern crate rustlex;

type Token = ();

rustlex! lexer(
    let ID = ['a'-'z']+;
    //~^ NOTE included in another pattern
    ID => println!("Saw an identifier")
    "sizeof" => println!("Saw token sizeof") //~ ERROR unreachable pattern
);

fn main() {
}
