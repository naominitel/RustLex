use std::rc::Rc;
use util::BinSetu8;

pub use self::Regex::{Or, Cat, Maybe, Closure, Class, NotClass, Var, Char, Any};

#[derive(Clone)]
pub enum Regex {
    // binary operators
    Or(Box<Regex>, Box<Regex>),
    Cat(Box<Regex>, Box<Regex>),

    // unary operators
    Maybe(Box<Regex>),
    Closure(Box<Regex>),

    // constants
    Class(Box<BinSetu8>),
    NotClass(Box<BinSetu8>),
    Var(Rc<Regex>),
    Char(u8),
    Any
}

pub fn string(string: &str) -> Option<Box<Regex>> {
    let mut it = string.bytes();
    let mut reg = Box::new(Char(match it.next() {
        Some(ch) => ch,
        None => return None
    }));

    for ch in it {
        reg = Box::new( Cat(reg, Box::new (Char(ch))) );
    }

    Some(reg)
}

impl Regex {
    #[allow(dead_code)]
    // prints the AST for debugging purposes
    pub fn show(&self, span: &str) {
        match self {
            &Or(ref l, ref r) => {
                println!("{} Or of: ", span);
                l.show(&format!("  {}", span));
                r.show(&format!("  {}", span));
            }

            &Cat(ref l, ref r) => {
                println!("{} Cat of: ", span);
                l.show(&format!("  {}", span));
                r.show(&format!("  {}", span));
            }

            &Maybe(ref reg) => {
                println!("{} Optionnally the regex:", span);
                reg.show(span);
            }

            &Closure(ref reg) => {
                println!("{} The eclosure of", span);
                reg.show(&format!("  {}", span))
            }

            &Var(ref reg) => {
                (**reg).show(span);
            }

            &Char(ref c) => println!("{} The char {}", span, *c as char),
            &Any => println!("Anything"),
            _ => ()
        }
    }
}
