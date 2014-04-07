use std::rc::Rc;
use util::BinSetu8;

#[deriving(Clone)]
pub enum Regex {
    // binary operators
    Or(~Regex, ~Regex),
    Cat(~Regex, ~Regex),

    // unary operators
    Maybe(~Regex),
    Closure(~Regex),

    // constants
    Class(~BinSetu8),
    NotClass(~BinSetu8),
    Var(Rc<Regex>),
    Char(u8),
    Any
}

pub fn string(string: &str) -> Option<~Regex> {
    let mut it = string.bytes();
    let mut reg = ~Char(match it.next() {
        Some(ch) => ch,
        None => return None
    });

    for ch in it {
        reg = ~Cat(reg, ~Char(ch));
    }

    Some(reg)
}

impl Regex {
    #[allow(dead_code)]
    // prints the AST for debugging purposes
    pub fn show(&self, span: &str) {
        match self {
            &Or(ref l, ref r) => {
                println!("{:s} Or of: ", span);
                l.show(format!("  {:s}", span));
                r.show(format!("  {:s}", span));
            }

            &Cat(ref l, ref r) => {
                println!("{:s} Cat of: ", span);
                l.show(format!("  {:s}", span));
                r.show(format!("  {:s}", span));
            }

            &Maybe(ref reg) => {
                println!("{:s} Optionnally the regex:", span);
                reg.show(span);
            }

            &Closure(ref reg) => {
                println!("{:s} The eclosure of", span);
                reg.show(format!("  {:s}", span))
            }

            &Var(ref reg) => {
                (**reg).show(span);
            }

            &Char(ref c) => println!("{:s} The char {:c}", span, *c as char),
            &Any => println!("Anything"),
            _ => ()
        }
    }
}
