use std::rc::Rc;

#[deriving(Clone)]
pub enum Regex {
    Or(~Regex, ~Regex),
    Cat(~Regex, ~Regex),
    Maybe(~Regex),
    Closure(~Regex),
    Char(u8),
    Class(Vec<u8>),
    Var(Rc<Regex>)
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
            &Char(ref c) => {
                println!("{:s} The char {:c}", span, *c as char);
            }
            &Closure(ref reg) => {
                println!("{:s} The eclosure of", span);
                reg.show(format!("  {:s}", span))
            }
            &Class(ref reg) => {
                println!("{:s} The character class {}", span, reg);
            }
            &Var(ref reg) => {
                (**reg).show(span);
            }
            &Maybe(ref reg) => {
                println!("{:s} Optionnally the regex:", span);
                reg.show(span);
            }
        }
    }
}
