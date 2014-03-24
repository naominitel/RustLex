use std::rc::Rc;

#[deriving(Clone)]
pub enum Regex {
    Or(~Regex, ~Regex),
    Cat(~Regex, ~Regex),
    Closure(~Regex),
    Maybe(~Regex),
    Char(u8),
    Var(Rc<Regex>)
}

pub fn seq(start: u8, end: u8) -> Option<~Regex> {
    if start >= end {
        None
    } else {
        let mut ret = ~Char(start);
        let mut c = start;

        while c < end {
            let op = ~Char(c);
            ret = ~Or(ret, op);
            c += 1
        }

        Some(~Or(ret, ~Char(end)))
    }
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
