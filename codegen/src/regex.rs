use std::rc::Rc;
use std::ops::Range;
use std::slice;

pub use self::Regex::{Or, Cat, Maybe, Closure, Class, NotClass, Var, Char, Any, Bind};

#[derive(Clone)]
pub struct CharSet(Vec<Range<u8>>);

impl CharSet {
    pub fn new() -> CharSet {
        CharSet(Vec::new())
    }

    pub fn push(&mut self, range: Range<u8>) {
        let CharSet(ref mut vec) = *self;
        vec.push(range);
    }

    pub fn contains(&self, item: u8) -> bool {
        let CharSet(ref vec) = *self;
        vec.iter().any(|x| x.start <= item && item < x.end)
    }

    // TODO: should FlatMap, when Rust is able to express it...
    pub fn iter(&self) -> slice::Iter<Range<u8>> {
        let CharSet(ref vec) = *self;
        vec.iter()
    }
}

#[derive(Clone)]
pub enum Regex {
    // binary operators
    Or(Box<Regex>, Box<Regex>),
    Cat(Box<Regex>, Box<Regex>),

    // unary operators
    Maybe(Box<Regex>),
    Closure(Box<Regex>),

    // constants
    Class(CharSet),
    NotClass(CharSet),
    Var(Rc<Regex>),
    Char(u8),
    Any,

    // bind
    Bind(::syntax::ast::Ident, Box<Regex>)
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
