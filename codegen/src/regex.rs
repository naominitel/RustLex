use std::ops::Range;
use std::option::IntoIter;
use fsa::nfa;
use fsa::nfa::{No, One, Two, More};
use unicode;

pub use self::RegexNode::{Or, Cat, Maybe, Closure, Var, Literal, Bind};
pub use self::Const::{Class, NotClass, Char, Any};

#[derive(Clone)]
pub struct CharSet<T>(pub Vec<Range<T>>);

impl<T> CharSet<T> {
    pub fn new() -> CharSet<T> {
        CharSet(Vec::new())
    }

    pub fn push(&mut self, range: Range<T>) {
        let CharSet(ref mut vec) = *self;
        vec.push(range);
    }

    pub fn contains(&self, item: T) -> bool where T: PartialOrd {
        let CharSet(ref vec) = *self;
        vec.iter().any(|x| x.start <= item && item <= x.end)
    }
}

#[derive(Clone)]
pub enum Const {
    Class(CharSet<char>),
    NotClass(CharSet<char>),
    Char(char),
    Any,
}

pub type Regex = Box<RegexNode>;

#[derive(Clone)]
pub enum RegexNode {
    // binary operators
    Or(Regex, Regex),
    Cat(Regex, Regex),

    // unary operators
    Maybe(Regex),
    Closure(Regex),

    // constants
    Var(usize),
    Literal(Const),

    // bind
    Bind(::syntax::ast::Ident, Regex)
}

pub fn string(string: &str) -> Option<Regex> {
    let mut it = string.chars();
    let mut reg = Box::new(Literal(Char(match it.next() {
        Some(ch) => ch,
        None => return None
    })));

    for ch in it {
        reg = Box::new(Cat(reg, Box::new(Literal(Char(ch)))));
    }

    Some(reg)
}

#[derive(Clone, Copy, Eq, Hash, PartialEq, PartialOrd)]
pub struct Action(pub usize);

impl nfa::StateData for Action {
    fn no_data() -> Action {
        Action(0)
    }

    fn combine(a: Action, b: Action) -> Action {
        if a >= b { a } else { b }
    }

    fn is_final(&self) -> bool {
        *self != Action(0)
    }
}

// labels on the transition of the NFA. similar to the Const type
// used in the regex AST except that we now work with bytes instead
// of characters. Unicode characters have been expanded into several
// states and transitions labeled with the bytes that correspond to
// the representation of this character in a given encoding (e.g.
// UTF-8).
// we don't have Any here, since it can easyly be represented as
// NotClass(vec![])
pub enum Label {
    Byte(u8),
    Class(CharSet<u8>),
    NotClass(CharSet<u8>)
}

pub struct State {
    // the McNaughton-Yamada-Thompson
    // construction algorithm will build
    // NFAs whose states have 0, 1 or
    // 2 e-transitions
    etrans: nfa::Etrans,

    // as for the transitions representation,
    // most of the time, there will be a single
    // transition or no transition at all but
    // we use a SmallVec here to optimize the
    // case in which there are many transitions
    // to a single state (typically a character
    // class)
    pub trans: Option<(Label, usize)>,

    // 0: no action. otherwise, it's
    // a f1nal state with an action
    action: Action
}

impl nfa::State for State {
    type Data = Action;
    type Iter = IntoIter<usize>;

    fn new() -> State {
        State {
            trans: None,
            etrans: No,
            action: Action(0)
        }
    }

    fn etransition<'a>(&'a self) -> &'a nfa::Etrans {
        &self.etrans
    }

    fn transition(&self, c: u8) -> IntoIter<usize> {
        match self.trans {
            Some((Label::Class(ref set), dst)) if set.contains(c) => Some(dst),
            Some((Label::NotClass(ref set), dst)) if !set.contains(c) => Some(dst),
            Some((Label::Byte(ch), dst)) if ch == c => Some(dst),
            _ => None
        }.into_iter()
    }

    fn data(&self) -> Action {
        self.action
    }
}

pub type Automaton = nfa::Automaton<State>;

// creates a new Non-deterministic Finite Automaton using the
// McNaughton-Yamada-Thompson construction
// takes several regular expressions, each with an attached action
pub fn build_nfa<Encoding>(regexs: &[(Regex, Action)], defs: &[Regex])
    -> Automaton where Encoding: unicode::Encode {
    let mut ret = Automaton {
        states: Vec::new(),
        initial: 0usize
    };

    let ini = ret.create_state();
    let mut etrans = Vec::new();

    for &(ref reg, act) in regexs.iter() {
        let (init, f1nal) = reg.to_automaton::<Encoding>(&mut ret, defs);
        etrans.push(init);
        ret.states[f1nal].action = act;
    }

    ret.states[ini].etrans = More(etrans);
    ret.initial = ini;
    ret
}

impl RegexNode {
    // the construction is implemented recursively. Each call builds a
    // sub-expression of the regex, and returns the f1nals and initial states
    // only thos states will have to be modified so transitions numbers
    // won't have to be changed
    // the initial state is always the last state created, this way we can reuse
    // it in the concatenation case and avoid adding useless e-transitions
    fn to_automaton<Encoding>(&self, auto: &mut Automaton, defs: &[Regex])
        -> (usize, usize) where Encoding: unicode::Encode {
        match *self {
            Or(ref left, ref right) => {
                // build sub-FSMs
                let (linit, lf1nal) = left.to_automaton::<Encoding>(auto, defs);
                let (rinit, rf1nal) = right.to_automaton::<Encoding>(auto, defs);

                // create new f1nal and initial states
                let new_f1nal = auto.create_state();
                let new_init = auto.create_state();

                // new initial state e-transitions to old init states
                auto.states[new_init].etrans = Two(linit, rinit);

                // old f1nal states e-transition to new f1nal state
                auto.states[lf1nal].etrans = One(new_f1nal);
                auto.states[rf1nal].etrans = One(new_f1nal);

                (new_init, new_f1nal)
            }

            Cat(ref fst, ref snd) => {
                let (  _  , sf1nal) = snd.to_automaton::<Encoding>(auto, defs);

                // remove the initial state of the right part
                // this is possible at a cheap cost since the initial
                // state is always the last created
                let State {
                    etrans, trans, ..
                } = auto.states.pop().unwrap();

                let (finit, ff1nal) = fst.to_automaton::<Encoding>(auto, defs);
                auto.states[ff1nal].etrans = etrans;
                auto.states[ff1nal].trans = trans;

                (finit, sf1nal)
            }

            Maybe(ref reg) => {
                let (init, f1nal) = reg.to_automaton::<Encoding>(auto, defs);
                let new_f1nal = auto.create_state();
                let new_init = auto.create_state();

                auto.states[new_init].etrans = Two(new_f1nal, init);
                auto.states[f1nal].etrans = One(new_f1nal);

                (new_init, new_f1nal)
            }

            Closure(ref reg) => {
                let (init, f1nal) = reg.to_automaton::<Encoding>(auto, defs);
                let new_f1nal = auto.create_state();
                let new_init = auto.create_state();

                auto.states[new_init].etrans = Two(new_f1nal, init);
                auto.states[f1nal].etrans = Two(new_f1nal, init);

                (new_init, new_f1nal)
            }

            Literal(Class(ref set)) => {
                Encoding::class_to_automaton(set, auto)
            }

            Literal(NotClass(ref set)) => {
                Encoding::not_class_to_automaton(set, auto)
            }

            Var(idx) => {
                defs[idx].to_automaton::<Encoding>(auto, defs)
            }

            Literal(Char(ch)) => {
                Encoding::char_to_automaton(ch, auto)
            }

            Literal(Any) => {
                Encoding::any_to_automaton(auto)
            }

            Bind(_, ref expr) => {
                expr.to_automaton::<Encoding>(auto, defs)
            }
        }
    }

    #[allow(dead_code)]
    // prints the AST for debugging purposes
    pub fn show(&self, span: &str, defs: &[Regex]) {
        match self {
            &Or(ref l, ref r) => {
                println!("{} Or of: ", span);
                l.show(&format!("  {}", span), defs);
                r.show(&format!("  {}", span), defs);
            }

            &Cat(ref l, ref r) => {
                println!("{} Cat of: ", span);
                l.show(&format!("  {}", span), defs);
                r.show(&format!("  {}", span), defs);
            }

            &Maybe(ref reg) => {
                println!("{} Optionnally the regex:", span);
                reg.show(span, defs);
            }

            &Closure(ref reg) => {
                println!("{} The eclosure of", span);
                reg.show(&format!("  {}", span), defs)
            }

            &Var(idx) => {
                defs[idx].show(span, defs);
            }

            &Literal(Char(ref c)) => println!("{} The char {}", span, *c as char),
            &Literal(Any) => println!("Anything"),
            _ => ()
        }
    }
}
