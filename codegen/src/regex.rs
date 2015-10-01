use std::ops::Range;
use std::option::IntoIter;
use nfa;
use nfa::{No, One, Two, More};
use util::svec;

pub use self::RegexNode::{Or, Cat, Maybe, Closure, Class, NotClass, Var, Char, Any, Bind};

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
}

pub type Regex = Box<RegexNode>;

#[derive(Clone)]
enum RegexNode {
    // binary operators
    Or(Regex, Regex),
    Cat(Regex, Regex),

    // unary operators
    Maybe(Regex),
    Closure(Regex),

    // constants
    Class(CharSet),
    NotClass(CharSet),
    Var(usize),
    Char(u8),
    Any,

    // bind
    Bind(::syntax::ast::Ident, Regex)
}

pub fn string(string: &str) -> Option<Regex> {
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
    trans: (svec::SVec, usize),

    // 0: no action. otherwise, it's
    // a f1nal state with an action
    action: usize
}

impl nfa::State for State {
    type Data = usize;
    type Iter = IntoIter<usize>;

    fn new() -> State {
        State {
            trans: (svec::Zero, 0),
            etrans: No,
            action: 0
        }
    }

    fn etransition<'a>(&'a self) -> &'a nfa::Etrans {
        &self.etrans
    }

    fn transition(&self, c: u8) -> IntoIter<usize> {
        let (ref set, dst) = self.trans;
        match *set {
            svec::Many(ref set) if set.contains(c) => Some(dst),
            svec::ManyBut(ref set) if !set.contains(c) => Some(dst),
            svec::One(ch) if ch == c => Some(dst),
            svec::Any => Some(dst),
            _ => None
        }.into_iter()
    }

    fn new_data() -> usize {
        0
    }

    fn data(&self) -> usize {
        self.action
    }

    fn combine_data(a: usize, b: usize) -> usize {
        if a >= b { a } else { b }
    }

    fn is_final(data: Self::Data) -> bool {
        data != 0
    }
}

pub type Automaton = nfa::Automaton<State>;

// creates a new Non-deterministic Finite Automaton using the
// McNaughton-Yamada-Thompson construction
// takes several regular expressions, each with an attached action
pub fn build_nfa(regexs: &[(Regex, usize)], defs: &[Regex]) -> Automaton {
    let mut ret = Automaton {
        states: Vec::new(),
        initial: 0usize
    };

    let ini = ret.create_state();
    let mut etrans = Vec::new();

    for &(ref reg, act) in regexs.iter() {
        let (init, f1nal) = reg.to_automaton(&mut ret, defs);
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
    fn to_automaton(&self, auto: &mut Automaton, defs: &[Regex]) -> (usize, usize) {
        match *self {
            Or(ref left, ref right) => {
                // build sub-FSMs
                let (linit, lf1nal) = left.to_automaton(auto, defs);
                let (rinit, rf1nal) = right.to_automaton(auto, defs);

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
                let (  _  , sf1nal) = snd.to_automaton(auto, defs);

                // remove the initial state of the right part
                // this is possible at a cheap cost since the initial
                // state is always the last created
                let State {
                    etrans, trans, ..
                } = auto.states.pop().unwrap();

                let (finit, ff1nal) = fst.to_automaton(auto, defs);
                auto.states[ff1nal].etrans = etrans;
                auto.states[ff1nal].trans = trans;

                (finit, sf1nal)
            }

            Maybe(ref reg) => {
                let (init, f1nal) = reg.to_automaton(auto, defs);
                let new_f1nal = auto.create_state();
                let new_init = auto.create_state();

                auto.states[new_init].etrans = Two(new_f1nal, init);
                auto.states[f1nal].etrans = One(new_f1nal);

                (new_init, new_f1nal)
            }

            Closure(ref reg) => {
                let (init, f1nal) = reg.to_automaton(auto, defs);
                let new_f1nal = auto.create_state();
                let new_init = auto.create_state();

                auto.states[new_init].etrans = Two(new_f1nal, init);
                auto.states[f1nal].etrans = Two(new_f1nal, init);

                (new_init, new_f1nal)
            }

            Class(ref vec) => {
                let f1nal = auto.create_state();
                let init = auto.create_state();
                auto.states[init].trans = (svec::Many(vec.clone()), f1nal);
                (init, f1nal)
            }

            NotClass(ref set) => {
                let f1nal = auto.create_state();
                let init = auto.create_state();
                auto.states[init].trans = (svec::ManyBut(set.clone()), f1nal);
                (init, f1nal)
            }

            Var(idx) => {
                defs[idx].to_automaton(auto, defs)
            }

            Char(ch) => {
                let f1nal = auto.create_state();
                let init = auto.create_state();
                auto.states[init].trans = (svec::One(ch), f1nal);
                (init, f1nal)
            }

            Any => {
                let f1nal = auto.create_state();
                let init = auto.create_state();
                auto.states[init].trans = (svec::Any, f1nal);
                (init, f1nal)
            }

            Bind(_, ref expr) => {
                expr.to_automaton(auto, defs)
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

            &Char(ref c) => println!("{} The char {}", span, *c as char),
            &Any => println!("Anything"),
            _ => ()
        }
    }
}
