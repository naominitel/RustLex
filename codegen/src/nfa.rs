use bit_set::BitSet;
use std::fmt::Display;
use std::io::Write;

pub use self::Etrans::{No, One, Two, More};

/* non-deterministic finite automaton */

// dirty optimisation
// states of the automaton built by the Thompson construction may have
// * A single e-trans, in the case of the f1nal states of an or
// * Two e-trans, in most cases
// * More, for the initial state on the NFA that may transition to the NFA
//   of each of the patterns
// To avoid systematically using an array, we use this structure:
pub enum Etrans {
    No,
    One(usize),
    Two(usize, usize),
    More(Vec<usize>)
}

pub trait State {
    type Data;
    type Iter: Iterator<Item = usize>;

    fn new() -> Self;
    fn etransition<'a>(&'a self) -> &'a Etrans;
    fn transition(&self, c: u8) -> Self::Iter;

    fn new_data() -> Self::Data;
    fn data(&self) -> Self::Data;
    fn combine_data(a: Self::Data, b: Self::Data) -> Self::Data;
    fn is_final(data: Self::Data) -> bool;
}

pub struct Automaton<T> where T: State {
    pub states: Vec<T>,
    pub initial: usize
}

impl<T: State> Automaton<T> {
    // insert a new empty state and return its number
    pub fn create_state(&mut self) -> usize {
        self.states.push(T::new());
        self.states.len() - 1
    }

    pub fn moves(&self, st: &BitSet, c: u8) -> Vec<usize> {
        let mut ret = Vec::with_capacity(st.len());

        for s in st.iter() {
            for dst in self.states[s].transition(c) {
                ret.push(dst);
            }
        }

        ret
    }

    #[inline(always)]
    pub fn eclosure_(&self, st: usize) -> (BitSet, T::Data) {
        self.eclosure(&[st])
    }

    pub fn eclosure(&self, st: &[usize]) -> (BitSet, T::Data) {
        let mut ret = BitSet::with_capacity(self.states.len());
        let mut ret_action = T::new_data();
        let mut stack = Vec::with_capacity(st.len());

        macro_rules! add {
            ($state: expr) => {
                if !ret.contains(&$state) {
                    ret.insert($state);
                    stack.push($state);

                    ret_action = T::combine_data(
                        self.states[$state].data(),
                        ret_action
                    );
                }
            }
        }

        for &s in st.iter() {
            add!(s);
        }

        while !stack.is_empty() {
            let st = stack.pop().unwrap();
            let st = &self.states[st];

            match *st.etransition() {
                No => (),
                One(i) => add!(i),
                Two(i, j) => { add!(i) ; add!(j) }
                More(ref v) => {
                    for &i in v.iter() {
                        add!(i);
                    }
                }
            }
        }

        (ret, ret_action)
    }

    #[allow(dead_code)]
    #[allow(unused_must_use)]
    // outs the automaton as a dot file for graphviz
    // for debugging purposes
    pub fn todot(&self, out: &mut Write) where T::Data: Display + Eq {
        writeln!(out, "digraph automata {{");
        writeln!(out, "\trankdir = LR;");
        writeln!(out, "\tsize = \"4,4\";");
        writeln!(out, "\tnode [shape=box]; {};", self.initial);
        writeln!(out, "\tnode [shape=doublecircle];");
        write!(out, "\t");

        // outputs f1nal states as doublecircle-shaped nodes
        for st in (0 .. self.states.len()) {
            if self.states[st].data() != T::new_data() {
                write!(out, "{} ", st);
            }
        }

        writeln!(out, ";\n");
        writeln!(out, "\tnode [shape=circle];");

        for st in (0 .. self.states.len()) {
            for ch in 0 .. 256 {
                for dst in self.states[st].transition(ch as u8) {
                    let mut esc = String::new();
                    esc.extend((ch as u8 as char).escape_default());
                    writeln!(out, "\t{} -> {} [label=\"{}\"];", st, dst, esc);
                }
            }

            match *self.states[st].etransition() {
                One(s) => {
                    writeln!(out, "\t{} -> {} [label=\"e\"];", st, s);
                }
                Two(s, t) => {
                    writeln!(out, "\t{} -> {} [label=\"e\"];", st, s);
                    writeln!(out, "\t{} -> {} [label=\"e\"];", st, t);
                }
                More(ref v) => {
                    for i in v.iter() {
                        writeln!(out, "\t{} -> {} [label=\"e\"];", st, *i);
                    }
                }
                _ => ()
            }
        }

        writeln!(out, "}}");
    }
}
