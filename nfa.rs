use regex::{Or, Cat, Char, Closure, Maybe, Var};
use regex::Regex;
use std::vec;

/* non-deterministic finite automaton */

// dirty optimisation
// states of the automaton built by the Thompson construction may have
// * A single e-trans, in the case of the final states of an or
// * Two e-trans, in most cases
// * More, for the initial state on the NFA that may transition to the NFA
//   of each of the patterns
// To avoid systematically using an array, we use this structure:
enum Etrans {
    No(),
    One(uint),
    Two(uint, uint),
    More(~[uint])
}

struct State {
    // the McNaughton-Yamada-Thompson
    // construction algorithm will build
    // NFAs whose states have a single
    // transition (or none), and 0, 1 or
    // 2 e-transitions
    trans: Option<(u8, uint)>,
    etrans: Etrans,

    // 0: no action. otherwise, it's
    // a final state with an action
    action: uint
}

pub struct Automaton {
    states: ~[State],
    initial: uint
}

// efficient data structure for representing a set of states in an NFA
// * data is a binary bit array that uses one bit per state in the NFA
//   that may be either 0 or 1 (for respectively not included or included in
//   the set). It allows for fast lookup/insertion. It is represented as an
//   array of (N/64)+1 64 bit integers
// * states is a redundant array that stores directly the numbers of the
//   states included. It allows for fast iteration over the contents of the
//   states
// * action is set whenever a final set is inserted into the set.
//   If several final states are inserted, the higher action is taken. It
//   should correspond to the first action defined in the lexer file.
pub struct StateSet {
    priv data: ~[u64],
    priv states: ~[uint],
    priv action: uint
}

impl StateSet {
    // checks or sets the presence of a given state in the set
    // bits 0-5 of the state num index the state in the chunk
    // higher bits give the index of the chunk in the data array

    #[inline(always)]
    fn contains(&self, state: uint) -> bool {
        let chunk = state >> 6;
        let idx = state & 0x3F;
        ((self.data[chunk] >> idx) & 1) != 0
    } 

    #[inline(always)]
    fn insert(&mut self, state: uint) {
        let chunk = state >> 6;
        let idx = state & 0x3F;
        self.data[chunk] = self.data[chunk] | (1 << idx);
        self.states.push(state);
    }

    #[inline(always)]
    pub fn new(state_count: uint) -> StateSet {
        // (state_count / 64) + 1
        let chunks = (state_count >> 6) + 1;
        StateSet {
            data: vec::from_elem(chunks, 0u64),
            states: vec::with_capacity(state_count),
            action: 0
        }
    }

    #[inline(always)]
    pub fn iter<'a>(&'a self) -> vec::Items<'a, uint> {
        self.states.iter()
    }

    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.states.is_empty()
    }

    #[inline(always)]
    pub fn len(&self) -> uint {
        self.states.len()
    }

    #[inline(always)]
    pub fn action(&self) -> uint {
        self.action
    }
}

impl Eq for StateSet {
    fn eq(&self, other: &StateSet) -> bool {
        // assumes both vectors have the same length
        let len = self.data.len();
        let mut i = 0;
        while i < len {
            if self.data[i] != other.data[i] {
                return false;
            }
            i += 1;
        }
        true
    }
}

// creates a new Non-deterministic Finite Automaton using the
// McNaughton-Yamada-Thompson construction
// takes several regular expressions, each with an attached action
pub fn build_nfa(regexs: ~[(~Regex, uint)]) -> ~Automaton {
    let mut ret = ~Automaton {
        states: ~[],
        initial: 0
    };

    let ini = ret.create_state();
    let mut etrans = ~[];

    for (reg, act) in regexs.move_iter() {
        let (init, final) = ret.init_from_regex(reg);
        etrans.push(init);
        ret.states[final].action = act;
    }

    ret.states[ini].etrans = More(etrans);
    ret.initial = ini;
    ret
}

impl Automaton {
    #[inline(always)]
    pub fn final(&self, state: uint) -> bool {
        self.states[state].action != 0
    }

    // insert a new empty state and return its number
    #[inline(always)]
    fn create_state(&mut self) -> uint {
        self.states.push(State {
            trans: None,
            etrans: No,
            action: 0
        });
        self.states.len() - 1
    }

    #[inline(always)]
    fn setnonfinal(&mut self, state: uint) {
        self.states[state].action = 0;
    }

    // the construction is implemented recursively. Each call builds a
    // sub-expression of the regex, and returns the finals and initial states
    // only thos states will have to be modified so transitions numbers
    // won't have to be changed
    // the initial state is always the last state created, this way we can reuse
    // it in the concatenation case and avoid adding useless e-transitions
    fn init_from_regex(&mut self, reg: &Regex) -> (uint, uint) {
        match reg {
            &Or(ref left, ref right) => {
                // build sub-FSMs
                let (linit, lfinal) = self.init_from_regex(&**left);
                let (rinit, rfinal) = self.init_from_regex(&**right);
                self.setnonfinal(lfinal);
                self.setnonfinal(rfinal);

                // create new final and initial states
                let new_final = self.create_state();
                let new_init = self.create_state();

                // new initial state e-transitions to old init states
                self.states[new_init].etrans = Two(linit, rinit);

                // old final states e-transition to new final state
                self.states[lfinal].etrans = One(new_final);
                self.states[rfinal].etrans = One(new_final);

                (new_init, new_final)
            }

            &Cat(ref fst, ref snd) => {
                let (  _  , sfinal) = self.init_from_regex(&**snd);

                // remove the initial state of the right part
                // this is possible at a cheap cost since the initial
                // state is always the last created
                let State {
                    etrans: etrans, trans: trans, ..
                } = self.states.pop().unwrap();

                let (finit, ffinal) = self.init_from_regex(&**fst);
                self.setnonfinal(ffinal);
                self.states[ffinal].etrans = etrans;
                self.states[ffinal].trans = trans;

                (finit, sfinal)
            }

            &Closure(ref reg) => {
                let (init, final) = self.init_from_regex(&**reg);
                let new_final = self.create_state();
                let new_init = self.create_state();

                self.setnonfinal(final);
                self.states[new_init].etrans = Two(new_final, init);
                self.states[final].etrans = Two(new_final, init);

                (new_init, new_final)
            }

            &Maybe(ref reg) => {
                let (init, final) = self.init_from_regex(&**reg);
                let new_final = self.create_state();
                let new_init = self.create_state();

                self.setnonfinal(final);
                self.states[new_init].etrans = Two(new_final, init);
                self.states[final].etrans = One(new_final);

                (new_init, new_final)
            }

            &Char(ch) => {
                let final = self.create_state();
                let init = self.create_state();
                self.states[init].trans = Some((ch, final));
                (init, final)
            }

            &Var(ref reg) => {
                self.init_from_regex(&**reg)
            }
        }
    }

    pub fn moves(&self, st: &StateSet) -> ~[(u8, ~[uint])] {
        let mut indexes: ~[Option<uint>] = vec::from_elem(256, None);
        let mut ret: ~[(u8, ~[uint])] = ~[];

        for s in st.iter() {
            match self.states[*s].trans {
                Some((ch, dst)) => {
                    match indexes[ch] {
                        Some(i) => ret[i].mut1().push(dst),
                        None => {
                            ret.push((ch, ~[dst]));
                            indexes[ch] = Some(ret.len() - 1);
                        }
                    }
                }
                None => ()
            }
        }

        ret
    }

    #[inline(always)]
    pub fn eclosure_(&self, st: uint) -> ~StateSet {
        self.eclosure(&[st])
    }

    pub fn eclosure(&self, st: &[uint]) -> ~StateSet {
        let mut ret = ~StateSet::new(self.states.len());
        let mut stack = vec::with_capacity(st.len());

        for s in st.iter() {
            stack.push(*s);
            ret.insert(*s);

            let action = self.states[*s].action;
            if action > ret.action {
                ret.action = action;
            }
        }

        while !stack.is_empty() {
            let st = stack.pop().unwrap();
            let st = &self.states[st];

            match st.etrans {
                One(i) if !ret.contains(i) => {
                    ret.insert(i);
                    stack.push(i);

                    let action = self.states[i].action;
                    if action > ret.action {
                        ret.action = action;
                    }
                }

                Two(i, j) => {
                    if !ret.contains(i) {
                        ret.insert(i);
                        stack.push(i);

                        let action = self.states[i].action;
                        if action > ret.action {
                            ret.action = action;
                        }
                    }

                    if !ret.contains(j) {
                        ret.insert(j);
                        stack.push(j);

                        let action = self.states[j].action;
                        if action > ret.action {
                            ret.action = action;
                        }
                    }
                }

                More(ref v) => {
                    for i in v.iter() {
                        if !ret.contains(*i) {
                            ret.insert(*i);
                            stack.push(*i);

                            let action = self.states[*i].action;
                            if action > ret.action {
                                ret.action = action;
                            }
                        }
                    }
                }

                _ => ()
            }
        }

        ret
    }

    #[allow(dead_code)]
    #[allow(unused_must_use)]
    // outs the automaton as a dot file for graphviz
    // for debugging purposes
    pub fn todot(&self, out: &mut Writer) {
        writeln!(out, "digraph automata \\{");
        writeln!(out, "\trankdir = LR;");
        writeln!(out, "\tsize = \"4,4\";");
        writeln!(out, "\tnode [shape=box]; {:u};", self.initial);
        writeln!(out, "\tnode [shape=doublecircle];");
        write!(out, "\t");

        // outputs final states as doublecircle-shaped nodes
        for st in range(0, self.states.len()) {
            if self.states[st].action != 0 {
                write!(out, "{:u} ", st);
            }
        }

        writeln!(out, ";\n");
        writeln!(out, "\tnode [shape=circle];");

        for st in range(0, self.states.len()) {
            match self.states[st].trans {
                Some((ch, dst)) => {
                    writeln!(out, "\t{:u} -> {:u} [label=\"{:c}\"];",
                        st, dst, ch as char);
                }

                _ => ()
            }

            
            match self.states[st].etrans {
                One(s) => {
                    writeln!(out, "\t{:u} -> {:u} [label=\"e\"];", st, s);
                }
                Two(s, t) => {
                    writeln!(out, "\t{:u} -> {:u} [label=\"e\"];", st, s);
                    writeln!(out, "\t{:u} -> {:u} [label=\"e\"];", st, t);
                }
                More(ref v) => {
                    for i in v.iter() {
                        writeln!(out, "\t{:u} -> {:u} [label=\"e\"];", st, *i);
                    }
                }
                _ => ()
            }
        }

        writeln!(out, "\\}");
    }
}
