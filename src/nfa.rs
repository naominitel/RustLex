use regex;
use util::BinSet;
use util::svec;

pub use self::Etrans::{No, One, Two, More};

/* non-deterministic finite automaton */

// dirty optimisation
// states of the automaton built by the Thompson construction may have
// * A single e-trans, in the case of the f1nal states of an or
// * Two e-trans, in most cases
// * More, for the initial state on the NFA that may transition to the NFA
//   of each of the patterns
// To avoid systematically using an array, we use this structure:
enum Etrans {
    No,
    One(usize),
    Two(usize, usize),
    More(Vec<usize>)
}

struct State {
    // the McNaughton-Yamada-Thompson
    // construction algorithm will build
    // NFAs whose states have 0, 1 or
    // 2 e-transitions
    etrans: Etrans,

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

pub struct Automaton {
    pub states: Vec<State>,
    pub initial: usize
}

// creates a new Non-deterministic Finite Automaton using the
// McNaughton-Yamada-Thompson construction
// takes several regular expressions, each with an attached action
pub fn build_nfa(regexs: Vec<(Box<regex::Regex>, usize)>) -> Box<Automaton> {
    let mut ret = Box::new(Automaton {
        states: Vec::new(),
        initial: 0us
    });

    let ini = ret.create_state();
    let mut etrans = Vec::new();

    for (reg, act) in regexs.into_iter() {
        let (init, f1nal) = ret.init_from_regex(&*reg);
        etrans.push(init);
        ret.states[f1nal].action = act;
    }

    ret.states[ini].etrans = More(etrans);
    ret.initial = ini;
    ret
}

impl Automaton {
    #[inline(always)]
    #[allow(dead_code)]
    pub fn f1nal(&self, state: usize) -> bool {
        self.states[state].action != 0
    }

    // insert a new empty state and return its number
    #[inline(always)]
    fn create_state(&mut self) -> usize {
        self.states.push(State {
            trans: (svec::Zero, 0),
            etrans: No,
            action: 0
        });
        self.states.len() - 1
    }

    #[inline(always)]
    fn setnonf1nal(&mut self, state: usize) {
        self.states[state].action = 0;
    }

    // the construction is implemented recursively. Each call builds a
    // sub-expression of the regex, and returns the f1nals and initial states
    // only thos states will have to be modified so transitions numbers
    // won't have to be changed
    // the initial state is always the last state created, this way we can reuse
    // it in the concatenation case and avoid adding useless e-transitions
    fn init_from_regex(&mut self, reg: &regex::Regex) -> (usize, usize) {
        match reg {
            &regex::Or(ref left, ref right) => {
                // build sub-FSMs
                let (linit, lf1nal) = self.init_from_regex(&**left);
                let (rinit, rf1nal) = self.init_from_regex(&**right);
                self.setnonf1nal(lf1nal);
                self.setnonf1nal(rf1nal);

                // create new f1nal and initial states
                let new_f1nal = self.create_state();
                let new_init = self.create_state();

                // new initial state e-transitions to old init states
                self.states[new_init].etrans = Two(linit, rinit);

                // old f1nal states e-transition to new f1nal state
                self.states[lf1nal].etrans = One(new_f1nal);
                self.states[rf1nal].etrans = One(new_f1nal);

                (new_init, new_f1nal)
            }

            &regex::Cat(ref fst, ref snd) => {
                let (  _  , sf1nal) = self.init_from_regex(&**snd);

                // remove the initial state of the right part
                // this is possible at a cheap cost since the initial
                // state is always the last created
                let State {
                    etrans, trans, ..
                } = self.states.pop().unwrap();

                let (finit, ff1nal) = self.init_from_regex(&**fst);
                self.setnonf1nal(ff1nal);
                self.states[ff1nal].etrans = etrans;
                self.states[ff1nal].trans = trans;

                (finit, sf1nal)
            }

            &regex::Maybe(ref reg) => {
                let (init, f1nal) = self.init_from_regex(&**reg);
                let new_f1nal = self.create_state();
                let new_init = self.create_state();

                self.setnonf1nal(f1nal);
                self.states[new_init].etrans = Two(new_f1nal, init);
                self.states[f1nal].etrans = One(new_f1nal);

                (new_init, new_f1nal)
            }

            &regex::Closure(ref reg) => {
                let (init, f1nal) = self.init_from_regex(&**reg);
                let new_f1nal = self.create_state();
                let new_init = self.create_state();

                self.setnonf1nal(f1nal);
                self.states[new_init].etrans = Two(new_f1nal, init);
                self.states[f1nal].etrans = Two(new_f1nal, init);

                (new_init, new_f1nal)
            }

            &regex::Class(ref vec) => {
                let f1nal = self.create_state();
                let init = self.create_state();
                self.states[init].trans = (svec::Many(vec.clone()), f1nal);
                (init, f1nal)
            }

            &regex::NotClass(ref set) => {
                let f1nal = self.create_state();
                let init = self.create_state();
                self.states[init].trans = (svec::ManyBut(set.clone()), f1nal);
                (init, f1nal)
            }

            &regex::Var(ref reg) => {
                self.init_from_regex(&**reg)
            }

            &regex::Char(ch) => {
                let f1nal = self.create_state();
                let init = self.create_state();
                self.states[init].trans = (svec::One(ch), f1nal);
                (init, f1nal)
            }

            &regex::Any => {
                let f1nal = self.create_state();
                let init = self.create_state();
                self.states[init].trans = (svec::Any, f1nal);
                (init, f1nal)
            }
        }
    }

    pub fn moves(&self, st: &BinSet) -> Vec<Vec<usize>> {
        let mut ret = Vec::new();
        for _ in range(0, 256us) {
            ret.push(vec!());
        }

        for s in st.iter() {
            match self.states[*s].trans {
                (svec::Many(ref v), dst) =>
                    for &ch in v.states.iter() {
                        ret[ch as usize].push(dst)
                    },
                (svec::ManyBut(ref set), dst) => {
                    let data = &set.data;
                    let mut chk = data[0];
                    let mut i = 0;

                    for ch in range(0, 256us) {
                        if (ch & 0x3F) == 0 {
                            chk = data[i];
                            i += 1;
                        }

                        if (chk & 1) == 0 {
                            ret[ch].push(dst);
                        }

                        chk = chk >> 1;
                    }
                }
                (svec::Any, dst) =>
                    for ch in range(0, 256us) {
                        ret[ch].push(dst);
                    },
                (svec::One(ch), dst) => ret[ch as usize].push(dst),
                (svec::Zero, _) => ()
            }
        }

        ret
    }

    #[inline(always)]
    pub fn eclosure_(&self, st: usize) -> Box<BinSet> {
        self.eclosure(&[st])
    }

    pub fn eclosure(&self, st: &[usize]) -> Box<BinSet> {
        let mut ret = Box::new(BinSet::new(self.states.len()));
        let mut stack = Vec::with_capacity(st.len());

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
        writeln!(out, "digraph automata {{");
        writeln!(out, "\trankdir = LR;");
        writeln!(out, "\tsize = \"4,4\";");
        writeln!(out, "\tnode [shape=box]; {};", self.initial);
        writeln!(out, "\tnode [shape=doublecircle];");
        write!(out, "\t");

        // outputs f1nal states as doublecircle-shaped nodes
        for st in range(0, self.states.len()) {
            if self.states[st].action != 0 {
                write!(out, "{} ", st);
            }
        }

        writeln!(out, ";\n");
        writeln!(out, "\tnode [shape=circle];");

        for st in range(0, self.states.len()) {
            match self.states[st].trans {
                (svec::One(ch), dst) => {
                    let mut esc = String::new();
                    esc.extend((ch as char).escape_default());
                    writeln!(out, "\t{} -> {} [label=\"{}\"];",
                        st, dst, esc);
                }

                (svec::Many(ref vec), dst) => {
                    for &ch in vec.states.iter() {
                        let mut esc = String::new();
                        esc.extend((ch as char).escape_default());
                        writeln!(out, "\t{} -> {} [label=\"{}\"];",
                            st, dst, esc);
                    }
                }

                (svec::ManyBut(ref vec), dst) => {
                    for &ch in vec.states.iter() {
                        let mut esc = String::new();
                        esc.extend((ch as char).escape_default());
                        writeln!(out, "\t{} -> {} [label=\"!{}\"];",
                            st, dst, esc);
                    }
                }

                (svec::Any, dst) => {
                    writeln!(out, "\t{} -> {} [label=\".\"];",
                        st, dst);
                }

                _ => ()
            }


            match self.states[st].etrans {
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
