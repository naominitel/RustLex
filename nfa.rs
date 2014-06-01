use regex;
use util::BinSet;
use util::svec;

/* non-deterministic finite automaton */

// dirty optimisation
// states of the automaton built by the Thompson construction may have
// * A single e-trans, in the case of the final states of an or
// * Two e-trans, in most cases
// * More, for the initial state on the NFA that may transition to the NFA
//   of each of the patterns
// To avoid systematically using an array, we use this structure:
enum Etrans {
    No,
    One(uint),
    Two(uint, uint),
    More(Vec<uint>)
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
    trans: (svec::SVec, uint),

    // 0: no action. otherwise, it's
    // a final state with an action
    action: uint
}

pub struct Automaton {
    pub states: Vec<State>,
    pub initial: uint
}

// creates a new Non-deterministic Finite Automaton using the
// McNaughton-Yamada-Thompson construction
// takes several regular expressions, each with an attached action
pub fn build_nfa(regexs: Vec<(Box<regex::Regex>, uint)>) -> Box<Automaton> {
    let mut ret = box Automaton {
        states: Vec::new(),
        initial: 0
    };

    let ini = ret.create_state();
    let mut etrans = Vec::new();

    for (reg, act) in regexs.move_iter() {
        let (init, final) = ret.init_from_regex(reg);
        etrans.push(init);
        ret.states.get_mut(final).action = act;
    }

    ret.states.get_mut(ini).etrans = More(etrans);
    ret.initial = ini;
    ret
}

impl Automaton {
    #[inline(always)]
    pub fn final(&self, state: uint) -> bool {
        self.states.get(state).action != 0
    }

    // insert a new empty state and return its number
    #[inline(always)]
    fn create_state(&mut self) -> uint {
        self.states.push(State {
            trans: (svec::Zero, 0),
            etrans: No,
            action: 0
        });
        self.states.len() - 1
    }

    #[inline(always)]
    fn setnonfinal(&mut self, state: uint) {
        self.states.get_mut(state).action = 0;
    }

    // the construction is implemented recursively. Each call builds a
    // sub-expression of the regex, and returns the finals and initial states
    // only thos states will have to be modified so transitions numbers
    // won't have to be changed
    // the initial state is always the last state created, this way we can reuse
    // it in the concatenation case and avoid adding useless e-transitions
    fn init_from_regex(&mut self, reg: &regex::Regex) -> (uint, uint) {
        match reg {
            &regex::Or(ref left, ref right) => {
                // build sub-FSMs
                let (linit, lfinal) = self.init_from_regex(&**left);
                let (rinit, rfinal) = self.init_from_regex(&**right);
                self.setnonfinal(lfinal);
                self.setnonfinal(rfinal);

                // create new final and initial states
                let new_final = self.create_state();
                let new_init = self.create_state();

                // new initial state e-transitions to old init states
                self.states.get_mut(new_init).etrans = Two(linit, rinit);

                // old final states e-transition to new final state
                self.states.get_mut(lfinal).etrans = One(new_final);
                self.states.get_mut(rfinal).etrans = One(new_final);

                (new_init, new_final)
            }

            &regex::Cat(ref fst, ref snd) => {
                let (  _  , sfinal) = self.init_from_regex(&**snd);

                // remove the initial state of the right part
                // this is possible at a cheap cost since the initial
                // state is always the last created
                let State {
                    etrans: etrans, trans: trans, ..
                } = self.states.pop().unwrap();

                let (finit, ffinal) = self.init_from_regex(&**fst);
                self.setnonfinal(ffinal);
                self.states.get_mut(ffinal).etrans = etrans;
                self.states.get_mut(ffinal).trans = trans;

                (finit, sfinal)
            }

            &regex::Maybe(ref reg) => {
                let (init, final) = self.init_from_regex(&**reg);
                let new_final = self.create_state();
                let new_init = self.create_state();

                self.setnonfinal(final);
                self.states.get_mut(new_init).etrans = Two(new_final, init);
                self.states.get_mut(final).etrans = One(new_final);

                (new_init, new_final)
            }

            &regex::Closure(ref reg) => {
                let (init, final) = self.init_from_regex(&**reg);
                let new_final = self.create_state();
                let new_init = self.create_state();

                self.setnonfinal(final);
                self.states.get_mut(new_init).etrans = Two(new_final, init);
                self.states.get_mut(final).etrans = Two(new_final, init);

                (new_init, new_final)
            }

            &regex::Class(ref vec) => {
                let final = self.create_state();
                let init = self.create_state();
                self.states.get_mut(init).trans = (svec::Many(vec.clone()), final);
                (init, final)
            }

            &regex::NotClass(ref set) => {
                let final = self.create_state();
                let init = self.create_state();
                self.states.get_mut(init).trans = (svec::ManyBut(set.clone()), final);
                (init, final)
            }

            &regex::Var(ref reg) => {
                self.init_from_regex(&**reg)
            }

            &regex::Char(ch) => {
                let final = self.create_state();
                let init = self.create_state();
                self.states.get_mut(init).trans = (svec::One(ch), final);
                (init, final)
            }

            &regex::Any => {
                let final = self.create_state();
                let init = self.create_state();
                self.states.get_mut(init).trans = (svec::Any, final);
                (init, final)
            }
        }
    }

    pub fn moves(&self, st: &BinSet) -> Vec<Vec<uint>> {
        let mut ret = Vec::new();
        for _ in range(0, 256u) {
            ret.push(vec!());
        }

        for s in st.iter() {
            match self.states.get(*s).trans {
                (svec::Many(ref v), dst) =>
                    for &ch in v.states.iter() {
                        ret.get_mut(ch as uint).push(dst)
                    },
                (svec::ManyBut(ref set), dst) => {
                    let data = &set.data;
                    let mut chk = *data.get(0);
                    let mut i = 0;

                    for ch in range(0, 256u) {
                        if (ch & 0x3F) == 0 {
                            chk = *data.get(i);
                            i += 1;
                        }

                        if (chk & 1) == 0 {
                            ret.get_mut(ch).push(dst);
                        }

                        chk = chk >> 1;
                    }
                }
                (svec::Any, dst) =>
                    for ch in range(0, 256u) {
                        ret.get_mut(ch).push(dst);
                    },
                (svec::One(ch), dst) => ret.get_mut(ch as uint).push(dst),
                (svec::Zero, _) => ()
            }
        }

        ret
    }

    #[inline(always)]
    pub fn eclosure_(&self, st: uint) -> Box<BinSet> {
        self.eclosure(&[st])
    }

    pub fn eclosure(&self, st: &[uint]) -> Box<BinSet> {
        let mut ret = box BinSet::new(self.states.len());
        let mut stack = Vec::with_capacity(st.len());

        for s in st.iter() {
            stack.push(*s);
            ret.insert(*s);

            let action = self.states.get(*s).action;
            if action > ret.action {
                ret.action = action;
            }
        }

        while !stack.is_empty() {
            let st = stack.pop().unwrap();
            let st = &self.states.get(st);

            match st.etrans {
                One(i) if !ret.contains(i) => {
                    ret.insert(i);
                    stack.push(i);

                    let action = self.states.get(i).action;
                    if action > ret.action {
                        ret.action = action;
                    }
                }

                Two(i, j) => {
                    if !ret.contains(i) {
                        ret.insert(i);
                        stack.push(i);

                        let action = self.states.get(i).action;
                        if action > ret.action {
                            ret.action = action;
                        }
                    }

                    if !ret.contains(j) {
                        ret.insert(j);
                        stack.push(j);

                        let action = self.states.get(j).action;
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

                            let action = self.states.get(*i).action;
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
            if self.states.get(st).action != 0 {
                write!(out, "{:u} ", st);
            }
        }

        writeln!(out, ";\n");
        writeln!(out, "\tnode [shape=circle];");

        for st in range(0, self.states.len()) {
            match self.states.get(st).trans {
                (svec::One(ch), dst) => {
                    let mut esc = String::new();
                    (ch as char).escape_default(|c| { esc.push_char(c); });
                    writeln!(out, "\t{:u} -> {:u} [label=\"{:s}\"];",
                        st, dst, esc);
                }

                (svec::Many(ref vec), dst) => {
                    for &ch in vec.states.iter() {
                        let mut esc = String::new();
                        (ch as char).escape_default(|c| { esc.push_char(c); });
                        writeln!(out, "\t{:u} -> {:u} [label=\"{:s}\"];",
                            st, dst, esc);
                    }
                }

                (svec::ManyBut(ref vec), dst) => {
                    for &ch in vec.states.iter() {
                        let mut esc = String::new();
                        (ch as char).escape_default(|c| { esc.push_char(c); });
                        writeln!(out, "\t{:u} -> {:u} [label=\"!{:s}\"];",
                            st, dst, esc);
                    }
                }

                (svec::Any, dst) => {
                    writeln!(out, "\t{:u} -> {:u} [label=\".\"];",
                        st, dst);
                }

                _ => ()
            }

            
            match self.states.get(st).etrans {
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
