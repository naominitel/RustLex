use std::iter;
use nfa;
use nfa::StateData;
use std::collections::HashMap;
use std::fmt::Display;
use std::hash::Hash;
use std::io::Write;
use bit_set::BitSet;

/* deterministic finite automaton */

pub struct State<T> {
    // this is not strictly speaking
    // a DFA since there may be no
    // transitions for a given input
    pub trans: [usize; 256],

    // for DFA determinization
    // remember the set of NFA states
    // that this state corresponds to
    states: BitSet,
    pub data: T
}

pub struct Automaton<T> {
    pub states: Vec<State<T>>,
    pub initials: Vec<usize>
}

impl<T> Automaton<T> where T: nfa::StateData {
    // determinize a nondeterministic finite automaton and "adds" it to this
    // deterministic automaton. adding it means that the newly built DFA will
    // use the next state number available in this DFA but there will be no
    // transition between the differents DFA.
    // The resulting DFA is thus not strictly a DFA but this is needed to
    // implement "conditions" in the lexical analysers
    // returns the index of the initial state of the created DFA in the
    // `initials' vector
    pub fn determinize<S: nfa::State<Data = T>>(&mut self, nfa: &nfa::Automaton<S>)
        -> usize {
        let (eclos, data) = nfa.eclosure_(nfa.initial);
        let ini = self.create_state(data, Some(eclos));
        let mut unmarked = vec!(ini);

        while !unmarked.is_empty() {
            let next = unmarked.pop().unwrap();

            for ch in 0 .. 256 {
                let moves = nfa.moves(&self.states[next].states, ch as u8);
                let (clos, action) = nfa.eclosure(&moves);
                if clos.is_empty() {
                    continue;
                }

                // do we have clos in dstates ?
                let mut i = ini;
                let mut dst = None;
                for s in self.states.iter().skip(ini) {
                    if s.states == clos {
                        dst = Some(i);
                        break;
                    }
                    i += 1;
                }

                match dst {
                    // in any case, add a transition
                    Some(i) => self.states[next].trans[ch] = i,
                    None => {
                        // create a new DFA state for this set
                        let st = self.create_state(action, Some(clos));
                        self.states[next].trans[ch] = st;
                        unmarked.push(st);
                    }
                }
            }
        }

        let ret = self.initials.len();
        self.initials.push(ini);
        ret
    }

    pub fn new() -> Automaton<T> {
        let mut ret = Automaton {
            states: vec!(),
            initials: vec!()
        };

        // create a dead state
        ret.create_state(T::no_data(), None);
        ret
    }

    #[inline(always)]
    fn create_state(&mut self, act: T, states: Option<BitSet>) -> usize {
        self.states.push(State {
            trans: [0; 256],
            states: match states {
                Some(s) => s,
                None => BitSet::new()
            },
            data: act
        });

        self.states.len() - 1
    }

    // construct an equivalent DFA whose number of state is minimal for the
    // recognized input langage
    pub fn minimize(&self) -> Automaton<T> where T: Clone + Eq + Hash {
        // groups are stored as an array indexed by a state number
        // giving a group number.
        let mut groups = Vec::with_capacity(self.states.len());

        let mut group_of_action = HashMap::new();
        let mut next_group = 0;

        // create one subgroup per action
        for st in self.states.iter() {
            groups.push(*group_of_action.entry(&st.data).or_insert_with(|| {
                let ret = next_group;
                next_group += 1;
                ret
            }));
        }

        // now iterate over the states and split the groups into
        // subgroups. This records the subgroups we have created for a
        // given group. It is indexed by a group number and give a list
        // of subgroups of the form (gr, st) where gr is the number of the
        // subgroup (it may be the same as the original group), and st the
        // number of a representing state
        let mut subgroups: Vec<Vec<(usize, usize)>> =
            iter::repeat(vec!()).take(next_group).collect();
        loop {
            // subgroups become groups, reinitialize subgroups
            for i in subgroups.iter_mut() {
                *i = vec!();
            }

            // create a new partition
            let mut new_groups = Vec::with_capacity(self.states.len());
            let mut modified = false;

            'g: for s in 0 .. groups.len() {
                let group = groups[s];

                // check if we have a subgroup of the group of s
                // that matches its transitions. st is the representing
                // state of the subgroup subgr
                'h: for &(subgr, st) in subgroups[group].iter() {
                    // if st and s are similar, s goes to subgr
                    // 2 states are said similar if for each input
                    // symbol they have a transition to states that
                    // are in the same group of the current partition
                    for i in 0 .. 255usize {
                        let (s1, s2) = (
                            self.states[st].trans[i],
                            self.states[s].trans[i]
                        );
                        if groups[s1] != groups[s2] {
                            continue 'h;
                        }
                    }

                    // okay, we have found a subgroup for s
                    // it may as well be the same so we may not have
                    // modified the partition here
                    new_groups.push(subgr);
                    continue 'g;
                }

                // no subgroup, create one
                // if there is no subgroup for this group, reuse the
                // same index
                if subgroups[group].is_empty() {
                    subgroups[group].push((group, s));
                    new_groups.push(group);
                } else {
                    // create a new subgroup with a new index
                    // take this state as a representing state
                    let subgroup = subgroups.len();
                    subgroups.push(vec!());
                    subgroups[group].push((subgroup, s));
                    new_groups.push(subgroup);
                    modified = true;
                }
            }

            groups = new_groups;

            // we stop when the partition is the same as before,
            // i.e. when we cannot create new subgroups anymore.
            if !modified {
                break;
            }
        }

        // construct the minimal DFA
        let mut ret = Automaton {
            states: Vec::with_capacity(subgroups.len()),
            initials: Vec::with_capacity(self.initials.len()),
        };

        // create the dead state
        // FIXME: is this really necessary ? it works
        // but in fine it looks like we end up with two
        // dead states. one should check if the dead state
        // of the initial automata is always preserved and
        // keep it instead of creating a new one
        ret.create_state(T::no_data(), None);

        // build representing states
        // now that we are here
        // - groups contains the final partition and lets us find the group
        //   in which a state of the initial DFA will be
        // - subgroups contains only one subgroup for each group because we
        //   didn't created new subgroups at the last iteration, so this will
        //   allow us to find representing states for each groups
        // the number of a state of the new DFA will be the number of the
        // group of which it is a representing state
        for gr in subgroups.iter() {
            let (_, st) = gr[0];
            let st = &self.states[st];
            let state = ret.create_state(st.data.clone(), None);
            let state = &mut ret.states[state];

            // adjust transitions
            // the new state transitions to the representing state of the group
            // that contains the state to which is previously transitionned
            let mut ch = 0usize;
            for t in st.trans.iter() {
                match *t {
                    0 => state.trans[ch] = 0,
                    _ => state.trans[ch] = groups[*t] + 1
                }
                ch += 1
            };
        }

        // update the initial state numbers of each condition
        for c in self.initials.iter() {
            ret.initials.push(groups[*c] + 1);
        }

        ret
    }

    #[allow(dead_code)]
    #[allow(unused_must_use)]
    // outs the automaton as a dot file for graphviz
    // for debugging purposes
    pub fn todot(&self, out: &mut Write) where T: Display + Eq {
        writeln!(out, "digraph automata {{");
        writeln!(out, "\trankdir = LR;");
        writeln!(out, "\tsize = \"4,4\";");

        for i in self.initials.iter() {
            writeln!(out, "\tnode [shape=box]; {};", i);
        }

        let mut i = 0usize;

        // outputs final states as doublecircle-shaped nodes
        for st in self.states.iter() {
            if st.data != T::no_data() {
                writeln!(out, "\tnode [shape=doublecircle, label=\"{} ({})\"] {};",
                    i, st.data, i);
            }

            i += 1;
        }

        writeln!(out, "\tnode [shape=circle];");

        let mut i = 0usize;
        for st in self.states.iter() {
            for ch in 0 .. 256usize {
                match st.trans[ch] {
                    0 => (),
                    dst => {
                        let mut esc = String::new();
                        esc.extend((ch as u8 as char).escape_default());
                        writeln!(out, "\t{} -> {} [label=\"{}\"];",
                            i, dst, esc);
                    }
                }
            }

            i += 1;
        }

        writeln!(out, "}}");
    }
}
