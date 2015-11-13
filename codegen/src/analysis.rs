use fsa::dfa::Automaton;
use std::collections::HashSet;
use std::iter;
use regex::Action;

pub struct Analysis {
    pub unreachable: Vec<Action>,
    pub incomplete: Vec<usize>
}

struct State {
    st: usize,
    init: usize,
    complete: bool
}

pub fn check_automaton(auto: &Automaton<Action>, acts: usize) -> Analysis {
    let mut reachable: Vec<_> = iter::repeat(false).take(acts).collect();
    let mut incomplete: Vec<_> = iter::repeat(false).take(auto.initials.len()).collect();
    let mut stack = auto.initials.iter().enumerate().map(|(i, &s)| {
        State { st: s, init: i, complete: false }
    }).collect::<Vec<_>>();
    let mut marked = HashSet::new();

    while let Some(next) = stack.pop() {
        let Action(act) = auto.states[next.st].data;
        reachable[act] = true;

        let complete =
            if act != 0 { true }
            else { next.complete };

        for ch in 0 .. 256 {
            let dst = auto.states[next.st].trans[ch];
            incomplete[next.init] |= dst == 0 && !complete;

            if !marked.contains(&dst) {
                marked.insert(dst);
                stack.push(State { st: dst, init: next.init, complete: complete });
            }
        }
    }

    Analysis {
        unreachable: reachable
            .into_iter().enumerate()
            .filter_map(|x| if x.1 { None } else { Some(Action(x.0)) })
            .collect(),

        incomplete: incomplete
            .into_iter().enumerate()
            .filter_map(|x| if x.1 { Some(x.0) } else { None })
            .collect()
    }
}
