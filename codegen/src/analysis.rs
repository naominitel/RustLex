use fsa::dfa::Automaton;
use std::collections::HashSet;
use std::iter;
use regex::Action;

pub fn find_unreachable_patterns(auto: &Automaton<Action>, acts: usize) -> Vec<Action> {
    let mut reachable: Vec<_> = iter::repeat(false).take(acts).collect();
    let mut stack = auto.initials.clone();
    let mut marked = HashSet::new();

    while let Some(next) = stack.pop() {
        let Action(act) = auto.states[next].data;
        reachable[act] = true;

        for ch in 0 .. 256 {
            let dst = auto.states[next].trans[ch];
            if !marked.contains(&dst) {
                marked.insert(dst);
                stack.push(dst);
            }
        }
    }

    reachable
        .into_iter().enumerate()
        .filter_map(|x| if x.1 { None } else { Some(Action(x.0)) })
        .collect()
}
