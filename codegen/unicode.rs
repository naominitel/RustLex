use regex::{Automaton, CharSet, Label};

pub trait Encode {
    fn char_to_automaton(ch: char, auto: &mut Automaton) -> (usize, usize);
    fn class_to_automaton(ch: &CharSet<char>, auto: &mut Automaton) -> (usize, usize);
    fn not_class_to_automaton(ch: &CharSet<char>, auto: &mut Automaton) -> (usize, usize);
    fn any_to_automaton(auto: &mut Automaton) -> (usize, usize);
}

pub struct Ascii;

impl Encode for Ascii {
    fn char_to_automaton(ch: char, auto: &mut Automaton) -> (usize, usize) {
        let f1nal = auto.create_state();
        let init = auto.create_state();
        auto.states[init].trans = Some((Label::Byte(ch as u8), f1nal));
        (init, f1nal)
    }

    fn class_to_automaton(&CharSet(ref ch): &CharSet<char>, auto: &mut Automaton)
        -> (usize, usize) {
        let f1nal = auto.create_state();
        let init = auto.create_state();
        auto.states[init].trans = Some((Label::Class(CharSet(
            ch.iter().map(|c| c.start as u8 .. c.end as u8).collect()
        )), f1nal));
        (init, f1nal)
    }

    fn not_class_to_automaton(&CharSet(ref ch): &CharSet<char>, auto: &mut Automaton)
        -> (usize, usize) {
        let f1nal = auto.create_state();
        let init = auto.create_state();
        auto.states[init].trans = Some((Label::NotClass(CharSet(
            ch.iter().map(|c| c.start as u8 .. c.end as u8).collect()
        )), f1nal));
        (init, f1nal)
    }

    fn any_to_automaton(auto: &mut Automaton) -> (usize, usize) {
        let f1nal = auto.create_state();
        let init = auto.create_state();
        auto.states[init].trans = Some((Label::NotClass(CharSet(vec![])), f1nal));
        (init, f1nal)
    }
}
