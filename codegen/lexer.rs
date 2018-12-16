use analysis;
use fsa::dfa;
use regex;
use unicode;
use proc_macro::TokenStream;
use proc_macro2::{Delimiter// , Term
                 , TokenNode, };
use proc_macro2::{Term, TokenTree, Span};
use proc_macro::{Diagnostic};
use quote::quote;

// a rule is a regular expression pattern associated
// to a Rust code snippet
// unfortunately we have to use borrowed pointers since
// this is what libsyntax gives usn
pub struct Rule {
    pub pattern: regex::Regex,
    pub action: TokenTree
}

// a condition is a "state" of the lexical analyser
// a single pattern may have different effects on different
// conditions. Each condition have an automaton for its own
// set of rules
pub struct Condition {
    pub name: Term,
    pub rules: Vec<Rule>,
    pub span: Span
}

pub type Prop = (Term, TokenTree, TokenTree);

// the definition of a lexical analyser is just
// all of the conditions
pub struct LexerDef {
    pub tokens: Term,
    pub ident: Term,
    pub defs: Vec<regex::Regex>,
    pub properties: Vec<Prop>,
    pub conditions: Vec<Condition>
}

// the RustLex representation of a lexical analyser
// * auto is an automata made of several Deterministic Finite Automata.
//   They each correspond to a single "condition" of the lexical analyser
//   but are stored in a single automata and thus have a single "state
//   number namespace"
// * actions is the list of actions associated with the final states of the
//   automata. They are indexed by a number, which is how they are stored
//   in the DFA, and they store the associated Rust code
// * conditions is the list of all the conditions in the lexcial analyser,
//   along the number of the initial state of the DFA that corresponds to
//   this condition in auto.
pub struct Lexer {
    pub tokens: Term,
    pub ident: Term,
    pub auto: dfa::Automaton<regex::Action>,
    pub actions: Vec<TokenTree>,
    pub conditions: Vec<(Term, usize)>,
    pub properties: Vec<Prop>
}

impl Lexer {
    // Main function of RustLex, compiles a set of rules into aName
    // lexical analyser
    pub fn new(def: LexerDef) -> Result<Lexer, ()> {
        // all the actions in the lexical analyser
        // 0 is a dummy action that represent no action
        let mut acts = vec![
            TokenNode::Group(Delimiter::Parenthesis,
                             quote!{ unreachable!() }.into()).into()
        ];
        let mut id = 1usize;

        // now build the automatas and record
        // the initial state number for each
        let mut dfas = dfa::Automaton::new();
        let mut conds = Vec::new();

 //       let LexerDef { ref properties, ref conditions } = *def;
        for cond in def.conditions.iter() {
            let mut asts = Vec::with_capacity(cond.rules.len());
            let name = cond.name;

            for &Rule { ref pattern, ref action } in cond.rules.iter() {
                asts.push((pattern.clone(), regex::Action(id)));
                // FIXME: this cloning shouldn't be necessary.
                acts.push(action.clone());
                id += 1;
            }

            // build a NFA for this condition, determinize it
            // and add it to the built DFA
            info!("building...");
            let nfa = regex::build_nfa::<unicode::Ascii>(&asts[..], &def.defs[..]);
            info!("determinizing...");

            // store the initial ID of auto for this condition
            conds.push((name, dfas.determinize(&nfa)));
        }

        // analysis
        let mut error = false;

        for &s in dfas.initials.iter() {
            let regex::Action(act) = dfas.states[s].data;
            if act != 0 {
                // FIXME: we sould report the span of the whole rule, or
                // pattern, instead of just the span of the action.
                // Diagnostic::spanned(acts[act].span, ::proc_macro::Level::Error,
                //                     "this rule accepts the empty word")
                //     .help("this might cause the automaton \
                //            to loop on some inputs")
                //     .emit();
                // error = true;
                panic!()
            }
        }

        info!("checking reachability and completeness... ");
        let analysis::Analysis { unreachable, incomplete } =
            analysis::check_automaton(&dfas, acts.len());

        for regex::Action(act) in unreachable.into_iter() {
            // Diagnostic::spanned(acts[act].span, ::proc_macro::Level::Error,
            //                     "unreachable pattern")
            //     .help("make sure it is not included in another pattern ; \
            //            latter patterns have precedence")
            //     .emit();
            // error = true;
            panic!()
        }

        for cond in incomplete.into_iter() {
            // Diagnostic::spanned(def.conditions[cond].span,
            //                     ::proc_macro::Level::Error,
            //                     "this automaton is incomplete")
            //   .help("maybe add a catch-all rule?")
            //   .emit();
            // error = true;
            panic!();
        }

        if error { return Err(()) }

        info!("minimizing...");
        Ok(Lexer {
            tokens: def.tokens,
            ident: def.ident,
            auto: dfas.minimize(),
            actions: acts.into(),
            conditions: conds,
            properties: def.properties.clone()
        })
    }

    // the generated code model consists of several Rust "items", i.e.
    // declarations that are not statements
    // - declaration of the transition table and accepting table
    // - declaration of the condition names and a few macros to
    //   make the writing of actions easier
    // - the Lexer struct and its impl block that implements the actual
    // code of simulation of the automaton
    pub fn gen_code(&self) -> TokenStream {
        info!("generating code...");
        ::codegen::codegen(self)
    }
}
