use dfa;
use nfa;
use regex::Regex;
use std::vec;
use syntax::ast::Name;
use syntax::ast::Stmt;
use syntax::codemap::Span;
use syntax::ext::base::ExtCtxt;
use syntax::ext::base::MacResult;
use syntax::ext::base::MRAny;
use syntax::ext::build::AstBuilder;

// a rule is a regular expression pattern associated
// to a Rust code snippet
// unfortunately we have to use borrowed pointers since
// this is what libsyntax gives usn
pub struct Rule {
    pattern: ~Regex,
    action: @Stmt
}

// a condition is a "state" of the lexical analyser
// a single pattern may have different effects on different
// conditions. Each condition have an automaton for its own
// set of rules
pub struct Condition {
    name: Name,
    rules: ~[Rule]
}

// the definition of a lexical analyser is just
// all of the conditions
pub struct LexerDef {
    conditions: ~[Condition]
}

// The RustLex representation of a lexical analyser
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
    priv auto: ~dfa::Automaton,
    priv actions: ~[@Stmt],
    priv conditions: ~[(Name, uint)]
}

mod codegen;

impl Lexer {
    // Main function of RustLex, compiles a set of rules into a
    // lexical analyser
    pub fn new(def: ~LexerDef, cx: &ExtCtxt) -> Lexer {
        // all the actions in the lexical analyser
        // 0 is a dummy action that represent no action
        let dummy_expr = cx.expr_unreachable(cx.call_site());
        let dummy_stmt = cx.stmt_expr(dummy_expr);
        let mut acts = ~[dummy_stmt];
        let mut id = 1u;

        // now build the automatas and record
        // the initial state number for each
        let mut dfas = dfa::Automaton::new();
        let mut conds = ~[];

        for cond in def.conditions.move_iter() {
            let mut asts = vec::with_capacity(cond.rules.len());
            let name = cond.name;

            for Rule { pattern, action } in cond.rules.move_iter() {
                asts.push((pattern, id));
                acts.push(action);
                id += 1;
            }

            // build a NFA for this condition, determinize it
            // and add it to the built DFA
            println!("building...");
            let nfa = nfa::build_nfa(asts);
            println!("determinizing...");
            dfas.determinize(nfa);

            // store the initial ID of auto for this condition
            conds.push((name, dfas.initial));
        }

        println!("minimizing...");
        let dfa = dfas.minimize(acts.len(), conds);
        Lexer { auto: dfa, actions: acts, conditions: conds }
    }

    // the generated code model consists of several Rust "items", i.e.
    // declarations that are not statements
    // - declaration of the transition table and accepting table
    // - declaration of the condition names and a few macros to
    //   make the writing of actions easier
    // - the Lexer struct and its impl block that implements the actual
    // code of simulation of the automaton
    pub fn genCode(&self, cx: &mut ExtCtxt, sp: Span) -> MacResult {
        println!("generating code...");
        MRAny(codegen::codegen(self, cx, sp))
    }
}
