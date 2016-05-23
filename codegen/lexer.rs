use analysis;
use fsa::dfa;
use regex;
use unicode;
use syntax::ast::Expr;
use syntax::ast::Ident;
use syntax::ast::Name;
use syntax::ast::Ty;
use syntax::codemap::Span;
use syntax::ext::base::ExtCtxt;
use syntax::ext::base::MacResult;
use syntax::ext::build::AstBuilder;
use syntax::ptr::P;

// a rule is a regular expression pattern associated
// to a Rust code snippet
// unfortunately we have to use borrowed pointers since
// this is what libsyntax gives usn
pub struct Rule {
    pub pattern: regex::Regex,
    pub action: P<Expr>
}

// a condition is a "state" of the lexical analyser
// a single pattern may have different effects on different
// conditions. Each condition have an automaton for its own
// set of rules
pub struct Condition {
    pub name: Name,
    pub rules: Vec<Rule>,
    pub span: Span
}

pub type Prop = (Name, P<Ty>, P<Expr>);
// the definition of a lexical analyser is just
// all of the conditions
pub struct LexerDef {
    pub tokens:     Ident,
    pub ident:      Ident,
    pub defs:       Vec<regex::Regex>,
    pub properties: Vec<Prop>,
    pub conditions: Vec<Condition>
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
    tokens:Ident,
    ident:Ident,
    pub auto: dfa::Automaton<regex::Action>,
    pub actions: Vec<P<Expr>>,
    conditions: Vec<(Name, usize)>,
    properties: Vec<Prop>
}

mod codegen {
#[cfg(feature = "with-syntex")] include!(concat!(env!("OUT_DIR"), "/codegen.rs"));
#[cfg(not(feature = "with-syntex"))] include!("codegen.in.rs");
}

impl Lexer {
    // Main function of RustLex, compiles a set of rules into a
    // lexical analyser
    pub fn new(def: LexerDef, cx: &ExtCtxt) -> Lexer {
        // all the actions in the lexical analyser
        // 0 is a dummy action that represent no action
        let dummy_expr = cx.expr_unreachable(cx.call_site());
        let mut acts = vec!(dummy_expr);
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

        for &s in dfas.initials.iter() {
            let regex::Action(act) = dfas.states[s].data;
            if act != 0 {
                cx.struct_span_err(acts[act].span, "this rule accepts the empty word")
                  .help("this might cause the automaton to loop on some inputs")
                  .emit();
            }
        }

        info!("checking reachability and completeness... ");
        let analysis::Analysis { unreachable, incomplete } =
            analysis::check_automaton(&dfas, acts.len());

        for regex::Action(act) in unreachable.into_iter() {
            cx.struct_span_err(acts[act].span, "unreachable pattern")
              .help("make sure it is not included in another pattern ; latter patterns have precedence")
              .emit();
        }

        for cond in incomplete.into_iter() {
            cx.struct_span_err(def.conditions[cond].span, "this automaton is incomplete")
              .help("maybe add a catch-all rule?")
              .emit();
        }

        cx.parse_sess.span_diagnostic.abort_if_errors();

        info!("minimizing...");
        Lexer {
            tokens: def.tokens,
            ident: def.ident,
            auto: dfas.minimize(),
            actions: acts,
            conditions: conds,
            properties: def.properties.clone()
        }
    }

    // the generated code model consists of several Rust "items", i.e.
    // declarations that are not statements
    // - declaration of the transition table and accepting table
    // - declaration of the condition names and a few macros to
    //   make the writing of actions easier
    // - the Lexer struct and its impl block that implements the actual
    // code of simulation of the automaton
    pub fn gen_code<'cx>(&self, cx: &'cx mut ExtCtxt, sp: Span) -> Box<MacResult + 'cx> {
        info!("generating code...");
        codegen::codegen(self, cx, sp) as Box<MacResult + 'cx>
    }
}
