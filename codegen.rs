use lexer::Lexer;
use syntax::ast;
use syntax::codemap::CodeMap;
use syntax::codemap::Span;
use syntax::diagnostic;
use syntax::ext::base::AnyMacro;
use syntax::ext::base::ExtCtxt;
use syntax::ext::build::AstBuilder;
use syntax::util::small_vector::SmallVector;
    
// struct returned by the code generator
// implements a trait containing method called by libsyntax
// on macro expansion
struct CodeGenerator {
    // we need this to report
    // errors when the macro is
    // not called correctly
    handler: diagnostic::SpanHandler,
    span: Span,

    // items
    items: Vec<@ast::Item>
}


impl AnyMacro for CodeGenerator {
    fn make_items(&self) -> SmallVector<@ast::Item> {
        SmallVector::many(self.items.clone())
    }

    fn make_stmt(&self) -> @ast::Stmt {
        self.handler.span_unimpl(self.span,
            "invoking rustlex on statement context is not implemented");
    }

    fn make_expr(&self) -> @ast::Expr {
        self.handler.span_fatal(self.span,
            "rustlex! invoked on expression context");
    }
}

#[inline(always)]
pub fn bufferStruct<'a>(cx: &mut ExtCtxt) -> @ast::Item {
    (quote_item!(cx,
        struct InputBuffer {
            buf: Vec<u8>,
            current_pos: uint
        }
    )).unwrap()
}

#[inline(always)]
pub fn lexerStruct<'a>(cx: &mut ExtCtxt) -> @ast::Item {
    (quote_item!(cx,
        struct Lexer {
            stream: ~std::io::Reader,
            inp: ~InputBuffer,
            condition: uint
        }
    )).unwrap()
}

pub fn codegen<'a>(lex: &Lexer, cx: &mut ExtCtxt, sp: Span) -> ~CodeGenerator {
    let mut items = Vec::new();

    // tables
    // * trans_table: an array of N arrays of 256 uints, N being the number
    //   of states in the FSM, which gives the transitions between states
    // * accepting: an array of N uints, giving the action associated to
    //   each state

    let ty_vec = cx.ty(sp, ast::TyFixedLengthVec(
        cx.ty_ident(sp, cx.ident_of("uint")),
        cx.expr_uint(sp, 256)));

    let ty_transtable = cx.ty(sp, ast::TyFixedLengthVec(
        ty_vec,
        cx.expr_uint(sp, lex.auto.states.len())));

    let ty_acctable = cx.ty(sp, ast::TyFixedLengthVec(
        cx.ty_ident(sp, cx.ident_of("uint")),
        cx.expr_uint(sp, lex.auto.states.len())));

    let mut transtable = Vec::new();
    let mut acctable = Vec::new();

    for st in lex.auto.states.iter() {
        let mut vec = Vec::new();
        for i in st.trans.iter() {
            vec.push(cx.expr_uint(sp, *i));
        }
        let trans_expr = cx.expr_vec(sp, vec);
        let acc_expr = cx.expr_uint(sp, st.action);
        transtable.push(trans_expr);
        acctable.push(acc_expr);
    }

    let transtable = cx.expr_vec(sp, transtable);
    let transtable = ast::ItemStatic(ty_transtable, ast::MutImmutable, transtable);
    let transtable = cx.item(sp, cx.ident_of("transition_table"), Vec::new(),
            transtable);
    let acctable = cx.expr_vec(sp, acctable);
    let acctable = ast::ItemStatic(ty_acctable, ast::MutImmutable, acctable);
    let acctable = cx.item(sp, cx.ident_of("accepting"), Vec::new(),
            acctable);

    items.push(transtable);
    items.push(acctable);

    // constants
    // a constant per condition, whose value is the initial
    // state of the DFA corresponding to that condition in
    // the main big DFA
    // the INPUT_BUFSIZE constant is used by the Lexer methods

    for &(cond, st) in lex.conditions.iter() {
        let cond = ast::Ident::new(cond);
        items.push(quote_item!(&*cx,
            static $cond: uint = $st;
        ).unwrap());
    }

    items.push(quote_item!(&*cx, static INPUT_BUFSIZE: uint = 256;).unwrap());

    // structs

    items.push(bufferStruct(cx));
    items.push(lexerStruct(cx));

    // functions of the Lexer and InputBuffer structs
    // TODO:

    let acts_match = actionsMatch(lex.actions, cx, sp);
    let lex_impl = lexerImpl(cx, acts_match);
    items.push(lex_impl);
    println!("done!");

    ~CodeGenerator {
        span: sp,
        // FIXME:
        handler: diagnostic::mk_span_handler(diagnostic::default_handler(), CodeMap::new()),
        items: items
    }
}

pub fn actionsMatch(acts: &[@ast::Stmt], cx: &mut ExtCtxt, sp: Span) -> @ast::Expr {
    let match_expr = quote_expr!(&*cx, last_matching_action);
    let mut arms = Vec::with_capacity(acts.len());
    let mut i = 1u;

    let yystr = quote_stmt!(&*cx,
        // FIXME: unused variable in generated code
        // a syntax reg as var => like OCamllex would be better
        let yystr = ::std::str::from_utf8(self.inp.buf.slice(
            oldpos, self.inp.current_pos)).unwrap();
    );

    for act in acts.iter().skip(1) {
        let pat_expr = quote_expr!(&*cx, $i);
        let pat = cx.pat_lit(sp, pat_expr);
        let block = cx.block(sp, vec!(yystr, *act), None);
        let expr = quote_expr!(&*cx, $block);
        let arm = cx.arm(sp, vec!(pat), expr);
        arms.push(arm);
        i += 1;
    }

    let def_act = quote_expr!(&*cx, {
        // default action is printing on stdout
        self.go_back(oldpos + 1);
        let s = self.inp.buf.slice(oldpos, self.inp.current_pos);
        print!("{:s}", ::std::str::from_utf8(s).unwrap());
    });

    let def_pat = cx.pat_wild(sp);
    arms.push(cx.arm(sp, vec!(def_pat), def_act));
    cx.expr_match(sp, match_expr, arms)
}

pub fn lexerImpl(cx: &mut ExtCtxt, actions_match: @ast::Expr) -> @ast::Item {
    // the actual simulation code
    (quote_item!(cx,
        impl Lexer {
            fn next_input(&mut self) -> Option<u8> {
                if self.inp.current_pos == self.inp.buf.len() {
                    // more input
                    self.inp.buf = Vec::from_elem(INPUT_BUFSIZE, 0 as u8);
                    match self.stream.read(self.inp.buf.mut_slice_from(0)) {
                        Err(_) => return None,
                        Ok(b) => if b < INPUT_BUFSIZE {
                            self.inp.buf.truncate(b); 
                        }
                    }

                    self.inp.current_pos = 0;
                }

                let &ret = self.inp.buf.get(self.inp.current_pos);
                self.inp.current_pos += 1;
                Some(ret)
            }

            fn go_back(&mut self, pos: uint) {
                self.inp.current_pos = pos;
            }

            fn next<'a>(&'a mut self) -> Option<Token> {
                let oldpos = self.inp.current_pos;
                let mut advance = self.inp.current_pos;
                let mut last_matching_action = 0;
                let mut current_st = self.condition;

                while current_st != 0 {
                    let i = match self.next_input() {
                        Some(i) => i,
                        None => return None
                    };

                    let new_st = transition_table[current_st][i as uint];
                    let action = accepting[new_st];

                    if action != 0 {
                        advance = self.inp.current_pos;

                        // final state
                        last_matching_action = action;
                    }

                    current_st = new_st;
                }

                // go back to last matching state in the input
                self.go_back(advance);

                // execute action corresponding to found state
                $actions_match

                // if the user code did not return, continue
                self.next()
            }

            fn new(stream: ~::std::io::Reader) -> ~Lexer {
                let buf = ~InputBuffer { buf: vec!(), current_pos: 0 };
                ~Lexer { stream: stream, inp: buf, condition: INITIAL }
            }
        }
    )).unwrap()
}
