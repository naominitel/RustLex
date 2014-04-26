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
pub fn structs<'a>(cx: &mut ExtCtxt) -> Vec<@ast::Item> {
    vec!(
        (quote_item!(&*cx,
            static RUSTLEX_BUFSIZE: uint = 4096;
        )).unwrap(),

        (quote_item!(&*cx,
            struct RustLexBuffer {
                d: Vec<u8>,
                valid: bool
            }
        )).unwrap(),

        (quote_item!(&*cx,
            impl<'a> RustLexBuffer {
                #[inline(always)]
                fn len(&self) -> uint {
                    self.d.len()
                }

                #[inline(always)]
                fn get(&'a self, idx: uint) -> &'a u8 {
                    self.d.get(idx)
                }

                #[inline(always)]
                fn as_slice(&'a self) -> &'a [u8] {
                    self.d.as_slice()
                }

                #[inline(always)]
                fn slice(&'a self, from: uint, to: uint) -> &'a [u8] {
                    self.d.slice(from, to)
                }

                #[inline(always)]
                fn slice_from(&'a self, from: uint) -> &'a [u8] {
                    self.d.slice_from(from)
                }
            }
        )).unwrap(),

        (quote_item!(&*cx,
            struct RustLexPos {
                buf: uint,
                off: uint
            }
        )).unwrap(),

        (quote_item!(&*cx,
            impl Eq for RustLexPos {
                fn eq(&self, other: &RustLexPos) -> bool {
                    self.buf == other.buf &&
                    self.off == other.off
                }
            }
        )).unwrap(),

        (quote_item!(cx,
            struct Lexer {
                stream: ~std::io::Reader,
                inp: Vec<RustLexBuffer>,
                condition: uint,
                advance: RustLexPos,
                pos: RustLexPos,
                tok: RustLexPos
            }
        )).unwrap()
    )
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

    // structs

    items.push_all(structs(cx).as_slice());

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
        let _yystr = {
            let RustLexPos { buf, off } = self.tok;
            let RustLexPos { buf: nbuf, off: noff } = self.pos;
            if buf == nbuf {
                let slice = self.inp.get(buf).slice(off, noff);
                let mut buf = StrBuf::with_capacity(slice.len());
                unsafe { buf.push_bytes(slice); }
                buf
            } else {
                // create a strbuf with the right capacity
                let mut capacity = self.inp.get(buf).len() - off;
                for i in range(buf + 1, nbuf) {
                    capacity += self.inp.get(i).len();
                }
                capacity += noff;
                let mut yystr = StrBuf::with_capacity(capacity);

                // unsafely pushes all bytes onto the buf
                let iter = self.inp.slice(buf + 1, nbuf).iter();
                let iter = iter.flat_map(|v| v.as_slice().iter());
                let iter = iter.chain(self.inp.get(nbuf).slice(0, noff).iter());
                let mut iter = self.inp.get(buf).slice_from(off).iter().chain(iter);
                for &i in iter {
                    unsafe { yystr.push_byte(i) }
                }
                yystr
            }
        };
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
        self.pos = self.tok;
        self.pos.off += 1;
        let b: &u8 = self.inp.get(self.tok.buf).get(self.tok.off);
        print!("{:c}", *b as char);
    });

    let def_pat = cx.pat_wild(sp);
    arms.push(cx.arm(sp, vec!(def_pat), def_act));
    cx.expr_match(sp, match_expr, arms)
}

pub fn lexerImpl(cx: &mut ExtCtxt, actions_match: @ast::Expr) -> @ast::Item {
    // the actual simulation code
    (quote_item!(cx,
    impl Lexer {
        fn fill_buf(&mut self) {
            let &RustLexBuffer {
                ref mut d,
                ref mut valid
            } = self.inp.get_mut(self.pos.buf);
            *valid = true;
            self.stream.push_exact(d, RUSTLEX_BUFSIZE);
            self.pos.off = 0;
        }

        fn getchar(&mut self) -> Option<u8> {
            if self.pos.off == RUSTLEX_BUFSIZE {
                let npos = self.pos.buf + 1;
                if self.inp.len() > npos && self.inp.get(npos).valid {
                    self.pos.buf = npos;
                    self.pos.off = 0;
                } else {
                    // we reached the end of the current buffer. We must get
                    // more input. First, see if we can get rid of buffers
                    // that won't be used anymore. Shifting the array can be
                    // done cheaply because most analysers won't need more
                    // than a couple of buffers
                    let unused_buffers_count = self.tok.buf;
                    for i in range(0, unused_buffers_count) {
                        self.inp.get_mut(i).valid = false;
                        self.inp.get_mut(i).d.truncate(0);
                        self.inp.as_mut_slice().swap(i + unused_buffers_count, i);
                    }
                    self.tok.buf -= unused_buffers_count;
                    self.pos.buf -= unused_buffers_count - 1;
                    self.advance.buf -= unused_buffers_count;

                    while self.pos.buf >= self.inp.len() {
                        // we couldn't free some space, we have to create a
                        // new buffer and add it to our vector
                        self.inp.push(RustLexBuffer {
                            d: Vec::with_capacity(RUSTLEX_BUFSIZE),
                            valid: false
                        });
                    }

                    self.fill_buf();
                }
            } else if self.pos.off >= self.inp.get(self.pos.buf).len() {
                // the current buffer wasn't full, this mean this is
                // actually EOF
                return None
            }

            let &ch = self.inp.get(self.pos.buf).get(self.pos.off);
            self.pos.off += 1;
            Some(ch)
        }

        fn next<'a>(&'a mut self) -> Option<Token> {
            loop {
                self.tok = self.pos;
                self.advance = self.pos;
                let mut last_matching_action = 0;
                let mut current_st = self.condition;

                while current_st != 0 {
                    let i = match self.getchar() {
                        None if self.tok == self.pos => return None,
                        Some(i) => i,
                        _ => break
                    };

                    let new_st = transition_table[current_st][i as uint];
                    let action = accepting[new_st];

                    if action != 0 {
                        self.advance = self.pos;

                        // final state
                        last_matching_action = action;
                    }

                    current_st = new_st;
                }

                // go back to last matching state in the input
                self.pos = self.advance;

                // execute action corresponding to found state
                $actions_match

                // if the user code did not return, continue
            }
        }

        fn new(stream: ~::std::io::Reader) -> ~Lexer {
            let mut lex = ~Lexer {
                stream: stream,
                inp: vec!(RustLexBuffer{
                    d: Vec::new(),
                    valid: false
                }),
                condition: INITIAL,
                advance: RustLexPos {
                    off: 0,
                    buf: 0
                },
                pos: RustLexPos {
                    off: 0,
                    buf: 0
                },
                tok: RustLexPos {
                    off: 0,
                    buf: 0
                }
            };
            lex.fill_buf();
            lex
        }
    })).unwrap()
}
