use lexer::Lexer;
use lexer::Prop;
use regex;
use syntax::attr;
use syntax::ast;
use syntax::ast::Ident;
use syntax::codemap::{Span, Spanned};
use syntax::ext::base::ExtCtxt;
use syntax::ext::base::MacResult;
use syntax::ext::build::AstBuilder;
use syntax::symbol::Symbol;
use syntax::ptr::P;
use syntax::util::small_vector::SmallVector;

// struct returned by the code generator
// implements a trait containing method called by libsyntax
// on macro expansion
pub struct CodeGenerator<'cx> {
    // we need this to report
    // errors when the macro is
    // not called correctly
    handler: &'cx ::rustc_errors::Handler,
    span: Span,

    // items
    items: Vec<P<ast::Item>>
}


impl<'cx> MacResult for CodeGenerator<'cx> {
    fn make_items(self:Box<CodeGenerator<'cx>>)
            -> Option<SmallVector<P<ast::Item>>> {
        Some(SmallVector::many(self.items.clone()))
    }

    #[allow(unreachable_code,unused_must_use)]
    fn make_stmts(self:Box<CodeGenerator<'cx>>) -> Option<SmallVector<ast::Stmt>> {
        self.handler.span_unimpl(self.span,
            "invoking rustlex on statement context is not implemented");
        panic!("invoking rustlex on statement context is not implemented")
    }

    #[allow(unreachable_code,unused_must_use)]
    fn make_expr(self:Box<CodeGenerator<'cx>>) -> Option<P<ast::Expr>> {
        self.handler.span_fatal(self.span,
            "rustlex! invoked on expression context");
        panic!("rustlex! invoked on expression context")
    }
}

fn span<T>(sp: Span, t: T) -> Spanned<T> {
    Spanned { span: sp, node: t }
}

#[inline(always)]
pub fn lexer_field(sp: Span, name: ast::Ident, ty: P<ast::Ty>) -> ast::StructField {
    ast::StructField {
        span: sp,
        ident: Some(name),
        vis: span(sp, ast::VisibilityKind::Public),
        id: ast::DUMMY_NODE_ID,
        ty: ty,
        attrs: vec![]
    }
}


#[inline(always)]
pub fn lexer_struct(cx: &mut ExtCtxt, sp: Span, ident:Ident, props: &[Prop]) -> P<ast::Item> {

    let mut fields = Vec::with_capacity(props.len() + 1);

    for &(name, ref ty, _) in props.iter() {
        fields.push(lexer_field(sp, ast::Ident::with_empty_ctxt(name), ty.clone()));
    }

    fields.push(ast::StructField {
        span: sp,
        ident: Some(ast::Ident::with_empty_ctxt(Symbol::intern("_input"))),
        vis: span(sp, ast::VisibilityKind::Public),
        id: ast::DUMMY_NODE_ID,
        ty: quote_ty!(&*cx, ::rustlex::rt::RustLexLexer<R>),
        attrs: vec![]
    });

    fields.push(ast::StructField {
        span: sp,
        ident: Some(ast::Ident::with_empty_ctxt(Symbol::intern("_state"))),
        vis: span(sp, ast::VisibilityKind::Public),
        id: ast::DUMMY_NODE_ID,
        ty: quote_ty!(&*cx, usize),
        attrs: vec![]
    });

    let docattr = attr::mk_attr_outer(
        sp, attr::mk_attr_id(),
        attr::mk_list_item(
            sp, ast::Ident::with_empty_ctxt(Symbol::intern("allow")),
            vec![span(sp, ast::NestedMetaItemKind::MetaItem(attr::mk_word_item(
                ast::Ident::with_empty_ctxt(Symbol::intern("missing_docs")))
            ))]
        )
    );

    P(ast::Item {
        ident:ident,
        attrs: vec![ docattr ],
        id:ast::DUMMY_NODE_ID,
        node: ast::ItemKind::Struct(
            ast::VariantData::Struct(fields, ast::DUMMY_NODE_ID),
            ast::Generics {
                params: vec![
                    ast::GenericParam::Type(cx.typaram(
                        sp, ast::Ident::with_empty_ctxt(Symbol::intern("R")), vec![],
                        vec![
                            cx.typarambound(cx.path_global(sp, vec![
                                ast::Ident::with_empty_ctxt(Symbol::intern("std")),
                                ast::Ident::with_empty_ctxt(Symbol::intern("io")),
                                ast::Ident::with_empty_ctxt(Symbol::intern("Read"))
                            ]))
                        ],
                        None
                    ))
                ],
                where_clause: ast::WhereClause {
                    id: ast::DUMMY_NODE_ID,
                    predicates: Vec::new(),
                    span: sp
                },
                span: sp
            }
        ),
        vis: span(sp, ast::VisibilityKind::Public),
        span: sp,
        tokens: None
    })
}

pub fn codegen<'cx>(lex: &Lexer, cx: &'cx mut ExtCtxt, sp: Span)
                    -> Box<CodeGenerator<'cx>> {
    let mut items = Vec::new();

    items.push(lexer_struct(cx, sp, lex.ident, &lex.properties));

    // functions of the Lexer and InputBuffer structs
    // TODO:

    items.extend(user_lexer_impl(cx, sp, lex).into_iter());
    info!("done!");

    Box::new(CodeGenerator {
        span: sp,
        // FIXME:
        handler: &cx.parse_sess.span_diagnostic,
        items: items
    })
}

pub fn actions_match(lex:&Lexer, cx: &mut ExtCtxt, sp: Span) -> P<ast::Expr> {
    let match_expr = quote_expr!(&*cx, last_matching_action);
    let mut arms = Vec::with_capacity(lex.actions.len());
    let mut i = 1usize;

    let tokens = lex.tokens;
    let ident = lex.ident;
    let action_type = quote_ty!(&*cx,  Fn(&mut $ident<R>) -> Option<$tokens>);

    for act in lex.actions.iter().skip(1) {
        let pat_expr = quote_expr!(&*cx, $i);
        let pat = cx.pat_lit(sp, pat_expr);
        let new_act = act.clone();
        let arm = cx.arm(sp, vec!(pat),
            quote_expr!(&*cx, (Box::new($new_act)) as Box<$action_type>));
        arms.push(arm);
        i += 1;
    }

    let def_act = quote_expr!(&*cx, Box::new(|_:&mut $ident<R>| -> Option<$tokens> {
        unreachable!()
    }) as Box<$action_type>);

    let def_pat = cx.pat_wild(sp);
    arms.push(cx.arm(sp, vec!(def_pat), def_act));
    cx.expr_match(sp, match_expr, arms)
}

fn simple_follow_method(cx:&mut ExtCtxt, sp:Span, lex:&Lexer) -> P<ast::Item> {
    // * transtable: an array of N arrays of 256 uints, N being the number
    //   of states in the FSM, which gives the transitions between states
    let ty_vec = cx.ty(sp, ast::TyKind::Array(
        cx.ty_ident(sp, cx.ident_of("usize")),
        ast::AnonConst {
            id: ast::DUMMY_NODE_ID,
            value: cx.expr_usize(sp, 256)
        }
    ));
    let mut transtable = Vec::new();

    for st in lex.auto.states.iter() {
        let mut vec = Vec::new();
        for i in st.trans.iter() {
            vec.push(cx.expr_usize(sp, *i));
        }
        let trans_expr = cx.expr_vec(sp, vec);
        transtable.push(trans_expr);
    }

    let ty_transtable = cx.ty(sp, ast::TyKind::Array(
        ty_vec, ast::AnonConst {
            id: ast::DUMMY_NODE_ID,
            value: cx.expr_usize(sp, lex.auto.states.len())
        }
    ));

    let transtable = cx.item_static(
        sp, cx.ident_of("TRANSITION_TABLE"), ty_transtable,
        ast::Mutability::Immutable, cx.expr_vec(sp, transtable)
    );

    let ident = lex.ident;
    quote_item!(cx,
        impl<R: ::std::io::Read> $ident<R> {
            #[inline(always)]
            fn follow(&self, current_state:usize, symbol:usize) -> usize {
                $transtable
                return TRANSITION_TABLE[current_state][symbol];
            }
        }
    ).unwrap()
}

fn simple_accepting_method(cx:&mut ExtCtxt, sp:Span, lex:&Lexer) -> P<ast::Item> {
    // * accepting: an array of N uints, giving the action associated to
    //   each state
    let ty_acctable = cx.ty(sp, ast::TyKind::Array(
        cx.ty_ident(sp, cx.ident_of("usize")),
        ast::AnonConst {
            id: ast::DUMMY_NODE_ID,
            value: cx.expr_usize(sp, lex.auto.states.len())
        }
    ));

    let mut acctable = Vec::new();
    for st in lex.auto.states.iter() {
        let regex::Action(act) = st.data;
        let acc_expr = cx.expr_usize(sp, act);
        acctable.push(acc_expr);
    }
    let acctable = cx.item_static(
        sp, cx.ident_of("ACCEPTING"), ty_acctable,
        ast::Mutability::Immutable, cx.expr_vec(sp, acctable)
    );

    let ident = lex.ident;
    quote_item!(cx,
        impl<R: ::std::io::Read> $ident<R> {
            #[inline(always)]
            fn accepting(&self, state:usize) -> usize {
                $acctable
                return ACCEPTING[state];
            }
        }
    ).unwrap()
}

pub fn user_lexer_impl(cx: &mut ExtCtxt, sp: Span, lex:&Lexer) -> Vec<P<ast::Item>> {
    let actions_match = actions_match(lex, cx, sp);
    let mut fields = Vec::with_capacity(lex.properties.len() + 1);

    for &(name, _, ref expr) in lex.properties.iter() {
        fields.push(cx.field_imm(sp, ast::Ident::with_empty_ctxt(name), expr.clone()));
    }

    let initial = lex.auto.initials[lex.conditions[0].1];
    fields.push(cx.field_imm(sp, ast::Ident::with_empty_ctxt(Symbol::intern("_input")),
        quote_expr!(&*cx, ::rustlex::rt::RustLexLexer::new(reader))));
    fields.push(cx.field_imm(sp, ast::Ident::with_empty_ctxt(Symbol::intern("_state")),
        quote_expr!(&*cx, $initial)));

    let init_expr = cx.expr_struct_ident(sp, lex.ident, fields);

    let ident = lex.ident;
    // condition methods
    let mut items:Vec<P<ast::Item>> = lex.conditions.iter().map(|&(cond,st)| {
        let st = lex.auto.initials[st];
        let cond = ast::Ident::with_empty_ctxt(cond);
        quote_item!(cx,
            impl<R: ::std::io::Read> $ident<R> {
                #[inline(always)]
                #[allow(dead_code)]
                #[allow(non_snake_case)]
                fn $cond(&mut self) { self._state = $st; }
            }
        ).unwrap()
    }).collect();

    items.push(quote_item!(cx,
        impl<R: ::std::io::Read> $ident<R> {
            pub fn new(reader:R) -> $ident<R> {
                $init_expr
            }

            #[allow(dead_code)]
            #[allow(unused_mut)]
            fn yystr(&mut self) -> String {
                let ::rustlex::rt::RustLexPos { buf, off } = self._input.tok;
                let ::rustlex::rt::RustLexPos { buf: nbuf, off: noff } = self._input.pos;
                if buf == nbuf {
                    let slice:&[u8] = self._input.inp[buf].slice(off, noff);
                    String::from_utf8(slice.to_vec()).unwrap()
                } else {
                    // create a strbuf
                    let mut yystr:Vec<u8> = vec!();

                    // unsafely pushes all bytes onto the buf
                    let iter = self._input.inp[buf + 1 .. nbuf].iter();
                    let iter = iter.flat_map(|v| v.as_slice().iter());
                    let iter = iter.chain(self._input.inp[nbuf]
                        .slice(0, noff).iter());

                    let mut iter = self._input.inp[buf].slice_from(off)
                        .iter().chain(iter);
                    for j in iter {
                        yystr.push(*j)
                    }
                    String::from_utf8(yystr).unwrap()
                }
            }
        }
    ).unwrap());

    items.push(simple_follow_method(cx, sp, lex));
    items.push(simple_accepting_method(cx, sp, lex));

    let tokens = lex.tokens;
    items.push(quote_item!(cx,
        impl <R: ::std::io::Read> Iterator for $ident<R> {
            type Item = $tokens;

            fn next(&mut self) -> Option<$tokens> {
                loop {
                    self._input.tok = self._input.pos;
                    self._input.advance = self._input.pos;
                    let mut last_matching_action = 0;
                    let mut current_st = self._state;

                    while current_st != 0 {
                        let i = match self._input.getchar() {
                            None if self._input.tok ==
                                    self._input.pos => return None,
                            Some(i) => i,
                            _ => break
                        };

                        let new_st:usize = self.follow(current_st, i as usize);
                        let action_id:usize = self.accepting(new_st);

                        if action_id != 0 {
                            self._input.advance = self._input.pos;

                            // final state
                            last_matching_action = action_id;
                        }

                        current_st = new_st;
                    }

                    // go back to last matching state in the input
                    self._input.pos = self._input.advance;

                    // execute action corresponding to found state
                    let action_result = $actions_match(self) ;

                    match action_result {
                        Some(token) => return Some(token),
                        None => ()
                    };
                    // if the user code did not return, continue
                }
            }
        }
    ).unwrap());
    items
}

