use lexer::Lexer;
use lexer::Prop;
use syntax::ast;
use syntax::ast::Ident;
use syntax::ast::Method;
use syntax::codemap;
use syntax::codemap::CodeMap;
use syntax::codemap::Span;
use syntax::diagnostic;
use syntax::ext::base::ExtCtxt;
use syntax::ext::base::MacResult;
use syntax::ext::build::AstBuilder;
use syntax::parse::token;
use syntax::ptr::P;
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
    items: Vec<P<ast::Item>>
}


impl MacResult for CodeGenerator {
    fn make_items(self:Box<CodeGenerator>) -> Option<SmallVector<P<ast::Item>>> {
        Some(SmallVector::many(self.items.clone()))
    }

    fn make_stmt(self:Box<CodeGenerator>) -> Option<P<ast::Stmt>> {
        self.handler.span_unimpl(self.span,
            "invoking rustlex on statement context is not implemented");
    }

    fn make_expr(self:Box<CodeGenerator>) -> Option<P<ast::Expr>> {
        self.handler.span_fatal(self.span,
            "rustlex! invoked on expression context");
    }
}

#[inline(always)]
pub fn lexer_field(sp: Span, name: ast::Ident, ty: P<ast::Ty>) -> ast::StructField {
    codemap::Spanned {
        span: sp,
        node: ast::StructField_ {
            kind: ast::NamedField(name, ast::Public),
            id: -1 as u32,
            ty: ty,
            attrs: vec!()
        }
    }
}


#[inline(always)]
pub fn lexer_struct(cx: &mut ExtCtxt, sp: Span, ident:Ident, props: &[Prop]) -> P<ast::Item> {

    let mut fields = Vec::with_capacity(props.len() + 1);

    for &(name, ref ty, _) in props.iter() {
        fields.push(lexer_field(sp, ast::Ident::new(name), ty.clone()));
    }

    fields.push(codemap::Spanned {
        span: sp,
        node: ast::StructField_ {
            kind: ast::NamedField(
                ast::Ident::new(token::intern("_input")),
                ast::Public
            ),
            id: -1 as u32,
            ty: quote_ty!(&*cx, ::rustlex::rt::RustLexLexer<R>),
            attrs: vec!()
        }
    });

    fields.push(codemap::Spanned {
        span: sp,
        node: ast::StructField_ {
            kind: ast::NamedField(
                ast::Ident::new(token::intern("_state")),
                ast::Public
            ),
            id: -1 as u32,
            ty: quote_ty!(&*cx, uint),
            attrs: vec!()
        }
    });

    let isp = cx.item_struct_poly(sp, ident,
        ast::StructDef { ctor_id: None, fields: fields },
        ast::Generics {
            lifetimes: Vec::new(),
            ty_params: ::syntax::owned_slice::OwnedSlice::from_vec(vec!(
                cx.typaram(sp, ast::Ident::new(token::intern("R")),
                ::syntax::owned_slice::OwnedSlice::from_vec(vec!(
                    cx.typarambound(cx.path_global(sp, vec!(
                        ast::Ident::new(token::intern("std")),
                        ast::Ident::new(token::intern("io")),
                        ast::Ident::new(token::intern("Reader"))
                ))))),
                None, None)
            )),
            where_clause: ast::WhereClause {
                id: ast::DUMMY_NODE_ID,
                predicates: Vec::new(),
            }
        }
    );
    isp
}

pub fn codegen(lex: &Lexer, cx: &mut ExtCtxt, sp: Span) -> Box<CodeGenerator> {
    let mut items = Vec::new();

    items.push(lexer_struct(cx, sp, lex.ident, lex.properties.as_slice()));

    // functions of the Lexer and InputBuffer structs
    // TODO:

    items.extend(user_lexer_impl(cx, sp, lex).into_iter());
    info!("done!");

    box CodeGenerator {
        span: sp,
        // FIXME:
        handler: diagnostic::mk_span_handler(
            diagnostic::default_handler(diagnostic::Auto, None),
            CodeMap::new()
        ),
        items: items
    }
}

pub fn actions_match(acts: &[P<ast::Expr>], ident:ast::Ident, cx: &mut ExtCtxt, sp: Span) -> P<ast::Expr> {
    let match_expr = quote_expr!(&*cx, last_matching_action);
    let mut arms = Vec::with_capacity(acts.len());
    let mut i = 1u;

    for act in acts.iter().skip(1) {
        let pat_expr = quote_expr!(&*cx, $i);
        let pat = cx.pat_lit(sp, pat_expr);
        let arm = cx.arm(sp, vec!(pat), act.clone());
        arms.push(arm);
        i += 1;
    }

    let def_act = quote_expr!(&*cx, |lexer:&mut $ident<R>| {
        // default action is printing on stdout
        lexer._input.pos = lexer._input.tok;
        lexer._input.pos.off += 1;
        let b: &u8 = lexer._input.inp[
            lexer._input.tok.buf].get(lexer._input.tok.off);
        print!("{}", *b as char);
        None
    });

    let def_pat = cx.pat_wild(sp);
    arms.push(cx.arm(sp, vec!(def_pat), def_act));
    cx.expr_match(sp, match_expr, arms)
}

fn simple_follow_method(cx:&mut ExtCtxt, sp:Span, lex:&Lexer) -> P<Method> {
    // * transtable: an array of N arrays of 256 uints, N being the number
    //   of states in the FSM, which gives the transitions between states
    let ty_vec = cx.ty(sp, ast::TyFixedLengthVec(
        cx.ty_ident(sp, cx.ident_of("uint")),
        cx.expr_uint(sp, 256)));
    let mut transtable = Vec::new();

    for st in lex.auto.states.iter() {
        let mut vec = Vec::new();
        for i in st.trans.iter() {
            vec.push(cx.expr_uint(sp, *i));
        }
        let trans_expr = cx.expr_vec(sp, vec);
        transtable.push(trans_expr);
    }

    let ty_transtable = cx.ty(sp, ast::TyFixedLengthVec(
        ty_vec,
        cx.expr_uint(sp, lex.auto.states.len())));

    let transtable = cx.expr_vec(sp, transtable);
    let transtable = ast::ItemStatic(ty_transtable, ast::MutImmutable, transtable);
    let transtable = cx.item(sp, cx.ident_of("TRANSITION_TABLE"), Vec::new(),
            transtable);

    quote_method!(cx,
        fn follow(&self, current_state:uint, symbol:uint) -> uint {
            $transtable
            return TRANSITION_TABLE[current_state][symbol];
        }
    )
}

fn simple_accepting_method(cx:&mut ExtCtxt, sp:Span, lex:&Lexer) -> P<Method> {
    // * accepting: an array of N uints, giving the action associated to
    //   each state
    let ty_acctable = cx.ty(sp, ast::TyFixedLengthVec(
        cx.ty_ident(sp, cx.ident_of("uint")),
        cx.expr_uint(sp, lex.auto.states.len())));

    let mut acctable = Vec::new();
    for st in lex.auto.states.iter() {
        let acc_expr = cx.expr_uint(sp, st.action);
        acctable.push(acc_expr);
    }
    let acctable = cx.expr_vec(sp, acctable);
    let acctable = ast::ItemStatic(ty_acctable, ast::MutImmutable, acctable);
    let acctable = cx.item(sp, cx.ident_of("ACCEPTING"), Vec::new(),
            acctable);

    quote_method!(cx,
        fn accepting(&self, state:uint) -> uint {
            $acctable
            return ACCEPTING[state];
        }
    )
}

pub fn user_lexer_impl(cx: &mut ExtCtxt, sp: Span, lex:&Lexer) -> Vec<P<ast::Item>> {
    let actions_match = actions_match(lex.actions.as_slice(), lex.ident, cx, sp);
    let mut fields = Vec::with_capacity(lex.properties.len() + 1);

    for &(name, _, ref expr) in lex.properties.iter() {
        fields.push(cx.field_imm(sp, ast::Ident::new(name), expr.clone()));
    }

    let initial = lex.conditions[0].val1();
    fields.push(cx.field_imm(sp, ast::Ident::new(token::intern("_input")),
        quote_expr!(&*cx, ::rustlex::rt::RustLexLexer::new(reader))));
    fields.push(cx.field_imm(sp, ast::Ident::new(token::intern("_state")),
        quote_expr!(&*cx, $initial)));

    let init_expr = cx.expr_struct_ident(sp, lex.ident, fields);

    let follow_method:P<Method> = simple_follow_method(cx, sp, lex);
    let accepting_method:P<Method> = simple_accepting_method(cx, sp, lex);

    let conditions:Vec<P<Method>> = lex.conditions.iter().map(|&(cond,st)| {
        let cond = ast::Ident::new(cond);
        quote_method!(&*cx,
            #[inline(always)]
            #[allow(dead_code)]
            #[allow(non_snake_case)]
            fn $cond(&mut self) { self._state = $st; }
        )
    }).collect();

    let ident = lex.ident;
    let i1 = (quote_item!(cx,
    impl<R: ::std::io::Reader> $ident<R> {
        fn new(reader:R) -> Box<$ident<R>> {
            box $init_expr
        }

        #[inline(always)] $follow_method
        #[inline(always)] $accepting_method
        $conditions

        #[allow(dead_code)]
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
                let iter = self._input.inp.slice(buf + 1, nbuf).iter();
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
    )).unwrap();

    let tokens = lex.tokens.unwrap_or(ast::Ident::new(token::intern("Token")));
    let i2 = (quote_item!(cx,
    impl <R: ::std::io::Reader> Iterator<$tokens> for $ident<R> {
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

                    let new_st:uint = self.follow(current_st, i as uint);
                    let action_id:uint = self.accepting(new_st);

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
                debug!("action returned {}", action_result);

                match action_result {
                    Some(token) => return Some(token),
                    None => ()
                };
                // if the user code did not return, continue
            }
        }
    }
    )).unwrap();
    vec!(i1,i2)
}

