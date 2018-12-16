use lexer::Lexer;
use lexer::Prop;
use regex;
use proc_macro::{Literal, TokenNode};
use proc_macro2::{Span, Term, TokenStream};
use quote::{quote, quote_spanned, Tokens};
use std::iter::FromIterator;

pub fn lexer_struct(ident: Term, props: &[Prop]) -> Tokens {
    let ident = ident;
    let fields: TokenStream = props.iter().flat_map(|&(name, ref ty, _)| {
        let ty = ty.clone();
        quote!{ pub #name: #ty, }
    }).collect();

    quote!{
        #[allow(missing_docs)]
        pub struct #ident<R: ::std::io::Read> {
            pub _input: ::rustlex::rt::RustLexLexer<R>,
            pub _state: usize,
            #fields
        }
    }
}

pub fn codegen<'cx>(lex: &Lexer) -> ::proc_macro::TokenStream {
    let strukt = lexer_struct(lex.ident, &lex.properties);
    // functions of the Lexer and InputBuffer structs
    let impls = user_lexer_impl(lex);
    let items = quote_spanned!{Span::def_site()=>
        #strukt
        #impls
    };

    info!("done!");
    items.into()
}

pub fn actions_match(lex:&Lexer) -> Tokens {
    let tokens = lex.tokens;
    let ident = lex.ident;

    let mut i = 1usize;
    let action_type = quote!( Fn(&mut #ident<R>) -> Option<#tokens>);

    let arms: TokenStream = lex.actions.iter().skip(1).flat_map(|act| {
        let lit = i;
        let act = act.clone();
        let arm = quote_spanned!{Span::call_site()=>
            #lit => (Box::new(#act)) as Box<#action_type>,
        };
        // FIXME: should use enumerate
        i += 1;
        arm
    }).collect();

    quote_spanned!{Span::call_site()=>
        match last_matching_action {
            #arms
            x => unreachable!()
        }
    }
}

fn simple_follow_method(lex: &Lexer) -> Tokens {
    let ident = lex.ident;
    let len = lex.auto.states.len();
    // * transtable: an array of N arrays of 256 uints, N being the number
    //   of states in the FSM, which gives the transitions between states
    let transtable: TokenStream = lex.auto.states.iter().flat_map(|st| {
        let vec: TokenStream = st.trans.iter().flat_map(|&i| {
            quote!{ #i, }
        }).collect();
        quote!{ [ #vec ], }
    }).collect();

    quote!{
        impl<R: ::std::io::Read> #ident<R> {
            #[inline(always)]
            fn follow(&self, current_state: usize, symbol: usize) -> usize {
                static TRANSITION_TABLE: [[usize; 256]; #len] = [ #transtable ];
                return TRANSITION_TABLE[current_state][symbol];
            }
        }
    }
}

fn simple_accepting_method(lex: &Lexer) -> Tokens {
    let ident = lex.ident;
    let len = lex.auto.states.len();
    // * accepting: an array of N uints, giving the action associated to
    //   each state
    let acctable: TokenStream = lex.auto.states.iter().flat_map(|st| {
        let regex::Action(act) = st.data;
        quote!{ #act, }
    }).collect();

    quote!{
        impl<R: ::std::io::Read> #ident<R> {
            #[inline(always)]
            fn accepting(&self, state: usize) -> usize {
                static ACCEPTING: [usize; #len] = [ #acctable ];
                return ACCEPTING[state];
            }
        }
    }
}

pub fn user_lexer_impl(lex: &Lexer) -> Tokens {
    let tokens = lex.tokens;
    let ident = lex.ident;
    let initial = lex.auto.initials[lex.conditions[0].1];

    let fields: TokenStream = lex.properties.iter().flat_map(|&(name, _, ref expr)| {
        let name = name;
        let expr = expr.clone();
        quote!{ #name: #expr }
    }).collect();

    // condition methods
    let conds: TokenStream = lex.conditions.iter().flat_map(|&(cond,st)| {
        let cond = cond;
        let init = lex.auto.initials[st];
        quote!{
            impl<R: ::std::io::Read> #ident<R> {
                #[inline(always)]
                #[allow(dead_code)]
                #[allow(non_snake_case)]
                fn #cond(&mut self) {
                    self._state = #init;
                }
            }
        }
    }).collect();

    let follow = simple_follow_method(lex);
    let accept = simple_accepting_method(lex);
    let actions = actions_match(lex);

    quote_spanned!{Span::def_site()=>
        impl<R: ::std::io::Read> #ident<R> {
            pub fn new(reader:R) -> #ident<R> {
                #ident {
                    _input: ::rustlex::rt::RustLexLexer::new(reader),
                    _state: #initial,
                    #fields
                }
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

        #conds

        #follow
        #accept

        impl <R: ::std::io::Read> Iterator for #ident<R> {
            type Item = #tokens;

            fn next(&mut self) -> Option<#tokens> {
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
                    let action_result = #actions(self);

                    match action_result {
                        Some(token) => return Some(token),
                        None => ()
                    };
                    // if the user code did not return, continue
                }
            }
        }
    }
}

