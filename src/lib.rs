use pratt::{Affix, Associativity, PrattError, PrattParser, Precedence};
use proc_macro2::{Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};
use proc_macro_error::{abort, abort_call_site, proc_macro_error};
use quote::{quote, ToTokens, TokenStreamExt};
use syn::{punctuated::Punctuated, Token};
use winnow::{
    combinator::{alt, opt, repeat, separated, trace},
    error::ContextError,
    token::any,
    PResult, Parser,
};
use wrapper::InputWrapper;

mod wrapper;

#[proc_macro]
#[proc_macro_error]
pub fn rule(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let tokens: TokenStream = tokens.into();
    let i: Vec<TokenTree> = tokens.into_iter().collect();

    let rule = parse_rule(i.iter().cloned().collect());
    rule.check_return_type();
    rule.to_token_stream().into()
}

#[derive(Debug, Clone)]
struct Path {
    segments: Vec<Ident>,
}

#[derive(Debug, Clone)]
enum Rule {
    MatchText(Span, Literal),
    MatchToken(Span, Path),
    ExternalFunction(Span, Path, Option<Group>),
    Context(Span, Literal, Box<Rule>),
    Peek(Span, Box<Rule>),
    Not(Span, Box<Rule>),
    Opt(Span, Box<Rule>),
    Cut(Span, Box<Rule>),
    Many0(Span, Box<Rule>),
    Many1(Span, Box<Rule>),
    Sequence(Span, Vec<Rule>),
    Alt(Span, Vec<Rule>),
}

#[derive(Debug, Clone)]
enum RuleElement {
    MatchText(Literal),
    MatchToken(Path),
    ExternalFunction(Path, Option<Group>),
    Context(Literal),
    Peek,
    Not,
    Opt,
    Cut,
    Many0,
    Many1,
    Sequence,
    Alt,
    SubRule(Rule),
}

#[derive(Debug, Clone)]
struct WithSpan {
    elem: RuleElement,
    span: Span,
}

#[derive(Debug, Clone)]
enum ReturnType {
    Option(Box<ReturnType>),
    Vec(Box<ReturnType>),
    Unit,
    Unknown,
}

type Input<'a> = InputWrapper<'a>;

fn match_punct<'a>(punct: char) -> impl Parser<Input<'a>, TokenTree, ContextError> {
    trace(
        punct,
        any.verify_map(move |token| match token {
            TokenTree::Punct(ref p) if p.as_char() == punct => Some(token.clone()),
            _ => None,
        }),
    )
}

fn group<'a>(input: &mut Input<'a>) -> PResult<Group> {
    any.verify_map(move |token| match token {
        TokenTree::Group(ref group) => Some(group.clone()),
        _ => None,
    })
    .parse_next(input)
}

fn literal<'a>(input: &mut Input<'a>) -> PResult<Literal> {
    any.verify_map(move |token| match token {
        TokenTree::Literal(ref lit) => Some(lit.clone()),
        _ => None,
    })
    .parse_next(input)
}

fn ident<'a>(input: &mut Input<'a>) -> PResult<Ident> {
    trace(
        "ident",
        any.verify_map(move |token| match token {
            TokenTree::Ident(ref ident) => Some(ident.clone()),
            _ => None,
        }),
    )
    .parse_next(input)
}

fn path<'a>(input: &mut Input<'a>) -> PResult<(Span, Path)> {
    separated(1.., ident, (match_punct(':'), match_punct(':')))
        .map(|segments: Vec<_>| {
            let span = segments[1..]
                .iter()
                .fold(segments[0].span(), |acc, segment| {
                    acc.join(segment.span()).unwrap()
                })
                .unwrap()
                .into();
            let path = Path { segments };
            (span, path)
        })
        .parse_next(input)
}

fn parse_rule(tokens: TokenStream) -> Rule {
    let i: Vec<TokenTree> = tokens.into_iter().collect();
    let i = &mut InputWrapper(&i[..]);

    let elems: Vec<_> = repeat(0.., parse_rule_element).parse_next(i).unwrap();
    let i = i.0;
    if !i.is_empty() {
        let rest: TokenStream = i.iter().cloned().collect();
        abort!(rest, "unable to parse the following rules: {}", rest);
    }

    let mut iter = elems.into_iter().peekable();
    let rule = unwrap_pratt(RuleParser.parse(&mut iter));
    if iter.peek().is_some() {
        let rest: Vec<_> = iter.collect();
        abort!(
            rest[0].span,
            "unable to parse the following rules: {:?}",
            rest
        );
    }

    rule
}

fn parse_rule_element<'a>(i: &mut Input<'a>) -> PResult<WithSpan> {
    let function_call = |i: &mut Input<'a>| {
        let hashtag = match_punct('#').parse_next(i)?;
        let (path_span, fn_path) = path(i)?;
        let args = opt(group).parse_next(i)?;
        let span = hashtag.span().join(path_span).unwrap();
        let span = args
            .as_ref()
            .map(|args| args.span().join(span).unwrap())
            .unwrap_or(span);

        Ok(WithSpan {
            elem: RuleElement::ExternalFunction(fn_path, args),
            span,
        })
    };
    let context = (match_punct(':'), literal).map(|(colon, msg)| {
        let span = colon.span().join(msg.span()).unwrap();
        WithSpan {
            elem: RuleElement::Context(msg),
            span,
        }
    });
    alt((
        match_punct('|').map(|token| WithSpan {
            span: token.span(),
            elem: RuleElement::Alt,
        }),
        match_punct('*').map(|token| WithSpan {
            span: token.span(),
            elem: RuleElement::Many0,
        }),
        match_punct('+').map(|token| WithSpan {
            span: token.span(),
            elem: RuleElement::Many1,
        }),
        match_punct('?').map(|token| WithSpan {
            span: token.span(),
            elem: RuleElement::Opt,
        }),
        match_punct('^').map(|token| WithSpan {
            span: token.span(),
            elem: RuleElement::Cut,
        }),
        match_punct('&').map(|token| WithSpan {
            span: token.span(),
            elem: RuleElement::Peek,
        }),
        match_punct('!').map(|token| WithSpan {
            span: token.span(),
            elem: RuleElement::Not,
        }),
        match_punct('~').map(|token| WithSpan {
            span: token.span(),
            elem: RuleElement::Sequence,
        }),
        literal.map(|lit| WithSpan {
            span: lit.span(),
            elem: RuleElement::MatchText(lit),
        }),
        path.map(|(span, p)| WithSpan {
            span,
            elem: RuleElement::MatchToken(p),
        }),
        group.map(|group| WithSpan {
            span: group.span(),
            elem: RuleElement::SubRule(parse_rule(group.stream())),
        }),
        function_call,
        context,
    ))
    .parse_next(i)
}

fn unwrap_pratt(res: Result<Rule, PrattError<WithSpan, pratt::NoError>>) -> Rule {
    match res {
        Ok(res) => res,
        Err(PrattError::EmptyInput) => abort_call_site!("expected more tokens for rule"),
        Err(PrattError::UnexpectedNilfix(input)) => {
            abort!(input.span, "unable to parse the value")
        }
        Err(PrattError::UnexpectedPrefix(input)) => {
            abort!(input.span, "unable to parse the prefix operator")
        }
        Err(PrattError::UnexpectedInfix(input)) => {
            abort!(input.span, "unable to parse the binary operator")
        }
        Err(PrattError::UnexpectedPostfix(input)) => {
            abort!(input.span, "unable to parse the postfix operator")
        }
        Err(PrattError::UserError(_)) => unreachable!(),
    }
}

struct RuleParser;

impl<I: Iterator<Item = WithSpan>> PrattParser<I> for RuleParser {
    type Error = pratt::NoError;
    type Input = WithSpan;
    type Output = Rule;

    fn query(&mut self, elem: &WithSpan) -> pratt::Result<Affix> {
        let affix = match elem.elem {
            RuleElement::Alt => Affix::Infix(Precedence(1), Associativity::Left),
            RuleElement::Context(_) => Affix::Postfix(Precedence(2)),
            RuleElement::Sequence => Affix::Infix(Precedence(3), Associativity::Left),
            RuleElement::Opt => Affix::Postfix(Precedence(4)),
            RuleElement::Many1 => Affix::Postfix(Precedence(4)),
            RuleElement::Many0 => Affix::Postfix(Precedence(4)),
            RuleElement::Cut => Affix::Prefix(Precedence(5)),
            RuleElement::Peek => Affix::Prefix(Precedence(5)),
            RuleElement::Not => Affix::Prefix(Precedence(5)),
            _ => Affix::Nilfix,
        };
        Ok(affix)
    }

    fn primary(&mut self, elem: WithSpan) -> pratt::Result<Rule> {
        let rule = match elem.elem {
            RuleElement::SubRule(rule) => rule,
            RuleElement::MatchText(text) => Rule::MatchText(elem.span, text),
            RuleElement::MatchToken(token) => Rule::MatchToken(elem.span, token),
            RuleElement::ExternalFunction(func, args) => {
                Rule::ExternalFunction(elem.span, func, args)
            }
            _ => unreachable!(),
        };
        Ok(rule)
    }

    fn infix(&mut self, lhs: Rule, elem: WithSpan, rhs: Rule) -> pratt::Result<Rule> {
        let rule = match elem.elem {
            RuleElement::Sequence => match lhs {
                Rule::Sequence(span, mut seq) => {
                    let span = span.join(elem.span).unwrap().join(rhs.span()).unwrap();
                    seq.push(rhs);
                    Rule::Sequence(span, seq)
                }
                lhs => {
                    let span = lhs.span().join(rhs.span()).unwrap();
                    Rule::Sequence(span, vec![lhs, rhs])
                }
            },
            RuleElement::Alt => match lhs {
                Rule::Alt(span, mut choices) => {
                    let span = span.join(elem.span).unwrap().join(rhs.span()).unwrap();
                    choices.push(rhs);
                    Rule::Alt(span, choices)
                }
                lhs => {
                    let span = lhs.span().join(rhs.span()).unwrap();
                    Rule::Alt(span, vec![lhs, rhs])
                }
            },
            _ => unreachable!(),
        };
        Ok(rule)
    }

    fn prefix(&mut self, elem: WithSpan, rhs: Rule) -> pratt::Result<Rule> {
        let rule = match elem.elem {
            RuleElement::Cut => {
                let span = elem.span.join(rhs.span()).unwrap();
                Rule::Cut(span, Box::new(rhs))
            }
            RuleElement::Peek => {
                let span = elem.span.join(rhs.span()).unwrap();
                Rule::Peek(span, Box::new(rhs))
            }
            RuleElement::Not => {
                let span = elem.span.join(rhs.span()).unwrap();
                Rule::Not(span, Box::new(rhs))
            }
            _ => unreachable!(),
        };
        Ok(rule)
    }

    fn postfix(&mut self, lhs: Rule, elem: WithSpan) -> pratt::Result<Rule> {
        let rule = match elem.elem {
            RuleElement::Opt => {
                let span = lhs.span().join(elem.span).unwrap();
                Rule::Opt(span, Box::new(lhs))
            }
            RuleElement::Many0 => {
                let span = lhs.span().join(elem.span).unwrap();
                Rule::Many0(span, Box::new(lhs))
            }
            RuleElement::Many1 => {
                let span = lhs.span().join(elem.span).unwrap();
                Rule::Many1(span, Box::new(lhs))
            }
            RuleElement::Context(msg) => {
                let span = lhs.span().join(elem.span).unwrap();
                Rule::Context(span, msg, Box::new(lhs))
            }
            _ => unreachable!(),
        };
        Ok(rule)
    }
}

impl std::fmt::Display for ReturnType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ReturnType::Option(ty) => write!(f, "Option<{}>", ty),
            ReturnType::Vec(ty) => write!(f, "Vec<{}>", ty),
            ReturnType::Unit => write!(f, "()"),
            ReturnType::Unknown => write!(f, "_"),
        }
    }
}

impl PartialEq for ReturnType {
    fn eq(&self, other: &ReturnType) -> bool {
        match (self, other) {
            (ReturnType::Option(lhs), ReturnType::Option(rhs)) => lhs == rhs,
            (ReturnType::Vec(lhs), ReturnType::Vec(rhs)) => lhs == rhs,
            (ReturnType::Unit, ReturnType::Unit) => true,
            (ReturnType::Unknown, _) => true,
            (_, ReturnType::Unknown) => true,
            _ => false,
        }
    }
}

impl Rule {
    fn check_return_type(&self) -> ReturnType {
        match self {
            Rule::MatchText(_, _) | Rule::MatchToken(_, _) | Rule::ExternalFunction(_, _, _) => {
                ReturnType::Unknown
            }
            Rule::Context(_, _, rule) | Rule::Peek(_, rule) => rule.check_return_type(),
            Rule::Not(_, _) => ReturnType::Unit,
            Rule::Opt(_, rule) => ReturnType::Option(Box::new(rule.check_return_type())),
            Rule::Cut(_, rule) => rule.check_return_type(),
            Rule::Many0(_, rule) | Rule::Many1(_, rule) => {
                ReturnType::Vec(Box::new(rule.check_return_type()))
            }
            Rule::Sequence(_, rules) => {
                rules.iter().for_each(|rule| {
                    rule.check_return_type();
                });
                ReturnType::Vec(Box::new(ReturnType::Unknown))
            }
            Rule::Alt(_, rules) => {
                for slice in rules.windows(2) {
                    match (slice[0].check_return_type(), slice[1].check_return_type()) {
                        (ReturnType::Option(_), _) => {
                            abort!(
                                slice[0].span(),
                                "optional shouldn't be in a choice because it will shortcut the following branches",
                            )
                        }
                        (a, b) if a != b => abort!(
                            slice[0].span().join(slice[1].span()).unwrap(),
                            "type mismatched between {:} and {:}",
                            a,
                            b,
                        ),
                        _ => (),
                    }
                }
                ReturnType::Vec(Box::new(rules[0].check_return_type()))
            }
        }
    }

    fn span(&self) -> Span {
        match self {
            Rule::MatchText(span, _)
            | Rule::MatchToken(span, _)
            | Rule::ExternalFunction(span, _, _)
            | Rule::Context(span, _, _)
            | Rule::Peek(span, _)
            | Rule::Not(span, _)
            | Rule::Opt(span, _)
            | Rule::Cut(span, _)
            | Rule::Many0(span, _)
            | Rule::Many1(span, _)
            | Rule::Sequence(span, _)
            | Rule::Alt(span, _) => *span,
        }
    }

    fn to_tokens(&self, tokens: &mut TokenStream) {
        let token = match self {
            Rule::ExternalFunction(_, name, arg) => {
                quote! { #name #arg }
            }
            Rule::Context(_, msg, rule) => {
                let rule = rule.to_token_stream();
                quote! { #rule.context(winnow::error::StrContext::Label(#msg)) }
            }
            Rule::Peek(_, rule) => {
                let rule = rule.to_token_stream();
                quote! { winnow::combinator::peek(#rule) }
            }
            Rule::Not(_, rule) => {
                let rule = rule.to_token_stream();
                quote! { winnow::combinator::not(#rule) }
            }
            Rule::Opt(_, rule) => {
                let rule = rule.to_token_stream();
                quote! { winnow::combinator::opt(#rule) }
            }
            Rule::Cut(_, rule) => {
                let rule = rule.to_token_stream();
                quote! { winnow::combinator::cut_err(#rule) }
            }
            Rule::Many0(_, rule) => {
                let rule = rule.to_token_stream();
                quote! { winnow::combinator::repeat(0.., #rule) }
            }
            Rule::Many1(_, rule) => {
                let rule = rule.to_token_stream();
                quote! { winnow::combinator::repeat(1.., #rule) }
            }
            Rule::Sequence(_, rules) => {
                let list: Punctuated<TokenStream, Token![,]> =
                    rules.iter().map(|rule| rule.to_token_stream()).collect();
                quote! { ((#list)) }
            }
            Rule::Alt(_, rules) => {
                let list: Punctuated<TokenStream, Token![,]> =
                    rules.iter().map(|rule| rule.to_token_stream()).collect();
                quote! { nom::branch::alt((#list)) }
            }
            _ => unimplemented!(),
        };

        tokens.extend(token);
    }

    fn to_token_stream(&self) -> TokenStream {
        let mut tokens = TokenStream::new();
        self.to_tokens(&mut tokens);
        tokens
    }
}

impl ToTokens for Path {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for (i, segment) in self.segments.iter().enumerate() {
            if i > 0 {
                // Double colon `::`
                tokens.append(Punct::new(':', Spacing::Joint));
                tokens.append(Punct::new(':', Spacing::Alone));
            }
            segment.to_tokens(tokens);
        }
    }
}
