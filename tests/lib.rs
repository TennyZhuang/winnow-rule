use logos::{Logos, Span};
use winnow::{
    error::{ContextError, ErrMode, ModalResult as PResult, ParserError, Result as ParseResult},
    stream::{Stream, StreamIsPartial},
    token::any,
    Parser,
};
use winnow_rule::rule;

use TokenKind::*;

#[test]
fn simple_sequence_preserves_order_and_shape() {
    let tokens = tokenise("create table user");

    let mut rule = rule!(#CREATE ~ #TABLE ~ #ident);

    let res: PResult<_> = rule.parse_next(&mut &tokens[..]);
    assert_eq!(
        res.unwrap(),
        (
            Token {
                kind: CREATE,
                text: "create",
                span: 0..6,
            },
            Token {
                kind: TABLE,
                text: "table",
                span: 7..12,
            },
            "user",
        ),
    );
}

#[test]
fn sequence_discards_remove_tuple_noise() {
    let tokens = tokenise("create table user");

    let mut rule = rule!(_:#CREATE ~ _:#TABLE ~ #ident);

    let res: PResult<_> = rule.parse_next(&mut &tokens[..]);
    assert_eq!(res.unwrap(), ("user",));
}

#[test]
fn sql_create_table() {
    let tokens = tokenise("create table user (id int, name varchar);");

    let mut rule = rule!(
        #CREATE ~ #TABLE ~ #ident ~ ^#LParen ~ (#ident ~ #ident ~ #Comma?)* ~ #RParen ~ #Semicolon : "CREATE TABLE statement"
    );

    let res: PResult<_> = rule.parse_next(&mut &tokens[..]);
    assert_eq!(
        res.unwrap(),
        (
            Token {
                kind: CREATE,
                text: "create",
                span: 0..6,
            },
            Token {
                kind: TABLE,
                text: "table",
                span: 7..12,
            },
            "user",
            Token {
                kind: LParen,
                text: "(",
                span: 18..19,
            },
            vec![
                (
                    "id",
                    "int",
                    Some(Token {
                        kind: Comma,
                        text: ",",
                        span: 25..26
                    })
                ),
                ("name", "varchar", None),
            ],
            Token {
                kind: RParen,
                text: ")",
                span: 39..40,
            },
            Token {
                kind: Semicolon,
                text: ";",
                span: 40..41,
            },
        ),
    );
}

#[test]
fn choice_uses_winnow_alt() {
    let tokens = tokenise("table");

    let mut rule = rule!(#CREATE | #TABLE);

    let res: PResult<_> = rule.parse_next(&mut &tokens[..]);
    assert_eq!(
        res.unwrap(),
        Token {
            kind: TABLE,
            text: "table",
            span: 0..5,
        },
    );
}

#[test]
fn repeat_zero_or_more_returns_vec_and_allows_empty() {
    let empty = tokenise("");
    let mut empty_rule = rule!(#TABLE*);
    let empty_res: PResult<Vec<_>> = empty_rule.parse_next(&mut &empty[..]);
    assert_eq!(empty_res.unwrap(), Vec::<Token<'_>>::new());

    let tokens = tokenise("table table table");
    let mut rule = rule!(#TABLE*);

    let res: PResult<Vec<_>> = rule.parse_next(&mut &tokens[..]);
    assert_eq!(
        res.unwrap(),
        vec![
            Token {
                kind: TABLE,
                text: "table",
                span: 0..5,
            },
            Token {
                kind: TABLE,
                text: "table",
                span: 6..11,
            },
            Token {
                kind: TABLE,
                text: "table",
                span: 12..17,
            },
        ],
    );
}

#[test]
fn repeat_one_or_more_requires_a_match() {
    let empty = tokenise("");
    let mut empty_rule = rule!(#TABLE+);
    let empty_res: PResult<Vec<_>> = empty_rule.parse_next(&mut &empty[..]);
    assert!(empty_res.is_err());

    let tokens = tokenise("table table");
    let mut rule = rule!(#TABLE+);

    let res: PResult<Vec<_>> = rule.parse_next(&mut &tokens[..]);
    assert_eq!(
        res.unwrap(),
        vec![
            Token {
                kind: TABLE,
                text: "table",
                span: 0..5,
            },
            Token {
                kind: TABLE,
                text: "table",
                span: 6..11,
            },
        ],
    );
}

#[test]
fn optional_returns_none_without_consuming_input() {
    let tokens = tokenise("create");
    let mut input = &tokens[..];

    let mut rule = rule!(#TABLE? ~ #CREATE);

    let res: PResult<_> = rule.parse_next(&mut input);
    assert_eq!(
        res.unwrap(),
        (
            None,
            Token {
                kind: CREATE,
                text: "create",
                span: 0..6,
            },
        ),
    );
    assert_eq!(input, &[]);
}

#[test]
fn peek_does_not_consume_input() {
    let tokens = tokenise("create");
    let mut input = &tokens[..];

    let mut rule = rule!(&#CREATE ~ #CREATE);

    let res: PResult<_> = rule.parse_next(&mut input);
    assert_eq!(
        res.unwrap(),
        (
            Token {
                kind: CREATE,
                text: "create",
                span: 0..6,
            },
            Token {
                kind: CREATE,
                text: "create",
                span: 0..6,
            },
        ),
    );
    assert_eq!(input, &[]);
}

#[test]
fn not_does_not_consume_input_on_success() {
    let tokens = tokenise("create table");
    let mut input = &tokens[..];

    let mut rule = rule!(!#TABLE ~ #CREATE ~ #TABLE);

    let res: PResult<_> = rule.parse_next(&mut input);
    assert_eq!(
        res.unwrap(),
        (
            (),
            Token {
                kind: CREATE,
                text: "create",
                span: 0..6,
            },
            Token {
                kind: TABLE,
                text: "table",
                span: 7..12,
            },
        ),
    );
    assert_eq!(input, &[]);
}

#[test]
fn not_fails_when_the_predicate_matches() {
    let tokens = tokenise("table");
    let mut rule = rule!(!#TABLE ~ #CREATE);

    let res: PResult<_> = rule.parse_next(&mut &tokens[..]);
    assert!(res.is_err());
}

#[test]
fn cut_and_context_report_a_labeled_cut() {
    let tokens = tokenise("table");
    let mut rule = rule!(^#CREATE : "expected CREATE");

    let res: Result<Token<'_>, ErrMode<ContextError>> = rule.parse_next(&mut &tokens[..]);
    assert!(matches!(res, Err(ErrMode::Cut(_))));

    let debug = format!("{res:?}");
    assert!(debug.contains("expected CREATE"), "{debug}");
}

#[test]
fn long_alt_more_than_ten_branches_still_compiles_and_selects() {
    let tokens = tokenise("eleven");

    let mut rule = rule!(
        #ONE
            | #TWO
            | #THREE
            | #FOUR
            | #FIVE
            | #SIX
            | #SEVEN
            | #EIGHT
            | #NINE
            | #TEN
            | #ELEVEN
    );

    let res: PResult<_> = rule.parse_next(&mut &tokens[..]);
    assert_eq!(
        res.unwrap(),
        Token {
            kind: ELEVEN,
            text: "eleven",
            span: 0..6,
        },
    );
}

#[test]
fn nested_combination_stays_composable() {
    let tokens = tokenise("create table");
    let mut rule = rule!((#CREATE ~ #TABLE) | (#TABLE ~ #TABLE));

    let res: PResult<_> = rule.parse_next(&mut &tokens[..]);
    assert_eq!(
        res.unwrap(),
        (
            Token {
                kind: CREATE,
                text: "create",
                span: 0..6,
            },
            Token {
                kind: TABLE,
                text: "table",
                span: 7..12,
            },
        ),
    );
}

#[test]
fn token_stream_paths_discards_and_context_work_together() {
    let tokens = tokenise("(value as int)");

    let mut rule = cast_rule();

    let res: PResult<_> = rule.parse_next(&mut &tokens[..]);
    assert_eq!(res.unwrap(), ("value", "int"));
}

#[test]
fn token_stream_cut_and_context_error_on_missing_data_type() {
    let tokens = tokenise("(value as )");

    let mut rule = cast_rule();

    let res: Result<(&str, &str), ErrMode<ContextError>> = rule.parse_next(&mut &tokens[..]);
    assert!(matches!(res, Err(ErrMode::Cut(_))));

    let debug = format!("{res:?}");
    assert!(debug.contains("CAST expression"), "{debug}");
}

#[derive(Logos, Clone, Copy, Debug, PartialEq)]
enum TokenKind {
    #[error]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Whitespace,

    // Keywords
    #[token("CREATE", ignore(ascii_case))]
    CREATE,
    #[token("TABLE", ignore(ascii_case))]
    TABLE,
    #[token("AS", ignore(ascii_case))]
    AS,
    #[token("one", ignore(ascii_case))]
    ONE,
    #[token("two", ignore(ascii_case))]
    TWO,
    #[token("three", ignore(ascii_case))]
    THREE,
    #[token("four", ignore(ascii_case))]
    FOUR,
    #[token("five", ignore(ascii_case))]
    FIVE,
    #[token("six", ignore(ascii_case))]
    SIX,
    #[token("seven", ignore(ascii_case))]
    SEVEN,
    #[token("eight", ignore(ascii_case))]
    EIGHT,
    #[token("nine", ignore(ascii_case))]
    NINE,
    #[token("ten", ignore(ascii_case))]
    TEN,
    #[token("eleven", ignore(ascii_case))]
    ELEVEN,

    // Symbols
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token(";")]
    Semicolon,
    #[token(",")]
    Comma,

    #[regex("[a-zA-Z][a-zA-Z0-9]*")]
    Ident,
}

impl<'a, I, E> Parser<I, Token<'a>, E> for TokenKind
where
    I: Stream<Token = Token<'a>> + StreamIsPartial,
    E: ParserError<I>,
{
    fn parse_next(&mut self, input: &mut I) -> ParseResult<Token<'a>, E> {
        any.verify(|t: &Token<'a>| t.kind == *self)
            .parse_next(input)
    }
}

#[derive(Clone, PartialEq, Debug)]
struct Token<'a> {
    kind: TokenKind,
    text: &'a str,
    span: Span,
}

fn tokenise(input: &str) -> Vec<Token<'_>> {
    let mut lex = TokenKind::lexer(input);
    let mut tokens = Vec::new();

    while let Some(kind) = lex.next() {
        tokens.push(Token {
            kind,
            text: lex.slice(),
            span: lex.span(),
        })
    }

    tokens
}

type Input<'a> = &'a [Token<'a>];

fn ident<'a>(input: &mut Input<'a>) -> PResult<&'a str> {
    any.verify_map(|token: Token| {
        if token.kind == TokenKind::Ident {
            Some(token.text)
        } else {
            None
        }
    })
    .parse_next(input)
}

fn expr_name<'a>(input: &mut Input<'a>) -> PResult<&'a str> {
    ident.parse_next(input)
}

fn data_type_name<'a>(input: &mut Input<'a>) -> PResult<&'a str> {
    ident.parse_next(input)
}

fn cast_rule<'a>() -> impl Parser<Input<'a>, (&'a str, &'a str), ErrMode<ContextError>> {
    rule!(
        _:#TokenParser::LParen
            ~ #expr_name
            ~ _:#KeywordParser::AS
            ~ ^#data_type_name
            ~ _:#TokenParser::RParen
            : "CAST expression"
    )
}

struct TokenParser;

#[allow(non_upper_case_globals)]
impl TokenParser {
    const LParen: TokenKind = TokenKind::LParen;
    const RParen: TokenKind = TokenKind::RParen;
}

struct KeywordParser;

#[allow(non_upper_case_globals)]
impl KeywordParser {
    const AS: TokenKind = TokenKind::AS;
}
