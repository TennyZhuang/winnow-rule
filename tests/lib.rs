use logos::{Logos, Span};
use winnow::{
    error::ParserError,
    stream::{Stream, StreamIsPartial},
    token::any,
    PResult, Parser,
};
use winnow_rule::rule;

use TokenKind::*;

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
    fn parse_next(&mut self, input: &mut I) -> PResult<Token<'a>, E> {
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

fn tokenise(input: &str) -> Vec<Token> {
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
