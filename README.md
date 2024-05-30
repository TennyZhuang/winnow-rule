# winnow-rule

[![Documentation](https://docs.rs/winnow-rule/badge.svg)](https://docs.rs/winnow-rule/)
[![Crates.io](https://img.shields.io/crates/v/winnow-rule.svg)](https://crates.io/crates/winnow-rule)
[![LICENSE from upstream](https://img.shields.io/github/license/andylokandy/nom-rule.svg)](https://github.com/andylokandy/nom-rule/blob/master/LICENSE)

A procedural macro for defining [winnow][winnow] combinators in simple DSL. Requires `winnow` v0.6+.

[winnow][winnow] is a fork of [nom][nom], see [Why `winnow`?][why-winnow] for more details.

Accordingly, winnow-rule is also a fork of [nom-rule][nom-rule], the main purpose is to create a similar DSL supporting [winnow][winnow], and then we may also add more advanced features to it.

Due to the difference between `nom` and `winnow`, the syntax between `nom-rule` and `winnow-rule` are not fully compatible. However, they are both designed for experiences similar to regular expressions.

Currently, the crate is under active development, and the syntax is not stable.

[winnow]: https://crates.io/crates/winnow
[nom]: https://crates.io/crates/nom
[nom-rule]: https://crates.io/crates/nom-rule
[why-winnow]: https://docs.rs/winnow/latest/winnow/_topic/why/index.html#nom

## Dependencies

```toml
[dependencies]
winnow = "0.6.13"
winnow-rule = "0.1"
```

## Syntax

The procedural macro `rule!` provided by this crate is designed for the ease of writing grammar spec as well as to improve maintainability, it follows these simple rules:

1. `#fn_name`: an external [`winnow::Parser`][winnow-parser]. In the example **below**, `ident` and `TokenKind::*` are predefined parsers.
2. `a ~ b ~ c`: a sequence of parsers to take one by one. It'll get expanded into `(a, b, c)`.
3. `(...)+`: one or more repeated patterns. It'll get expanded into `winnow::combinator::repeat(1.., #next)`.
4. `(...)*`: zero or more repeated patterns. It'll get expanded into `winnow::combinator::repeat(0.., #next)`.
5. `(...)?`: Optional parser. It'll get expanded into `winnow::combinator::opt`.
6. `a | b | c`: Choices between a, b, and c. It'll get expanded into `winnow::combinator::alt`.
7. `&a`: Peek. It'll get expanded into `winnow::combinator::peek(a)`. Note that it doesn't consume the input.
8. `!a`: Negative predicate. It'll get expanded into `winnow::combinator::not`. Note that it doesn't consume the input.
9. `^a`:  Cut parser. It'll get expanded into `winnow::combinator::cut_err`.
10. `... : "description"`: Context description for error reporting. It'll get expanded into [`winnow::Parser::context`][winnow-parser-context].

[winnow-parser]: https://docs.rs/winnow/latest/winnow/trait.Parser.html
[winnow-parser-context]: https://docs.rs/winnow/latest/winnow/trait.Parser.html#method.context

## Example

Implement [`winnow::Parser`][winnow-parser] for your `TokenKind`, so that it can be used as an external parser described above.

```rust
use winnow::{Parser, PResult, Stream};

#[derive(Clone, Debug, PartialEq)]
struct Token<'a> {
    kind: TokenKind,
    text: &'a str,
    span: Span,
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum TokenKind {
    Whitespace,

    // Keywords
    CREATE,
    TABLE,

    // Symbols
    LParen,
    RParen,
    Semicolon,
    Comma,

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
```

To define a parser for the SQL of creating table:

```rust
use winnow_rule::rule;
use TokenKind::*;

let mut rule = rule!(
    CREATE ~ TABLE ~ #ident ~ ^"(" ~ (#ident ~ #ident ~ ","?)* ~ ")" ~ ";" : "CREATE TABLE statement"
);
```

It will get expanded into:

```rust
    let mut rule = ((
        CREATE,
        TABLE,
        ident,
        winnow::combinator::cut_err(LParen),
        winnow::combinator::repeat(
            0..,
            ((ident, ident, winnow::combinator::opt(Comma))),
        ),
        RParen,
        Semicolon,
    ))
    .context(winnow::error::StrContext::Label("CREATE TABLE statement"));
```

> See more example in `tests/lib.rs`.

## Roadmap

There are several features in plan:

* Implement auto-sequence feature in [nom-rule][nom-rule].
* Implement Capturing group syntax, to reduce the unused output, e.g. `#CREATE ~ #TABLE ~ (#ident)` can only output the captured table name. The feature can significantly reduce the complexity for caller.
* Considering add back `match_text` and `match_token` in [nom-rule][nom-rule]. Although winnow can define an external parser easily, it may still be useful in some cases.
