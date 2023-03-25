//! Nom parser trying to preserve all information
//!
//! Probably the "AST" structure will be reworked quite a bit.
use std::fmt::{self, Display, Formatter};

use derive_more::Display;
use macro_rules_attribute::{apply, attribute_alias};
use nom::branch::*;
use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::combinator::*;
use nom::error::{context, VerboseError};
use nom::multi::*;
use nom::sequence::*;
use nom::{Finish, Parser};
#[cfg(test)]
use serde::Serialize;
use unicode_ident::{is_xid_continue, is_xid_start};
pub use value::{Char, Int, List, Map, MapItem, NamedField, Str, Struct, Tuple, Value};

type Error<'a> = VerboseError<&'a str>;
type IResult<'a, T> = nom::IResult<&'a str, T, Error<'a>>;

mod value;

attribute_alias! {
    #[apply(ast)] =
        #[cfg_attr(test, derive(Serialize))]
        #[derive(Debug, PartialEq)];
}

#[apply(ast)]
pub struct File<'s> {
    pub extentions: Vec<WsLead<'s, Attribute<'s>>>,
    pub value: WsLead<'s, Value<'s>>,
    pub trailing_ws: Whitespace<'s>,
}

impl Display for File<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.extentions.iter().try_for_each(|e| write!(f, "{e}"))?;
        write!(f, "{}{}", self.value, self.trailing_ws)
    }
}

impl<'s> TryFrom<&'s str> for File<'s> {
    type Error = String;

    fn try_from(s: &'s str) -> Result<Self, Self::Error> {
        match file(s).finish() {
            Ok(("", file)) => Ok(file),
            Ok((rem, _)) => Err(format!("unexpected: {rem}")),
            Err(e) => Err(e.to_string()),
        }
    }
}

#[apply(ast)]
#[derive(Display)]
#[display(fmt = "#{after_pound}!\
           {after_exclamation}[{after_bracket}enable{after_enable}({extentions}){after_paren}]")]
pub struct Attribute<'s> {
    pub after_pound: Whitespace<'s>,
    pub after_exclamation: Whitespace<'s>,
    pub after_bracket: Whitespace<'s>,
    pub after_enable: Whitespace<'s>,
    pub extentions: Separated<'s, &'s str>,
    pub after_paren: Whitespace<'s>,
}

#[apply(ast)]
pub struct WsLead<'s, T> {
    pub leading: Whitespace<'s>,
    pub content: T,
}
impl<T: Display> Display for WsLead<'_, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.leading, self.content)
    }
}
fn ws_lead<'a, T>(
    t: impl Parser<&'a str, T, Error<'a>>,
) -> impl Parser<&'a str, WsLead<'a, T>, Error<'a>> {
    map(pair(ws, t), |(leading, content)| WsLead {
        leading,
        content,
    })
}

#[apply(ast)]
#[derive(Default)]
pub struct WsFollowed<'s, T> {
    pub content: T,
    pub following: Whitespace<'s>,
}
impl<T: Display> Display for WsFollowed<'_, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.content, self.following)
    }
}
fn ws_followed<'a, T>(
    t: impl Parser<&'a str, T, Error<'a>>,
) -> impl Parser<&'a str, WsFollowed<'a, T>, Error<'a>> {
    map(pair(t, ws), |(content, following)| WsFollowed {
        content,
        following,
    })
}

#[apply(ast)]
#[derive(Default)]
pub struct WsWrapped<'s, T> {
    pub leading: Whitespace<'s>,
    pub content: T,
    pub following: Whitespace<'s>,
}
impl<T: Display> Display for WsWrapped<'_, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}{}", self.leading, self.content, self.following)
    }
}
fn ws_wrapped<'a, T>(
    t: impl Parser<&'a str, T, Error<'a>>,
) -> impl Parser<&'a str, WsWrapped<'a, T>, Error<'a>> {
    map(tuple((ws, t, ws)), |(leading, content, following)| {
        WsWrapped {
            leading,
            content,
            following,
        }
    })
}

#[apply(ast)]
pub enum Ws<'s> {
    LineComment(&'s str),
    Space(&'s str),
    BlockComment(&'s str),
}
impl Display for Ws<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Ws::LineComment(c) => write!(f, "//{c}"),
            Ws::Space(s) => write!(f, "{s}"),
            Ws::BlockComment(c) => write!(f, "/*{c}*/"),
        }
    }
}
impl Ws<'_> {
    pub fn is_comment(&self) -> bool {
        !matches!(self, Self::Space(_))
    }

    pub fn is_line_comment(&self) -> bool {
        matches!(self, Self::LineComment(_))
    }
}

#[apply(ast)]
#[derive(Default)]
pub struct Whitespace<'s>(pub Vec<Ws<'s>>);

impl Display for Whitespace<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.0.iter().try_for_each(|ws| write!(f, "{ws}"))
    }
}

#[apply(ast)]
pub struct Separated<'s, T> {
    pub values: Vec<WsWrapped<'s, T>>,
    /// When formatting with [`Display`] comma is only shown when
    /// [`Self::values`] is not empty.
    pub trailing_comma: bool,
    pub trailing_ws: Whitespace<'s>,
}

impl<T: Display> Display for Separated<'_, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let Self {
            values,
            trailing_comma,
            trailing_ws,
        } = self;
        let Some((last, rest)) = values.split_last() else {
            return write!(f, "{trailing_ws}")
        };
        rest.iter().try_for_each(|i| write!(f, "{i},"))?;
        write!(f, "{last}")?;
        if *trailing_comma {
            write!(f, ",")?;
        }
        write!(f, "{trailing_ws}")
    }
}

#[apply(ast)]
pub struct ExtentionIdent {}

fn file(input: &str) -> IResult<File> {
    map(
        tuple((
            context("attributes", many0(ws_lead(extention_attr))),
            context("value", ws_lead(value::value)),
            ws,
        )),
        |(extentions, value, trailing_ws)| File {
            extentions,
            value,
            trailing_ws,
        },
    )(input)
}

#[allow(unused)]
fn dbg_dmp<'a, O>(
    context: impl Display,
    mut p: impl Parser<&'a str, O, Error<'a>>,
) -> impl FnMut(&'a str) -> IResult<O> {
    move |s: &str| match p.parse(s) {
        Err(nom::Err::Error(c)) => {
            println!("{}: Parsing Error: {} at:\n{:?}", context, c, s);
            Err(nom::Err::Error(c))
        }
        Err(e) => {
            println!("{}: Error({}) at:\n{:?}", context, e, s);
            Err(e)
        }
        a => a,
    }
}

fn extention_attr(input: &str) -> IResult<Attribute> {
    map(
        tuple((
            char('#'),
            ws,
            char('!'),
            ws,
            char('['),
            ws,
            tag("enable"),
            ws,
            char('('),
            separated(ident),
            char(')'),
            ws,
            char(']'),
        )),
        |(
            _,
            after_pound,
            _,
            after_exclamation,
            _,
            after_bracket,
            _,
            after_ident,
            _,
            extentions,
            _,
            after_paren,
            _,
        )| {
            Attribute {
                after_pound,
                after_exclamation,
                extentions,
                after_bracket,
                after_enable: after_ident,
                after_paren,
            }
        },
    )(input)
}

fn ident(input: &str) -> IResult<&str> {
    alt((
        recognize(pair(
            tag("r#"),
            cut(many1(alt((take_while1(is_xid_continue), is_a("+-."))))),
        )),
        recognize(pair(
            // TODO make issue about adding a take_one(fn(char)->bool)
            alt((tag("_"), take_while_m_n(1, 1, is_xid_start))),
            take_while(is_xid_continue),
        )),
    ))(input)
}

fn block_comment(input: &str) -> IResult<Ws> {
    map(
        delimited(
            tag("/*"),
            recognize(many0(alt((
                is_not("*/"),
                recognize(block_comment),
                take_until1("*/"),
            )))),
            tag("*/"),
        ),
        Ws::BlockComment,
    )(input)
}

fn ws(input: &str) -> IResult<Whitespace> {
    map(
        many0(alt((
            map(is_a("\n\t\r "), Ws::Space),
            map(
                preceded(tag("//"), recognize(pair(is_not("\n"), line_ending))),
                Ws::LineComment,
            ),
            block_comment,
        ))),
        Whitespace,
    )(input)
}

fn separated<'a, T, P: Parser<&'a str, T, Error<'a>>>(
    p: P,
) -> impl FnMut(&'a str) -> IResult<Separated<T>> {
    context(
        "separated",
        map(
            tuple((
                separated_list0(char(','), ws_wrapped(p)),
                opt(char(',')),
                ws,
            )),
            |(values, trailing_comma, trailing_ws)| Separated {
                values,
                trailing_comma: trailing_comma.is_some(),
                trailing_ws,
            },
        ),
    )
}

struct FormatOption<'a, T>(&'a Option<T>);
impl<T: Display> Display for FormatOption<'_, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.0 {
            Some(t) => write!(f, "{t}"),
            None => Ok(()),
        }
    }
}

#[cfg(test)]
mod test {
    use include_dir::include_dir;

    use super::*;

    const INPUTS: include_dir::Dir = include_dir!("$CARGO_MANIFEST_DIR/tests/input");

    #[test]
    fn whitespace() {
        assert_eq!(
            block_comment("/* a */ ").finish().unwrap(),
            (" ", Ws::BlockComment(" a ".into()))
        );
        assert_eq!(
            block_comment("/* a /* inner */ */ ").finish().unwrap(),
            (" ", Ws::BlockComment(" a /* inner */ ".into()))
        );
        assert_eq!(
            ws("/* *//* a /* inner */ */").finish().unwrap(),
            (
                "",
                Whitespace(vec![
                    Ws::BlockComment(" ".into()),
                    Ws::BlockComment(" a /* inner */ ".into())
                ])
            )
        );
    }

    #[test]
    fn ensure_valid_ron() {
        for file in INPUTS.entries() {
            let _: ron::Value = ron::de::from_bytes(file.as_file().unwrap().contents()).unwrap();
        }
    }

    #[test]
    fn parse() {
        for file in INPUTS.entries() {
            let contents = file.as_file().unwrap().contents_utf8().unwrap();
            let ron: File = contents.try_into().map_err(|e| eprintln!("{e}")).unwrap();

            assert_eq!(ron.to_string(), contents);

            insta::with_settings!({snapshot_path => "../tests/snapshots"},{
                insta::assert_ron_snapshot!(file.path().display().to_string(), ron);
            })
        }
    }
}
