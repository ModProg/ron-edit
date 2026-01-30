use crate::*;

#[apply(ast)]
#[derive(Display)]
pub enum Value<'s> {
    Int(Int<'s>),
    Float(Float<'s>),
    Str(Str<'s>),
    Char(Char<'s>),
    Bool(bool),
    List(List<'s>),
    Map(Map<'s>),
    Unit(&'s str),
    Tuple(Tuple<'s>),
    Struct(Struct<'s>),
}
pub(crate) fn value(input: &str) -> IResult<Value> {
    alt((
        map(value_int, Value::Int),
        map(value_float, Value::Float),
        map(value_str, Value::Str),
        map(value_char, Value::Char),
        map(value_list, Value::List),
        map(value_map, Value::Map),
        map(value_tuple, Value::Tuple),
        map(value_struct, Value::Struct),
        map(ident, Value::Unit),
    ))(input)
}

#[apply(ast)]
#[derive(Display)]
pub struct Int<'s>(&'s str);
fn value_int(s: &str) -> IResult<Int> {
    map(
        recognize(tuple((
            sign,
            alt((
                is_a("0123456789_"),
                preceded(
                    char('0'),
                    cut(preceded(is_a("box"), is_a("0123456789ABCDEFabcdef_"))),
                ),
            )),
        ))),
        Int,
    )(s)
}

fn sign(s: &str) -> IResult<Option<char>> {
    opt(alt((char('+'), char('-'))))(s)
}

#[apply(ast)]
#[derive(Display)]
pub struct Float<'s>(&'s str);
fn value_float(s: &str) -> IResult<Float> {
    map(
        // TODO test `double`
        recognize(tuple((
            opt(alt((char('+'), char('-')))),
            alt((
                tag("inf"),
                tag("NaN"),
                recognize(tuple((
                    alt((
                        recognize(tuple((digit1, opt(tuple((char('.'), digit0)))))),
                        recognize(tuple((char('.'), digit1))),
                    )),
                    opt(tuple((alt((char('e'), char('E'))), sign, digit1))),
                ))),
            )),
        ))),
        Float,
    )(s)
}

#[apply(ast)]
#[derive(Display)]
pub enum Str<'s> {
    #[display(fmt = "\"{_0}\"")]
    Baked(&'s str),
    #[display(fmt = "r{0}\"{content}\"{0}", "\"#\".repeat(*pounds)")]
    Raw { pounds: usize, content: &'s str },
}
fn value_str(s: &str) -> IResult<Str> {
    alt((
        map(
            preceded(
                char::<&str, Error>('"'),
                cut(terminated(
                    recognize(many0(alt((
                        tag("\\\""),
                        tag("\\\\"),
                        tag("\\"),
                        is_not("\\\""),
                    )))),
                    char('"'),
                )),
            ),
            Str::Baked,
        ),
        map(raw_string, |(pounds, content)| Str::Raw { pounds, content }),
    ))(s)
}

fn raw_string(s: &str) -> IResult<(usize, &str)> {
    let (s, _) = char('r')(s)?;
    let (s, pounds) = many0_count(char('#'))(s)?;
    let terminator = format!("\"{}", "#".repeat(pounds));
    let res = preceded(
        char('"'),
        cut(terminated(
            take_until(terminator.as_str()),
            tag(terminator.as_str()),
        )),
    )
    .map(|content| (pounds, content))
    .parse(s);
    res
}

#[apply(ast)]
#[derive(Display)]
#[display(fmt = "'{_0}'")]
pub struct Char<'s>(pub &'s str);
fn value_char(s: &str) -> IResult<Char> {
    map(
        preceded(
            char::<&str, Error>('\''),
            cut(terminated(tag("\\'").or(is_not("'")), char('\''))),
        ),
        Char,
    )(s)
}

#[apply(ast)]
#[derive(Display)]
#[display(fmt = "[{_0}]")]
pub struct List<'s>(pub Separated<'s, Value<'s>>);
fn value_list(s: &str) -> IResult<List> {
    map(
        preceded(char('['), cut(terminated(separated(value), char(']')))),
        List,
    )(s)
}

#[apply(ast)]
pub struct MapItem<'s> {
    pub attributes: Option<Vec<WsLead<'s, crate::InlineAttribute<'s>>>>,
    pub key: Value<'s>,
    pub after_key: Whitespace<'s>,
    pub value: WsLead<'s, Value<'s>>,
}

impl<'s> Display for MapItem<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(ref attrs) = self.attributes {
            for attr in attrs {
                write!(f, "{}", attr)?;
            }
        }
        write!(
            f,
            "{}{after_key}:{value}",
            self.key,
            after_key = self.after_key,
            value = self.value
        )
    }
}

#[apply(ast)]
#[derive(Display)]
#[display(fmt = "{{{_0}}}")]
pub struct Map<'s>(pub Separated<'s, MapItem<'s>>);

fn value_map(s: &str) -> IResult<Map> {
    context(
        "map",
        map(
            preceded(
                char('{'),
                cut(terminated(
                    separated(
                        tuple((
                            ws,
                            many0(terminated(ws_lead(inline_attribute), ws)),
                            value,
                            ws,
                            char(':'),
                            ws_lead(value),
                        ))
                        .map(
                            |(_leading_ws, attributes, key, after_key, _, value)| {
                                MapItem {
                                    attributes: if attributes.is_empty() {
                                        None
                                    } else {
                                        Some(attributes)
                                    },
                                    key,
                                    after_key,
                                    value,
                                }
                            },
                        ),
                    ),
                    char('}'),
                )),
            ),
            Map,
        ),
    )(s)
}

#[apply(ast)]
#[derive(Display)]
#[display(fmt = "{}({fields})", "FormatOption(ident)")]
pub struct Tuple<'s> {
    pub ident: Option<WsFollowed<'s, &'s str>>,
    pub fields: Separated<'s, Value<'s>>,
}
fn value_tuple(s: &str) -> IResult<Tuple> {
    map(
        pair(
            opt(ws_followed(ident)),
            delimited(char('('), separated(value), char(')')),
        ),
        |(ident, fields)| Tuple { ident, fields },
    )(s)
}

#[apply(ast)]
#[derive(Display)]
#[display(fmt = "{key}{after_key}:{value}")]
pub struct NamedField<'s> {
    pub key: &'s str,
    pub after_key: Whitespace<'s>,
    pub value: WsLead<'s, Value<'s>>,
}
#[apply(ast)]
#[derive(Display)]
#[display(fmt = "{}({fields})", "FormatOption(ident)")]
pub struct Struct<'s> {
    pub ident: Option<WsFollowed<'s, &'s str>>,
    pub fields: Separated<'s, NamedField<'s>>,
}
fn value_struct(s: &str) -> IResult<Struct> {
    context(
        "value struct",
        map(
            pair(
                opt(ws_followed(ident)),
                delimited(
                    char('('),
                    separated(map(
                        tuple((ident, terminated(ws, char(':')), ws_lead(value))),
                        |(key, after_key, value)| NamedField {
                            key,
                            after_key,
                            value,
                        },
                    )),
                    char(')'),
                ),
            ),
            |(ident, fields)| Struct { ident, fields },
        ),
    )(s)
}
