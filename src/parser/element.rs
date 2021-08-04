use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{space0, space1},
    combinator::map,
    multi::{many0, many1},
    sequence::{terminated, tuple},
};

use super::{name, pereference, Name, PEReference};

/// 元素是 XML 以及 HTML 文档的主要构建模块。
///
/// HTML 元素的例子是 "body" 和 "table"。
/// XML 元素的例子是 "note" 和 "message" 。
/// 元素可包含文本、其他元素或者是空的。空的 HTML 元素的例子是 "hr"、"br" 以及 "img"。
#[derive(Debug)]
pub struct Element<'i> {
    name: Name<'i>,
    category: ElementCategory<'i>,
}

#[derive(Debug)]
pub enum ElementCategory<'i> {
    Empty,
    PCDATA,
    CDATA,
    Any,
    Mixed(MixedPCDATA<'i>),
    Children(Repeatable<Child<'i>>),
}

#[derive(Debug)]
pub struct SubElements<'i>(Vec<Repeatable<Element<'i>>>);

#[derive(Debug)]
pub enum Repeatable<T> {
    AtLeastOnce(T),
    AtMostOnce(T),
    ZeroOrManyTimes(T),
}

#[derive(Debug)]
pub struct MixedPCDATA<'i>(Vec<NameOrRef<'i>>);

#[derive(Debug)]
pub enum NameOrRef<'i> {
    Name(Name<'i>),
    Ref(PEReference<'i>),
}

fn map_name(i: &str) -> nom::IResult<&str, NameOrRef> {
    map(name, |n| NameOrRef::Name(n))(i)
}

fn map_pereference(i: &str) -> nom::IResult<&str, NameOrRef> {
    map(pereference, |n| NameOrRef::Ref(n))(i)
}

/// Mixed	   ::=   	'(' S? '#PCDATA' (S? '|' S? Name)* S? ')*'
/// 			| '(' S? '#PCDATA' S? ')'
fn mixed(i: &str) -> nom::IResult<&str, ElementCategory> {
    map(
        tuple((
            tuple((tag("("), space0, tag("#PCDATA"), space0)),
            alt((
                map(tuple((space0, tag(")"))), |_| MixedPCDATA(Vec::new())),
                map(
                    terminated(
                        many0(tuple((
                            space0,
                            tag("|"),
                            space0,
                            alt((map_name, map_pereference)),
                        ))),
                        tuple((space0, tag(")*"))),
                    ),
                    |x| MixedPCDATA(x.into_iter().map(|(_, _, _, x)| x).collect()),
                ),
            )),
        )),
        |(_, x)| ElementCategory::Mixed(x),
    )(i)
}

#[derive(Debug)]
pub struct Seq<T>(Vec<T>);

#[derive(Debug)]
pub struct Choices<T>(Vec<T>);

#[derive(Debug)]
pub enum Child<'i> {
    Name(Name<'i>),
    Seq(Seq<Repeatable<Child<'i>>>),
    Choices(Choices<Repeatable<Child<'i>>>),
}

///   	children	   ::=   	(choice | seq) ('?' | '*' | '+')?
fn children(i: &str) -> nom::IResult<&str, ElementCategory> {
    map(
        tuple((
            alt((map(choices, Child::Choices), map(sequence, Child::Seq))),
            alt((tag("?"), tag("*"), tag("+"))),
        )),
        |(child, repeatable)| {
            let repeatable = match repeatable {
                "?" => |child| Repeatable::AtMostOnce(child),
                "*" => |child| Repeatable::ZeroOrManyTimes(child),
                "+" => |child| Repeatable::AtLeastOnce(child),
                _ => unreachable!(),
            };
            ElementCategory::Children(repeatable(child))
        },
    )(i)
}

///   	seq	   ::=   	'(' S? cp ( S? ',' S? cp )* S? ')'	[VC: Proper Group/PE Nesting]
fn sequence(i: &str) -> nom::IResult<&str, Seq<Repeatable<Child>>> {
    map(
        tuple((
            tag("("),
            space0,
            cuple,
            many0(tuple((space0, tag(","), space0, cuple))),
            space0,
            tag(")"),
        )),
        |(_, _, cp, many, _, _)| {
            Seq({
                std::iter::once(cp)
                    .chain(many.into_iter().map(|(_, _, _, x)| x))
                    .collect()
            })
        },
    )(i)
}

///   	choice	   ::=   	'(' S? cp ( S? '|' S? cp )+ S? ')'	[VC: Proper Group/PE Nesting]
fn choices(i: &str) -> nom::IResult<&str, Choices<Repeatable<Child>>> {
    map(
        tuple((
            tag("("),
            space0,
            cuple,
            many1(tuple((space0, tag("|"), space0, cuple))),
            space0,
            tag(")"),
        )),
        |(_, _, cp, many, _, _)| {
            Choices(
                std::iter::once(cp)
                    .chain(many.into_iter().map(|(_, _, _, x)| x))
                    .collect(),
            )
        },
    )(i)
}

///   	cp	   ::=   	(Name | choice | seq) ('?' | '*' | '+')?
fn cuple(i: &str) -> nom::IResult<&str, Repeatable<Child>> {
    map(
        tuple((
            alt((
                map(name, Child::Name),
                map(choices, Child::Choices),
                map(sequence, Child::Seq),
            )),
            alt((tag("?"), tag("*"), tag("+"))),
        )),
        |(child, repeatable)| {
            let repeatable = match repeatable {
                "?" => |child| Repeatable::AtMostOnce(child),
                "*" => |child| Repeatable::ZeroOrManyTimes(child),
                "+" => |child| Repeatable::AtLeastOnce(child),
                _ => unreachable!(),
            };
            repeatable(child)
        },
    )(i)
}

fn empty(i: &str) -> nom::IResult<&str, ElementCategory> {
    map(tag("EMPTY"), |_| ElementCategory::Empty)(i)
}

fn any(i: &str) -> nom::IResult<&str, ElementCategory> {
    map(tag("ANY"), |_| ElementCategory::Any)(i)
}

/// <!ELEMENT 元素名称 类别>
/// ```
/// use nom::Finish;
/// use dtd::parser::element::{element, Element};
/// use dtd::parser::Name;
/// let el = element("<!ELEMENT b (#PCDATA)>");
/// assert!(el.is_ok(), "{}", el.as_ref().unwrap_err().to_string());
/// let el = element("<!ELEMENT p (#PCDATA|a|ul|b|i|em)*>").finish();
/// assert!(el.is_ok(), "{}", el.as_ref().unwrap_err().to_string());
/// let el = element("<!ELEMENT p (#PCDATA | %font; | %phrase; | %special; | %form;)* >").finish();
/// assert!(el.is_ok(), "{}", el.as_ref().unwrap_err().to_string());
/// ```
pub fn element(i: &str) -> nom::IResult<&str, Element> {
    map(
        tuple((
            tag("<!ELEMENT"),
            space1,
            name,
            space1,
            alt((empty, any, mixed, children)),
            space0,
            tag(">"),
        )),
        |(_, _, n, _, c, _, _)| Element {
            name: n,
            category: c,
        },
    )(i)
}

// <!ELEMENT 元素名称 (元素内容)>
// 空元素通过类别关键词EMPTY进行声明：
// <!ELEMENT 元素名称 EMPTY>
// 只有 PCDATA 的元素通过圆括号中的 #PCDATA 进行声明：
// <!ELEMENT 元素名称 (#PCDATA)>
// 通过类别关键词 ANY 声明的元素，可包含任何可解析数据的组合：
// <!ELEMENT 元素名称 ANY>
// 带有一个或多个子元素的元素通过圆括号中的子元素名进行声明：
// <!ELEMENT 元素名称 (子元素名称 1)>
// 或者
// <!ELEMENT 元素名称 (子元素名称 1,子元素名称 2,.....)>
// 声明只出现一次的元素
// <!ELEMENT 元素名称 (子元素名称)>
// 例子：
// <!ELEMENT note (message)>
// 上面的例子声明了：message 子元素必须出现一次，并且必须只在 "note" 元素中出现一次。
//
// 声明最少出现一次的元素
// <!ELEMENT 元素名称 (子元素名称+)>
// 例子：
// <!ELEMENT note (message+)>
// 上面的例子中的加号声明了：message 子元素必须在 "note" 元素内出现至少一次。
//
// 声明出现零次或多次的元素
// <!ELEMENT 元素名称 (子元素名称*)>
// 例子：
// <!ELEMENT note (message*)>
// 上面的例子中的星号声明了：子元素 message 可在 "note" 元素内出现零次或多次。
//
// 声明出现零次或一次的元素
// <!ELEMENT 元素名称 (子元素名称?)>
// 例子：
// <!ELEMENT note (message?)>
// 上面的例子中的问号声明了：子元素 message 可在 "note" 元素内出现零次或一次。
//
// 声明“非.../既...”类型的内容
// 例子：
// <!ELEMENT note (to,from,header,(message|body))>
// 上面的例子声明了："note" 元素必须包含 "to" 元素、"from" 元素、"header" 元素，以及非 "message" 元素既 "body" 元素。
//
// 声明混合型的内容
// 例子：
// <!ELEMENT note (#PCDATA|to|from|header|message)*>
// 上面的例子声明了："note" 元素可包含出现零次或多次的 PCDATA、"to"、"from"、"header" 或者 "message"。
