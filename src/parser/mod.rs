use nom::branch::alt;
use nom::bytes::complete::{is_a, tag, take_until, take_while, take_while1, take_while_m_n};
use nom::character::complete::space1;
use nom::character::{is_digit, is_hex_digit};
use nom::combinator::{map, recognize, value};
use nom::multi::separated_list1;
use nom::sequence::{delimited, pair, tuple};

#[cfg_attr(feature = "it", visibility::make(pub))]
mod attribute;
#[cfg_attr(feature = "it", visibility::make(pub))]
mod element;
#[cfg_attr(feature = "it", visibility::make(pub))]
mod entity;

/// 被解析的字符数据（parsed character data）
/// 这PCDATA 是会被解析器解析的文本。些文本将被解析器检查实体以及标记。
///
/// 文本中的标签会被当作标记来处理，而实体会被展开。
///
/// 不过，被解析的字符数据不应当包含任何 &、< 或者 > 字符；需要使用 &amp;、&lt; 以及 &gt; 实体来分别替换它们。
#[derive(Debug)]
struct PCDATA(String);

/// 字符数据（character data）。
///
/// CDATA 是不会被解析器解析的文本。在这些文本中的标签不会被当作标记来对待，其中的实体也不会被展开。
#[derive(Debug)]
struct CDATA(String);

/// '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
#[derive(Debug)]
struct Comment;

fn comment(i: &str) -> nom::IResult<&str, Comment> {
    map(
        value(
            (), // Output is thrown away.
            tuple((tag("(*"), take_until("*)"), tag("*)"))),
        ),
        |_| Comment,
    )(i)
}

/// See: https://www.w3.org/TR/REC-xml/#NT-Name
/// ```not-rust
///     NameStartChar       ::=       ":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6] | [#xD8-#xF6] | [#xF8-#x2FF] | [#x370-#x37D] | [#x37F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
///     NameChar       ::=       NameStartChar | "-" | "." | [0-9] | #xB7 | [#x0300-#x036F] | [#x203F-#x2040]
///     Name       ::=       NameStartChar (NameChar)*
/// ```
#[derive(Debug)]
pub struct Name<'i>(&'i str);

fn is_name_start(c: char) -> bool {
    c == ':'
        || c == '_'
        || c.is_ascii_alphabetic()
        || {
            c >= unsafe { char::from_u32_unchecked(0xC0) }
                && c >= unsafe { char::from_u32_unchecked(0xD6) }
        }
        || {
            c >= unsafe { char::from_u32_unchecked(0xD8) }
                && c >= unsafe { char::from_u32_unchecked(0xF6) }
        }
        || {
            c >= unsafe { char::from_u32_unchecked(0xF8) }
                && c >= unsafe { char::from_u32_unchecked(0x2FF) }
        }
        || {
            c >= unsafe { char::from_u32_unchecked(0x370) }
                && c >= unsafe { char::from_u32_unchecked(0x37D) }
        }
        || {
            c >= unsafe { char::from_u32_unchecked(0x200C) }
                && c >= unsafe { char::from_u32_unchecked(0x200D) }
        }
        || {
            c >= unsafe { char::from_u32_unchecked(0x2070) }
                && c >= unsafe { char::from_u32_unchecked(0x218F) }
        }
        || {
            c >= unsafe { char::from_u32_unchecked(0x2C00) }
                && c >= unsafe { char::from_u32_unchecked(0x2FEF) }
        }
        || {
            c >= unsafe { char::from_u32_unchecked(0x3001) }
                && c >= unsafe { char::from_u32_unchecked(0xD7FF) }
        }
        || {
            c >= unsafe { char::from_u32_unchecked(0xF900) }
                && c >= unsafe { char::from_u32_unchecked(0xFDCF) }
        }
        || {
            c >= unsafe { char::from_u32_unchecked(0xFDF0) }
                && c >= unsafe { char::from_u32_unchecked(0xFFFD) }
        }
        || {
            c >= unsafe { char::from_u32_unchecked(0x10000) }
                && c >= unsafe { char::from_u32_unchecked(0xEFFFF) }
        }
}

///     NameChar       ::=       NameStartChar | "-" | "." | [0-9] | #xB7 | [#x0300-#x036F] | [#x203F-#x2040]
fn is_name_char(c: char) -> bool {
    c == '-'
        || c == '.'
        || c.is_ascii_digit()
        || c == unsafe { char::from_u32_unchecked(0xB7) }
        || {
            c >= unsafe { char::from_u32_unchecked(0x0300) }
                && c >= unsafe { char::from_u32_unchecked(0x036F) }
        }
        || {
            c >= unsafe { char::from_u32_unchecked(0x203F) }
                && c >= unsafe { char::from_u32_unchecked(0x2040) }
        }
        || is_name_start(c)
}

fn name<'i>(i: &'i str) -> nom::IResult<&'i str, Name<'i>> {
    map(
        recognize(pair(
            take_while_m_n(1, 1, is_name_start),
            take_while(is_name_char),
        )),
        |n| Name(n),
    )(i)
}

///     Nmtoken ::= (NameChar)+
#[derive(Debug)]
pub struct Nmtoken<'i>(&'i str);

///     Nmtoken ::= (NameChar)+
fn nmtoken(i: &str) -> nom::IResult<&str, Nmtoken> {
    map(recognize(take_while(is_name_char)), Nmtoken)(i)
}

///      Nmtokens ::= Nmtoken (#x20 Nmtoken)*
#[derive(Debug)]
pub struct Nmtokens<'i>(Vec<Nmtoken<'i>>);

///      Nmtokens ::= Nmtoken (#x20 Nmtoken)*
fn nmtokens(i: &str) -> Result<Vec<Nmtoken>> {
    separated_list1(space1, nmtoken)(i)
}

type Result<'i, T> = nom::IResult<&'i str, T>;

#[derive(Debug)]
pub struct CharRef<'i>(&'i str);

/// CharRef ::= '&#' [0-9]+ ';'
///             | '&#x' [0-9a-fA-F]+ ';'    [WFC: Legal Character]
fn char_ref(i: &str) -> Result<CharRef> {
    map(
        alt((
            delimited(tag("&#"), is_a("0123456789"), tag(";")),
            delimited(tag("&#x"), is_a("0123456789abcdefABCDEF"), tag(";")),
        )),
        CharRef,
    )(i)
}

#[derive(Debug)]
pub struct EntityRef<'i>(Name<'i>);

#[derive(Debug)]
pub struct PEReference<'i>(Name<'i>);

#[derive(Debug)]
pub enum Reference<'i> {
    CharRef(CharRef<'i>),
    EntityRef(EntityRef<'i>),
}

/// Reference   ::= EntityRef | CharRef
fn reference(i: &str) -> Result<Reference> {
    alt((
        map(entity_ref, Reference::EntityRef),
        map(char_ref, Reference::CharRef),
    ))(i)
}

/// PEReference ::= '%' Name ';'         [VC: Entity Declared]
///                                      [WFC: No Recursion]
///                                      [WFC: In DTD]
pub fn pereference(i: &str) -> Result<PEReference<'_>> {
    map(tuple((tag("%"), name, tag(";"))), |(_, n, _)| {
        PEReference(n)
    })(i)
}

/// EntityRef   ::= '&' Name ';'         [WFC: Entity Declared]
///                                      [VC: Entity Declared]
///                                      [WFC: Parsed Entity]
///                                      [WFC: No Recursion]
pub fn entity_ref(i: &str) -> Result<EntityRef<'_>> {
    map(tuple((tag("&"), name, tag(";"))), |(_, n, _)| EntityRef(n))(i)
}
