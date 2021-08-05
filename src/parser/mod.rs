use nom::branch::alt;
use nom::bytes::complete::{is_a, tag, take_till, take_until, take_while, take_while_m_n};
use nom::character::complete::{char, multispace0, space1};
//use nom::Finish;
use nom::combinator::{map, recognize, value};
use nom::multi::{many0, separated_list1};
use nom::sequence::{delimited, pair, tuple};

mod attlist;
mod element;
mod entity;

/// 被解析的字符数据（parsed character data）
/// 这PCDATA 是会被解析器解析的文本。些文本将被解析器检查实体以及标记。
///
/// 文本中的标签会被当作标记来处理，而实体会被展开。
///
/// 不过，被解析的字符数据不应当包含任何 &、< 或者 > 字符；需要使用 &amp;、&lt; 以及 &gt; 实体来分别替换它们。
#[derive(Debug)]
pub struct PCDATA<'i>(&'i str);

/// 字符数据（character data）。
///
/// CDATA 是不会被解析器解析的文本。在这些文本中的标签不会被当作标记来对待，其中的实体也不会被展开。
#[derive(Debug)]
pub struct CDATA<'i>(&'i str);

#[derive(Debug)]
pub struct CommentDecl;

/// '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
fn comment_decl(i: &str) -> nom::IResult<&str, CommentDecl> {
    map(
        value(
            (), // Output is thrown away.
            tuple((tag("<!--"), many0(char('-')), take_until("-->"), tag("-->"))),
        ),
        |_| CommentDecl,
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

#[derive(Debug)]
pub struct MixedPCDATA<'i>(Vec<NameOrReference<'i>>);

#[derive(Debug)]
pub enum NameOrReference<'i> {
    Name(Name<'i>),
    Reference(PEReference<'i>),
}

fn map_name(i: &str) -> nom::IResult<&str, NameOrReference> {
    map(name, |n| NameOrReference::Name(n))(i)
}

fn map_pereference(i: &str) -> nom::IResult<&str, NameOrReference> {
    map(pereference, |n| NameOrReference::Reference(n))(i)
}

fn name_or_reference(i: &str) -> Result<NameOrReference> {
    alt((map_name, map_pereference))(i)
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
fn pereference(i: &str) -> Result<PEReference<'_>> {
    map(delimited(tag("%"), name, tag(";")), |n| PEReference(n))(i)
}

/// EntityRef   ::= '&' Name ';'         [WFC: Entity Declared]
///                                      [VC: Entity Declared]
///                                      [WFC: Parsed Entity]
///                                      [WFC: No Recursion]
fn entity_ref(i: &str) -> Result<EntityRef<'_>> {
    map(tuple((tag("&"), name, tag(";"))), |(_, n, _)| EntityRef(n))(i)
}

#[derive(Debug)]
pub struct SystemLiteral<'i>(&'i str);

///	SystemLiteral	   ::=   	('"' [^"]* '"') | ("'" [^']* "'")
fn system_literal(i: &str) -> Result<SystemLiteral> {
    map(
        alt((
            delimited(char('"'), take_until("\""), char('"')),
            delimited(char('\''), take_until("'"), char('\'')),
        )),
        SystemLiteral,
    )(i)
}

#[derive(Debug)]
pub struct PubidLiteral<'i>(&'i str);

/// PubidLiteral	   ::=   	'"' PubidChar* '"' | "'" (PubidChar - "'")* "'"
fn pubid_literal(i: &str) -> Result<PubidLiteral> {
    map(
        alt((
            delimited(char('"'), take_till(is_pubid_char), char('"')),
            delimited(char('\''), take_till(is_pubid_char), char('\'')),
        )),
        |s| PubidLiteral(s),
    )(i)
}

/// PubidChar	   ::=   	#x20 | #xD | #xA | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%]
fn is_pubid_char(c: char) -> bool {
    !(c == ' '
        || c == '\r'
        || c == '\n'
        || c.is_ascii_alphanumeric()
        || "-'()+,./:=?;!*#@$_%".contains(c))
}

#[derive(Debug)]
pub enum ElementType<'i> {
    Element(element::ElementDecl<'i>),
    Entity(entity::EntityDecl<'i>),
    Attlist(attlist::AttlistDecl<'i>),
    Comment(CommentDecl),
}

pub fn parse(i: &str) -> Result<Vec<ElementType>> {
    many0(alt((
        map(
            delimited(multispace0, attlist::attlist_decl, multispace0),
            ElementType::Attlist,
        ),
        map(
            delimited(multispace0, element::element_decl, multispace0),
            ElementType::Element,
        ),
        map(
            delimited(multispace0, entity::entity_decl, multispace0),
            ElementType::Entity,
        ),
        map(
            delimited(multispace0, comment_decl, multispace0),
            ElementType::Comment,
        ),
    )))(i)
    // .finish()
}

#[cfg(test)]
mod tests {
    use nom::Finish;

    use super::{comment_decl, pereference};

    #[test]
    fn test_comment_decl() {
        let result = comment_decl(
            r#"<!--
======================================================================
    Docutils Generic DTD
======================================================================
:Author: David Goodger
:Contact: docutils-develop@lists.sourceforge.net
:Revision: $Revision: 8767 $
:Date: $Date: 2021-06-17 16:33:28 +0200 (Do, 17. Jun 2021) $
:Copyright: This DTD has been placed in the public domain.
:Filename: docutils.dtd

More information about this DTD (document type definition) and the
Docutils project can be found at http://docutils.sourceforge.net/.
The latest version of this DTD is available from
http://docutils.sourceforge.net/docs/ref/docutils.dtd.

The formal public identifier for this DTD is::

    +//IDN docutils.sourceforge.net//DTD Docutils Generic//EN//XML
-->"#,
        )
        .finish();
        assert!(
            result.is_ok(),
            "{}",
            result.as_ref().unwrap_err().to_string()
        );
    }

    #[test]
    fn test_pereference() {
        let result = pereference("%align-h.att;").finish();
        assert!(
            result.is_ok(),
            "{}",
            result.as_ref().unwrap_err().to_string()
        );
    }
}
