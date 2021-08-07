#[macro_use]
extern crate derive_more;

use std::path::{Path, PathBuf};

use either::Either;
use indexmap::IndexMap;
use nom::branch::alt;
use nom::bytes::complete::{is_a, tag, take_till, take_until, take_while, take_while_m_n};
use nom::character::complete::{anychar, char, multispace0, multispace1};
use nom::combinator::{eof, iterator, map, recognize, value};
use nom::error::ErrorKind;
use nom::multi::{many0, separated_list1};
use nom::sequence::{delimited, pair, terminated, tuple};
use nom::Finish;
use nom_greedyerror::{convert_error, GreedyError};
use nom_locate::LocatedSpan;
#[cfg(feature = "trace")]
use nom_tracable::{cumulative_histogram, histogram};
use nom_tracable::{tracable_parser, TracableInfo};

mod attlist;
mod element;
mod entity;

type Span<'i> = LocatedSpan<&'i str, TracableInfo>;

type Result<'i, T> = nom::IResult<Span<'i>, T, GreedyError<Span<'i>, ErrorKind>>;

#[cfg(test)]
fn span(i: &str) -> Span {
    let extra = TracableInfo::new();
    Span::new_extra(i, extra)
}

/// like nom::dbg_dmp but eat &str.
fn dbg_dmp<'i, F, O, E: std::fmt::Debug>(
    mut f: F,
    context: &'static str,
) -> impl FnMut(&'i str) -> nom::IResult<&'i str, O, E>
where
    F: FnMut(&'i str) -> nom::IResult<&'i str, O, E>,
{
    move |i: &'i str| match f(i) {
        Err(e) => {
            println!("{}: Error({:?}) at:\n{}", context, e, i);
            Err(e)
        }
        a => a,
    }
}

/// 被解析的字符数据（parsed character data）
/// 这PCDATA 是会被解析器解析的文本。些文本将被解析器检查实体以及标记。
///
/// 文本中的标签会被当作标记来处理，而实体会被展开。
///
/// 不过，被解析的字符数据不应当包含任何 &、< 或者 > 字符；需要使用 &amp;、&lt; 以及 &gt; 实体来分别替换它们。
#[derive(Debug, Display, AsMut, AsRef, Deref, DerefMut, Into)]
pub struct PCDATA(String);

/// 字符数据（character data）。
///
/// CDATA 是不会被解析器解析的文本。在这些文本中的标签不会被当作标记来对待，其中的实体也不会被展开。
#[derive(Debug, Display, AsMut, AsRef, Deref, DerefMut, Into)]
pub struct CDATA(String);

/// How namy accurrences of child.
#[derive(Debug, Display)]
pub enum Repeatable<T> {
    /// Occur once.
    #[display(fmt = "{}+", "_0")]
    Once(T),
    /// Occur once or more times.
    #[display(fmt = "{}+", "_0")]
    AtLeastOnce(T),
    /// Optional
    #[display(fmt = "{}?", "_0")]
    AtMostOnce(T),
    /// Not occurring or occurring more than once
    #[display(fmt = "{}*", "_0")]
    ZeroOrManyTimes(T),
}

#[derive(Debug, Display, AsMut, AsRef)]
pub struct CommentDecl;

/// '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
#[tracable_parser]
fn comment_decl(i: Span) -> Result<CommentDecl> {
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
#[derive(Debug, Display, AsMut, AsRef, Deref, DerefMut, Into)]
pub struct Name(String);

impl Name {
    fn to_string(&self) -> String {
        self.0.to_string()
    }
}

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

#[tracable_parser]
fn name(i: Span) -> Result<Name> {
    map(
        recognize(pair(
            take_while_m_n(1, 1, is_name_start),
            take_while(is_name_char),
        )),
        |n: Span| Name(n.to_string()),
    )(i)
}

///     Nmtoken ::= (NameChar)+
#[derive(Debug, Display, AsMut, AsRef, Deref, DerefMut, Into)]
pub struct Nmtoken(String);

///     Nmtoken ::= (NameChar)+
#[tracable_parser]
fn nmtoken(i: Span) -> Result<Nmtoken> {
    map(recognize(take_while(is_name_char)), |s: Span| {
        Nmtoken(s.to_string())
    })(i)
}

///      Nmtokens ::= Nmtoken (#x20 Nmtoken)*
#[derive(Debug, AsMut, AsRef, Deref, DerefMut, Into)]
pub struct Nmtokens(Vec<Nmtoken>);

///      Nmtokens ::= Nmtoken (#x20 Nmtoken)*
#[tracable_parser]
fn nmtokens(i: Span) -> Result<Vec<Nmtoken>> {
    separated_list1(multispace1, nmtoken)(i)
}

#[derive(Debug, AsMut, AsRef, Deref, DerefMut, Into)]
pub struct MixedPCDATA(Vec<NameOrReference>);

#[derive(Debug, TryInto)]
pub enum NameOrReference {
    Name(Name),
    Reference(PEReference),
}

#[tracable_parser]
fn map_name(i: Span) -> Result<NameOrReference> {
    map(name, |n| NameOrReference::Name(n))(i)
}

#[tracable_parser]
fn map_pereference(i: Span) -> Result<NameOrReference> {
    map(pereference, |n| NameOrReference::Reference(n))(i)
}

/*
fn name_or_reference(i: &str) -> Result<NameOrReference> {
    alt((map_name, map_pereference))(i)
}
*/

#[derive(Debug, Display, TryInto)]
pub enum CharRef {
    #[display(fmt = "{}", "_0")]
    Decimal(isize),
    #[display(fmt = "{:x}", "_0")]
    Hexadecimal(isize),
}

/// CharRef ::= '&#' [0-9]+ ';'
///             | '&#x' [0-9a-fA-F]+ ';'    [WFC: Legal Character]
#[tracable_parser]
fn char_ref(i: Span) -> Result<CharRef> {
    alt((
        map(
            delimited(tag("&#"), is_a("0123456789"), tag(";")),
            |v: Span| CharRef::Decimal(isize::from_str_radix(&v, 10).unwrap()),
        ),
        map(
            delimited(tag("&#x"), is_a("0123456789abcdefABCDEF"), tag(";")),
            |v: Span| CharRef::Hexadecimal(isize::from_str_radix(&v, 16).unwrap()),
        ),
    ))(i)
}

#[derive(Debug, Display, AsMut, AsRef, Deref, DerefMut, Into)]
pub struct EntityRef(Name);

#[derive(Debug, Display, AsMut, AsRef, Deref, DerefMut, Into)]
pub struct PEReference(Name);

#[derive(Debug, Display, TryInto)]
pub enum Reference {
    #[display(fmt = "{}", "_0")]
    CharRef(CharRef),
    #[display(fmt = "&{};", "_0")]
    EntityRef(EntityRef),
}

/// Reference   ::= EntityRef | CharRef
#[tracable_parser]
fn reference(i: Span) -> Result<Reference> {
    alt((
        map(entity_ref, Reference::EntityRef),
        map(char_ref, Reference::CharRef),
    ))(i)
}

/// PEReference ::= '%' Name ';'         [VC: Entity Declared]
///                                      [WFC: No Recursion]
///                                      [WFC: In DTD]
#[tracable_parser]
fn pereference(i: Span) -> Result<PEReference> {
    map(delimited(tag("%"), name, tag(";")), |n| PEReference(n))(i)
}

/// EntityRef   ::= '&' Name ';'         [WFC: Entity Declared]
///                                      [VC: Entity Declared]
///                                      [WFC: Parsed Entity]
///                                      [WFC: No Recursion]
#[tracable_parser]
fn entity_ref(i: Span) -> Result<EntityRef> {
    map(tuple((tag("&"), name, tag(";"))), |(_, n, _)| EntityRef(n))(i)
}

#[derive(Debug, Display, AsMut, AsRef, Deref, DerefMut, Into)]
pub struct SystemLiteral(String);

///	SystemLiteral	   ::=   	('"' [^"]* '"') | ("'" [^']* "'")
#[tracable_parser]
fn system_literal(i: Span) -> Result<SystemLiteral> {
    map(
        alt((
            delimited(char('"'), take_until("\""), char('"')),
            delimited(char('\''), take_until("'"), char('\'')),
        )),
        |sl: Span| SystemLiteral(sl.to_string()),
    )(i)
}

#[derive(Debug, Display, AsMut, AsRef, Deref, DerefMut, Into)]
pub struct PubidLiteral(String);

/// PubidLiteral	   ::=   	'"' PubidChar* '"' | "'" (PubidChar - "'")* "'"
#[tracable_parser]
fn pubid_literal(i: Span) -> Result<PubidLiteral> {
    map(
        alt((
            delimited(char('"'), take_till(is_pubid_char), char('"')),
            delimited(char('\''), take_till(is_pubid_char), char('\'')),
        )),
        |s: Span| PubidLiteral(s.to_string()),
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

#[derive(Debug, TryInto)]
pub enum ElementType {
    Element(element::ElementDecl),
    Entity(entity::EntityDecl),
    Attlist(attlist::AttlistDecl),
    Comment(CommentDecl),
}

fn entity_definitions(i: Span) -> IndexMap<String, entity::PEDecl> {
    iterator(i, alt((map(entity::pedecl, Some), map(anychar, |_| None))))
        .filter_map(|entity| entity.map(|entity| (entity.name().to_string(), entity)))
        .collect()
}

pub fn resolve_entity_definitions<P: AsRef<Path>, I: Into<Option<P>>>(
    i: Span,
    path: I,
) -> IndexMap<String, String> {
    let path = path.into();
    let definitions = entity_definitions(i);
    let iter = definitions.into_iter();
    let mut definitions: IndexMap<String, String> = IndexMap::new();
    for (name, definition) in iter {
        match definition.pedef {
            entity::PEDef::EntityValue(values) => {
                let mut value = Vec::with_capacity(values.len());
                for value_or_reference in values.into_iter() {
                    // println!("entity: {} -> {}", &name, &value_or_reference);
                    let v = match value_or_reference {
                        entity::ValueOrReference::Value(value) => value.into(),
                        entity::ValueOrReference::Reference(reference) => reference.to_string(),
                        entity::ValueOrReference::PEReference(pereference) => {
                            match definitions.get(&pereference.to_string()) {
                                Some(def) => def.to_owned(),
                                None => {
                                    eprintln!(
                                        "ERROR: PEReference(`{}`) is not defined yet.",
                                        pereference
                                    );
                                    continue;
                                }
                            }
                        }
                    };
                    value.push(v);
                }
                definitions.insert(name.to_owned(), value.join(" "));
            }
            entity::PEDef::ExternalID(external_id) => match external_id {
                entity::ExternalID::SystemLiteral(system_literal) => {
                    eprintln!(
                            "ERROR: ExternalID SystemLiteral(`{}`) not implemented yet, this will cause problom.",
                            system_literal
                        );
                    continue;
                }
                entity::ExternalID::PubidLiteralWithSystemLiteral(
                    _pubid_literal,
                    system_literal,
                ) => {
                    if system_literal.starts_with("http") || system_literal.starts_with("ftp") {
                        eprintln!(
                                "ERROR: ExternalID PubidLiteral SystemLiteral(`{}`) from network not implemented yet, this will cause problom.",
                                system_literal
                            );
                    }
                    let include = if let Some(ref path) = path {
                        let path: &Path = path.as_ref();
                        if let Some(_ext) = path.extension() {
                            path.with_file_name(system_literal.as_ref())
                        } else {
                            path.join(system_literal.as_ref())
                        }
                    } else {
                        PathBuf::from(system_literal.as_ref())
                    };
                    match std::fs::read_to_string(&include) {
                        Err(err) => {
                            eprintln!(
                                    "ERROR: Failed to include ExternalID PubidLiteral SystemLiteral(`{}`), {}",
                                    system_literal,
                                    &err
                                );
                        }
                        Ok(included) => definitions.extend(
                            resolve_entity_definitions::<PathBuf, Option<PathBuf>>(
                                Span::new_extra(&included, i.extra),
                                include.into(),
                            )
                            .into_iter(),
                        ),
                    }
                    continue;
                }
            },
        }
    }
    definitions
}

pub fn resolve_references(i: Span, definitions: &IndexMap<String, String>) -> String {
    iterator(
        i,
        alt((
            map(delimited(tag("%"), name, tag(";")), Either::Left),
            map(recognize(anychar), Either::Right),
        )),
    )
    .map(|either| match either {
        Either::Left(name) => match definitions.get(name.as_ref()) {
            Some(definition) => definition.as_str(),
            None => {
                eprintln!("ERROR: PEReference(`{}`) is not defined yet.", &name);
                ""
            }
        },
        Either::Right(chars) => *chars,
    })
    .collect::<Vec<_>>()
    .join("")
}

pub fn parse<F: AsRef<Path>>(f: F) -> std::result::Result<Vec<ElementType>, String> {
    let f = f.as_ref();
    let content =
        std::fs::read_to_string(f).expect(&format!("Can not read from file {}", f.display()));

    let tracer = TracableInfo::new().fold("entity-resolver");
    let span = LocatedSpan::new_extra(content.as_str(), tracer);

    let definitions = resolve_entity_definitions::<&Path, Option<&Path>>(span, f.into());
    let span = LocatedSpan::new_extra(content.as_str(), tracer);
    let resolved = resolve_references(span, &definitions);
    // println!("resolved -----------------------------\n{}", &resolved);
    std::fs::write("/tmp/resolved.dtd", &resolved).unwrap();
    let span = LocatedSpan::new_extra(resolved.as_str(), tracer);
    #[cfg(feature = "trace")]
    histogram();
    #[cfg(feature = "trace")]
    cumulative_histogram();
    let result = terminated(
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
        ))),
        eof,
    )(span)
    .finish()
    .map(|(_, definitions)| definitions)
    .map_err(|err| convert_error(span, err));
    result
}

pub fn parse_str(i: &str) -> std::result::Result<Vec<ElementType>, String> {
    let tracer = TracableInfo::new().fold("entity-resolver");
    let span = LocatedSpan::new_extra(i, tracer);
    let definitions = resolve_entity_definitions::<&str, Option<&str>>(span, None);
    let span = LocatedSpan::new_extra(i, tracer);
    let resolved = resolve_references(span, &definitions);
    let span = LocatedSpan::new_extra(resolved.as_str(), tracer);
    #[cfg(feature = "trace")]
    histogram();
    #[cfg(feature = "trace")]
    cumulative_histogram();
    let result = terminated(
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
        ))),
        eof,
    )(span)
    .finish()
    .map(|(_, elements)| elements)
    .map_err(|err| convert_error(span, err));
    result
}

#[cfg(test)]
mod tests {
    use nom::Finish;

    use super::{comment_decl, pereference, span};

    #[test]
    fn test_comment_decl() {
        let result = comment_decl(span(
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
        ))
        .finish();
        assert!(result.is_ok(), "{:?}", result.as_ref().unwrap_err());
    }

    #[test]
    fn test_pereference() {
        let result = pereference(span("%align-h.att;")).finish();
        assert!(result.is_ok(), "{:?}", result.as_ref().unwrap_err());
    }
}
