use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::{char, multispace0, multispace1, space1},
    combinator::{map, opt},
    multi::{many0, separated_list1},
    sequence::{delimited, pair, preceded, terminated, tuple},
};
use nom_tracable::tracable_parser;

use super::{name, nmtoken, reference, Name, Nmtoken, Reference, Repeatable, Result, Span};

/// 属性可提供有关元素的额外信息。
///
/// 属性总是被置于某元素的开始标签中。属性总是以名称/值的形式成对出现的。
#[derive(Debug)]
pub struct AttlistDecl {
    name: Name,
    attdefs: Repeatable<Vec<AttDef>>,
}

/// AttlistDecl ::= '<!ATTLIST' S Name AttDef* S? '>'
#[tracable_parser]
pub(super) fn attlist_decl(i: Span) -> Result<AttlistDecl> {
    map(
        tuple((
            preceded(tuple((tag("<!ATTLIST"), multispace1)), name),
            terminated(many0(attdef), tuple((multispace0, tag(">")))),
        )),
        |(name, attdefs)| AttlistDecl {
            name,
            attdefs: Repeatable::ZeroOrManyTimes(attdefs),
        },
    )(i)
}

#[derive(Debug)]
pub struct AttDef {
    name: Name,
    atttype: AttType,
    default_decl: DefaultDecl,
}

/// AttDef ::= S Name S AttType S DefaultDecl
#[tracable_parser]
fn attdef(i: Span) -> Result<AttDef> {
    map(
        tuple((
            preceded(multispace0, name),
            preceded(multispace0, atttype),
            preceded(multispace0, default_decl),
        )),
        |(name, atttype, default_decl)| AttDef {
            name,
            atttype,
            default_decl,
        },
    )(i)
}

#[derive(Debug)]
pub enum AttType {
    /// StringType         ::=     'CDATA'
    StringType,
    TokenizedType(TokenizedType),
    EnumeratedType(EnumeratedType),
}

#[derive(Debug)]
pub enum TokenizedType {
    ID,
    IDREF,
    IDREFS,
    ENTITY,
    ENTITIES,
    NMTOKEN,
    NMTOKENS,
}

///      StringType         ::=     'CDATA'
#[tracable_parser]
fn string_type(i: Span) -> Result<AttType> {
    map(tag("CDATA"), |_| AttType::StringType)(i)
}

///      TokenizedType      ::=     'ID'            [VC: ID]
///                                                 [VC: One ID per Element Type]
///                                                 [VC: ID Attribute Default]
///                                 | 'IDREF'       [VC: IDREF]
///                                 | 'IDREFS'      [VC: IDREF]
///                                 | 'ENTITY'      [VC: Entity Name]           
///                                 | 'ENTITIES'    [VC: Entity Name]
///                                 | 'NMTOKEN'     [VC: Name Token]
///                                 | 'NMTOKENS'    [VC: Name Token]]
#[tracable_parser]
fn tokenized_type(i: Span) -> Result<AttType> {
    map(
        alt((
            terminated(tag("ID"), space1),
            terminated(tag("IDREF"), space1),
            terminated(tag("IDREFS"), space1),
            terminated(tag("ENTITY"), space1),
            terminated(tag("ENTITIES"), space1),
            terminated(tag("NMTOKEN"), space1),
            terminated(tag("NMTOKENS"), space1),
        )),
        |ty: Span| match *ty {
            "ID" => AttType::TokenizedType(TokenizedType::ID),
            "IDREF" => AttType::TokenizedType(TokenizedType::IDREF),
            "IDREFS" => AttType::TokenizedType(TokenizedType::IDREFS),
            "ENTITY" => AttType::TokenizedType(TokenizedType::ENTITY),
            "ENTITIES" => AttType::TokenizedType(TokenizedType::ENTITIES),
            "NMTOKEN" => AttType::TokenizedType(TokenizedType::NMTOKEN),
            "NMTOKENS" => AttType::TokenizedType(TokenizedType::NMTOKENS),
            _ => unreachable!(),
        },
    )(i)
}

/// AttType            ::=     StringType | TokenizedType | EnumeratedType
#[tracable_parser]
fn atttype(i: Span) -> Result<AttType> {
    alt((
        string_type,
        tokenized_type,
        map(enumerated_type, AttType::EnumeratedType),
    ))(i)
}

/// EnumeratedType ::= NotationType | Enumeration
#[derive(Debug)]
pub enum EnumeratedType {
    NotationType(NotationType),
    Enumeration(Enumeration),
}

/// EnumeratedType ::= NotationType | Enumeration
#[tracable_parser]
fn enumerated_type(i: Span) -> Result<EnumeratedType> {
    alt((
        map(notation_type, EnumeratedType::NotationType),
        map(enumeration, EnumeratedType::Enumeration),
    ))(i)
}

/// NotationType   ::= 'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')'  [VC: Notation Attributes]
///                                                                       [VC: One Notation Per Element Type]
///                                                                       [VC: No Notation on Empty Element]
///                                                                       [VC: No Duplicate Tokens]
#[derive(Debug)]
pub struct NotationType(Vec<Name>);

#[tracable_parser]
fn notation_type(i: Span) -> Result<NotationType> {
    map(
        tuple((
            tag("NOTATION"),
            multispace1,
            tag("("),
            multispace0,
            separated_list1(tuple((multispace0, tag("|"), multispace0)), name),
            multispace0,
            tag(")"),
        )),
        |(_, _, _, _, names, _, _)| NotationType(names),
    )(i)
}

/// Enumeration    ::= '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'         [VC: Enumeration]
///                                                                       [VC: No Duplicate Tokens]
#[derive(Debug)]
pub struct Enumeration(Vec<Nmtoken>);

/// Enumeration    ::= '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'         [VC: Enumeration]
///                                                                       [VC: No Duplicate Tokens]
#[tracable_parser]
fn enumeration(i: Span) -> Result<Enumeration> {
    map(
        tuple((
            tag("("),
            multispace0,
            separated_list1(tuple((multispace0, tag("|"), multispace0)), nmtoken),
            multispace0,
            tag(")"),
        )),
        |(_, _, tokens, _, _)| Enumeration(tokens),
    )(i)
}

#[derive(Debug)]
pub enum DefaultDecl {
    Required,
    Implied,
    Fixed(AttValue),
    Default(AttValue),
}
/// DefaultDecl ::= '#REQUIRED' | '#IMPLIED'
///                 | (('#FIXED' S)? AttValue) [VC: Required Attribute]
///                                            [VC: Attribute Default Value Syntactically Correct]
///                                            [WFC: No < in Attribute Values]
///                                            [VC: Fixed Attribute Default]
///                                            [WFC: No External Entity References]
///
/// <!ATTLIST termdef
///           id      ID      #REQUIRED
///           name    CDATA   #IMPLIED>
/// <!ATTLIST list
///           type    (bullets|ordered|glossary)  "ordered">
/// <!ATTLIST form
///           method  CDATA   #FIXED "POST">
#[tracable_parser]
fn default_decl(i: Span) -> Result<DefaultDecl> {
    alt((
        map(tag("#REQUIRED"), |_| DefaultDecl::Required),
        map(tag("#IMPLIED"), |_| DefaultDecl::Implied),
        map(
            pair(opt(pair(tag("#FIXED"), multispace1)), attvalue),
            |(isfixed, attvalue)| {
                if isfixed.is_some() {
                    DefaultDecl::Fixed(attvalue)
                } else {
                    DefaultDecl::Default(attvalue)
                }
            },
        ),
    ))(i)
}

#[derive(Debug)]
pub struct AttValue(Vec<ValueOrReference>);

#[derive(Debug)]
pub enum ValueOrReference {
    Value(Value),
    Reference(Reference),
}

#[derive(Debug)]
pub struct Value(String);

/// AttValue ::= '"' ([^<&"] | Reference)* '"'
///              |  "'" ([^<&'] | Reference)* "'"
#[tracable_parser]
fn attvalue(i: Span) -> Result<AttValue> {
    map(
        alt((
            delimited(
                char('"'),
                many0(alt((
                    map(is_not("<&\""), |v: Span| {
                        ValueOrReference::Value(Value(v.to_string()))
                    }),
                    map(reference, |r| ValueOrReference::Reference(r)),
                ))),
                char('"'),
            ),
            delimited(
                char('\''),
                many0(alt((
                    map(is_not("<&'"), |v: Span| {
                        ValueOrReference::Value(Value(v.to_string()))
                    }),
                    map(reference, |r| ValueOrReference::Reference(r)),
                ))),
                char('\''),
            ),
        )),
        |v| AttValue(v),
    )(i)
}

#[cfg(test)]
mod tests {
    use nom::Finish;

    use super::attlist_decl;
    use crate::span;

    // <!ATTLIST termdef
    //           id      ID      #REQUIRED
    //           name    CDATA   #IMPLIED>
    #[test]
    fn test_attlist_1() {
        let attlist = attlist_decl(span(
            r#"<!ATTLIST termdef
             id      ID      #REQUIRED
             name    CDATA   #IMPLIED>"#,
        ))
        .finish();
        assert!(attlist.is_ok(), "{:?}", attlist.as_ref().unwrap_err());
    }
    // <!ATTLIST list
    //           type    (bullets|ordered|glossary)  "ordered">
    #[test]
    fn test_attlist_2() {
        let attlist = attlist_decl(span(
            r#"<!ATTLIST list
             type    (bullets|ordered|glossary)  "ordered">"#,
        ))
        .finish();
        assert!(attlist.is_ok(), "{:?}", attlist.as_ref().unwrap_err());
    }
    // <!ATTLIST form
    //           method  CDATA   #FIXED "POST">
    #[test]
    fn test_attlist_3() {
        let attlist = attlist_decl(span(
            r#"<!ATTLIST form
             method  CDATA   #FIXED "POST">"#,
        ))
        .finish();
        assert!(attlist.is_ok(), "{:?}", attlist.as_ref().unwrap_err());
    }

    #[test]
    fn test_attlist_4() {
        let attlist = attlist_decl(span(
            r#"<!ATTLIST document
     ids        NMTOKENS          #IMPLIED
    names      CDATA     #IMPLIED
    dupnames   CDATA     #IMPLIED
    source    CDATA              #IMPLIED
    classes    NMTOKENS   #IMPLIED

    title     CDATA     #IMPLIED>"#,
        ))
        .finish();
        // dbg!(attlist.as_ref().unwrap_err());
        assert!(attlist.is_ok(), "{:?}", attlist.as_ref().unwrap_err());
    }
}
