use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::{char, space0, space1},
    combinator::{map, opt},
    multi::{many0, separated_list1},
    sequence::{delimited, pair, tuple},
};

use super::{name, nmtoken, reference, Name, Nmtoken, Reference, Result};

/// 属性可提供有关元素的额外信息。
///
/// 属性总是被置于某元素的开始标签中。属性总是以名称/值的形式成对出现的。
#[derive(Debug)]
struct AttlistDecl<'i> {
    name: Name<'i>,
    att_defs: Vec<AttDef<'i>>,
}

/// AttlistDecl ::= '<!ATTLIST' S Name AttDef* S? '>'
fn attlist_decl(i: &str) -> nom::IResult<&str, AttlistDecl> {
    map(
        tuple((
            tag("<!ATTLIST"),
            space1,
            name,
            many0(att_def),
            space0,
            tag(">"),
        )),
        |(_, _, name, att_defs, _, _)| AttlistDecl { name, att_defs },
    )(i)
}

#[derive(Debug)]
pub struct AttDef<'i> {
    name: Name<'i>,
    att_type: AttType<'i>,
    default_decl: DefaultDecl<'i>,
}

/// AttDef ::= S Name S AttType S DefaultDecl
fn att_def(i: &str) -> nom::IResult<&str, AttDef> {
    map(
        tuple((space1, name, space1, att_type, space1, default_decl)),
        |(_, name, _, att_type, _, default_decl)| AttDef {
            name,
            att_type,
            default_decl,
        },
    )(i)
}

#[derive(Debug)]
pub enum AttType<'i> {
    /// StringType         ::=     'CDATA'
    StringType,
    TokenizedType(TokenizedType),
    EnumeratedType(EnumeratedType<'i>),
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
fn string_type(i: &str) -> nom::IResult<&str, AttType> {
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
fn tokenized_type(i: &str) -> Result<AttType> {
    map(
        alt((
            tag("ID"),
            tag("IDREF"),
            tag("IDREFS"),
            tag("ENTITY"),
            tag("ENTITIES"),
            tag("NMTOKEN"),
            tag("NMTOKENS"),
        )),
        |ty| match ty {
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

///      AttType            ::=     StringType | TokenizedType | EnumeratedType
fn att_type(i: &str) -> Result<AttType> {
    alt((
        string_type,
        tokenized_type,
        map(enumerated_type, AttType::EnumeratedType),
    ))(i)
}

/// EnumeratedType ::= NotationType | Enumeration
#[derive(Debug)]
pub enum EnumeratedType<'i> {
    NotationType(NotationType<'i>),
    Enumeration(Enumeration<'i>),
}

/// EnumeratedType ::= NotationType | Enumeration
fn enumerated_type(i: &str) -> Result<EnumeratedType> {
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
pub struct NotationType<'i>(Vec<Name<'i>>);

fn notation_type(i: &str) -> Result<NotationType> {
    map(
        tuple((
            tag("NOTATION"),
            space1,
            tag("("),
            space0,
            separated_list1(tuple((space0, tag("|"), space0)), name),
            space0,
            tag(")"),
        )),
        |(_, _, _, _, names, _, _)| NotationType(names),
    )(i)
}

/// Enumeration    ::= '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'         [VC: Enumeration]
///                                                                       [VC: No Duplicate Tokens]
#[derive(Debug)]
pub struct Enumeration<'i>(Vec<Nmtoken<'i>>);

/// Enumeration    ::= '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'         [VC: Enumeration]
///                                                                       [VC: No Duplicate Tokens]
fn enumeration(i: &str) -> Result<Enumeration> {
    map(
        tuple((
            tag("("),
            space0,
            separated_list1(tuple((space0, tag("|"), space0)), nmtoken),
            space0,
            tag(")"),
        )),
        |(_, _, tokens, _, _)| Enumeration(tokens),
    )(i)
}

#[derive(Debug)]
pub enum DefaultDecl<'i> {
    Required,
    Implied,
    Fixed(AttValue<'i>),
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
fn default_decl(i: &str) -> Result<DefaultDecl> {
    alt((
        map(tag("#REQUIRED"), |_| DefaultDecl::Required),
        map(tag("#IMPLIED"), |_| DefaultDecl::Implied),
        map(
            pair(opt(pair(tag("#FIXED"), space1)), att_value),
            |(_, att_value)| DefaultDecl::Fixed(att_value),
        ),
    ))(i)
}

#[derive(Debug)]
pub struct AttValue<'i>(Vec<ValueOrReference<'i>>);

#[derive(Debug)]
pub enum ValueOrReference<'i> {
    Value(Value<'i>),
    Reference(Reference<'i>),
}

#[derive(Debug)]
pub struct Value<'i>(&'i str);

/// AttValue ::= '"' ([^<&"] | Reference)* '"'
///              |  "'" ([^<&'] | Reference)* "'"
fn att_value(i: &str) -> Result<AttValue> {
    map(
        alt((
            delimited(
                char('"'),
                many0(alt((
                    map(is_not("<&\""), |v| ValueOrReference::Value(Value(v))),
                    map(reference, |r| ValueOrReference::Reference(r)),
                ))),
                char('"'),
            ),
            delimited(
                char('\''),
                many0(alt((
                    map(is_not("<&'"), |v| ValueOrReference::Value(Value(v))),
                    map(reference, |r| ValueOrReference::Reference(r)),
                ))),
                char('\''),
            ),
        )),
        AttValue,
    )(i)
}
