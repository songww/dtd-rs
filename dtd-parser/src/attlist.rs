use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::{char, multispace0, multispace1},
    combinator::{map, opt},
    multi::{many0, separated_list1},
    sequence::{delimited, pair, tuple},
};

use super::{name, nmtoken, reference, Name, Nmtoken, Reference, Repeatable, Result};

/// 属性可提供有关元素的额外信息。
///
/// 属性总是被置于某元素的开始标签中。属性总是以名称/值的形式成对出现的。
#[derive(Debug)]
pub struct AttlistDecl {
    name: Name,
    att_defs: Repeatable<Vec<AttDef>>,
}

/// AttlistDecl ::= '<!ATTLIST' S Name AttDef* S? '>'
pub(super) fn attlist_decl(i: &str) -> nom::IResult<&str, AttlistDecl> {
    map(
        // dbg_dmp(
        tuple((
            /* dbg_dmp( */ tag("<!ATTLIST"), // "attlist start-tag"),
            /* dbg_dmp( */ multispace1, // "attlist many spaces after tag"),
            /* dbg_dmp( */ name, // "attlist name"),
            /* dbg_dmp( */ many0(attdef), // "attlist attdef"),
            /* dbg_dmp( */ multispace0, // "attlist many spaces before close-tag"),
            /* dbg_dmp( */ tag(">"), // "attlist close-tag"),
        )),
        // "attlist",
        // ),
        |(_, _, name, att_defs, _, _)| AttlistDecl {
            name,
            att_defs: Repeatable::ZeroOrManyTimes(att_defs),
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
fn attdef(i: &str) -> nom::IResult<&str, AttDef> {
    map(
        tuple((
            multispace1,
            /* dbg_dmp( */ name, // "attlist attdef name_or_reference"),
            multispace1,
            /* dbg_dmp( */ atttype, // "attlist attdef atttype"),
            multispace1,
            /* dbg_dmp( */ default_decl, // "attlist attdef default_decl"),
        )),
        |(_, name, _, atttype, _, default_decl)| {
            // dbg!(&name);
            // dbg!(&atttype);
            // dbg!(&default_decl);
            AttDef {
                name,
                atttype,
                default_decl,
            }
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

/// AttType            ::=     StringType | TokenizedType | EnumeratedType
fn atttype(i: &str) -> Result<AttType> {
    alt((
        /* dbg_dmp( */ string_type, // "atttype string_type"),
        /* dbg_dmp( */ tokenized_type, // "atttype tokenized_type"),
        map(
            /* dbg_dmp( */ enumerated_type, // "atttype enumerated_type"),
            AttType::EnumeratedType,
        ),
    ))(i)
}

/// EnumeratedType ::= NotationType | Enumeration
#[derive(Debug)]
pub enum EnumeratedType {
    NotationType(NotationType),
    Enumeration(Enumeration),
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
pub struct NotationType(Vec<Name>);

fn notation_type(i: &str) -> Result<NotationType> {
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
fn enumeration(i: &str) -> Result<Enumeration> {
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
fn default_decl(i: &str) -> Result<DefaultDecl> {
    alt((
        map(tag("#REQUIRED"), |_| DefaultDecl::Required),
        map(tag("#IMPLIED"), |_| DefaultDecl::Implied),
        map(
            pair(opt(pair(tag("#FIXED"), multispace1)), att_value),
            |(isfixed, att_value)| {
                if isfixed.is_some() {
                    DefaultDecl::Fixed(att_value)
                } else {
                    DefaultDecl::Default(att_value)
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
fn att_value(i: &str) -> Result<AttValue> {
    map(
        alt((
            delimited(
                char('"'),
                many0(alt((
                    map(is_not("<&\""), |v: &str| {
                        ValueOrReference::Value(Value(v.to_string()))
                    }),
                    map(reference, |r| ValueOrReference::Reference(r)),
                ))),
                char('"'),
            ),
            delimited(
                char('\''),
                many0(alt((
                    map(is_not("<&'"), |v: &str| {
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

    // <!ATTLIST termdef
    //           id      ID      #REQUIRED
    //           name    CDATA   #IMPLIED>
    #[test]
    fn test_att_list_1() {
        let attlist = attlist_decl(
            r#"<!ATTLIST termdef
             id      ID      #REQUIRED
             name    CDATA   #IMPLIED>"#,
        )
        .finish();
        assert!(
            attlist.is_ok(),
            "{}",
            attlist.as_ref().unwrap_err().to_string()
        );
    }
    // <!ATTLIST list
    //           type    (bullets|ordered|glossary)  "ordered">
    #[test]
    fn test_att_list_2() {
        let attlist = attlist_decl(
            r#"<!ATTLIST list
             type    (bullets|ordered|glossary)  "ordered">"#,
        )
        .finish();
        assert!(
            attlist.is_ok(),
            "{}",
            attlist.as_ref().unwrap_err().to_string()
        );
    }
    // <!ATTLIST form
    //           method  CDATA   #FIXED "POST">
    #[test]
    fn test_att_list_3() {
        let attlist = attlist_decl(
            r#"<!ATTLIST form
             method  CDATA   #FIXED "POST">"#,
        )
        .finish();
        assert!(
            attlist.is_ok(),
            "{}",
            attlist.as_ref().unwrap_err().to_string()
        );
    }
}
