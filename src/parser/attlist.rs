use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::{char, multispace0, multispace1},
    combinator::{map, opt},
    multi::{many0, separated_list1},
    sequence::{delimited, pair, tuple},
};

use super::{
    name, name_or_reference, nmtoken, reference, Name, NameOrReference, Nmtoken, Reference, Result,
};

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

/// 属性可提供有关元素的额外信息。
///
/// 属性总是被置于某元素的开始标签中。属性总是以名称/值的形式成对出现的。
#[derive(Debug)]
pub struct AttlistDecl<'i> {
    name: Name<'i>,
    att_defs: Vec<AttDef<'i>>,
}

/// AttlistDecl ::= '<!ATTLIST' S Name AttDef* S? '>'
pub(super) fn attlist_decl(i: &str) -> nom::IResult<&str, AttlistDecl> {
    map(
        dbg_dmp(
            tuple((
                dbg_dmp(tag("<!ATTLIST"), "attlist start-tag"),
                dbg_dmp(multispace1, "attlist many spaces after tag"),
                dbg_dmp(name, "attlist name"),
                dbg_dmp(many0(attdef), "attlist attdef"),
                dbg_dmp(multispace0, "attlist many spaces before close-tag"),
                dbg_dmp(tag(">"), "attlist close-tag"),
            )),
            "attlist",
        ),
        |(_, _, name, att_defs, _, _)| AttlistDecl { name, att_defs },
    )(i)
}

#[derive(Debug)]
pub struct AttDef<'i> {
    name: NameOrReference<'i>,
    atttype: AttType<'i>,
    default_decl: DefaultDecl<'i>,
}

/// AttDef ::= S Name S AttType S DefaultDecl
fn attdef(i: &str) -> nom::IResult<&str, AttDef> {
    map(
        tuple((
            multispace1,
            dbg_dmp(name_or_reference, "attlist attdef name_or_reference"),
            multispace1,
            dbg_dmp(atttype, "attlist attdef atttype"),
            multispace1,
            dbg_dmp(default_decl, "attlist attdef default_decl"),
        )),
        |(_, name, _, atttype, _, default_decl)| {
            dbg!(&name);
            dbg!(&atttype);
            dbg!(&default_decl);
            AttDef {
                name,
                atttype,
                default_decl,
            }
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

/// AttType            ::=     StringType | TokenizedType | EnumeratedType
fn atttype(i: &str) -> Result<AttType> {
    alt((
        dbg_dmp(string_type, "atttype string_type"),
        dbg_dmp(tokenized_type, "atttype tokenized_type"),
        map(
            dbg_dmp(enumerated_type, "atttype enumerated_type"),
            AttType::EnumeratedType,
        ),
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
pub struct Enumeration<'i>(Vec<Nmtoken<'i>>);

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
pub enum DefaultDecl<'i> {
    Required,
    Implied,
    Fixed(AttValue<'i>),
    Default(AttValue<'i>),
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

    #[test]
    fn test_att_list_4() {
        let attlist = attlist_decl(
            r#"<!ATTLIST table
        %align-h.att;
        width     %measure;  #IMPLIED>"#,
        )
        .finish();
        assert!(
            attlist.is_ok(),
            "{}",
            attlist.as_ref().unwrap_err().to_string()
        );
    }
}
