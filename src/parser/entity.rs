use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::{char, multispace0, multispace1},
    combinator::{map, opt},
    multi::many0,
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
};

use super::{
    name, pereference, pubid_literal, reference, system_literal, Name, PEReference, PubidLiteral,
    Reference, Result, SystemLiteral,
};

/// 用来定义普通文本的变量。实体引用是对实体的引用。
///
/// 例如："&nbsp;"。这个“无折行空格”实体在 HTML 中被用于在某个文档中插入一个额外的空格。
///
/// 当文档被 XML 解析器解析时，实体就会被展开。

#[derive(Debug)]
pub struct EntityValue<'i>(Vec<ValueOrReference<'i>>);

#[derive(Debug)]
pub enum ValueOrReference<'i> {
    Value(Value<'i>),
    Reference(Reference<'i>),
    PEReference(PEReference<'i>),
}

#[derive(Debug)]
pub struct Value<'i>(&'i str);

/// EntityValue ::= '"' ([^%&"] | PEReference | Reference)* '"'
///                 |  "'" ([^%&'] | PEReference | Reference)* "'"
fn entity_value(i: &str) -> Result<EntityValue> {
    map(
        alt((
            delimited(
                char('"'),
                many0(alt((
                    map(is_not("%&\""), |v| ValueOrReference::Value(Value(v))),
                    map(reference, |r| ValueOrReference::Reference(r)),
                    map(pereference, |peref| ValueOrReference::PEReference(peref)),
                ))),
                char('"'),
            ),
            delimited(
                char('\''),
                many0(alt((
                    map(is_not("%&'"), |v| ValueOrReference::Value(Value(v))),
                    map(reference, |r| ValueOrReference::Reference(r)),
                    map(pereference, |peref| ValueOrReference::PEReference(peref)),
                ))),
                char('\''),
            ),
        )),
        EntityValue,
    )(i)
}

#[derive(Debug)]
pub enum EntityDecl<'i> {
    GEDecl(GEDecl<'i>),
    PEDecl(PEDecl<'i>),
}
// EntityDecl ::= GEDecl | PEDecl
pub(super) fn entity_decl(i: &str) -> Result<EntityDecl> {
    alt((
        map(gedecl, EntityDecl::GEDecl),
        map(pedecl, EntityDecl::PEDecl),
    ))(i)
}

#[derive(Debug)]
pub struct GEDecl<'i> {
    name: Name<'i>,
    entity_def: EntityDef<'i>,
}
/// GEDecl ::= '<!ENTITY' S Name S EntityDef S? '>'
fn gedecl(i: &str) -> Result<GEDecl> {
    map(
        tuple((
            preceded(tag("<!ENTITY"), delimited(multispace1, name, multispace1)),
            terminated(entity_def, pair(multispace0, tag(">"))),
        )),
        |(name, entity_def)| GEDecl { name, entity_def },
    )(i)
}

#[derive(Debug)]
pub struct PEDecl<'i> {
    name: Name<'i>,
    pedef: PEDef<'i>,
}

/// PEDecl	   ::=   	'<!ENTITY' S '%' S Name S PEDef S? '>'
fn pedecl(i: &str) -> Result<PEDecl> {
    map(
        tuple((
            delimited(
                tuple((tag("<!ENTITY"), multispace1, tag("%"), multispace1)),
                name,
                multispace1,
            ),
            terminated(pedef, pair(multispace0, tag(">"))),
        )),
        |(name, pedef)| PEDecl { name, pedef },
    )(i)
}

#[derive(Debug)]
pub enum EntityDef<'i> {
    EntityValue(EntityValue<'i>),
    ExternalIDWithNDataDecl(ExternalID<'i>, Option<NDataDecl<'i>>),
}

/// EntityDef	   ::=   	EntityValue | (ExternalID NDataDecl?)
fn entity_def(i: &str) -> Result<EntityDef> {
    alt((
        map(entity_value, EntityDef::EntityValue),
        map(tuple((external_id, opt(ndata_decl))), |(eid, ndata)| {
            EntityDef::ExternalIDWithNDataDecl(eid, ndata)
        }),
    ))(i)
}

#[derive(Debug)]
pub enum PEDef<'i> {
    EntityValue(EntityValue<'i>),
    ExternalID(ExternalID<'i>),
}

/// PEDef	   ::=   	EntityValue | ExternalID
fn pedef(i: &str) -> Result<PEDef> {
    alt((
        map(entity_value, PEDef::EntityValue),
        map(external_id, PEDef::ExternalID),
    ))(i)
}

#[derive(Debug)]
pub enum ExternalID<'i> {
    SystemLiteral(SystemLiteral<'i>),
    PubidLiteralWithSystemLiteral(PubidLiteral<'i>, SystemLiteral<'i>),
}
/// ExternalID	   ::=   	'SYSTEM' S SystemLiteral
///                         | 'PUBLIC' S PubidLiteral S SystemLiteral
fn external_id(i: &str) -> Result<ExternalID> {
    alt((
        map(
            preceded(pair(tag("SYSTEM"), multispace1), system_literal),
            ExternalID::SystemLiteral,
        ),
        map(
            preceded(
                pair(tag("PUBLIC"), multispace1),
                separated_pair(pubid_literal, multispace1, system_literal),
            ),
            |(pubid, system)| ExternalID::PubidLiteralWithSystemLiteral(pubid, system),
        ),
    ))(i)
}

#[derive(Debug)]
pub struct NDataDecl<'i>(Name<'i>);

/// NDataDecl	   ::=   	S 'NDATA' S Name 	[VC: Notation Declared]
fn ndata_decl(i: &str) -> Result<NDataDecl> {
    map(
        preceded(tuple((multispace1, tag("NDATA"), multispace1)), name),
        NDataDecl,
    )(i)
}
#[cfg(test)]
mod tests {
    use nom::Finish;

    use super::entity_decl;

    #[test]
    fn test_internal_entity_parse() {
        let decl = entity_decl(
            r#"<!ENTITY Pub-Status "This is a pre-release of the
 specification.">"#,
        )
        .finish();
        assert!(decl.is_ok(), "{}", decl.as_ref().unwrap_err().to_string());
    }

    #[test]
    fn test_external_entity_parse() {
        let decl = entity_decl(
            r#"<!ENTITY open-hatch
         SYSTEM "http://www.textuality.com/boilerplate/OpenHatch.xml">"#,
        )
        .finish();
        assert!(decl.is_ok(), "{}", decl.as_ref().unwrap_err().to_string());
        let decl = entity_decl(
            r#"<!ENTITY open-hatch
         PUBLIC "-//Textuality//TEXT Standard open-hatch boilerplate//EN"
         "http://www.textuality.com/boilerplate/OpenHatch.xml">"#,
        )
        .finish();
        assert!(decl.is_ok(), "{}", decl.as_ref().unwrap_err().to_string());
        let decl = entity_decl(
            r#"<!ENTITY hatch-pic
         SYSTEM "../grafix/OpenHatch.gif"
         NDATA gif >"#,
        )
        .finish();
        assert!(decl.is_ok(), "{}", decl.as_ref().unwrap_err().to_string());
    }

    #[test]
    fn test_entity_multiline() {
        let result = entity_decl(
            r#"<!ENTITY % basic.atts
  " ids       %ids.type;         #IMPLIED
    names     %refnames.type;    #IMPLIED
    dupnames  %refnames.type;    #IMPLIED
    source    CDATA              #IMPLIED
    classes   %classnames.type;  #IMPLIED
    %additional.basic.atts; ">"#,
        )
        .finish();
        assert!(
            result.is_ok(),
            "{}",
            result.as_ref().unwrap_err().to_string()
        );
    }
}
