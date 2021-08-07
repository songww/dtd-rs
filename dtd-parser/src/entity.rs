use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::{char, multispace0, multispace1},
    combinator::{map, opt},
    multi::many0,
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
};
use nom_tracable::tracable_parser;

use super::{
    name, pereference, pubid_literal, reference, system_literal, Name, PEReference, PubidLiteral,
    Reference, Result, Span, SystemLiteral,
};

/// 用来定义普通文本的变量。实体引用是对实体的引用。
///
/// 例如："&nbsp;"。这个“无折行空格”实体在 HTML 中被用于在某个文档中插入一个额外的空格。
///
/// 当文档被 XML 解析器解析时，实体就会被展开。

#[derive(Debug, Display, Into, IntoIterator, AsRef, AsMut, Deref, DerefMut)]
#[display(
    fmt = "{}",
    "_0.iter().map(|v|v.to_string()).collect::<Vec<_>>().join(\" \")"
)]
pub struct EntityValue(Vec<ValueOrReference>);

#[derive(Debug, Display, TryInto)]
pub enum ValueOrReference {
    #[display(fmt = "{}", "_0")]
    Value(Value),
    #[display(fmt = "{}", "_0")]
    Reference(Reference),
    #[display(fmt = "%{};", "_0")]
    PEReference(PEReference),
}

#[derive(Debug, Display, AsMut, AsRef, Deref, DerefMut, Into)]
pub struct Value(String);

/// EntityValue ::= '"' ([^%&"] | PEReference | Reference)* '"'
///                 |  "'" ([^%&'] | PEReference | Reference)* "'"
fn entity_value(i: Span) -> Result<EntityValue> {
    map(
        alt((
            delimited(
                char('"'),
                many0(alt((
                    map(is_not("%&\""), |v: Span| {
                        ValueOrReference::Value(Value(v.to_string()))
                    }),
                    map(reference, |r| ValueOrReference::Reference(r)),
                    map(pereference, |peref| ValueOrReference::PEReference(peref)),
                ))),
                char('"'),
            ),
            delimited(
                char('\''),
                many0(alt((
                    map(is_not("%&'"), |v: Span| {
                        ValueOrReference::Value(Value(v.to_string()))
                    }),
                    map(reference, |r| ValueOrReference::Reference(r)),
                    map(pereference, |peref| ValueOrReference::PEReference(peref)),
                ))),
                char('\''),
            ),
        )),
        |v| EntityValue(v),
    )(i)
}

#[derive(Debug, Display, TryInto)]
pub enum EntityDecl {
    #[display(fmt = "<ENTITY {}>", "_0")]
    GEDecl(GEDecl),
    #[display(fmt = "<ENTITY % {}>", "_0")]
    PEDecl(PEDecl),
}
// EntityDecl ::= GEDecl | PEDecl
#[tracable_parser]
pub(super) fn entity_decl(i: Span) -> Result<EntityDecl> {
    alt((
        map(gedecl, EntityDecl::GEDecl),
        map(pedecl, EntityDecl::PEDecl),
    ))(i)
}

#[derive(Debug, Display, AsMut, AsRef, Into)]
#[display(fmt = "{} {}", name, entity_def)]
pub struct GEDecl {
    name: Name,
    entity_def: EntityDef,
}
/// GEDecl ::= '<!ENTITY' S Name S EntityDef S? '>'
fn gedecl(i: Span) -> Result<GEDecl> {
    map(
        tuple((
            preceded(tag("<!ENTITY"), delimited(multispace1, name, multispace1)),
            terminated(entity_def, pair(multispace0, tag(">"))),
        )),
        |(name, entity_def)| GEDecl { name, entity_def },
    )(i)
}

#[derive(Debug, Display, AsMut, AsRef, Into)]
#[display(fmt = "{} {}", name, pedef)]
pub struct PEDecl {
    pub(super) name: Name,
    pub(super) pedef: PEDef,
}

impl PEDecl {
    pub fn name(&self) -> &Name {
        &self.name
    }

    pub fn pedef(&self) -> &PEDef {
        &self.pedef
    }
}

/// PEDecl	   ::=   	'<!ENTITY' S '%' S Name S PEDef S? '>'
pub(super) fn pedecl(i: Span) -> Result<PEDecl> {
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

#[derive(Debug, Display, TryInto)]
pub enum EntityDef {
    #[display(fmt = "{}", "_0")]
    EntityValue(EntityValue),
    #[display(fmt = "{} {:?}", "_0", "_1")]
    ExternalIDWithNDataDecl(ExternalID, Option<NDataDecl>),
}

/// EntityDef	   ::=   	EntityValue | (ExternalID NDataDecl?)
fn entity_def(i: Span) -> Result<EntityDef> {
    alt((
        map(entity_value, EntityDef::EntityValue),
        map(tuple((external_id, opt(ndata_decl))), |(eid, ndata)| {
            EntityDef::ExternalIDWithNDataDecl(eid, ndata)
        }),
    ))(i)
}

#[derive(Debug, Display, TryInto)]
pub enum PEDef {
    #[display(fmt = "{}", "_0")]
    EntityValue(EntityValue),
    #[display(fmt = "{}", "_0")]
    ExternalID(ExternalID),
}

/// PEDef	   ::=   	EntityValue | ExternalID
fn pedef(i: Span) -> Result<PEDef> {
    alt((
        map(entity_value, PEDef::EntityValue),
        map(external_id, PEDef::ExternalID),
    ))(i)
}

#[derive(Debug, Display, TryInto)]
pub enum ExternalID {
    #[display(fmt = "SYSTEM {}", "_0")]
    SystemLiteral(SystemLiteral),
    #[display(fmt = "PUBLIC {} {}", "_0", "_1")]
    PubidLiteralWithSystemLiteral(PubidLiteral, SystemLiteral),
}
/// ExternalID	   ::=   	'SYSTEM' S SystemLiteral
///                         | 'PUBLIC' S PubidLiteral S SystemLiteral
fn external_id(i: Span) -> Result<ExternalID> {
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

#[derive(Debug, Display, AsMut, AsRef, Deref, DerefMut, Into)]
pub struct NDataDecl(Name);

/// NDataDecl	   ::=   	S 'NDATA' S Name 	[VC: Notation Declared]
fn ndata_decl(i: Span) -> Result<NDataDecl> {
    map(
        preceded(tuple((multispace1, tag("NDATA"), multispace1)), name),
        NDataDecl,
    )(i)
}
#[cfg(test)]
mod tests {
    use nom::Finish;

    use crate::span;

    use super::entity_decl;

    #[test]
    fn test_internal_entity_parse() {
        let decl = entity_decl(span(
            r#"<!ENTITY Pub-Status "This is a pre-release of the
 specification.">"#,
        ))
        .finish();
        assert!(decl.is_ok(), "{:?}", decl.as_ref().unwrap_err());
    }

    #[test]
    fn test_external_entity_parse() {
        let decl = entity_decl(span(
            r#"<!ENTITY open-hatch
         SYSTEM "http://www.textuality.com/boilerplate/OpenHatch.xml">"#,
        ))
        .finish();
        assert!(decl.is_ok(), "{:?}", decl.as_ref().unwrap_err());
        let decl = entity_decl(span(
            r#"<!ENTITY open-hatch
         PUBLIC "-//Textuality//TEXT Standard open-hatch boilerplate//EN"
         "http://www.textuality.com/boilerplate/OpenHatch.xml">"#,
        ))
        .finish();
        assert!(decl.is_ok(), "{:?}", decl.as_ref().unwrap_err());
        let decl = entity_decl(span(
            r#"<!ENTITY hatch-pic
         SYSTEM "../grafix/OpenHatch.gif"
         NDATA gif >"#,
        ))
        .finish();
        assert!(decl.is_ok(), "{:?}", decl.as_ref().unwrap_err());
    }

    #[test]
    fn test_entity_multiline() {
        let result = entity_decl(span(
            r#"<!ENTITY % basic.atts
  " ids       %ids.type;         #IMPLIED
    names     %refnames.type;    #IMPLIED
    dupnames  %refnames.type;    #IMPLIED
    source    CDATA              #IMPLIED
    classes   %classnames.type;  #IMPLIED
    %additional.basic.atts; ">"#,
        ))
        .finish();
        assert!(result.is_ok(), "{:?}", result.as_ref().unwrap_err());
        dbg!(result.unwrap());
    }

    #[test]
    fn test_external_entity_parse_2() {
        let result = entity_decl(span(
            r#"<!ENTITY % calstblx PUBLIC
    "-//OASIS//DTD XML Exchange Table Model 19990315//EN"
    "soextblx.dtd">"#,
        ))
        .finish();
        assert!(result.is_ok(), "{:?}", result.as_ref().unwrap_err());
        dbg!(result.unwrap());
    }
}
