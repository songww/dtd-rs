use nom::{
    branch::alt, bytes::complete::is_not, character::complete::char, combinator::map, multi::many0,
    sequence::delimited,
};

use super::{pereference, reference, PEReference, Reference, Result};

/// 用来定义普通文本的变量。实体引用是对实体的引用。
///
/// 例如："&nbsp;"。这个“无折行空格”实体在 HTML 中被用于在某个文档中插入一个额外的空格。
///
/// 当文档被 XML 解析器解析时，实体就会被展开。
struct Entity;

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
