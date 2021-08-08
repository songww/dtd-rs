#![feature(proc_macro_diagnostic)]

use std::collections::HashMap;
use std::iter::FromIterator;

use inflector::Inflector;
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote, quote_spanned};
use syn::parse::{Parse, ParseStream, Result};
use syn::spanned::Spanned;
use syn::{parse_macro_input, Expr, Ident, LitStr, Token, Type, Visibility};

use dtd_parser as parser;

enum DefinitionsOrPath {
    Definetions(LitStr),
    Path(LitStr),
}

impl Parse for DefinitionsOrPath {
    fn parse(input: ParseStream) -> Result<Self> {
        let maybe_path: LitStr = input.parse()?;
        let value = maybe_path.value();
        let trimed = value.trim();
        if trimed.starts_with("<") && trimed.ends_with(">") {
            Ok(DefinitionsOrPath::Definetions(maybe_path))
        } else {
            Ok(DefinitionsOrPath::Path(maybe_path))
        }
    }
}

/// Accept dtd file path in type AsRef<Path> or Definetions startswith '<' and endswith '>'.
///
///     dtd!($PATH);
///     dtd!($DEFINETIONS);
///
/// For example:
///
///     dtd!("testdata/docutils.dtd");
///     dtd! {
///         <!ELEMENT decoration (header?, footer?)>
///         <!ATTLIST decoration %basic.atts;>
///
///         <!ELEMENT header (%body.elements;)+>
///         <!ATTLIST header %basic.atts;>
///
///         <!ELEMENT footer (%body.elements;)+>
///         <!ATTLIST footer %basic.atts;>
///     }
#[proc_macro]
pub fn dtd(input: TokenStream) -> TokenStream {
    let definitions_or_path = parse_macro_input!(input as DefinitionsOrPath);

    // The warning looks like this.
    //
    //     warning: come on, pick a more creative name
    //       --> src/main.rs:10:16
    //        |
    //     10 |     static ref FOO: String = "lazy_static".to_owned();
    //        |                ^^^
    /*
    if name == "FOO" {
        name.span()
            .unwrap()
            .warning("come on, pick a more creative name")
            .emit();
    }

    // The error looks like this.
    //
    //     error: I can't think of a legitimate use for lazily initializing the value `()`
    //       --> src/main.rs:10:27
    //        |
    //     10 |     static ref UNIT: () = ();
    //        |                           ^^
    if let Expr::Tuple(ref init) = init {
        if init.elems.is_empty() {
            init.span()
                .unwrap()
                .error("I can't think of a legitimate use for lazily initializing the value `()`")
                .emit();
            return TokenStream::new();
        }
    }
    */
    let (parsing, span) = match definitions_or_path {
        DefinitionsOrPath::Definetions(definitions) => {
            (parser::parse_str(&definitions.value()), definitions)
        }
        DefinitionsOrPath::Path(path) => {
            let value = path.value();
            if !value.ends_with(".dtd") {
                path.span()
                    .unwrap()
                    .error(&format!(
                        "dtd file path shold ends with `.dtd`, but got {}.",
                        &value
                    ))
                    .emit();
                return TokenStream::new();
            }
            let pathbuf = match std::path::PathBuf::from(value).canonicalize() {
                Ok(absolute) => absolute,
                Err(err) => {
                    path.span()
                        .unwrap()
                        .error(&format!("invald dtd file path: `{}`.", err))
                        .emit();
                    return TokenStream::new();
                }
            };
            if !pathbuf.exists() {
                path.span()
                    .unwrap()
                    .error(&format!(
                        "dtd file `{}` dose not exists!",
                        pathbuf.display()
                    ))
                    .emit();
                return TokenStream::new();
            }
            (parser::parse(pathbuf), path)
        }
    };

    let definitions = match parsing {
        Ok(definitions) => definitions,
        Err(err) => {
            span.span().unwrap().error(&err).emit();
            return TokenStream::new();
        }
    };

    // let mut structs = HashMap::default();
    let mut tokens = Vec::new();

    println!("{} definitions found.", definitions.len());
    for definition in definitions.into_iter().rev() {
        match definition {
            parser::ElementType::Element(element) => {
                let struct_name = format_ident!("{}", element.name().to_class_case());
                match element.category() {
                    parser::ElementCategory::Empty => {
                        tokens.push(quote! {
                            #[derive(Clone, Debug)]
                            struct #struct_name;
                        });
                    }
                    parser::ElementCategory::PCDATA => {
                        // String
                        tokens.push(quote! {
                            #[derive(Clone, Debug)]
                            struct #struct_name(String);
                        });
                    }
                    parser::ElementCategory::CDATA => {
                        // String
                        tokens.push(quote! {
                            #[derive(Clone, Debug)]
                            struct #struct_name(String);
                        });
                    }
                    parser::ElementCategory::Any => {
                        // Any?
                        tokens.push(quote! {
                            #[derive(Clone, Debug)]
                            struct #struct_name(Box<dyn ::std::any::Any>);
                        });
                    }
                    parser::ElementCategory::Mixed(repeatable) => {
                        match repeatable {
                            parser::Repeatable::Once(_) => {
                                // No child, it is String.
                                tokens.push(quote! {
                                    #[derive(Clone, Debug)]
                                    pub struct #struct_name(pub String);
                                });
                            }
                            parser::Repeatable::ZeroOrManyTimes(parser::MixedPCDATA(names)) => {
                                // Has child, Mixed with String.
                                let variants = names.into_iter().map(|name| {
                                    format_ident!(
                                        "{}: Option<Vec<{}>>",
                                        name.as_str(),
                                        name.to_class_case()
                                    )
                                });
                                tokens.push(quote! {
                                    #[derive(Clone, Debug)]
                                    pub struct #struct_name {
                                        pcdata: Vec<String>,
                                        #(#variants, )*
                                    }
                                });
                            }
                            _ => unreachable!(),
                        }
                    }
                    parser::ElementCategory::Children(children) => {
                        match children {
                            parser::Repeatable::Once(child) => {
                                match child {
                                    parser::Child::Name(name) => {
                                        // struct
                                    }
                                    parser::Child::Seq(_) => {
                                        // Tuple
                                    }
                                    parser::Child::Choices(_) => {
                                        // Enum
                                    }
                                }
                            }
                            parser::Repeatable::AtMostOnce(child) => {
                                //
                            }
                            parser::Repeatable::AtLeastOnce(child) => {
                                //
                            }
                            parser::Repeatable::ZeroOrManyTimes(child) => {
                                //
                            }
                        }
                    }
                }
            }
            parser::ElementType::Entity(_entity) => {
                // println!("[dtd-macro/src/lib.rs:131] &entity = {}", entity);
            }
            parser::ElementType::Attlist(_attlist) => {
                // dbg!(idx, &attlist);
            }
            parser::ElementType::Comment(_) => {
                //
            }
        }
    }

    /*
    let expanded = quote! {
        #visibility struct #name;

        impl std::ops::Deref for #name {
            type Target = #ty;

            fn deref(&self) -> &#ty {
                #assert_sync
                #assert_sized

                static ONCE: std::sync::Once = std::sync::Once::new();
                static mut VALUE: *mut #ty = 0 as *mut #ty;

                unsafe {
                    ONCE.call_once(|| VALUE = #init_ptr);
                    &*VALUE
                }
            }
        }
    };
    */

    // TokenStream::from(expanded)
    TokenStream2::from_iter(tokens.into_iter()).into()
}
