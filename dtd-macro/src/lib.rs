#![feature(proc_macro_diagnostic)]

use std::collections::HashMap;
use std::iter::FromIterator;

use inflector::Inflector;
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote, quote_spanned, TokenStreamExt};
use syn::parse::{Parse, ParseStream, Result};
use syn::{parse_macro_input, Ident, LitStr};

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

macro_rules! safe {
    ($ident:expr) => {
        match $ident.to_string().as_str() {
            "as" | "break" | "const" | "continue" | "crate" | "else" | "enum" | "extern"
            | "false" | "fn" | "for" | "if" | "impl" | "in" | "let" | "loop" | "match" | "mod"
            | "move" | "mut" | "pub" | "ref" | "return" | "self" | "Self" | "static" | "struct"
            | "super" | "trait" | "true" | "type" | "unsafe" | "use" | "where" | "while"
            | "async" | "await" | "dyn" | "abstract" | "become" | "box" | "do" | "final"
            | "macro" | "override" | "priv" | "typeof" | "unsized" | "virtual" | "yield"
            | "try" | "union" => format_ident!("{}_", $ident),
            _ => $ident,
        }
    };
}

/// Accept dtd file path in type AsRef<Path> or Definetions startswith '<' and endswith '>'.
///
///     dtd!($PATH);
///     dtd!($DEFINETIONS);
///
/// Example:
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

    let mut context = Context::new();

    for definition in definitions.into_iter().rev() {
        match definition {
            parser::ElementType::Element(element) => {
                let struct_name = format_ident!("{}", element.name().to_pascal_case());
                match element.category() {
                    parser::ElementCategory::Empty => {
                        let token = quote! {
                            #[derive(Clone, Debug)]
                            pub struct #struct_name;
                        };
                        // println!(" > EMPTY: {} {}", struct_name, token.to_string());
                        if context.get(&struct_name).is_none() {
                            context.insert(struct_name.clone(), token.clone());
                            tokens.push(token);
                        }
                    }
                    parser::ElementCategory::PCDATA => {
                        // String
                        let token = quote! {
                            #[derive(Clone, Debug)]
                            pub struct #struct_name(pub String);
                        };
                        // println!(" > PCDATA: {}", struct_name);
                        if context.get(&struct_name).is_none() {
                            context.insert(struct_name.clone(), token.clone());
                            tokens.push(token);
                        }
                    }
                    parser::ElementCategory::CDATA => {
                        // String
                        let token = quote! {
                            #[derive(Clone, Debug)]
                            pub struct #struct_name(pub String);
                        };
                        // println!(" > CDATA: {}", struct_name);
                        if context.get(&struct_name).is_none() {
                            context.insert(struct_name.clone(), token.clone());
                            tokens.push(token);
                        }
                    }
                    parser::ElementCategory::Any => {
                        // Any?
                        let token = quote! {
                            #[derive(Clone, Debug)]
                            pub struct #struct_name(pub Box<dyn ::std::any::Any>);
                        };
                        // println!(" > Any: {}", struct_name);
                        if context.get(&struct_name).is_none() {
                            context.insert(struct_name.clone(), token.clone());
                            tokens.push(token);
                        }
                    }
                    parser::ElementCategory::Mixed(repeatable) => {
                        match repeatable {
                            parser::Repeatable::Once(_) => {
                                // No child, it is String.
                                let token = quote! {
                                    #[derive(Clone, Debug)]
                                    pub struct #struct_name(pub String);
                                };
                                // println!(" > Mixed: {}", struct_name);

                                if context.get(&struct_name).is_none() {
                                    context.insert(struct_name.clone(), token.clone());
                                    tokens.push(token);
                                }
                            }
                            parser::Repeatable::ZeroOrManyTimes(parser::MixedPCDATA(names)) => {
                                // Has child, Mixed with String.
                                let fields = names.iter().map(|name| {
                                    let typ = format_ident!(
                                        "{}",
                                        name.to_pascal_case(),
                                        span = struct_name.span()
                                    );
                                    quote! {
                                        #typ(::std::vec::Vec<#typ>)
                                    }
                                });
                                let token = quote! {
                                    #[derive(Clone, Debug)]
                                    pub enum #struct_name {
                                        PCDATA(::std::vec::Vec<String>),
                                        #(#fields, )*
                                    }
                                };
                                // println!(" > ZeroOrManyTimes: {}", struct_name);
                                context.entry(struct_name).or_insert_with(|| {
                                    tokens.push(token.clone());
                                    token
                                });
                            }
                            _ => unreachable!(),
                        }
                    }
                    parser::ElementCategory::Children(children) => {
                        let (ident, token_stream) =
                            children.to_token_stream(&mut context, &struct_name);
                        let token = quote! {
                            pub type #struct_name = #ident;
                        };
                        context.entry(struct_name).or_insert_with(|| {
                            tokens.push(token.clone());
                            token
                        });
                        tokens.push(token_stream.clone());
                    }
                }
            }
            parser::ElementType::Entity(_entity) => {
                //
            }
            parser::ElementType::Attlist(attlist) => {
                let attrname = format_ident!("{}Attributes", attlist.name.to_pascal_case());
                let mut names: Vec<Ident> = Vec::new();
                let mut types: Vec<TokenStream2> = Vec::new();
                attlist.attdefs.iter().for_each(|attdef| {
                    let name = format_ident!("{}", attdef.name.to_pascal_case());
                    let (typename, token_stream) = attdef.to_token_stream(&mut context, &name);
                    let typename = match attdef.default_decl {
                        parser::DefaultDecl::Implied => {
                            quote! { ::std::option::Option<#typename> }
                        }
                        parser::DefaultDecl::Required => {
                            quote! { #typename }
                        }
                        parser::DefaultDecl::Fixed(ref default)
                        | parser::DefaultDecl::Default(ref default) => {
                            let default = default.to_string();
                            if !token_stream.is_empty() {
                                tokens.push(quote! {
                                    impl ::std::default::Default for #typename {
                                        fn default() -> #typename {
                                            use ::std::convert::TryFrom;
                                            #typename::try_from(#default).unwrap()
                                        }
                                    }
                                });
                            }
                            quote!( #typename )
                        }
                    };
                    names.push(safe!(format_ident!("{}", name.to_string().to_snake_case())));
                    types.push(typename);
                    tokens.push(token_stream);
                });
                tokens.push(quote! {
                    #[derive(Clone, Debug)]
                    pub struct #attrname {
                        #(pub #names: #types), *
                    }
                });
            }
            parser::ElementType::Comment(_) => {
                //
            }
        }
    }

    /*
    tokens
        .iter()
        .for_each(|token| println!("{}", token.to_string()));
    */

    TokenStream2::from_iter(tokens.into_iter()).into()
}

type Context = HashMap<Ident, TokenStream2>;

trait ToTokenStream {
    fn to_token_stream(&self, context: &mut Context, ident: &Ident) -> (Ident, TokenStream2);
}

impl<T> ToTokenStream for parser::Repeatable<T>
where
    T: ToTokenStream,
{
    fn to_token_stream(&self, context: &mut Context, ident: &Ident) -> (Ident, TokenStream2) {
        match self {
            parser::Repeatable::Once(once) => once.to_token_stream(context, ident),
            parser::Repeatable::AtMostOnce(opt) => {
                // opt
                let (ident, mut token_stream) = opt.to_token_stream(context, ident);
                let name = format_ident!("Opt{}", ident, span = ident.span());

                let defined = context.get(&name).is_some();
                if defined {
                    // (ident, TokenStream2::new())
                } else {
                    let token = quote! {
                        pub type #name = ::std::option::Option<#ident>;
                    };

                    context.insert(name.clone(), token.clone());

                    token_stream.extend(token);
                }

                (name, token_stream)
            }
            parser::Repeatable::AtLeastOnce(more_than_zero) => {
                let (ident, mut token_stream) = more_than_zero.to_token_stream(context, ident);
                let name = format_ident!("NonEmpty{}", ident, span = ident.span());

                let defined = context.get(&name).is_some();

                if !defined {
                    let token = quote! {
                        pub type #name = ::std::vec::Vec<#ident>;
                    };
                    context.insert(name.clone(), token.clone());

                    token_stream.extend(token);
                } else {
                    //
                }
                (name, token_stream)
            }
            parser::Repeatable::ZeroOrManyTimes(multi_or_empty) => {
                let (ident, mut token_stream) = multi_or_empty.to_token_stream(context, ident);
                let name = format_ident!(
                    "{}",
                    ident.to_string().to_table_case().to_pascal_case(),
                    span = ident.span()
                );

                // println!("ZeroOrManyTimes -> {}", name);

                let token = quote! {
                    pub type #name = ::std::vec::Vec<#ident>;
                };

                let defined = context.get(&name).is_some();
                if !defined {
                    context.insert(name.clone(), token.clone());

                    token_stream.extend(token);
                } else {
                    // TODO
                }

                (ident, token_stream)
            }
        }
    }
}

impl ToTokenStream for parser::Child {
    fn to_token_stream(&self, context: &mut Context, ident: &Ident) -> (Ident, TokenStream2) {
        match self {
            parser::Child::Name(name) => name.to_token_stream(context, ident),
            parser::Child::Seq(seq) => seq.to_token_stream(context, ident),
            parser::Child::Choices(choices) => choices.to_token_stream(context, ident),
        }
    }
}

impl<T> ToTokenStream for parser::Seq<T>
where
    T: ToTokenStream + ::std::fmt::Display,
{
    fn to_token_stream(&self, context: &mut Context, ident: &Ident) -> (Ident, TokenStream2) {
        let (names, mut token_streams): (::std::vec::Vec<_>, ::std::vec::Vec<_>) = self
            .iter()
            .map(|c| c.to_token_stream(context, ident))
            .unzip();

        let mut names_iter = names.iter();
        let mut ident = format_ident!("Tuple{}", names_iter.next().unwrap(), span = ident.span());
        for name in names_iter {
            ident = format_ident!("{}{}", ident, name, span = ident.span());
        }
        let fields = names
            .iter()
            .map(|name| safe!(format_ident!("{}", name.to_string().to_snake_case())));

        let token = quote! {
            #[derive(Clone, Debug)]
            pub struct #ident {
                #(pub #fields: #names), *
            }
        };
        let defined = context.get(&ident).is_some();

        if !defined {
            context.insert(ident.clone(), token.clone());

            token_streams.push(token);
        } else {
            // TODO: Ensure that's same.
        }
        (ident, TokenStream2::from_iter(token_streams.into_iter()))
    }
}

impl<T> ToTokenStream for parser::Choices<T>
where
    T: ToTokenStream + ::std::fmt::Display,
{
    fn to_token_stream(&self, context: &mut Context, ident: &Ident) -> (Ident, TokenStream2) {
        let ident = format_ident!("{}Choices", ident, span = ident.span());
        let (names, mut token_streams): (Vec<_>, Vec<_>) = self
            .iter()
            .map(|c| c.to_token_stream(context, &ident))
            .unzip();

        let token = quote! {
            #[derive(Clone, Debug)]
            pub enum #ident {
                #(#names(#names),)*
            }
        };

        assert!(context.get(&ident).is_none());
        context.insert(ident.clone(), token.clone());

        token_streams.push(token);
        (ident, TokenStream2::from_iter(token_streams.into_iter()))
    }
}

impl ToTokenStream for parser::Name {
    fn to_token_stream(&self, context: &mut Context, ident: &Ident) -> (Ident, TokenStream2) {
        let ident = format_ident!("{}", self.as_str().to_pascal_case(), span = ident.span());

        let defined = context.get(&ident).is_some();
        if defined {
            // println!(" --> {} already exists.", ident.to_string());
            (ident, TokenStream2::new())
        } else {
            let token = quote! {
                #[derive(Clone, Debug)]
                pub struct #ident;
            };
            context.insert(ident.clone(), token.clone());
            (ident, token.into())
        }
    }
}

/// attdefs: [
///     AttDef {
///         name: Name(
///             "align",
///         ),
///         atttype: EnumeratedType(
///             Enumeration(
///                 Enumeration(
///                     [
///                         Nmtoken(
///                             "left",
///                         ),
///                         Nmtoken(
///                             "center",
///                         ),
///                         Nmtoken(
///                             "right",
///                         ),
///                     ],
///                 ),
///             ),
///         ),
///         default_decl: Implied,
///     },
///     AttDef {
///         name: Name(
///             "width",
///         ),
///         atttype: StringType,
///         default_decl: Implied,
///     },
/// ],
impl ToTokenStream for parser::AttDef {
    fn to_token_stream(&self, context: &mut Context, ident: &Ident) -> (Ident, TokenStream2) {
        let mut tokens = Vec::new();
        let (name, tokens) = match self.atttype {
            parser::AttType::StringType => {
                let ident = format_ident!("{}StringType", ident);
                if context.get(&ident).is_none() {
                    let token = quote! {
                        #[derive(Clone, Debug)]
                        pub struct #ident(pub String);
                        impl ::std::convert::TryFrom<&str> for #ident {
                            type Error = ::std::string::String;
                            fn try_from(s: &str) -> Result<#ident, Self::Error> {
                                ::std::result::Result::Ok(#ident(s.to_string()))
                            }
                        }
                    };
                    context.insert(ident.clone(), token.clone());
                    tokens.push(token);
                }
                (ident, tokens)
            }
            parser::AttType::TokenizedType(ref _tokenized_type) => {
                // FIXME: https://www.w3.org/TR/REC-xml/#NT-TokenizedType
                let ident = format_ident!("{}TokenizedType", ident);
                if context.get(&ident).is_none() {
                    eprintln!(
                        "TokenizedType `{}` has been implemented as `String`.",
                        _tokenized_type
                    );
                    let token = quote! {
                        #[derive(Clone, Debug)]
                        pub struct #ident(pub String);
                        impl ::std::convert::TryFrom<&str> for #ident {
                            type Error = ::std::string::String;
                            fn try_from(s: &str) -> Result<#ident, Self::Error> {
                                ::std::result::Result::Ok(#ident(s.to_string()))
                            }
                        }
                    };
                    context.insert(ident.clone(), token.clone());
                    tokens.push(token);
                }
                (ident, tokens)
            }
            parser::AttType::EnumeratedType(ref enumerated_type) => {
                match enumerated_type {
                    parser::EnumeratedType::NotationType(_notation_type) => {
                        // FIXME: For compatibility, an attribute of type NOTATION MUST NOT be declared on an element declared EMPTY.
                        unimplemented!("NotationType {}", _notation_type);
                    }
                    parser::EnumeratedType::Enumeration(enumeration) => {
                        let name_types = format_ident!("{}EnumerationType", ident);
                        let variants = enumeration
                            .iter()
                            .map(|e| format_ident!("{}", e.to_pascal_case()));
                        let variants2 = variants.clone();
                        let values = enumeration.iter().map(|v| {
                            let v = v.as_str();
                            quote! { #v }
                        });
                        let values2: Vec<&str> = enumeration.iter().map(|v| v.as_str()).collect();
                        let values2 = values2.join(", ");
                        if context.get(&name_types).is_none() {
                            let token = quote! {
                                #[derive(Clone, Debug)]
                                pub enum #name_types {
                                    #(#variants, )*
                                }
                                impl ::std::convert::TryFrom<&str> for #name_types {
                                    type Error = ::std::string::String;
                                    fn try_from(s: &str) -> Result<#name_types, Self::Error> {
                                        match s {
                                            #(#values => ::std::result::Result::Ok(#name_types::#variants2),)*
                                            _ => ::std::result::Result::Err(format!("value must be one of `{}`, but not `{}`.", #values2, s))
                                        }
                                    }
                                }
                            };
                            context.insert(name_types.clone(), token.clone());
                            tokens.push(token);
                        } else {
                        }
                        (name_types, tokens)
                    }
                }
            }
        };
        (name, TokenStream2::from_iter(tokens.into_iter()))
    }
}
