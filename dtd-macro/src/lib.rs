#![feature(proc_macro_diagnostic)]

use proc_macro::TokenStream;
use quote::{quote, quote_spanned};
use syn::parse::{Parse, ParseStream, Result};
use syn::spanned::Spanned;
use syn::{parse_macro_input, Expr, Ident, LitStr, Token, Type, Visibility};

enum DefinetionsOrPath {
    Definetions(String),
    Path(LitStr),
}

impl Parse for DefinetionsOrPath {
    fn parse(input: ParseStream) -> Result<Self> {
        let maybe_path: LitStr = input.parse()?;
        let value = maybe_path.value();
        let trimed = value.trim();
        if trimed.starts_with("<") && trimed.ends_with(">") {
            Ok(DefinetionsOrPath::Definetions(value))
        } else {
            Ok(DefinetionsOrPath::Path(maybe_path))
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
    let definitions_or_path = parse_macro_input!(input as DefinetionsOrPath);

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
    let parsed = match definitions_or_path {
        DefinetionsOrPath::Definetions(definitions) => dtd_parser::parse_str(&definitions),
        DefinetionsOrPath::Path(path) => {
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
            let pathbuf = std::path::PathBuf::from(value);
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
            dtd_parser::parse(pathbuf)
        }
    };

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
    TokenStream::new()
}

#[cfg(test)]
mod tests {
    #[test]
    fn from_path() {
        assert_eq!(2 + 2, 4);
    }
}
