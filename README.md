# DTD Parser

## Install

```shell
cargo add dtd-rs --allow-prerelease
```

## Usage

```Rust
use dtd::dtd;

// parse dtd elements.
dtd! {
    "<!ELEMENT note (to,from,heading,body)>
    <!ELEMENT to (#PCDATA)>
    <!ELEMENT from (#PCDATA)>
    <!ELEMENT heading (#PCDATA)>
    <!ELEMENT body (#PCDATA)>"
}

// Generated:
//    pub struct Body(pub String);
//    #[derive(Clone, Debug)]
//    pub struct Body(pub String);
//    #[derive(Clone, Debug)]
//    pub struct Heading(pub String);
//    #[derive(Clone, Debug)]
//    pub struct From(pub String);
//    #[derive(Clone, Debug)]
//    pub struct To(pub String);
//    #[derive(Clone, Debug)]
//    pub struct TupleToFromHeadingBody {
//        pub to: To,
//        pub from: From,
//        pub heading: Heading,
//        pub body: Body,
//    }
//    pub type Note = TupleToFromHeadingBody;

let note = Note {
    to: To(String::From("to somewhere")),
    from: From(String::From("from somewhere")),
    heading: Heading(String::From("this is heading")),
    body: Body(String::From("Some Body"))
}

println!("{:?}", note);

// Or parse from file:
//
// content of `path/to/file.dtd`
//    <!ELEMENT note (to,from,heading,body)>
//    <!ELEMENT to (#PCDATA)>
//    <!ELEMENT from (#PCDATA)>
//    <!ELEMENT heading (#PCDATA)>
//    <!ELEMENT body (#PCDATA)>
// parse from file
dtd!("path/to/file.dtd");
```
