# DTD Parser

[<img alt="github" src="https://img.shields.io/badge/github-songww/dtd-rs?style=for-the-badge&labelColor=555555&logo=github" height="20">](https://github.com/songww/dtd-rs)
[<img alt="crates.io" src="https://img.shields.io/crates/v/dtd-rs.svg?style=for-the-badge&color=fc8d62&logo=rust" height="20">](https://crates.io/crates/dtd-rs)
[<img alt="docs.rs" src="https://img.shields.io/badge/docs.rs-dtd-rs?style=for-the-badge&labelColor=555555&logoColor=white&logo=data:image/svg+xml;base64,PHN2ZyByb2xlPSJpbWciIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyIgdmlld0JveD0iMCAwIDUxMiA1MTIiPjxwYXRoIGZpbGw9IiNmNWY1ZjUiIGQ9Ik00ODguNiAyNTAuMkwzOTIgMjE0VjEwNS41YzAtMTUtOS4zLTI4LjQtMjMuNC0zMy43bC0xMDAtMzcuNWMtOC4xLTMuMS0xNy4xLTMuMS0yNS4zIDBsLTEwMCAzNy41Yy0xNC4xIDUuMy0yMy40IDE4LjctMjMuNCAzMy43VjIxNGwtOTYuNiAzNi4yQzkuMyAyNTUuNSAwIDI2OC45IDAgMjgzLjlWMzk0YzAgMTMuNiA3LjcgMjYuMSAxOS45IDMyLjJsMTAwIDUwYzEwLjEgNS4xIDIyLjEgNS4xIDMyLjIgMGwxMDMuOS01MiAxMDMuOSA1MmMxMC4xIDUuMSAyMi4xIDUuMSAzMi4yIDBsMTAwLTUwYzEyLjItNi4xIDE5LjktMTguNiAxOS45LTMyLjJWMjgzLjljMC0xNS05LjMtMjguNC0yMy40LTMzLjd6TTM1OCAyMTQuOGwtODUgMzEuOXYtNjguMmw4NS0zN3Y3My4zek0xNTQgMTA0LjFsMTAyLTM4LjIgMTAyIDM4LjJ2LjZsLTEwMiA0MS40LTEwMi00MS40di0uNnptODQgMjkxLjFsLTg1IDQyLjV2LTc5LjFsODUtMzguOHY3NS40em0wLTExMmwtMTAyIDQxLjQtMTAyLTQxLjR2LS42bDEwMi0zOC4yIDEwMiAzOC4ydi42em0yNDAgMTEybC04NSA0Mi41di03OS4xbDg1LTM4Ljh2NzUuNHptMC0xMTJsLTEwMiA0MS40LTEwMi00MS40di0uNmwxMDItMzguMiAxMDIgMzguMnYuNnoiPjwvcGF0aD48L3N2Zz4K" height="20">](https://docs.rs/dtd-rs)
[<img alt="build status" src="https://img.shields.io/github/workflow/status/songww/dtd-rs/CI/master?style=for-the-badge" height="20">](https://github.com/songww/dtd-rs/actions?query=branch%3Amain)

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
