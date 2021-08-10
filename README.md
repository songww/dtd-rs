# DTD Parser

## Install

```shell
cargo add dtd-rs
```

## Usage

```Rust
use dtd::dtd;

// parse dtd elements.
assert_eq!(dtd! {
    "<!ELEMENT note (to,from,heading,body)>
    <!ELEMENT to (#PCDATA)>
    <!ELEMENT from (#PCDATA)>
    <!ELEMENT heading (#PCDATA)>
    <!ELEMENT body (#PCDATA)>"
}, struct Note {
    to: String,
    from: String,
    heading: String,
    body: String,
});

// content of `path/to/file.dtd`
//    <!ELEMENT note (to,from,heading,body)>
//    <!ELEMENT to (#PCDATA)>
//    <!ELEMENT from (#PCDATA)>
//    <!ELEMENT heading (#PCDATA)>
//    <!ELEMENT body (#PCDATA)>
// parse from file
assert_eq!(dtd!("path/to/file.dtd"), struct Note {
    to: String,
    from: String,
    heading: String,
    body: String,
});
```
