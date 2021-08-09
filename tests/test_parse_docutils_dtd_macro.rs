use dtd_macro::dtd;

#[test]
fn test_docutils_parse_file() {
    dtd!("testdata/docutils.dtd");
}

/*
#[test]
fn test_docutils_parse_str() {
    let parsed = dtd!(
        r#"<!ENTITY Pub-Status "This is a pre-release of the
 specification.">"#
    );
    // assert!(
    //     parsed.is_ok(),
    //     "{}",
    //     parsed.as_ref().unwrap_err().to_string()
    // );
    // let elements = parsed.unwrap();
    // assert!(elements.len() > 0);
}

#[test]
fn test_docutils_parse_str_element() {
    let parsed = dtd!("<!ELEMENT math (#PCDATA)>");
    // assert!(
    //     parsed.is_ok(),
    //     "{}",
    //     parsed.as_ref().unwrap_err().to_string()
    // );
    // let elements = parsed.unwrap();
    // assert!(elements.len() > 0);
}
*/
