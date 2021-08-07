/*
use dtd::parser;

#[test]
fn test_docutils_parse_file() {
    let parsed = parser::parse("testdata/docutils.dtd");
    assert!(
        parsed.is_ok(),
        "{}",
        parsed.as_ref().unwrap_err().to_string()
    );
    let elements = parsed.unwrap();
    assert!(elements.len() > 0);
}

#[test]
fn test_docutils_parse_str() {
    let data = include_str!("../testdata/docutils.dtd");
    let parsed = parser::parse_str(data);
    assert!(
        parsed.is_ok(),
        "{}",
        parsed.as_ref().unwrap_err().to_string()
    );
    let elements = parsed.unwrap();
    assert!(elements.len() > 0);
}
*/
