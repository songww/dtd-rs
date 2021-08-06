use dtd_parser::parser;

#[test]
fn test_docutils() {
    let data = include_str!("../testdata/docutils.dtd");

    let parsed = parser::parse(data);
    assert!(
        parsed.is_ok(),
        "{}",
        parsed.as_ref().unwrap_err().to_string()
    );
    let elements = parsed.unwrap();
    assert!(elements.len() > 0);
}
