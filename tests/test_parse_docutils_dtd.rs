use dtd::parser;
use nom::Finish;

#[test]
fn test_docutils() {
    let data = include_str!("../testdata/docutils.dtd");

    let parsed = parser::parse(data).finish();
    assert!(
        parsed.is_ok(),
        "{}",
        parsed.as_ref().unwrap_err().to_string()
    );
    let (remaind, elements) = parsed.unwrap();
    assert_eq!(remaind, "");
    assert!(elements.len() > 0);
}
