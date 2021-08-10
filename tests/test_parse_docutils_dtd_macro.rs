use dtd_macro::dtd;

#[test]
fn test_docutils_parse_file() {
    dtd!("testdata/docutils.dtd");
}
