pub use dtd_parser as parser;

pub use dtd_macro::dtd;

//mod generated;

mod testing {
    use super::dtd;

    dtd!("testdata/docutils.dtd");
}
