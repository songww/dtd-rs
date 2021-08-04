#![feature(proc_macro_hygiene)]

#[cfg_attr(feature = "it", visibility::make(pub))]
mod parser;
