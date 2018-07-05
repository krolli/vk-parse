#[macro_use]
extern crate serde_derive;
extern crate serde;
extern crate vkxml;
extern crate xml;

#[macro_use]
mod parse;
mod convert;
mod types;

pub use convert::parse_file_as_vkxml;
pub use parse::parse_file;
pub use types::*;
