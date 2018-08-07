//! This crate parses the Vulkan XML registry into a Rust object.
//!
//! The entry point into this library is `parse_file_as_vkxml`, which will
//! return a `Registry` object. This object contains all the information contained
//! in the Vulkan API registry.

#[macro_use]
extern crate serde_derive;
extern crate serde;
extern crate vkxml;
extern crate xml;

#[macro_use]
mod parse;
mod c;
mod convert;
mod types;

pub use convert::parse_file_as_vkxml;
pub use convert::parse_stream_as_vkxml;
pub use parse::parse_file;
pub use parse::parse_stream;
pub use types::*;
