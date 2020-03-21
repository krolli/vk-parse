//! This crate parses the Vulkan XML registry into a Rust object.
//!
//! The entry point into this library is `parse_file_as_vkxml`, which will
//! return a `Registry` object. This object contains all the information contained
//! in the Vulkan API registry.

extern crate xml;

#[cfg(feature = "serialize")]
#[macro_use]
extern crate serde_derive;
#[cfg(feature = "serialize")]
extern crate serde;

#[cfg(feature = "vkxml-convert")]
extern crate vkxml;

#[macro_use]
mod parse;
mod c;
#[cfg(feature = "vkxml-convert")]
mod convert;
mod types;

#[cfg(feature = "vkxml-convert")]
pub use convert::parse_file_as_vkxml;
#[cfg(feature = "vkxml-convert")]
pub use convert::parse_stream_as_vkxml;
pub use parse::parse_file;
pub use parse::parse_stream;
pub use types::*;
