#[cfg(feature = "vkxml-convert")]
mod convert;
mod parse;
mod types;

#[cfg(feature = "vkxml-convert")]
extern crate vkxml;
#[cfg(feature = "vkxml-convert")]
pub use vk::convert::parse_file_as_vkxml;
#[cfg(feature = "vkxml-convert")]
pub use vk::convert::parse_stream_as_vkxml;

pub use vk::types::*;
pub use types::*;
pub use vk::parse::parse_file;
pub use vk::parse::parse_stream;

