#[cfg(feature = "vkxml-convert")]
mod convert;
mod parse;
mod types;

#[cfg(feature = "vkxml-convert")]
extern crate vkxml;
#[cfg(feature = "vkxml-convert")]
pub use convert::parse_file_as_vkxml;
#[cfg(feature = "vkxml-convert")]
pub use convert::parse_stream_as_vkxml;
pub use parse::parse_file;
pub use parse::parse_stream;
