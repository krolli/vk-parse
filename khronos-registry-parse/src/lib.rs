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
pub use types::*;

#[macro_use]
mod util;
mod c;
mod types;

#[cfg(feature = "opengl")]
pub mod gl;
#[cfg(feature = "vulkan")]
pub mod vk;
