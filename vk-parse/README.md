# vk-parse

[![LICENSE](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE-MIT)
[![LICENSE](https://img.shields.io/badge/license-apache-blue.svg)](LICENSE-APACHE)
[![Documentation](https://docs.rs/vk-parse/badge.svg)](https://docs.rs/vk-parse)
[![Crates.io Version](https://img.shields.io/crates/v/vk-parse.svg)](https://crates.io/crates/vk-parse)
[![](https://tokei.rs/b1/github/krolli/vk-parse)](https://github.com/krolli/vk-parse)
[![Build Status](https://travis-ci.org/krolli/vk-parse.svg?branch=master)](https://travis-ci.org/krolli/vk-parse)

`vk-parse` is a Rust crate which parses the Vulkan API registry XML and converts it to a Rust representation.

This crate provides a library which parses the Vulkan registry XML and either provides its own lossless representation of the registry, or converts it to structures from [vkxml](https://github.com/terrybrashaw/vkxml).

## Usage

To get started, you'll need `vk.xml` file which is used for generating Vulkan header files and is stored in [Vulkan-Docs](https://github.com/KhronosGroup/Vulkan-Docs) repository.

After that, in your Rust project:

`Cargo.toml`
```toml
[dependencies]
vk-parse = "0.4"
vkxml = "0.3"
```

`main.rs`
```rust
extern crate vk_parse;
extern crate vkxml;
use std::path::Path;

fn main() {
    let registry_ir = vk_parse::parse_file(Path::new("vk.xml"));
    println!("{:?}", registry_ir);
    let registry_vkxml: vkxml::Registry = registry_ir.into();
    println!("{:?}", registry_vkxml);
}
```

## License

This software is dual-licensed under Apache-2.0/MIT, same as Rust itself.
