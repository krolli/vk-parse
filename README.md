# vk-parse

[![LICENSE](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE-MIT)
[![LICENSE](https://img.shields.io/badge/license-apache-blue.svg)](LICENSE-APACHE)
[![Documentation](https://docs.rs/vk-parse/badge.svg)](https://docs.rs/vk-parse)
[![Crates.io Version](https://img.shields.io/crates/v/vk-parse.svg)](https://crates.io/crates/vk-parse)
[![](https://tokei.rs/b1/github/krolli/vk-parse)](https://github.com/krolli/vk-parse)

`vk-parse` is a Rust crate which parses the Vulkan API registry XML and converts it to a Rust representation.

This crate consists of a

- library which does the actual parsing of the Vulkan registry
- command line tool which parses Vulkan XML and serializes it to a [RON](https://github.com/ron-rs/ron) file.

## Command Line Tool usage

```sh
cargo run --release 'vulkan.xml' -o 'generated-file.ron'
```

## License

This software is dual-licensed under Apache-2.0/MIT, same as Rust itself.
