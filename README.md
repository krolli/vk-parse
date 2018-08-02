# vk-parse

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
