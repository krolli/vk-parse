extern crate clap;
extern crate ron;
extern crate vk_parse;
extern crate vkxml;
extern crate xml;

fn save_registry_ron(registry: &vkxml::Registry, path: &std::path::Path) {
    use std::io::Write;
    let text = ron::ser::to_string_pretty(
        registry,
        ron::ser::PrettyConfig {
            depth_limit: usize::max_value(),
            new_line: String::from("\r\n"),
            indentor: String::from("\t"),
            separate_tuple_members: false,
            enumerate_arrays: false,
        },
    ).unwrap();

    let mut file = std::fs::File::create(path).unwrap();
    file.write_all(text.as_bytes()).unwrap();
}

fn main() {
    let matches = clap::App::new("vk-parse")
        .version("0.1")
        .author("krolli")
        .about("Convert vk.xml to RON represenation of New Vulkan XML format.")
        .arg(
            clap::Arg::with_name("input")
                .value_name("SRC")
                .help("Path to vk.xml file to use as input.")
                .takes_value(true)
                .required(true),
        )
        .arg(
            clap::Arg::with_name("output")
                .value_name("DST")
                .short("o")
                .help("Output directory for directory structure contained within PBO source.")
                .takes_value(true)
                .default_value("vk.ron"),
        )
        .get_matches();

    let input_str = matches.value_of("input").unwrap();
    let output_str = matches.value_of("output").unwrap();
    let output_path = std::path::Path::new(output_str);
    let input_path = std::path::Path::new(input_str);

    let registry = vk_parse::parse_file_as_vkxml(input_path);
    save_registry_ron(&registry, output_path);
}
