#![deny(warnings)]
extern crate reqwest;

extern crate ron;
extern crate serde;
extern crate vk_parse;
extern crate vkxml;
extern crate xml;

use std::path::{Path, PathBuf};

const URL_REPO: &str = "https://raw.githubusercontent.com/KhronosGroup/Vulkan-Docs";
const URL_MASTER: &str =
    "https://raw.githubusercontent.com/KhronosGroup/Vulkan-Docs/master/xml/vk.xml";

struct VkXmlSourceRange {
    version: &'static str,
    url_suffix: &'static str,
    patches: std::ops::Range<u32>,
}

fn save_registry_ron<T: serde::Serialize>(registry: &T, path: &Path) {
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

fn download<T: std::io::Write>(dst: &mut T, url: &str) {
    use std::io::BufRead;

    let resp = reqwest::get(url).expect(&format!("Failed to GET resource: {:?}", url));
    let _size = resp
        .headers()
        .get::<reqwest::header::ContentLength>()
        .map(|ct_len| **ct_len)
        .unwrap_or(0);
    if !resp.status().is_success() {
        panic!("Download request failed with status: {:?}", resp.status())
    }
    // if size == 0 {
    //     panic!("Size of content returned was 0")
    // }

    let mut src = std::io::BufReader::new(resp);
    loop {
        let n = {
            let mut buf = src.fill_buf().unwrap();
            dst.write_all(&mut buf).unwrap();
            buf.len()
        };
        if n == 0 {
            break;
        }
        src.consume(n);
    }
}

fn download_to(url: &str, dst: &Path) {
    if dst.exists() {
        return;
    }

    let mut dst_file = std::io::BufWriter::new(std::fs::File::create(dst).unwrap());
    download(&mut dst_file, url);
}

fn download_spec(ranges: &[VkXmlSourceRange], download_dir: &str) {
    for range in ranges.iter() {
        for patch in range.patches.clone() {
            println!("Downloading spec {}{}", range.version, patch);

            let vk_xml = format!("{}/{}{}.vk.xml", download_dir, range.version, patch);
            let vk_rnc = format!("{}/{}{}.registry.rnc", download_dir, range.version, patch);

            {
                let src = format!(
                    "{}/{}{}{}/vk.xml",
                    URL_REPO, range.version, patch, range.url_suffix
                );
                let dst = &vk_xml;
                println!("\tDownloading {:?} -> {:?}", src, dst);

                let dst_path = PathBuf::from(dst);
                std::fs::create_dir_all(dst_path.parent().unwrap()).unwrap();
                download_to(&src, &dst_path);
            }

            {
                let src = format!(
                    "{}/{}{}{}/registry.rnc",
                    URL_REPO, range.version, patch, range.url_suffix
                );
                let dst = &vk_rnc;
                println!("\tDownloading {:?} -> {:?}", src, dst);

                let dst_path = PathBuf::from(dst);
                std::fs::create_dir_all(dst_path.parent().unwrap()).unwrap();
                download_to(&src, &dst_path);
            }
        }
    }
}

const _RANGES: &[VkXmlSourceRange] = &[
    VkXmlSourceRange {
        version: "v1.0.",
        url_suffix: "-core/src/spec",
        patches: 33..37,
    },
    VkXmlSourceRange {
        version: "v1.0.",
        url_suffix: "-core/src/spec",
        patches: 38..52,
    },
    VkXmlSourceRange {
        version: "v1.0.",
        url_suffix: "-core/src/spec",
        patches: 53..70,
    },
    VkXmlSourceRange {
        version: "v1.1.",
        url_suffix: "/src/spec",
        patches: 70..72,
    },
    VkXmlSourceRange {
        version: "v1.1.",
        url_suffix: "/xml",
        patches: 72..85,
    },
];

const RANGES: &[VkXmlSourceRange] = &[
    VkXmlSourceRange {
        version: "v1.1.",
        url_suffix: "/xml",
        patches: 82..85,
    },
    VkXmlSourceRange {
        version: "v1.1.",
        url_suffix: "/xml",
        patches: 79..80,
    },
    VkXmlSourceRange {
        version: "v1.1.",
        url_suffix: "/xml",
        patches: 75..76,
    },
    VkXmlSourceRange {
        version: "v1.0.",
        url_suffix: "-core/src/spec",
        patches: 69..70,
    },
];

// #[test]
#[allow(dead_code)]
fn test_vkxml() {
    let ranges = RANGES;
    let download_dir = "test-data";
    download_spec(ranges, download_dir);
    for range in ranges.iter() {
        for patch in range.patches.clone() {
            println!("Processing {}{}", range.version, patch);

            let vk_xml = format!("{}/{}{}.vk.xml", download_dir, range.version, patch);
            let vk_ron = format!("{}/{}{}.vk.ron", download_dir, range.version, patch);

            println!("\tGenerating {:?}", vk_ron);
            let registry = vk_parse::parse_file_as_vkxml(Path::new(&vk_xml));
            save_registry_ron(&registry, Path::new(&vk_ron));
        }
    }
}

// #[test]
#[allow(dead_code)]
fn test() {
    let ranges = RANGES;
    let download_dir = "test-data";
    download_spec(ranges, download_dir);
    for range in ranges.iter() {
        for patch in range.patches.clone() {
            println!("Processing {}{}", range.version, patch);

            let vk_xml = format!("{}/{}{}.vk.xml", download_dir, range.version, patch);
            let vk_ron = format!("{}/{}{}.vk.ir.ron", download_dir, range.version, patch);

            println!("\tGenerating {:?}", vk_ron);
            let registry = vk_parse::parse_file(Path::new(&vk_xml));
            save_registry_ron(&registry, Path::new(&vk_ron));
        }
    }
}

// #[test]
#[allow(dead_code)]
fn test_ir_debug() {
    let ranges = RANGES;
    let download_dir = "test-data";
    download_spec(ranges, download_dir);
    for range in ranges.iter() {
        for patch in range.patches.clone() {
            println!("Processing {}{}", range.version, patch);

            let vk_xml = format!("{}/{}{}.vk.xml", download_dir, range.version, patch);
            let vk_rs = format!("{}/{}{}.vk.ir.rs", download_dir, range.version, patch);

            println!("\tGenerating {:?}", vk_rs);
            let registry = vk_parse::parse_file(Path::new(&vk_xml));

            use std::io::Write;
            let mut file = std::io::BufWriter::new(std::fs::File::create(vk_rs).unwrap());
            write!(file, "{:#?}", registry).unwrap();
        }
    }
}

fn parsing_test(major: u32, minor: u32, patch: u32, url_suffix: &str) {
    let src = format!(
        "{}/v{}.{}.{}{}/vk.xml",
        URL_REPO, major, minor, patch, url_suffix
    );
    use std::io::Cursor;
    let mut buf = Cursor::new(vec![0; 15]);
    download(&mut buf, &src);
    buf.set_position(0);
    let _ir_parse = vk_parse::parse_stream(buf.clone());
    let _vkxml_parse = vk_parse::parse_stream_as_vkxml(buf.clone());
}

macro_rules! test_version {
    ($test_name:ident, $major:expr, $minor:expr, $patch:expr, $url_suffix:expr) => {
        #[test]
        fn $test_name() {
            parsing_test($major, $minor, $patch, $url_suffix);
        }
    };
}

#[test]
fn test_master() {
    use std::io::Cursor;
    let mut buf = Cursor::new(vec![0; 15]);
    download(&mut buf, URL_MASTER);
    buf.set_position(0);
    let _ir_parse = vk_parse::parse_stream(buf.clone());
    let _vkxml_parse = vk_parse::parse_stream_as_vkxml(buf.clone());
}

test_version!{test_v1_0_33, 1, 0, 33, "-core/src/spec"}
test_version!{test_v1_0_34, 1, 0, 34, "-core/src/spec"}
test_version!{test_v1_0_35, 1, 0, 35, "-core/src/spec"}
test_version!{test_v1_0_36, 1, 0, 36, "-core/src/spec"}
// test_version!{test_v1_1_37, 1, 0, 37, "-core/src/spec"} // no tag for v1.0.37
test_version!{test_v1_0_38, 1, 0, 38, "-core/src/spec"}
test_version!{test_v1_0_39, 1, 0, 39, "-core/src/spec"}
test_version!{test_v1_0_40, 1, 0, 40, "-core/src/spec"}
test_version!{test_v1_0_41, 1, 0, 41, "-core/src/spec"}
test_version!{test_v1_0_42, 1, 0, 42, "-core/src/spec"}
test_version!{test_v1_0_43, 1, 0, 43, "-core/src/spec"}
test_version!{test_v1_0_44, 1, 0, 44, "-core/src/spec"}
test_version!{test_v1_0_45, 1, 0, 45, "-core/src/spec"}
test_version!{test_v1_0_46, 1, 0, 46, "-core/src/spec"}
test_version!{test_v1_0_47, 1, 0, 47, "-core/src/spec"}
test_version!{test_v1_0_48, 1, 0, 48, "-core/src/spec"}
test_version!{test_v1_0_49, 1, 0, 49, "-core/src/spec"}
test_version!{test_v1_0_50, 1, 0, 50, "-core/src/spec"}
test_version!{test_v1_0_51, 1, 0, 51, "-core/src/spec"}
// test_version!{test_v1_0_52, 1, 0, 52, "-core/src/spec"} // no tag for v1.0.52
test_version!{test_v1_0_53, 1, 0, 53, "-core/src/spec"}
test_version!{test_v1_0_54, 1, 0, 54, "-core/src/spec"}
test_version!{test_v1_0_55, 1, 0, 55, "-core/src/spec"}
test_version!{test_v1_0_56, 1, 0, 56, "-core/src/spec"}
test_version!{test_v1_0_57, 1, 0, 57, "-core/src/spec"}
test_version!{test_v1_0_58, 1, 0, 58, "-core/src/spec"}
test_version!{test_v1_0_59, 1, 0, 59, "-core/src/spec"}
test_version!{test_v1_0_60, 1, 0, 60, "-core/src/spec"}
test_version!{test_v1_0_61, 1, 0, 61, "-core/src/spec"}
test_version!{test_v1_0_62, 1, 0, 62, "-core/src/spec"}
test_version!{test_v1_0_63, 1, 0, 63, "-core/src/spec"}
test_version!{test_v1_0_64, 1, 0, 64, "-core/src/spec"}
test_version!{test_v1_0_65, 1, 0, 65, "-core/src/spec"}
test_version!{test_v1_0_66, 1, 0, 66, "-core/src/spec"}
test_version!{test_v1_0_67, 1, 0, 67, "-core/src/spec"}
test_version!{test_v1_0_68, 1, 0, 68, "-core/src/spec"}
test_version!{test_v1_0_69, 1, 0, 69, "-core/src/spec"}
test_version!{test_v1_1_70, 1, 1, 70, "/src/spec"}
test_version!{test_v1_1_71, 1, 1, 71, "/src/spec"}
test_version!{test_v1_1_72, 1, 1, 72, "/xml"}
test_version!{test_v1_1_73, 1, 1, 73, "/xml"}
test_version!{test_v1_1_74, 1, 1, 74, "/xml"}
test_version!{test_v1_1_75, 1, 1, 75, "/xml"}
test_version!{test_v1_1_76, 1, 1, 76, "/xml"}
test_version!{test_v1_1_77, 1, 1, 77, "/xml"}
test_version!{test_v1_1_78, 1, 1, 78, "/xml"}
test_version!{test_v1_1_79, 1, 1, 79, "/xml"}
test_version!{test_v1_1_80, 1, 1, 80, "/xml"}
test_version!{test_v1_1_81, 1, 1, 81, "/xml"}
test_version!{test_v1_1_82, 1, 1, 82, "/xml"}
test_version!{test_v1_1_83, 1, 1, 83, "/xml"}
test_version!{test_v1_1_84, 1, 1, 84, "/xml"}
