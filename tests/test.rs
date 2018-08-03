#![deny(warnings)]
extern crate reqwest;

extern crate ron;
extern crate serde;
extern crate serde_xml_rs;
extern crate vk_parse;
extern crate vkxml;
extern crate xml;

use std::path::{Path, PathBuf};

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

fn download_to(url: &str, dst: &Path) {
    use std::io::{BufRead, Write};

    if dst.exists() {
        return;
    }

    let resp = reqwest::get(url).expect(&format!("Failed to GET resource: {:?}", url));
    let _size = resp.headers()
        .get::<reqwest::header::ContentLength>()
        .map(|ct_len| **ct_len)
        .unwrap_or(0);
    if !resp.status().is_success() {
        panic!("Download request failed with status: {:?}", resp.status())
    }
    // if size == 0 {
    //     panic!("Size of content returned was 0")
    // }

    let mut dst_file = std::io::BufWriter::new(std::fs::File::create(dst).unwrap());
    let mut src = std::io::BufReader::new(resp);
    loop {
        let n = {
            let mut buf = src.fill_buf().unwrap();
            dst_file.write_all(&mut buf).unwrap();
            buf.len()
        };
        if n == 0 {
            break;
        }
        src.consume(n);
    }
}

fn download_spec(ranges: &[VkXmlSourceRange], download_dir: &str) {
    for range in ranges.iter() {
        for patch in range.patches.clone() {
            println!("Downloading spec {}{}", range.version, patch);

            let url_repo = "https://raw.githubusercontent.com/KhronosGroup/Vulkan-Docs";

            let vk_xml = format!("{}/{}{}.vk.xml", download_dir, range.version, patch);
            let vk_rnc = format!("{}/{}{}.registry.rnc", download_dir, range.version, patch);

            {
                let src = format!(
                    "{}/{}{}{}/vk.xml",
                    url_repo, range.version, patch, range.url_suffix
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
                    url_repo, range.version, patch, range.url_suffix
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

fn generate_references(ranges: &[VkXmlSourceRange], download_dir: &str) {
    for range in ranges.iter() {
        for patch in range.patches.clone() {
            println!("Generating reference {}{}", range.version, patch);

            let dir_ref_gen = "c:/Users/Melesie/Downloads/New-Vulkan-XML-Format-master";

            let vk_xml = format!("{}/{}{}.vk.xml", download_dir, range.version, patch);
            let vk_ron = format!("{}/{}{}.vk.ron", download_dir, range.version, patch);

            let final_copy_src = format!("{}/vk_new.xml", dir_ref_gen);
            let final_copy_dst = PathBuf::from(format!(
                "{}/{}{}.vk_new.xml",
                download_dir, range.version, patch
            ));
            let setup_copy_src = &vk_xml;
            let setup_copy_dst = format!("{}/src/vk.xml", dir_ref_gen);

            println!("\tGenerating reference {:?}", vk_ron);

            println!("\t\tCopying {:?} -> {:?}", setup_copy_src, setup_copy_dst);
            std::fs::copy(setup_copy_src, setup_copy_dst).unwrap();

            println!("\t\tRunning ConvertToNewFmt.lua");
            let return_code = std::process::Command::new("lua53.exe")
                .arg("ConvertToNewFmt.lua")
                .current_dir(dir_ref_gen)
                .spawn()
                .unwrap()
                .wait()
                .unwrap();

            if return_code.success() {
                println!("\t\tRenaming {:?} -> {:?}", final_copy_src, final_copy_dst);
                std::fs::rename(final_copy_src, &final_copy_dst).unwrap();

                let file = std::io::BufReader::new(std::fs::File::open(&final_copy_dst).unwrap());
                let result = serde_xml_rs::from_reader(file);
                let registry: vkxml::Registry = result.unwrap();
                println!("\t\tSaving reference version to {:?}", vk_ron);
                save_registry_ron(&registry, Path::new(&vk_ron));
            }
        }
    }
}

const RANGES: &[VkXmlSourceRange] = &[
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
        patches: 72..83,
    },
];

const _RANGES: &[VkXmlSourceRange] = &[
    VkXmlSourceRange {
        version: "v1.1.",
        url_suffix: "/xml",
        patches: 82..83,
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

#[test]
fn test_vkxml() {
    let ranges = RANGES;
    let download_dir = "test-data";
    download_spec(ranges, download_dir);
    if false {
        generate_references(ranges, download_dir);
    }
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

#[test]
fn test() {
    let ranges = RANGES;
    let download_dir = "test-data";
    download_spec(ranges, download_dir);
    if false {
        generate_references(ranges, download_dir);
    }
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

#[test]
fn test_ir_debug() {
    let ranges = RANGES;
    let download_dir = "test-data";
    download_spec(ranges, download_dir);
    if false {
        generate_references(ranges, download_dir);
    }
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
