#![deny(warnings)]

extern crate minreq;
extern crate ron;
extern crate serde;
extern crate vk_parse;
extern crate vkxml;
extern crate xml;

const URL_REPO: &str = "https://raw.githubusercontent.com/KhronosGroup/Vulkan-Docs";
const URL_MAIN: &str = "https://raw.githubusercontent.com/KhronosGroup/Vulkan-Docs/main/xml/vk.xml";
const URL_MAIN_VIDEO: &str =
    "https://raw.githubusercontent.com/KhronosGroup/Vulkan-Docs/main/xml/video.xml";

fn download<T: std::io::Write>(dst: &mut T, url: &str) {
    let resp = minreq::get(url)
        .send()
        .unwrap_or_else(|_| panic!("Failed to GET resource: {:?}", url));

    let is_success = 200 <= resp.status_code && resp.status_code < 300;
    if !is_success {
        panic!(
            "Download request failed with status: {:?}",
            resp.status_code
        )
    }

    dst.write_all(resp.as_bytes())
        .expect("Failed to write response body.");
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

    match vk_parse::parse_stream(buf.clone()) {
        Ok((_reg, errors)) => {
            if !errors.is_empty() {
                panic!("{:?}", errors);
            }
        }
        Err(fatal_error) => panic!("{:?}", fatal_error),
    }

    match vk_parse::parse_stream_as_vkxml(buf) {
        Ok(_) => (),
        Err(fatal_error) => panic!("{:?}", fatal_error),
    }
}

macro_rules! test_version {
    ($major:expr, $minor:expr, $patch:expr, $url_suffix:expr) => {
        paste::paste! {
            #[test]
            fn [< test_v $major _ $minor _ $patch >] () {
                parsing_test($major, $minor, $patch, $url_suffix);
            }
        }
    };
}

#[test]
fn test_main() {
    use std::io::Cursor;
    let mut buf = Cursor::new(vec![0; 15]);
    download(&mut buf, URL_MAIN);
    buf.set_position(0);

    match vk_parse::parse_stream(buf.clone()) {
        Ok((_reg, errors)) => {
            if !errors.is_empty() {
                panic!("{:?}", errors);
            }
        }
        Err(fatal_error) => panic!("{:?}", fatal_error),
    }

    match vk_parse::parse_stream_as_vkxml(buf) {
        Ok(_) => (),
        Err(fatal_error) => panic!("{:?}", fatal_error),
    }
}

#[test]
fn test_main_video() {
    use std::io::Cursor;
    let mut buf = Cursor::new(vec![0; 15]);
    download(&mut buf, URL_MAIN_VIDEO);
    buf.set_position(0);

    match vk_parse::parse_stream(buf.clone()) {
        Ok((_reg, errors)) => {
            if !errors.is_empty() {
                panic!("{:?}", errors);
            }
        }
        Err(fatal_error) => panic!("{:?}", fatal_error),
    }

    match vk_parse::parse_stream_as_vkxml(buf) {
        Ok(_) => (),
        Err(fatal_error) => panic!("{:?}", fatal_error),
    }
}

test_version! {1, 0, 33, "-core/src/spec"}
test_version! {1, 0, 34, "-core/src/spec"}
test_version! {1, 0, 35, "-core/src/spec"}
test_version! {1, 0, 36, "-core/src/spec"}
// test_version!{1, 0, 37, "-core/src/spec"} // no tag for v1.0.37
test_version! {1, 0, 38, "-core/src/spec"}
test_version! {1, 0, 39, "-core/src/spec"}
test_version! {1, 0, 40, "-core/src/spec"}
test_version! {1, 0, 41, "-core/src/spec"}
test_version! {1, 0, 42, "-core/src/spec"}
test_version! {1, 0, 43, "-core/src/spec"}
test_version! {1, 0, 44, "-core/src/spec"}
test_version! {1, 0, 45, "-core/src/spec"}
test_version! {1, 0, 46, "-core/src/spec"}
test_version! {1, 0, 47, "-core/src/spec"}
test_version! {1, 0, 48, "-core/src/spec"}
test_version! {1, 0, 49, "-core/src/spec"}
test_version! {1, 0, 50, "-core/src/spec"}
test_version! {1, 0, 51, "-core/src/spec"}
// test_version!{1, 0, 52, "-core/src/spec"} // no tag for v1.0.52
test_version! {1, 0, 53, "-core/src/spec"}
test_version! {1, 0, 54, "-core/src/spec"}
test_version! {1, 0, 55, "-core/src/spec"}
test_version! {1, 0, 56, "-core/src/spec"}
test_version! {1, 0, 57, "-core/src/spec"}
test_version! {1, 0, 58, "-core/src/spec"}
test_version! {1, 0, 59, "-core/src/spec"}
test_version! {1, 0, 60, "-core/src/spec"}
test_version! {1, 0, 61, "-core/src/spec"}
test_version! {1, 0, 62, "-core/src/spec"}
test_version! {1, 0, 63, "-core/src/spec"}
test_version! {1, 0, 64, "-core/src/spec"}
test_version! {1, 0, 65, "-core/src/spec"}
test_version! {1, 0, 66, "-core/src/spec"}
test_version! {1, 0, 67, "-core/src/spec"}
test_version! {1, 0, 68, "-core/src/spec"}
test_version! {1, 0, 69, "-core/src/spec"}
test_version! {1, 1, 70, "/src/spec"}
test_version! {1, 1, 71, "/src/spec"}
test_version! {1, 1, 72, "/xml"}
test_version! {1, 1, 73, "/xml"}
test_version! {1, 1, 74, "/xml"}
test_version! {1, 1, 75, "/xml"}
test_version! {1, 1, 76, "/xml"}
test_version! {1, 1, 77, "/xml"}
test_version! {1, 1, 78, "/xml"}
test_version! {1, 1, 79, "/xml"}
test_version! {1, 1, 80, "/xml"}
test_version! {1, 1, 81, "/xml"}
test_version! {1, 1, 82, "/xml"}
test_version! {1, 1, 83, "/xml"}
test_version! {1, 1, 84, "/xml"}
test_version! {1, 1, 85, "/xml"}
test_version! {1, 1, 86, "/xml"}
test_version! {1, 1, 87, "/xml"}
test_version! {1, 1, 88, "/xml"}
test_version! {1, 1, 89, "/xml"}
test_version! {1, 1, 90, "/xml"}
test_version! {1, 1, 91, "/xml"}
test_version! {1, 1, 92, "/xml"}
test_version! {1, 1, 93, "/xml"}
test_version! {1, 1, 94, "/xml"}
test_version! {1, 1, 95, "/xml"}
test_version! {1, 1, 96, "/xml"}
test_version! {1, 1, 97, "/xml"}
test_version! {1, 1, 98, "/xml"}
test_version! {1, 1, 99, "/xml"}
test_version! {1, 1, 100, "/xml"}
test_version! {1, 1, 101, "/xml"}
test_version! {1, 1, 102, "/xml"}
test_version! {1, 1, 103, "/xml"}
test_version! {1, 1, 104, "/xml"}
test_version! {1, 1, 105, "/xml"}
test_version! {1, 1, 106, "/xml"}
test_version! {1, 1, 107, "/xml"}
test_version! {1, 1, 108, "/xml"}
test_version! {1, 1, 109, "/xml"}
test_version! {1, 1, 110, "/xml"}
test_version! {1, 1, 111, "/xml"}
test_version! {1, 1, 112, "/xml"}
test_version! {1, 1, 113, "/xml"}
test_version! {1, 1, 114, "/xml"}
test_version! {1, 1, 115, "/xml"}
test_version! {1, 1, 116, "/xml"}
test_version! {1, 1, 117, "/xml"}
test_version! {1, 1, 118, "/xml"}
test_version! {1, 1, 119, "/xml"}
test_version! {1, 1, 120, "/xml"}
test_version! {1, 1, 121, "/xml"}
test_version! {1, 1, 122, "/xml"}
test_version! {1, 1, 123, "/xml"}
test_version! {1, 1, 124, "/xml"}
test_version! {1, 1, 125, "/xml"}
test_version! {1, 1, 126, "/xml"}
test_version! {1, 1, 127, "/xml"}
test_version! {1, 1, 128, "/xml"}
test_version! {1, 1, 129, "/xml"}
test_version! {1, 1, 130, "/xml"}
test_version! {1, 2, 131, "/xml"}
test_version! {1, 2, 132, "/xml"}
test_version! {1, 2, 133, "/xml"}
test_version! {1, 2, 134, "/xml"}
test_version! {1, 2, 135, "/xml"}
test_version! {1, 2, 136, "/xml"}
test_version! {1, 2, 137, "/xml"}
test_version! {1, 2, 138, "/xml"}
test_version! {1, 2, 139, "/xml"}
test_version! {1, 2, 140, "/xml"}
test_version! {1, 2, 141, "/xml"}
test_version! {1, 2, 142, "/xml"}
test_version! {1, 2, 143, "/xml"}
test_version! {1, 2, 144, "/xml"}
test_version! {1, 2, 145, "/xml"}
test_version! {1, 2, 146, "/xml"}
test_version! {1, 2, 147, "/xml"}
test_version! {1, 2, 148, "/xml"}
test_version! {1, 2, 149, "/xml"}
test_version! {1, 2, 150, "/xml"}
test_version! {1, 2, 151, "/xml"}
test_version! {1, 2, 152, "/xml"}
test_version! {1, 2, 153, "/xml"}
test_version! {1, 2, 154, "/xml"}
test_version! {1, 2, 155, "/xml"}
test_version! {1, 2, 156, "/xml"}
test_version! {1, 2, 157, "/xml"}
test_version! {1, 2, 158, "/xml"}
test_version! {1, 2, 159, "/xml"}
test_version! {1, 2, 160, "/xml"}
test_version! {1, 2, 161, "/xml"}
test_version! {1, 2, 162, "/xml"}
test_version! {1, 2, 163, "/xml"}
test_version! {1, 2, 164, "/xml"}
test_version! {1, 2, 165, "/xml"}
test_version! {1, 2, 166, "/xml"}
test_version! {1, 2, 167, "/xml"}
test_version! {1, 2, 168, "/xml"}
test_version! {1, 2, 169, "/xml"}
test_version! {1, 2, 170, "/xml"}
test_version! {1, 2, 171, "/xml"}
test_version! {1, 2, 172, "/xml"}
test_version! {1, 2, 173, "/xml"}
test_version! {1, 2, 174, "/xml"}
test_version! {1, 2, 175, "/xml"}
test_version! {1, 2, 176, "/xml"}
test_version! {1, 2, 177, "/xml"}
test_version! {1, 2, 178, "/xml"}
test_version! {1, 2, 179, "/xml"}
test_version! {1, 2, 180, "/xml"}
test_version! {1, 2, 181, "/xml"}
test_version! {1, 2, 182, "/xml"}
test_version! {1, 2, 183, "/xml"}
test_version! {1, 2, 184, "/xml"}
test_version! {1, 2, 185, "/xml"}
test_version! {1, 2, 186, "/xml"}
test_version! {1, 2, 187, "/xml"}
test_version! {1, 2, 188, "/xml"}
test_version! {1, 2, 189, "/xml"}
test_version! {1, 2, 190, "/xml"}
test_version! {1, 2, 191, "/xml"}
test_version! {1, 2, 192, "/xml"}
test_version! {1, 2, 193, "/xml"}
test_version! {1, 2, 194, "/xml"}
test_version! {1, 2, 195, "/xml"}
test_version! {1, 2, 196, "/xml"}
test_version! {1, 2, 197, "/xml"}
test_version! {1, 2, 198, "/xml"}
test_version! {1, 2, 199, "/xml"}
test_version! {1, 2, 200, "/xml"}
test_version! {1, 2, 201, "/xml"}
test_version! {1, 2, 202, "/xml"}
test_version! {1, 2, 203, "/xml"}
test_version! {1, 3, 204, "/xml"}
test_version! {1, 3, 205, "/xml"}
test_version! {1, 3, 206, "/xml"}
test_version! {1, 3, 207, "/xml"}
test_version! {1, 3, 208, "/xml"}
test_version! {1, 3, 209, "/xml"}
test_version! {1, 3, 210, "/xml"}
test_version! {1, 3, 211, "/xml"}
test_version! {1, 3, 212, "/xml"}
test_version! {1, 3, 213, "/xml"}
test_version! {1, 3, 214, "/xml"}
test_version! {1, 3, 215, "/xml"}
test_version! {1, 3, 216, "/xml"}
test_version! {1, 3, 217, "/xml"}
test_version! {1, 3, 218, "/xml"}
test_version! {1, 3, 219, "/xml"}
test_version! {1, 3, 220, "/xml"}
test_version! {1, 3, 221, "/xml"}
test_version! {1, 3, 222, "/xml"}
test_version! {1, 3, 223, "/xml"}
test_version! {1, 3, 224, "/xml"}
test_version! {1, 3, 225, "/xml"}
test_version! {1, 3, 226, "/xml"}
test_version! {1, 3, 227, "/xml"}
test_version! {1, 3, 228, "/xml"}
