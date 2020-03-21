#![deny(warnings)]

extern crate minreq;
extern crate ron;
extern crate serde;
extern crate vk_parse;
extern crate vkxml;
extern crate xml;

const URL_REPO: &str = "https://raw.githubusercontent.com/KhronosGroup/Vulkan-Docs";
const URL_MASTER: &str =
    "https://raw.githubusercontent.com/KhronosGroup/Vulkan-Docs/master/xml/vk.xml";

fn download<T: std::io::Write>(dst: &mut T, url: &str) {
    let resp = minreq::get(url)
        .send()
        .expect(&format!("Failed to GET resource: {:?}", url));

    let is_success = 200 <= resp.status_code && resp.status_code < 300;
    if !is_success {
        panic!(
            "Download request failed with status: {:?}",
            resp.status_code
        )
    }

    dst.write_all(resp.body.as_bytes())
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

test_version! {test_v1_0_33, 1, 0, 33, "-core/src/spec"}
test_version! {test_v1_0_34, 1, 0, 34, "-core/src/spec"}
test_version! {test_v1_0_35, 1, 0, 35, "-core/src/spec"}
test_version! {test_v1_0_36, 1, 0, 36, "-core/src/spec"}
// test_version!{test_v1_1_37, 1, 0, 37, "-core/src/spec"} // no tag for v1.0.37
test_version! {test_v1_0_38, 1, 0, 38, "-core/src/spec"}
test_version! {test_v1_0_39, 1, 0, 39, "-core/src/spec"}
test_version! {test_v1_0_40, 1, 0, 40, "-core/src/spec"}
test_version! {test_v1_0_41, 1, 0, 41, "-core/src/spec"}
test_version! {test_v1_0_42, 1, 0, 42, "-core/src/spec"}
test_version! {test_v1_0_43, 1, 0, 43, "-core/src/spec"}
test_version! {test_v1_0_44, 1, 0, 44, "-core/src/spec"}
test_version! {test_v1_0_45, 1, 0, 45, "-core/src/spec"}
test_version! {test_v1_0_46, 1, 0, 46, "-core/src/spec"}
test_version! {test_v1_0_47, 1, 0, 47, "-core/src/spec"}
test_version! {test_v1_0_48, 1, 0, 48, "-core/src/spec"}
test_version! {test_v1_0_49, 1, 0, 49, "-core/src/spec"}
test_version! {test_v1_0_50, 1, 0, 50, "-core/src/spec"}
test_version! {test_v1_0_51, 1, 0, 51, "-core/src/spec"}
// test_version!{test_v1_0_52, 1, 0, 52, "-core/src/spec"} // no tag for v1.0.52
test_version! {test_v1_0_53, 1, 0, 53, "-core/src/spec"}
test_version! {test_v1_0_54, 1, 0, 54, "-core/src/spec"}
test_version! {test_v1_0_55, 1, 0, 55, "-core/src/spec"}
test_version! {test_v1_0_56, 1, 0, 56, "-core/src/spec"}
test_version! {test_v1_0_57, 1, 0, 57, "-core/src/spec"}
test_version! {test_v1_0_58, 1, 0, 58, "-core/src/spec"}
test_version! {test_v1_0_59, 1, 0, 59, "-core/src/spec"}
test_version! {test_v1_0_60, 1, 0, 60, "-core/src/spec"}
test_version! {test_v1_0_61, 1, 0, 61, "-core/src/spec"}
test_version! {test_v1_0_62, 1, 0, 62, "-core/src/spec"}
test_version! {test_v1_0_63, 1, 0, 63, "-core/src/spec"}
test_version! {test_v1_0_64, 1, 0, 64, "-core/src/spec"}
test_version! {test_v1_0_65, 1, 0, 65, "-core/src/spec"}
test_version! {test_v1_0_66, 1, 0, 66, "-core/src/spec"}
test_version! {test_v1_0_67, 1, 0, 67, "-core/src/spec"}
test_version! {test_v1_0_68, 1, 0, 68, "-core/src/spec"}
test_version! {test_v1_0_69, 1, 0, 69, "-core/src/spec"}
test_version! {test_v1_1_70, 1, 1, 70, "/src/spec"}
test_version! {test_v1_1_71, 1, 1, 71, "/src/spec"}
test_version! {test_v1_1_72, 1, 1, 72, "/xml"}
test_version! {test_v1_1_73, 1, 1, 73, "/xml"}
test_version! {test_v1_1_74, 1, 1, 74, "/xml"}
test_version! {test_v1_1_75, 1, 1, 75, "/xml"}
test_version! {test_v1_1_76, 1, 1, 76, "/xml"}
test_version! {test_v1_1_77, 1, 1, 77, "/xml"}
test_version! {test_v1_1_78, 1, 1, 78, "/xml"}
test_version! {test_v1_1_79, 1, 1, 79, "/xml"}
test_version! {test_v1_1_80, 1, 1, 80, "/xml"}
test_version! {test_v1_1_81, 1, 1, 81, "/xml"}
test_version! {test_v1_1_82, 1, 1, 82, "/xml"}
test_version! {test_v1_1_83, 1, 1, 83, "/xml"}
test_version! {test_v1_1_84, 1, 1, 84, "/xml"}
test_version! {test_v1_1_85, 1, 1, 85, "/xml"}
test_version! {test_v1_1_86, 1, 1, 86, "/xml"}
test_version! {test_v1_1_87, 1, 1, 87, "/xml"}
test_version! {test_v1_1_88, 1, 1, 88, "/xml"}
test_version! {test_v1_1_89, 1, 1, 89, "/xml"}
test_version! {test_v1_1_90, 1, 1, 90, "/xml"}
test_version! {test_v1_1_91, 1, 1, 91, "/xml"}
test_version! {test_v1_1_92, 1, 1, 92, "/xml"}
test_version! {test_v1_1_93, 1, 1, 93, "/xml"}
test_version! {test_v1_1_94, 1, 1, 94, "/xml"}
test_version! {test_v1_1_95, 1, 1, 95, "/xml"}
test_version! {test_v1_1_96, 1, 1, 96, "/xml"}
test_version! {test_v1_1_97, 1, 1, 97, "/xml"}
test_version! {test_v1_1_98, 1, 1, 98, "/xml"}
test_version! {test_v1_1_99, 1, 1, 99, "/xml"}
test_version! {test_v1_1_100, 1, 1, 100, "/xml"}
test_version! {test_v1_1_101, 1, 1, 101, "/xml"}
test_version! {test_v1_1_102, 1, 1, 102, "/xml"}
test_version! {test_v1_1_103, 1, 1, 103, "/xml"}
test_version! {test_v1_1_104, 1, 1, 104, "/xml"}
test_version! {test_v1_1_105, 1, 1, 105, "/xml"}
test_version! {test_v1_1_106, 1, 1, 106, "/xml"}
test_version! {test_v1_1_107, 1, 1, 107, "/xml"}
test_version! {test_v1_1_108, 1, 1, 108, "/xml"}
test_version! {test_v1_1_109, 1, 1, 109, "/xml"}
test_version! {test_v1_1_110, 1, 1, 110, "/xml"}
test_version! {test_v1_1_111, 1, 1, 111, "/xml"}
test_version! {test_v1_1_112, 1, 1, 112, "/xml"}
test_version! {test_v1_1_113, 1, 1, 113, "/xml"}
test_version! {test_v1_1_114, 1, 1, 114, "/xml"}
test_version! {test_v1_1_115, 1, 1, 115, "/xml"}
test_version! {test_v1_1_116, 1, 1, 116, "/xml"}
test_version! {test_v1_1_117, 1, 1, 117, "/xml"}
test_version! {test_v1_1_118, 1, 1, 118, "/xml"}
test_version! {test_v1_1_119, 1, 1, 119, "/xml"}
test_version! {test_v1_1_120, 1, 1, 120, "/xml"}
test_version! {test_v1_1_121, 1, 1, 121, "/xml"}
test_version! {test_v1_1_122, 1, 1, 122, "/xml"}
test_version! {test_v1_1_123, 1, 1, 123, "/xml"}
test_version! {test_v1_1_124, 1, 1, 124, "/xml"}
test_version! {test_v1_1_125, 1, 1, 125, "/xml"}
test_version! {test_v1_1_126, 1, 1, 126, "/xml"}
test_version! {test_v1_1_127, 1, 1, 127, "/xml"}
test_version! {test_v1_1_128, 1, 1, 128, "/xml"}
test_version! {test_v1_1_129, 1, 1, 129, "/xml"}
test_version! {test_v1_1_130, 1, 1, 130, "/xml"}
test_version! {test_v1_2_131, 1, 2, 131, "/xml"}
test_version! {test_v1_2_132, 1, 2, 132, "/xml"}
test_version! {test_v1_2_133, 1, 2, 133, "/xml"}
test_version! {test_v1_2_134, 1, 2, 134, "/xml"}
test_version! {test_v1_2_135, 1, 2, 135, "/xml"}
