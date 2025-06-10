#![deny(warnings)]

extern crate minreq;
extern crate ron;
extern crate serde;
extern crate vk_parse;
extern crate vkxml;
extern crate xml;

const URL_REPO: &str = "https://raw.githubusercontent.com/KhronosGroup/Vulkan-Docs";
const URL_MAIN: &str = "https://raw.githubusercontent.com/KhronosGroup/Vulkan-Docs/main/xml/vk.xml";

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

    dst.write_all(resp.as_bytes())
        .expect("Failed to write response body.");
}

#[allow(dead_code)]
fn write_code(path: &str, reg: &vk_parse::Registry) {
    use std::io::Write;

    let mut file = std::io::BufWriter::new(std::fs::File::create(path).unwrap());

    for child in reg.0.iter() {
        match child {
            vk_parse::RegistryChild::Types(types) => {
                for type_child in types.children.iter() {
                    match type_child {
                        vk_parse::TypesChild::Type(t) => match &t.spec {
                            vk_parse::TypeSpec::Code(c) => {
                                writeln!(&mut file, "").unwrap();
                                write!(&mut file, "// --- xpath: /registry/types/type",).unwrap();
                                match (&t.name, &t.alias) {
                                    (&Some(ref name), &Some(ref alias)) => {
                                        write!(&mut file, "[@name='{}', @alias='{}']", name, alias)
                                            .unwrap()
                                    }
                                    (&None, &Some(ref alias)) => {
                                        write!(&mut file, "[@alias='{}']", alias).unwrap()
                                    }
                                    (&Some(ref name), &None) => {
                                        write!(&mut file, "[@name='{}']", name).unwrap()
                                    }
                                    (&None, &None) => (),
                                }
                                writeln!(&mut file, " ---").unwrap();
                                writeln!(&mut file, "{}", c.code).unwrap();
                            }

                            vk_parse::TypeSpec::Members(members) => {
                                writeln!(&mut file, "").unwrap();
                                write!(&mut file, "// --- xpath: /registry/types/type",).unwrap();
                                match (&t.name, &t.alias) {
                                    (&Some(ref name), &Some(ref alias)) => {
                                        write!(&mut file, "[@name='{}', @alias='{}']", name, alias)
                                            .unwrap()
                                    }
                                    (&None, &Some(ref alias)) => {
                                        write!(&mut file, "[@alias='{}']", alias).unwrap()
                                    }
                                    (&Some(ref name), &None) => {
                                        write!(&mut file, "[@name='{}']", name).unwrap()
                                    }
                                    (&None, &None) => (),
                                }
                                writeln!(&mut file, " ---").unwrap();
                                for member in members.iter() {
                                    match member {
                                        vk_parse::TypeMember::Definition(def) => {
                                            writeln!(&mut file, "/**/{}", def.code).unwrap();
                                        }
                                        _ => (),
                                    }
                                }
                            }

                            _ => (),
                        },

                        _ => (),
                    }
                }
            }

            vk_parse::RegistryChild::Commands(commands) => {
                for command in commands.children.iter() {
                    match command {
                        vk_parse::Command::Definition(cmd_def) => {
                            writeln!(&mut file, "").unwrap();
                            writeln!(
                                &mut file,
                                "// --- /registry/commands/command/proto/name['{}'] ---",
                                cmd_def.proto.name
                            )
                            .unwrap();
                            writeln!(&mut file, "{}", cmd_def.code).unwrap();
                            for param in cmd_def.params.iter() {
                                writeln!(&mut file, "\t// {}", param.definition.code).unwrap();
                            }
                        }
                        _ => (),
                    }
                }
            }

            _ => (),
        }
    }
}

#[allow(dead_code)]
fn write_debug(path: &str, reg: &vk_parse::Registry) {
    use std::io::Write;

    let mut file = std::io::BufWriter::new(std::fs::File::create(path).unwrap());
    writeln!(&mut file, "{:#?}", reg).unwrap();
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
            // write_code(&format!("v{}.{}.{}.c", major, minor, patch), &_reg);
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
    ($test_name:ident, $major:expr, $minor:expr, $patch:expr, $url_suffix:expr) => {
        #[test]
        fn $test_name() {
            parsing_test($major, $minor, $patch, $url_suffix);
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
            // write_code("main.c", &_reg);
            // write_debug("main.debug", &_reg);
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
/*
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
test_version! {test_v1_2_136, 1, 2, 136, "/xml"}
test_version! {test_v1_2_137, 1, 2, 137, "/xml"}
test_version! {test_v1_2_138, 1, 2, 138, "/xml"}
test_version! {test_v1_2_139, 1, 2, 139, "/xml"}
test_version! {test_v1_2_140, 1, 2, 140, "/xml"}
test_version! {test_v1_2_141, 1, 2, 141, "/xml"}
test_version! {test_v1_2_142, 1, 2, 142, "/xml"}
test_version! {test_v1_2_143, 1, 2, 143, "/xml"}
test_version! {test_v1_2_144, 1, 2, 144, "/xml"}
test_version! {test_v1_2_145, 1, 2, 145, "/xml"}
test_version! {test_v1_2_146, 1, 2, 146, "/xml"}
test_version! {test_v1_2_147, 1, 2, 147, "/xml"}
test_version! {test_v1_2_148, 1, 2, 148, "/xml"}
test_version! {test_v1_2_149, 1, 2, 149, "/xml"}
test_version! {test_v1_2_150, 1, 2, 150, "/xml"}
test_version! {test_v1_2_151, 1, 2, 151, "/xml"}
test_version! {test_v1_2_152, 1, 2, 152, "/xml"}
test_version! {test_v1_2_153, 1, 2, 153, "/xml"}
test_version! {test_v1_2_154, 1, 2, 154, "/xml"}
test_version! {test_v1_2_155, 1, 2, 155, "/xml"}
test_version! {test_v1_2_156, 1, 2, 156, "/xml"}
test_version! {test_v1_2_157, 1, 2, 157, "/xml"}
test_version! {test_v1_2_158, 1, 2, 158, "/xml"}
test_version! {test_v1_2_159, 1, 2, 159, "/xml"}
test_version! {test_v1_2_160, 1, 2, 160, "/xml"}
test_version! {test_v1_2_161, 1, 2, 161, "/xml"}
test_version! {test_v1_2_162, 1, 2, 162, "/xml"}
test_version! {test_v1_2_163, 1, 2, 163, "/xml"}
test_version! {test_v1_2_164, 1, 2, 164, "/xml"}
test_version! {test_v1_2_165, 1, 2, 165, "/xml"}
test_version! {test_v1_2_166, 1, 2, 166, "/xml"}
test_version! {test_v1_2_167, 1, 2, 167, "/xml"}
test_version! {test_v1_2_168, 1, 2, 168, "/xml"}
test_version! {test_v1_2_169, 1, 2, 169, "/xml"}
test_version! {test_v1_2_170, 1, 2, 170, "/xml"}
test_version! {test_v1_2_171, 1, 2, 171, "/xml"}
test_version! {test_v1_2_172, 1, 2, 172, "/xml"}
test_version! {test_v1_2_173, 1, 2, 173, "/xml"}
test_version! {test_v1_2_174, 1, 2, 174, "/xml"}
test_version! {test_v1_2_175, 1, 2, 175, "/xml"}
test_version! {test_v1_2_176, 1, 2, 176, "/xml"}
test_version! {test_v1_2_177, 1, 2, 177, "/xml"}
test_version! {test_v1_2_178, 1, 2, 178, "/xml"}
test_version! {test_v1_2_179, 1, 2, 179, "/xml"}
test_version! {test_v1_2_180, 1, 2, 180, "/xml"}
test_version! {test_v1_2_181, 1, 2, 181, "/xml"}
test_version! {test_v1_2_182, 1, 2, 182, "/xml"}
test_version! {test_v1_2_183, 1, 2, 183, "/xml"}
test_version! {test_v1_2_184, 1, 2, 184, "/xml"}
test_version! {test_v1_2_185, 1, 2, 185, "/xml"}
test_version! {test_v1_2_186, 1, 2, 186, "/xml"}
test_version! {test_v1_2_187, 1, 2, 187, "/xml"}
test_version! {test_v1_2_188, 1, 2, 188, "/xml"}
test_version! {test_v1_2_189, 1, 2, 189, "/xml"}
test_version! {test_v1_2_190, 1, 2, 190, "/xml"}
test_version! {test_v1_2_191, 1, 2, 191, "/xml"}
test_version! {test_v1_2_192, 1, 2, 192, "/xml"}
test_version! {test_v1_2_193, 1, 2, 193, "/xml"}
test_version! {test_v1_2_194, 1, 2, 194, "/xml"}
test_version! {test_v1_2_195, 1, 2, 195, "/xml"}
test_version! {test_v1_2_196, 1, 2, 196, "/xml"}
test_version! {test_v1_2_197, 1, 2, 197, "/xml"}
test_version! {test_v1_2_198, 1, 2, 198, "/xml"}
test_version! {test_v1_2_199, 1, 2, 199, "/xml"}
test_version! {test_v1_2_200, 1, 2, 200, "/xml"}
test_version! {test_v1_2_201, 1, 2, 201, "/xml"}
test_version! {test_v1_2_202, 1, 2, 202, "/xml"}
test_version! {test_v1_2_203, 1, 2, 203, "/xml"}
test_version! {test_v1_3_204, 1, 3, 204, "/xml"}
test_version! {test_v1_3_205, 1, 3, 205, "/xml"}
test_version! {test_v1_3_206, 1, 3, 206, "/xml"}
test_version! {test_v1_3_207, 1, 3, 207, "/xml"}
test_version! {test_v1_3_208, 1, 3, 208, "/xml"}
test_version! {test_v1_3_209, 1, 3, 209, "/xml"}
test_version! {test_v1_3_210, 1, 3, 210, "/xml"}
test_version! {test_v1_3_211, 1, 3, 211, "/xml"}
test_version! {test_v1_3_212, 1, 3, 212, "/xml"}
test_version! {test_v1_3_213, 1, 3, 213, "/xml"}
test_version! {test_v1_3_214, 1, 3, 214, "/xml"}
test_version! {test_v1_3_215, 1, 3, 215, "/xml"}
test_version! {test_v1_3_216, 1, 3, 216, "/xml"}
test_version! {test_v1_3_217, 1, 3, 217, "/xml"}
test_version! {test_v1_3_218, 1, 3, 218, "/xml"}
test_version! {test_v1_3_219, 1, 3, 219, "/xml"}
test_version! {test_v1_3_220, 1, 3, 220, "/xml"}
test_version! {test_v1_3_221, 1, 3, 221, "/xml"}
test_version! {test_v1_3_222, 1, 3, 222, "/xml"}
test_version! {test_v1_3_223, 1, 3, 223, "/xml"}
test_version! {test_v1_3_224, 1, 3, 224, "/xml"}
test_version! {test_v1_3_225, 1, 3, 225, "/xml"}
test_version! {test_v1_3_226, 1, 3, 226, "/xml"}
test_version! {test_v1_3_227, 1, 3, 227, "/xml"}
test_version! {test_v1_3_228, 1, 3, 228, "/xml"}
test_version! {test_v1_3_229, 1, 3, 229, "/xml"}
test_version! {test_v1_3_230, 1, 3, 230, "/xml"}
test_version! {test_v1_3_231, 1, 3, 231, "/xml"}
test_version! {test_v1_3_232, 1, 3, 232, "/xml"}
test_version! {test_v1_3_233, 1, 3, 233, "/xml"}
test_version! {test_v1_3_234, 1, 3, 234, "/xml"}
test_version! {test_v1_3_235, 1, 3, 235, "/xml"}
test_version! {test_v1_3_236, 1, 3, 236, "/xml"}
test_version! {test_v1_3_237, 1, 3, 237, "/xml"}
test_version! {test_v1_3_238, 1, 3, 238, "/xml"}
test_version! {test_v1_3_239, 1, 3, 239, "/xml"}
test_version! {test_v1_3_240, 1, 3, 240, "/xml"}
test_version! {test_v1_3_241, 1, 3, 241, "/xml"}
test_version! {test_v1_3_242, 1, 3, 242, "/xml"}
test_version! {test_v1_3_243, 1, 3, 243, "/xml"}
test_version! {test_v1_3_244, 1, 3, 244, "/xml"}
test_version! {test_v1_3_245, 1, 3, 245, "/xml"}
test_version! {test_v1_3_246, 1, 3, 246, "/xml"}
test_version! {test_v1_3_247, 1, 3, 247, "/xml"}
test_version! {test_v1_3_248, 1, 3, 248, "/xml"}
test_version! {test_v1_3_249, 1, 3, 249, "/xml"}
test_version! {test_v1_3_250, 1, 3, 250, "/xml"}
test_version! {test_v1_3_251, 1, 3, 251, "/xml"}
test_version! {test_v1_3_252, 1, 3, 252, "/xml"}
test_version! {test_v1_3_253, 1, 3, 253, "/xml"}
test_version! {test_v1_3_254, 1, 3, 254, "/xml"}
test_version! {test_v1_3_255, 1, 3, 255, "/xml"}
test_version! {test_v1_3_256, 1, 3, 256, "/xml"}
test_version! {test_v1_3_257, 1, 3, 257, "/xml"}
test_version! {test_v1_3_258, 1, 3, 258, "/xml"}
test_version! {test_v1_3_259, 1, 3, 259, "/xml"}
test_version! {test_v1_3_260, 1, 3, 260, "/xml"}
test_version! {test_v1_3_261, 1, 3, 261, "/xml"}
test_version! {test_v1_3_262, 1, 3, 262, "/xml"}
test_version! {test_v1_3_263, 1, 3, 263, "/xml"}
test_version! {test_v1_3_264, 1, 3, 264, "/xml"}
test_version! {test_v1_3_265, 1, 3, 265, "/xml"}
test_version! {test_v1_3_266, 1, 3, 266, "/xml"}
test_version! {test_v1_3_267, 1, 3, 267, "/xml"}
test_version! {test_v1_3_268, 1, 3, 268, "/xml"}
test_version! {test_v1_3_269, 1, 3, 269, "/xml"}
test_version! {test_v1_3_270, 1, 3, 270, "/xml"}
test_version! {test_v1_3_271, 1, 3, 271, "/xml"}
test_version! {test_v1_3_272, 1, 3, 272, "/xml"}
test_version! {test_v1_3_273, 1, 3, 273, "/xml"}
test_version! {test_v1_3_274, 1, 3, 274, "/xml"}
test_version! {test_v1_3_275, 1, 3, 275, "/xml"}
test_version! {test_v1_3_276, 1, 3, 276, "/xml"}
test_version! {test_v1_3_277, 1, 3, 277, "/xml"}
test_version! {test_v1_3_278, 1, 3, 278, "/xml"}
test_version! {test_v1_3_279, 1, 3, 279, "/xml"}
test_version! {test_v1_3_280, 1, 3, 280, "/xml"}
test_version! {test_v1_3_281, 1, 3, 281, "/xml"}
test_version! {test_v1_3_282, 1, 3, 282, "/xml"}
test_version! {test_v1_3_283, 1, 3, 283, "/xml"}
test_version! {test_v1_3_284, 1, 3, 284, "/xml"}
test_version! {test_v1_3_285, 1, 3, 285, "/xml"}
test_version! {test_v1_3_286, 1, 3, 286, "/xml"}
test_version! {test_v1_3_287, 1, 3, 287, "/xml"}
test_version! {test_v1_3_288, 1, 3, 288, "/xml"}
test_version! {test_v1_3_289, 1, 3, 289, "/xml"}
test_version! {test_v1_3_290, 1, 3, 290, "/xml"}
test_version! {test_v1_3_291, 1, 3, 291, "/xml"}
test_version! {test_v1_3_292, 1, 3, 292, "/xml"}
test_version! {test_v1_3_293, 1, 3, 293, "/xml"}
test_version! {test_v1_3_294, 1, 3, 294, "/xml"}
test_version! {test_v1_3_295, 1, 3, 295, "/xml"}
test_version! {test_v1_3_296, 1, 3, 296, "/xml"}
test_version! {test_v1_3_297, 1, 3, 297, "/xml"}
test_version! {test_v1_3_298, 1, 3, 298, "/xml"}
test_version! {test_v1_3_299, 1, 3, 299, "/xml"}
*/
test_version! {test_v1_3_300, 1, 3, 300, "/xml"}
test_version! {test_v1_3_301, 1, 3, 301, "/xml"}
test_version! {test_v1_3_302, 1, 3, 302, "/xml"}
test_version! {test_v1_4_303, 1, 4, 303, "/xml"}
test_version! {test_v1_4_304, 1, 4, 304, "/xml"}
test_version! {test_v1_4_305, 1, 4, 305, "/xml"}
test_version! {test_v1_4_306, 1, 4, 306, "/xml"}
test_version! {test_v1_4_307, 1, 4, 307, "/xml"}
test_version! {test_v1_4_308, 1, 4, 308, "/xml"}
test_version! {test_v1_4_309, 1, 4, 309, "/xml"}
test_version! {test_v1_4_310, 1, 4, 310, "/xml"}
test_version! {test_v1_4_311, 1, 4, 311, "/xml"}
test_version! {test_v1_4_312, 1, 4, 312, "/xml"}
test_version! {test_v1_4_313, 1, 4, 313, "/xml"}
test_version! {test_v1_4_314, 1, 4, 314, "/xml"}
test_version! {test_v1_4_315, 1, 4, 315, "/xml"}
test_version! {test_v1_4_316, 1, 4, 316, "/xml"}
test_version! {test_v1_4_317, 1, 4, 317, "/xml"}
