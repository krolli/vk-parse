extern crate ron;
extern crate serde_xml_rs;
extern crate vk_parse;
extern crate vkxml;
extern crate xml;

type XmlEventReader<R> = xml::reader::EventReader<R>;
use xml::reader::XmlEvent;

fn save_registry_ron(registry: &vkxml::Registry, path: &str) {
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
    {
        let file = std::io::BufReader::new(std::fs::File::open("test/vk.xml").unwrap());
        let parser = XmlEventReader::new(file);

        let mut events = parser.into_iter();
        while let Some(Ok(e)) = events.next() {
            match e {
                XmlEvent::StartElement { ref name, .. } if name.local_name == "registry" => {
                    let registry = vk_parse::parse_registry(&mut events);
                    save_registry_ron(&registry, "test/vk.ron");
                }
                _ => {}
            }
        }
    }

    {
        let file = std::io::BufReader::new(std::fs::File::open("test/vk_new.xml").unwrap());
        let result = serde_xml_rs::from_reader(file);
        let registry: vkxml::Registry = result.unwrap();
        save_registry_ron(&registry, "test/vk_ref.ron");
    }
}
