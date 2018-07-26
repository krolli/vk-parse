extern crate xml;

use std;
use std::io::Read;
use std::str::FromStr;
use xml::reader::XmlEvent;

use types::*;

type XmlEvents<R> = xml::reader::Events<R>;
type XmlAttribute = xml::attribute::OwnedAttribute;

//--------------------------------------------------------------------------------------------------
macro_rules! unwrap_attribute (
    ( $element:ident, $attribute:ident ) => {
        let $attribute = match $attribute {
            Some(val) => val,
            None => panic!(
                "Missing attribute '{}' on element '{}'.",
                stringify!($attribute),
                stringify!($element),
            ),
        };
    };
);

macro_rules! match_attributes {
    ($a:ident in $attributes:expr, $($p:pat => $e:expr),+) => {
        for $a in $attributes {
            let n = $a.name.local_name.as_str();
            match n {
                $(
                    $p => $e,
                )+
                _ => panic!("Unexpected attribute {:?}", n),
            }
        }
    };
}

macro_rules! match_elements {
    ( $events:expr, $($p:pat => $e:expr),+) => {
        while let Some(Ok(e)) = $events.next() {
            match e {
                XmlEvent::StartElement { name, .. } => {
                    let name = name.local_name.as_str();
                    match name {
                        $(
                            $p => $e,
                        )+
                        _ => panic!("Unexpected element {:?}", name),
                    }
                }
                XmlEvent::EndElement { .. } => break,
                _ => {}
            }
        }
    };

    ( $attributes:ident in $events:expr, $($p:pat => $e:expr),+) => {
        while let Some(Ok(e)) = $events.next() {
            match e {
                XmlEvent::StartElement { name, $attributes, .. } => {
                    let name = name.local_name.as_str();
                    match name {
                        $(
                            $p => $e,
                        )+
                        _ => panic!("Unexpected element {:?}", name),
                    }
                }
                XmlEvent::EndElement { .. } => break,
                _ => {}
            }
        }
    };
}

macro_rules! match_elements_combine_text {
    ( $events:expr, $buffer:ident, $($p:pat => $e:expr),+) => {
        while let Some(Ok(e)) = $events.next() {
            match e {
                XmlEvent::Characters(text) => $buffer.push_str(&text),
                XmlEvent::Whitespace(text) => $buffer.push_str(&text),
                XmlEvent::StartElement { name, .. } => {
                    let name = name.local_name.as_str();
                    match name {
                        $(
                            $p => $e,
                        )+
                        _ => panic!("Unexpected element {:?}", name),
                    }
                }
                XmlEvent::EndElement { .. } => break,
                _ => {}
            }
        }
    };

    ( $attributes:ident in $events:expr, $buffer:ident, $($p:pat => $e:expr),+) => {
        while let Some(Ok(e)) = $events.next() {
            match e {
                XmlEvent::Characters(text) => $buffer.push_str(&text),
                XmlEvent::Whitespace(text) => $buffer.push_str(&text),
                XmlEvent::StartElement { name, $attributes, .. } => {
                    let name = name.local_name.as_str();
                    match name {
                        $(
                            $p => $e,
                        )+
                        _ => panic!("Unexpected element {:?}", name),
                    }
                }
                XmlEvent::EndElement { .. } => break,
                _ => {}
            }
        }
    };
}

//--------------------------------------------------------------------------------------------------
pub fn parse_file(path: &std::path::Path) -> Registry {
    let file = std::io::BufReader::new(std::fs::File::open(path).unwrap());
    let parser = xml::reader::ParserConfig::new().create_reader(file);

    let mut events = parser.into_iter();
    match_elements!{events,
        "registry" => return parse_registry(&mut events)
    }

    panic!("Couldn't find 'registry' element in file {:?}", path);
}

fn parse_registry<R: Read>(events: &mut XmlEvents<R>) -> Registry {
    let mut registry = Registry(Vec::new());

    match_elements!{attributes in events,
        "comment" => registry.0.push(RegistryItem::Comment(parse_text_element(events))),
        "vendorids" => registry.0.push(parse_vendorids(attributes, events)),
        "platforms" => {
            let mut comment = None;
            let mut items = Vec::new();

            match_attributes!{a in attributes,
                "comment" => comment = Some(a.value)
            }

            match_elements!{attributes in events,
                "platform" => items.push(parse_platform(attributes, events))
            }

            registry.0.push(RegistryItem::Platforms { comment, items });
        },

        "tags" => registry.0.push(parse_tags(attributes, events)),
        "types" => {
            let mut comment = None;
            let mut items = Vec::new();
            match_attributes!{a in attributes,
                "comment" => comment = Some(a.value)
            }
            match_elements!{attributes in events,
                "comment" => items.push(TypeItem::Comment(parse_text_element(events))),
                "type" => items.push(parse_type(attributes, events))
            }
            registry.0.push(RegistryItem::Types{
                comment,
                items
            });
        },
        "enums" => {
            let mut name = None;
            let mut kind = None;
            let mut start = None;
            let mut end = None;
            let mut vendor = None;
            let mut comment = None;
            let mut items = Vec::new();
            match_attributes!{a in attributes,
                "name"    => name    = Some(a.value),
                "type"    => kind    = Some(a.value),
                "start"   => start   = Some(a.value),
                "end"     => end     = Some(a.value),
                "vendor"  => vendor  = Some(a.value),
                "comment" => comment = Some(a.value)
            }
            match_elements!{attributes in events,
                "enum" => items.push(EnumsItem::Enum(parse_enum(attributes, events))),
                "unused" => {
                    let mut start = None;
                    let mut end = None;
                    let mut vendor = None;
                    let mut comment = None;
                    match_attributes!{a in attributes,
                        "start"   => start   = Some(a.value),
                        "end"     => end     = Some(a.value),
                        "vendor"  => vendor  = Some(a.value),
                        "comment" => comment = Some(a.value)
                    }
                    consume_current_element(events);
                    unwrap_attribute!(unused, start);
                    let start = parse_integer(&start);
                    let end = end.map(|val| parse_integer(&val));
                    items.push(EnumsItem::Unused{start, end, vendor, comment});
                },
                "comment" => items.push(EnumsItem::Comment(parse_text_element(events)))
            }

            let start = start.map(|val| parse_integer(&val));
            let end = end.map(|val| parse_integer(&val));

            registry.0.push(RegistryItem::Enums{ name, kind, start, end, vendor, comment, items });
        },
        "commands" => {
            let mut comment = None;
            let mut items = Vec::new();

            match_attributes!{a in attributes,
                "comment" => comment = Some(a.value)
            }

            match_elements!{attributes in events,
                "command" => items.push(parse_command(attributes, events))
            }

            registry.0.push(RegistryItem::Commands{comment, items});
        },
        "feature" => {
            registry.0.push(parse_feature(attributes, events));
        },
        "extensions" => registry.0.push(parse_extensions(attributes, events))
    }

    registry
}

pub fn parse_vendorids<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> RegistryItem {
    let mut comment = None;
    let mut items = Vec::new();

    match_attributes!{a in attributes,
        "comment" => comment = Some(a.value)
    }

    match_elements!{attributes in events,
        "vendorid" => items.push(parse_vendorid(attributes, events))
    }

    RegistryItem::VendorIds { comment, items }
}

fn parse_vendorid<R: Read>(attributes: Vec<XmlAttribute>, events: &mut XmlEvents<R>) -> VendorId {
    let mut name = None;
    let mut comment = None;
    let mut id = None;

    match_attributes!{a in attributes,
        "name" => name = Some(a.value),
        "comment" => comment = Some(a.value),
        "id" => {
            if !a.value.starts_with("0x") {
                panic!("Expected hexadecimal integer. Found {:?}", a.value);
            }
            id = Some(u32::from_str_radix(&a.value.split_at(2).1, 16).unwrap());
        }
    }

    consume_current_element(events);

    unwrap_attribute!(vendorid, name);
    unwrap_attribute!(vendorid, id);

    VendorId { name, comment, id }
}

fn parse_platform<R: Read>(attributes: Vec<XmlAttribute>, events: &mut XmlEvents<R>) -> Platform {
    let mut name = None;
    let mut comment = None;
    let mut protect = None;

    match_attributes!{a in attributes,
        "name"    => name    = Some(a.value),
        "comment" => comment = Some(a.value),
        "protect" => protect = Some(a.value)
    }

    consume_current_element(events);

    unwrap_attribute!(platform, name);
    unwrap_attribute!(platform, protect);

    Platform {
        name,
        comment,
        protect,
    }
}

pub fn parse_tags<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> RegistryItem {
    let mut comment = None;
    let mut items = Vec::new();

    match_attributes!{a in attributes,
        "comment" => comment = Some(a.value)
    }

    match_elements!{attributes in events,
        "tag" => items.push(parse_tag(attributes, events))
    }

    RegistryItem::Tags { comment, items }
}

fn parse_tag<R: Read>(attributes: Vec<XmlAttribute>, events: &mut XmlEvents<R>) -> Tag {
    let mut name = None;
    let mut author = None;
    let mut contact = None;

    match_attributes!{a in attributes,
        "name"    => name    = Some(a.value),
        "author"  => author  = Some(a.value),
        "contact" => contact = Some(a.value)
    }

    consume_current_element(events);

    unwrap_attribute!(tag, name);
    unwrap_attribute!(tag, author);
    unwrap_attribute!(tag, contact);

    Tag {
        name,
        author,
        contact,
    }
}

pub fn parse_type<R: Read>(attributes: Vec<XmlAttribute>, events: &mut XmlEvents<R>) -> TypeItem {
    let mut api = None;
    let mut alias = None;
    let mut requires = None;
    let mut name = None;
    let mut category = None;
    let mut parent = None;
    let mut returnedonly = None;
    let mut structextends = None;
    let mut comment = None;

    let mut code = String::new();
    let mut markup = Vec::new();
    let mut members = Vec::new();

    match_attributes!{a in attributes,
        "api"           => api           = Some(a.value),
        "alias"         => alias         = Some(a.value),
        "requires"      => requires      = Some(a.value),
        "name"          => name          = Some(a.value),
        "category"      => category      = Some(a.value),
        "parent"        => parent        = Some(a.value),
        "returnedonly"  => returnedonly  = Some(a.value),
        "structextends" => structextends = Some(a.value),
        "comment"       => comment       = Some(a.value)
    }

    match_elements_combine_text!{attributes in events, code,
        "member" => {
            let mut len = None;
            let mut altlen = None;
            let mut externsync = None;
            let mut optional = None;
            let mut noautovalidity = None;
            let mut validextensionstructs = None;
            let mut values = None;
            let mut code = String::new();
            let mut markup = Vec::new();
            match_attributes!{a in attributes,
                "len"                   => len                   = Some(a.value),
                "altlen"                => altlen                = Some(a.value),
                "externsync"            => externsync            = Some(a.value),
                "optional"              => optional              = Some(a.value),
                "noautovalidity"        => noautovalidity        = Some(a.value),
                "validextensionstructs" => validextensionstructs = Some(a.value),
                "values"                => values                = Some(a.value)
            }
            match_elements_combine_text!{events, code,
                "type" => {
                    let text = parse_text_element(events);
                    code.push_str(&text);
                    markup.push(TypeMemberMarkup::Type(text));
                },
                "name" => {
                    let text = parse_text_element(events);
                    code.push_str(&text);
                    markup.push(TypeMemberMarkup::Name(text));
                },
                "enum" => {
                    let text = parse_text_element(events);
                    code.push_str(&text);
                    markup.push(TypeMemberMarkup::Enum(text));
                },
                "comment" => {
                    let text = parse_text_element(events);
                    markup.push(TypeMemberMarkup::Comment(text));
                }
            }
            members.push(TypeMember::Definition {
                len,
                altlen,
                externsync,
                optional,
                noautovalidity,
                validextensionstructs,
                values,
                code,
                markup,
            })
        },
        "comment" => members.push(TypeMember::Comment(parse_text_element(events))),
        "name" => {
            let text = parse_text_element(events);
            code.push_str(&text);
            markup.push(TypeCodeMarkup::Name(text));
        },
        "type" => {
            let text = parse_text_element(events);
            code.push_str(&text);
            markup.push(TypeCodeMarkup::Type(text));
        },
        "apientry" => {
            let text = parse_text_element(events);
            code.push_str(&text);
            markup.push(TypeCodeMarkup::ApiEntry(text));
        }
    }

    TypeItem::Type {
        api,
        alias,
        requires,
        name,
        category,
        parent,
        returnedonly,
        structextends,
        comment,
        contents: if members.len() > 0 {
            TypeContents::Members(members)
        } else if code.len() > 0 {
            TypeContents::Code { code, markup }
        } else {
            TypeContents::None
        },
    }
}

pub fn parse_command<R: Read>(attributes: Vec<XmlAttribute>, events: &mut XmlEvents<R>) -> Command {
    let mut name = None;
    let mut alias = None;
    let mut queues = None;
    let mut successcodes = None;
    let mut errorcodes = None;
    let mut renderpass = None;
    let mut cmdbufferlevel = None;
    let mut pipeline = None;
    let mut comment = None;

    match_attributes!{a in attributes,
        "name" => name = Some(a.value),
        "alias" => alias = Some(a.value),
        "queues" => queues = Some(a.value),
        "successcodes" => successcodes = Some(a.value),
        "errorcodes" => errorcodes = Some(a.value),
        "renderpass" => renderpass = Some(a.value),
        "cmdbufferlevel" => cmdbufferlevel = Some(a.value),
        "pipeline" => pipeline = Some(a.value),
        "comment" => comment = Some(a.value)
    }

    if let Some(alias) = alias {
        unwrap_attribute!(command, name);
        consume_current_element(events);
        Command::Alias { alias, name }
    } else {
        let mut code = String::new();
        let mut proto = None;
        let mut params = Vec::new();
        let mut description = None;
        let mut implicitexternsyncparams = Vec::new();

        fn parse_name_with_type<R: Read>(
            buffer: &mut String,
            events: &mut XmlEvents<R>,
        ) -> NameWithType {
            let mut name = None;
            let mut type_name = None;
            match_elements_combine_text!{events, buffer,
                "type" => {
                    let text = parse_text_element(events);
                    buffer.push_str(&text);
                    type_name = Some(text);
                },
                "name" => {
                    let text = parse_text_element(events);
                    buffer.push_str(&text);
                    name = Some(text);
                }
            }
            NameWithType {
                name: match name {
                    Some(name) => name,
                    None => panic!("Missing name element."),
                },
                type_name,
            }
        }

        match_elements!{attributes in events,
            "proto" => {
                proto = Some(parse_name_with_type(&mut code, events));
                code.push('(');
            },

            "param" => {
                let mut len = None;
                let mut altlen = None;
                let mut externsync = None;
                let mut optional = None;
                let mut noautovalidity = None;

                match_attributes!{a in attributes,
                    "len"            => len            = Some(a.value),
                    "altlen"         => altlen         = Some(a.value),
                    "externsync"     => externsync     = Some(a.value),
                    "optional"       => optional       = Some(a.value),
                    "noautovalidity" => noautovalidity = Some(a.value)
                }

                if params.len() > 0 {
                    code.push_str(", ");
                }
                let definition = parse_name_with_type(&mut code, events);
                params.push(CommandParam {
                    len,
                    altlen,
                    externsync,
                    optional,
                    noautovalidity,
                    definition,
                });
            },

            "alias" => {
                match_attributes!{a in attributes,
                    "name" => alias = Some(a.value)
                }
                consume_current_element(events);
            },

            "description" => description = Some(parse_text_element(events)),
            "implicitexternsyncparams" => {
                match_elements!{events,
                    "param" => implicitexternsyncparams.push(parse_text_element(events))
                }
            }
        }
        code.push_str(");");

        Command::Definition {
            queues,
            successcodes,
            errorcodes,
            renderpass,
            cmdbufferlevel,
            pipeline,
            comment,
            proto: match proto {
                Some(proto) => proto,
                None => panic!("Missing proto element in command definition."),
            },
            params,
            alias,
            description,
            implicitexternsyncparams,
            code,
        }
    }
}

fn parse_enum<R: Read>(attributes: Vec<XmlAttribute>, events: &mut XmlEvents<R>) -> Enum {
    let mut name = None;
    let mut comment = None;
    let mut type_suffix = None;
    let mut api = None;
    let mut extends = None;
    let mut value = None;
    let mut bitpos = None;
    let mut extnumber = None;
    let mut offset = None;
    let mut positive = true;
    let mut alias = None;

    match_attributes!{a in attributes,
        "name" => name = Some(a.value),
        "comment" => comment = Some(a.value),
        "type" => type_suffix = Some(a.value),
        "api" => api = Some(a.value),
        "extends" => extends = Some(a.value),
        "value" => value = Some(a.value),
        "offset" => offset = Some(a.value),
        "dir" => {
            if a.value.as_str() == "-" {
                positive = false;
            } else {
                panic!(
                    "Unexpected value of attribute {:?}, expected \"-\", found {:?}",
                    name, a.value
                );
            }
        },
        "bitpos" => bitpos = Some(a.value),
        "extnumber" => extnumber = Some(a.value),
        "alias" => alias = Some(a.value)
    }

    consume_current_element(events);

    unwrap_attribute!(enum, name);

    let mut count = 0;
    if offset.is_some() {
        count += 1;
    }
    if bitpos.is_some() {
        count += 1;
    }
    if value.is_some() {
        count += 1;
    }
    if alias.is_some() {
        count += 1;
    }
    if count > 1 {
        panic!(
            "Unable to determine correct specification of enum: {:?}, {:?}, {:?}, {:?}",
            offset, bitpos, value, alias
        );
    }

    let spec = if let Some(alias) = alias {
        EnumSpec::Alias { alias, extends }
    } else if let Some(offset) = offset {
        if let Some(extends) = extends {
            EnumSpec::Offset {
                offset: parse_integer(&offset),
                extends,
                extnumber: match extnumber {
                    Some(extnumber) => Some(parse_integer(&extnumber)),
                    None => None,
                },
                dir: positive,
            }
        } else {
            panic!("Missing extends on enum with offset spec.");
        }
    } else if let Some(bitpos) = bitpos {
        EnumSpec::Bitpos {
            bitpos: parse_integer(&bitpos),
            extends,
        }
    } else if let Some(value) = value {
        EnumSpec::Value { value, extends }
    } else {
        EnumSpec::None
    };

    Enum {
        name,
        comment,
        type_suffix,
        api,
        spec,
    }
}

pub fn parse_feature<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> RegistryItem {
    let mut api = None;
    let mut name = None;
    let mut number = None;
    let mut protect = None;
    let mut comment = None;
    let mut items = Vec::new();

    match_attributes!{a in attributes,
        "api"     => api     = Some(a.value),
        "name"    => name    = Some(a.value),
        "number"  => number  = Some(a.value),
        "protect" => protect = Some(a.value),
        "comment" => comment = Some(a.value)
    }

    match_elements!{attributes in events,
        "require" => items.push(parse_extension_item_require(attributes, events)),
        "remove"  => items.push(parse_extension_item_remove(attributes, events))
    }

    unwrap_attribute!(feature, api);
    unwrap_attribute!(feature, name);
    unwrap_attribute!(feature, number);

    let number = f32::from_str(&number).unwrap();

    RegistryItem::Feature {
        api,
        name,
        number,
        protect,
        comment,
        items,
    }
}

pub fn parse_extensions<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> RegistryItem {
    let mut comment = None;
    let mut items = Vec::new();

    match_attributes!{a in attributes,
        "comment" => comment = Some(a.value)
    }

    match_elements!{attributes in events,
        "extension" => items.push(parse_extension(attributes, events))
    }

    RegistryItem::Extensions { comment, items }
}

fn parse_extension<R: Read>(attributes: Vec<XmlAttribute>, events: &mut XmlEvents<R>) -> Extension {
    let mut name = None;
    let mut comment = None;
    let mut number = None;
    let mut protect = None;
    let mut platform = None;
    let mut author = None;
    let mut contact = None;
    let mut ext_type = None;
    let mut requires = None;
    let mut requires_core = None;
    let mut supported = None;
    let mut items = Vec::new();

    match_attributes!{a in attributes,
        "name"         => name          = Some(a.value),
        "comment"      => comment       = Some(a.value),
        "number"       => number        = Some(a.value),
        "protect"      => protect       = Some(a.value),
        "platform"     => platform      = Some(a.value),
        "author"       => author        = Some(a.value),
        "contact"      => contact       = Some(a.value),
        "type"         => ext_type      = Some(a.value),
        "requires"     => requires      = Some(a.value),
        "requiresCore" => requires_core = Some(a.value),
        "supported"    => supported     = Some(a.value)
    }

    match_elements!{attributes in events,
        "require" => items.push(parse_extension_item_require(attributes, events)),
        "remove" => items.push(parse_extension_item_remove(attributes, events))
    }

    let number = match number {
        Some(text) => Some(parse_integer(&text)),
        None => None,
    };

    unwrap_attribute!(extension, name);
    Extension {
        name,
        comment,
        number,
        protect,
        platform,
        author,
        contact,
        ext_type,
        requires,
        requires_core,
        supported,
        items,
    }
}

fn parse_extension_item_require<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> ExtensionItem {
    let mut api = None;
    let mut profile = None;
    let mut extension = None;
    let mut feature = None;
    let mut comment = None;
    let mut items = Vec::new();

    match_attributes!{a in attributes,
        "api"       => api       = Some(a.value),
        "profile"   => profile   = Some(a.value),
        "extension" => extension = Some(a.value),
        "feature"   => feature   = Some(a.value),
        "comment"   => comment   = Some(a.value)
    }

    while let Some(Ok(e)) = events.next() {
        match e {
            XmlEvent::StartElement {
                name, attributes, ..
            } => items.push(parse_interface_item(
                name.local_name.as_str(),
                attributes,
                events,
            )),
            XmlEvent::EndElement { .. } => break,
            _ => {}
        }
    }

    ExtensionItem::Require {
        api,
        profile,
        extension,
        feature,
        comment,
        items,
    }
}

fn parse_extension_item_remove<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> ExtensionItem {
    let mut api = None;
    let mut profile = None;
    let mut comment = None;
    let mut items = Vec::new();

    match_attributes!{a in attributes,
        "api"     => api     = Some(a.value),
        "profile" => profile = Some(a.value),
        "comment" => comment = Some(a.value)
    }

    while let Some(Ok(e)) = events.next() {
        match e {
            XmlEvent::StartElement {
                name, attributes, ..
            } => items.push(parse_interface_item(
                name.local_name.as_str(),
                attributes,
                events,
            )),
            XmlEvent::EndElement { .. } => break,
            _ => {}
        }
    }

    ExtensionItem::Remove {
        api,
        profile,
        comment,
        items,
    }
}

fn parse_interface_item<R: Read>(
    name: &str,
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> InterfaceItem {
    match name {
        "comment" => InterfaceItem::Comment(parse_text_element(events)),
        "type" => {
            let mut name = None;
            let mut comment = None;
            match_attributes!{a in attributes,
                "name"    => name    = Some(a.value),
                "comment" => comment = Some(a.value)
            }
            unwrap_attribute!(type, name);
            consume_current_element(events);
            InterfaceItem::Type { name, comment }
        }
        "enum" => InterfaceItem::Enum(parse_enum(attributes, events)),
        "command" => {
            let mut name = None;
            let mut comment = None;
            match_attributes!{a in attributes,
                "name"    => name    = Some(a.value),
                "comment" => comment = Some(a.value)
            }
            unwrap_attribute!(type, name);
            consume_current_element(events);
            InterfaceItem::Command { name, comment }
        }
        _ => panic!("Unexpected element {:?}", name),
    }
}

pub fn parse_integer(text: &str) -> i64 {
    if text.starts_with("0x") {
        i64::from_str_radix(text.split_at(2).1, 16).unwrap()
    } else {
        if let Ok(val) = i64::from_str_radix(text, 10) {
            val
        } else {
            panic!("Couldn't parse integer from {:?}", text);
        }
    }
}

pub fn consume_current_element<R: Read>(events: &mut XmlEvents<R>) {
    let mut depth = 1;
    while let Some(Ok(e)) = events.next() {
        match e {
            XmlEvent::StartElement { .. } => depth += 1,
            XmlEvent::EndElement { .. } => {
                depth -= 1;
                if depth == 0 {
                    break;
                }
            }
            _ => (),
        }
    }
}

pub fn parse_text_element<R: Read>(events: &mut XmlEvents<R>) -> String {
    let mut result = String::new();
    let mut depth = 1;
    while let Some(Ok(e)) = events.next() {
        match e {
            XmlEvent::StartElement { .. } => depth += 1,
            XmlEvent::Characters(text) => result.push_str(&text),
            XmlEvent::EndElement { .. } => {
                depth -= 1;
                if depth == 0 {
                    break;
                }
            }
            _ => (),
        }
    }
    result
}
