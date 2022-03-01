extern crate xml;

use std;
use std::io;
use std::io::{BufReader, Read};
use std::str::FromStr;
use types::*;
use util::*;
use gl::types::*;
use xml::reader::XmlEvent;
use std::str;

pub const BOM: &'static [u8] = &[0xEF, 0xBB, 0xBF];


pub fn parse_file(path: &std::path::Path) -> Result<(Registry, Vec<Error>), FatalError> {
    let file = std::io::BufReader::new(std::fs::File::open(path)?);
    parse_stream(file)
}

pub fn parse_stream<T: std::io::Read>(mut stream: T) -> Result<(Registry, Vec<Error>), FatalError> {
    let mut buffer = vec![0; 0];
    stream.read_to_end(&mut buffer);

    let mut offset = 0;
    for (i, _) in buffer.iter().enumerate() {
        if buffer[i..].starts_with(BOM) {
            offset = i + BOM.len();
        }
        if i != 0 {
            break
        }
    }

    let parser = xml::reader::ParserConfig::new().create_reader(&buffer[offset..]);
    parse_xml(parser.into_iter())
}

fn parse_xml<R: Read>(events: XmlEvents<R>) -> Result<(Registry, Vec<Error>), FatalError> {
    let mut ctx = ParseCtx {
        events,
        xpath: String::from(""),
        errors: Vec::new(),
    };

    let mut result = Err(FatalError::MissingRegistryElement);

    {
        let ctx = &mut ctx;
        match_elements! {ctx,
            "registry" => result = parse_registry(ctx)
        }
    }

    result.map(|r| (r, ctx.errors))
}

fn parse_registry<R: Read>(ctx: &mut ParseCtx<R>) -> Result<Registry, FatalError> {
    let mut registry = Registry(Vec::new());

    match_elements! {ctx, attributes,
        "comment" => registry.0.push(RegistryChild::Comment(parse_text_element(ctx))),
        "enums" => {
            let mut namespace = None;
            let mut group = None;
            let mut enum_type = None;
            let mut start = None;
            let mut end = None;
            let mut vendor = None;
            let mut comment = None;
            let mut children = Vec::new();

            match_attributes!{ctx, a in attributes,
                "namespace" => namespace = Some(a.value),
                "group"     => group = Some(a.value),
                "type"     =>  enum_type = Some(a.value),
                "start"     => start     = Some(a.value),
                "end"       => end       = Some(a.value),
                "vendor"    => vendor    = Some(a.value),
                "comment"   => comment   = Some(a.value)
            }

             match_elements!{ctx, attributes,
                "enum" => if let Some(v) = parse_enum(ctx, attributes) {
                    children.push(EnumsChild::Enum(v));
                },
                "comment" => children.push(EnumsChild::Comment(parse_text_element(ctx)))
            }
            registry.0.push(RegistryChild::Enums(
                Enums{namespace, group, enum_type,start,end,vendor,comment, children }));

        },
        "types" => {
            let mut children = Vec::new();
            match_elements!{ctx, attributes,
                "type" => children.push(parse_type(ctx, attributes))
            }
            registry.0.push(RegistryChild::Types(Types { children }));
        },
        "commands" => {
            let mut namespace = None;
            let mut children = Vec::new();
            match_attributes!{ctx, a in attributes,
                "namespace" => namespace = Some(a.value)
            }
            match_elements!{ctx, attributes,
                "command" => if let Some(v) = parse_command(ctx, attributes) {
                    children.push(v);
                }
            }
           registry.0.push(RegistryChild::Commands(Commands{namespace, children}));
        },
        "extensions" => registry.0.push(parse_extensions(ctx, attributes))
    }

    Ok(registry)
}


fn parse_type<R: Read>(
    ctx: &mut ParseCtx<R>,
    attributes: Vec<XmlAttribute>,
) -> Type {
    let mut name = None;
    let mut requires = None;
    let mut comment = None;
    let mut code: String = String::new();

    match_attributes! {ctx, a in attributes,
        "requires" => requires  = Some(a.value),
        "comment"  => comment = Some(a.value)
    }

    match_elements_combine_text! {ctx, code,
        "name" => name = Some(parse_text_element(ctx))
    }

    Type {
        requires,
        name,
        comment,
        code
    }
}

fn parse_extensions<R: Read>(
    ctx: &mut ParseCtx<R>,
    attributes: Vec<XmlAttribute>,
) -> RegistryChild {
    let mut children = Vec::new();
    match_elements! {ctx, attributes,
        "extension" => if let Some(v) = parse_extension(ctx, attributes) {
            children.push(v);
        }
    }
    RegistryChild::Extensions(Extensions { children })
}

fn parse_extension_item_require<R: Read>(
    ctx: &mut ParseCtx<R>,
    attributes: Vec<XmlAttribute>,
) -> ExtensionChild {
    let mut items = Vec::new();

    while let Some(Ok(e)) = ctx.events.next() {
        match e {
            XmlEvent::StartElement {
                name, attributes, ..
            } => {
                let name = name.local_name.as_str();
                ctx.push_element(name);
                if let Some(v) = parse_interface_item(ctx, name, attributes) {
                    items.push(v);
                }
            }
            XmlEvent::EndElement { .. } => {
                ctx.pop_element();
                break;
            }
            _ => {}
        }
    }
    ExtensionChild::Require { items }
}

fn parse_extension<R: Read>(
    ctx: &mut ParseCtx<R>,
    attributes: Vec<XmlAttribute>,
) -> Option<Extension> {
    let mut name = None;
    let mut supported = None;
    let mut children = Vec::new();

    match_attributes! {ctx, a in attributes,
        "name"      => name  = Some(a.value),
        "supported" => supported = Some(a.value)
    }

    match_elements! { ctx, attributes,
        "require" => children.push(parse_extension_item_require(ctx, attributes))
    }

    Some(Extension {
        name,
        supported,
        children,
    })
}

fn parse_interface_item<R: Read>(
    ctx: &mut ParseCtx<R>,
    name: &str,
    attributes: Vec<XmlAttribute>,
) -> Option<InterfaceItem> {
    match name {
        // "comment" => Some(InterfaceItem::Comment(parse_text_element(ctx))),
        "enum" => parse_enum(ctx, attributes).map(|v| InterfaceItem::Enum(v)),
        "command" => {
            let mut name = None;
            let mut comment = None;
            match_attributes! {ctx, a in attributes,
                "name"    => name    = Some(a.value),
                "comment" => comment = Some(a.value)
            }
            unwrap_attribute!(ctx, type, name);
            consume_current_element(ctx);
            Some(InterfaceItem::Command { name, comment })
        }
        _ => {
            ctx.errors.push(Error::UnexpectedElement {
                xpath: ctx.xpath.clone(),
                name: String::from(name),
            });
            return None;
        }
    }
}

fn parse_command<R: Read>(ctx: &mut ParseCtx<R>, attributes: Vec<XmlAttribute>) -> Option<Command> {
    let mut code = String::new();
    let mut proto = None;
    let mut vec_equiv = None;
    let mut params = Vec::new();

    match_elements! {ctx, attributes,
        "proto" => {
            proto = parse_name_with_type(ctx, &mut code);
            code.push('(');
        },
        "param" => {
             let mut group = None;
             let mut class = None;
             let mut len = None;

             match_attributes!{ctx, a in attributes,
                "group"   => group  = Some(a.value),
                "class"   => class  = Some(a.value),
                "len"     => len    = Some(a.value)
            }
            if params.len() > 0 {
                code.push_str(", ");
            }

            if let Some(definition) = parse_name_with_type(ctx, &mut code) {
                params.push(CommandParam {
                    group,
                    class,
                    len,
                    definition
                });
            }
        },
        "vecequiv" => {
            match_attributes!{ctx, a in attributes,
                "name"   => vec_equiv = Some(a.value)
            }
        }
    }

    let proto = if let Some(v) = proto {
        v
    } else {
        ctx.errors.push(Error::MissingElement {
            xpath: ctx.xpath.clone(),
            name: String::from("proto"),
        });
        return None;
    };
    Some(Command::Definition(CommandDefinition {
        proto,
        params,
        code,
        vec_equiv,
    }))
}

fn parse_name_with_type<R: Read>(
    ctx: &mut ParseCtx<R>,
    buffer: &mut String,
) -> Option<NameWithType> {
    let mut name = None;
    let mut type_name = None;

    match_elements_combine_text! {ctx, buffer,
        "ptype" => {
            let text = parse_text_element(ctx);
            buffer.push_str(&text);
            type_name = Some(text);
        },
        "name" => {
            let text = parse_text_element(ctx);
            buffer.push_str(&text);
            name = Some(text);
        }
    }
    let name = if let Some(v) = name {
        v
    } else {
        ctx.errors.push(Error::MissingElement {
            xpath: ctx.xpath.clone(),
            name: String::from("name"),
        });
        return None;
    };

    Some(NameWithType { name, type_name })
}

fn parse_enum<R: Read>(ctx: &mut ParseCtx<R>, attributes: Vec<XmlAttribute>) -> Option<Enum> {
    let mut name = None;
    let mut value = None;
    let mut group = None;
    match_attributes! {ctx, a in attributes,
        "name" => name = Some(a.value),
        "value" => value = Some(a.value),
        "group" => group = Some(a.value)
    }
    unwrap_attribute!(ctx, enum, name);
    Some(Enum { name, value, group })
}
