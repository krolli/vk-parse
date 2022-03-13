extern crate xml;

use gl::types::*;
use std;
use std::io::Read;
use types::*;
use util::*;
use xml::reader::XmlEvent;

pub const BOM: &'static [u8] = &[0xEF, 0xBB, 0xBF];

pub fn parse_file(path: &std::path::Path) -> Result<(Registry, Vec<Error>), FatalError> {
    let file = std::io::BufReader::new(std::fs::File::open(path)?);
    parse_stream(file)
}

pub fn parse_stream<T: std::io::Read>(mut stream: T) -> Result<(Registry, Vec<Error>), FatalError> {
    let mut buffer = vec![0; 0];
    stream.read_to_end(&mut buffer)?;

    let mut offset = 0;
    for (i, _) in buffer.iter().enumerate() {
        if buffer[i..].starts_with(BOM) {
            offset = i + BOM.len();
        }
        if i != 0 {
            break;
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
                "unused" => {
                    consume_current_element(ctx);
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
        "extensions" => registry.0.push(parse_extensions(ctx, attributes)),
        "feature" => {
            let mut children = Vec::new();
            let mut api = None;
            let mut name = None;
            let mut number = None;
            match_attributes!{ctx, a in attributes,
               "api"=> api = Some(a.value),
                "name" => name = Some(a.value),
                "number" => number = Some(a.value)
            }
            match_elements!{ctx, attributes,
                "require" => if let Some(ext) = parse_extension_items(ctx, ExtensionType::Required, attributes) {
                    children.push(ext);
                },
                "remove" => if let Some(ext) = parse_extension_items(ctx, ExtensionType::Removed, attributes) {
                    children.push(ext);
                }
            }
          registry.0.push(RegistryChild::Features(Features{name, api, number, children}));
        }
    }
    Ok(registry)
}

fn parse_extension_items<R: Read>(
    ctx: &mut ParseCtx<R>,
    ext_type: ExtensionType,
    attributes: Vec<XmlAttribute>,
) -> Option<ExtensionChild> {
    let mut required_children = Vec::new();
    let mut required_comment = None;
    let mut profile = None;
    let mut api = None;

    match_attributes! {ctx, a in attributes,
        "comment" => required_comment = Some(a.value),
        "profile" => profile = Some(a.value),
        "api" => api = Some(a.value)
    }

    match_elements! { ctx, attributes,
         "enum"  => if let Some(e) = parse_enum(ctx, attributes) {
            required_children.push(InterfaceItem::Enum(e));
        },
        "type" => {
            let mut name = None;
            let mut comment = None;
            match_attributes! {ctx, a in attributes,
                "name"    => name    = Some(a.value),
                "comment" => comment = Some(a.value)
            }
            unwrap_attribute!(ctx, type, name);
            consume_current_element(ctx);
            required_children.push(InterfaceItem::Type { name, comment });
        },
         "command" => {
            let mut name = None;
            let mut comment = None;
            match_attributes! {ctx, a in attributes,
                "name"    => name    = Some(a.value),
                "comment" => comment = Some(a.value)
            }
            unwrap_attribute!(ctx, type, name);
            consume_current_element(ctx);
            required_children.push(InterfaceItem::Command { name, comment });
        }
    }
    match ext_type {
        ExtensionType::Required => Some(ExtensionChild::Require {
            items: required_children,
            comment: required_comment,
            api,
        }),
        ExtensionType::Removed => Some(ExtensionChild::Removed {
            items: required_children,
            comment: required_comment,
            profile,
        }),
    }
}

fn parse_type<R: Read>(ctx: &mut ParseCtx<R>, attributes: Vec<XmlAttribute>) -> Type {
    let mut name = None;
    let mut type_name = None;
    let mut requires = None;
    let mut comment = None;
    let mut code: String = String::new();
    let mut api_entry = false;
    match_attributes! {ctx, a in attributes,
        "requires" => requires  = Some(a.value),
        "comment"  => comment = Some(a.value),
        "name" => name = Some(a.value)
    }

    match_elements_combine_text! {ctx, code,
        "name" => {
            type_name = Some(parse_text_element(ctx));
        },
        "apientry" => {
            api_entry = true;
            consume_current_element(ctx) //skip
        }
    }

    Type {
        api_entry,
        requires,
        type_name,
        name,
        comment,
        code,
    }
}

fn parse_extensions<R: Read>(
    ctx: &mut ParseCtx<R>,
    _attributes: Vec<XmlAttribute>,
) -> RegistryChild {
    let mut children = Vec::new();
    match_elements! {ctx, attributes,
        "extension" => if let Some(v) = parse_extension(ctx, attributes) {
            children.push(v);
        }
    }
    RegistryChild::Extensions(Extensions { children })
}

fn parse_extension<R: Read>(
    ctx: &mut ParseCtx<R>,
    attributes: Vec<XmlAttribute>,
) -> Option<Extension> {
    let mut name = None;
    let mut supported = None;
    let mut comment = None;
    let mut children: Vec<ExtensionChild> = Vec::new();

    match_attributes! {ctx, a in attributes,
        "name"      => name  = Some(a.value),
        "supported" => supported = Some(a.value),
        "comment" => comment = Some(a.value)
    }

    match_elements! { ctx, attributes,
        "require" => if let Some(ext) = parse_extension_items(ctx, ExtensionType::Required, attributes) {
            children.push(ext);
        }
    }

    Some(Extension {
        name,
        supported,
        comment,
        children,
    })
}

fn parse_command<R: Read>(
    ctx: &mut ParseCtx<R>,
    _attributes: Vec<XmlAttribute>,
) -> Option<Command> {
    let mut code = String::new();
    let mut proto = None;
    let mut vec_equiv = None;
    let mut params = Vec::new();
    let mut aliases = Vec::new();
    let mut glx = None;

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
        "alias" => {
             match_attributes!{ctx, a in attributes,
                "name"   => aliases.push(a.value)
            }
            consume_current_element(ctx);
        },
        "glx" => {
            let mut glx_type = None;
            let mut opcode = None;
            let mut name = None;
            let mut comment = None;
            match_attributes!{ctx, a in attributes,
                "type"   => glx_type = Some(a.value),
                "opcode"   => opcode = Some(a.value),
                "name"   => name = Some(a.value),
                "comment"   => comment = Some(a.value)
            }
            glx = Some(Glx {
                glx_type,
                opcode,
                name,
                comment
            });
            consume_current_element(ctx);
        },
        "vecequiv" => {
            match_attributes!{ctx, a in attributes,
                "name"   => vec_equiv = Some(a.value)
            }
            consume_current_element(ctx);
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
    Some(Command {
        proto,
        params,
        code,
        vec_equiv,
        glx,
        aliases,
    })
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

    Some(NameWithType { name, type_name, buffer: buffer.to_string() })
}

fn parse_enum<R: Read>(ctx: &mut ParseCtx<R>, attributes: Vec<XmlAttribute>) -> Option<Enum> {
    let mut name = None;
    let mut value = None;
    let mut group = None;
    match_attributes! {ctx, a in attributes,
        "name" => name = Some(a.value),
        "value" => value = Some(a.value),
        "group" => group = Some(a.value),
        "alias" => {},
        "type" => {},
        "api" => {},
        "comment" => {}
    }
    consume_current_element(ctx);
    unwrap_attribute!(ctx, enum, name);
    Some(Enum { name, value, group })
}
