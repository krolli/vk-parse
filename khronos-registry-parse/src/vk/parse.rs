extern crate xml;

use std;
use std::io::Read;
use types::*;
use util::*;
use vk::types::*;
use xml::reader::XmlEvent;

//--------------------------------------------------------------------------------------------------
/// Parses the Vulkan XML file into a Rust object.
pub fn parse_file(path: &std::path::Path) -> Result<(Registry, Vec<Error>), FatalError> {
    let file = std::io::BufReader::new(std::fs::File::open(path)?);
    let parser = xml::reader::ParserConfig::new().create_reader(file);
    parse_xml(parser.into_iter())
}

/// Parses the Vulkan XML file from stream into a Rust object.
pub fn parse_stream<T: std::io::Read>(stream: T) -> Result<(Registry, Vec<Error>), FatalError> {
    let parser = xml::reader::ParserConfig::new().create_reader(stream);
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
        "vendorids" => registry.0.push(parse_vendorids(ctx, attributes)),
        "platforms" => {
            let mut comment = None;
            let mut children = Vec::new();

            match_attributes!{ctx, a in attributes,
                "comment" => comment = Some(a.value)
            }

            match_elements!{ctx, attributes,
                "platform" => if let Some(v) = parse_platform(ctx, attributes) {
                    children.push(v);
                }
            }

            registry.0.push(RegistryChild::Platforms(Platforms { comment, children }));
        },

        "tags" => registry.0.push(parse_tags(ctx, attributes)),
        "types" => {
            let mut comment = None;
            let mut children = Vec::new();
            match_attributes!{ctx, a in attributes,
                "comment" => comment = Some(a.value)
            }
            match_elements!{ctx, attributes,
                "comment" => children.push(TypesChild::Comment(parse_text_element(ctx))),
                "type" => children.push(parse_type(ctx, attributes))
            }
            registry.0.push(RegistryChild::Types(Types{
                comment,
                children
            }));
        },
        "enums" => {
            let mut name = None;
            let mut kind = None;
            let mut start = None;
            let mut end = None;
            let mut vendor = None;
            let mut comment = None;
            let mut bitwidth = None;
            let mut children = Vec::new();
            match_attributes!{ctx, a in attributes,
                "name"     => name     = Some(a.value),
                "type"     => kind     = Some(a.value),
                "start"    => start    = Some(a.value),
                "end"      => end      = Some(a.value),
                "vendor"   => vendor   = Some(a.value),
                "comment"  => comment  = Some(a.value),
                "bitwidth" => bitwidth = Some(a.value)
            }
            match_elements!{ctx, attributes,
                "enum" => if let Some(v) = parse_enum(ctx, attributes) {
                    children.push(EnumsChild::Enum(v));
                },
                "unused" => if let Some(v) = parse_enums_child_unused(ctx, attributes) {
                    children.push(v);
                },
                "comment" => children.push(EnumsChild::Comment(parse_text_element(ctx)))
            }

            let start = start.and_then(|val| parse_integer(ctx, &val));
            let end = end.and_then(|val| parse_integer(ctx, &val));
            let bitwidth = bitwidth.and_then(|val| parse_integer(ctx, &val)).map(|val| val as u32);

            registry.0.push(RegistryChild::Enums(Enums{ name, kind, start, end, vendor, comment, children, bitwidth }));
        },
        "commands" => {
            let mut comment = None;
            let mut children = Vec::new();

            match_attributes!{ctx, a in attributes,
                "comment" => comment = Some(a.value)
            }

            match_elements!{ctx, attributes,
                "command" => if let Some(v) = parse_command(ctx, attributes) {
                    children.push(v);
                }
            }

            registry.0.push(RegistryChild::Commands(Commands{comment, children}));
        },
        "feature" => if let Some(v) = parse_feature(ctx, attributes) {
            registry.0.push(v);
        },
        "extensions" => registry.0.push(parse_extensions(ctx, attributes)),
        "formats" => registry.0.push(parse_formats(ctx)),
        "spirvextensions" => registry.0.push(parse_spirvextensions(ctx, attributes)),
        "spirvcapabilities" => registry.0.push(parse_spirvcapabilities(ctx, attributes))
    }

    Ok(registry)
}

fn parse_vendorids<R: Read>(ctx: &mut ParseCtx<R>, attributes: Vec<XmlAttribute>) -> RegistryChild {
    let mut comment = None;
    let mut children = Vec::new();

    match_attributes! {ctx, a in attributes,
        "comment" => comment = Some(a.value)
    }

    match_elements! {ctx, attributes,
        "vendorid" => if let Some(v) = parse_vendorid(ctx, attributes) {
            children.push(v);
        }
    }

    RegistryChild::VendorIds(VendorIds { comment, children })
}

fn parse_vendorid<R: Read>(
    ctx: &mut ParseCtx<R>,
    attributes: Vec<XmlAttribute>,
) -> Option<VendorId> {
    let mut name = None;
    let mut comment = None;
    let mut id = None;

    match_attributes! {ctx, a in attributes,
        "name" => name = Some(a.value),
        "comment" => comment = Some(a.value),
        "id" => {
            let mut v = None;
            if a.value.starts_with("0x") {
                v = u32::from_str_radix(&a.value.split_at(2).1, 16).ok();
            }

            if let Some(v) = v {
                id = Some(v);
            } else {
                ctx.errors.push(Error::UnexpectedAttributeValue {
                    xpath: ctx.xpath.clone(),
                    name: String::from("id"),
                    value: a.value.clone(),
                });
            }
        }
    }

    consume_current_element(ctx);

    unwrap_attribute!(ctx, vendorid, name);
    unwrap_attribute!(ctx, vendorid, id);

    Some(VendorId { name, comment, id })
}

fn parse_platform<R: Read>(
    ctx: &mut ParseCtx<R>,
    attributes: Vec<XmlAttribute>,
) -> Option<Platform> {
    let mut name = None;
    let mut comment = None;
    let mut protect = None;

    match_attributes! {ctx, a in attributes,
        "name"    => name    = Some(a.value),
        "comment" => comment = Some(a.value),
        "protect" => protect = Some(a.value)
    }

    consume_current_element(ctx);

    unwrap_attribute!(ctx, platform, name);
    unwrap_attribute!(ctx, platform, protect);

    Some(Platform {
        name,
        comment,
        protect,
    })
}

fn parse_tags<R: Read>(ctx: &mut ParseCtx<R>, attributes: Vec<XmlAttribute>) -> RegistryChild {
    let mut comment = None;
    let mut children = Vec::new();

    match_attributes! {ctx, a in attributes,
        "comment" => comment = Some(a.value)
    }

    match_elements! {ctx, attributes,
        "tag" => if let Some(v) = parse_tag(ctx, attributes) {
            children.push(v);
        }
    }

    RegistryChild::Tags(Tags { comment, children })
}

fn parse_tag<R: Read>(ctx: &mut ParseCtx<R>, attributes: Vec<XmlAttribute>) -> Option<Tag> {
    let mut name = None;
    let mut author = None;
    let mut contact = None;

    match_attributes! {ctx, a in attributes,
        "name"    => name    = Some(a.value),
        "author"  => author  = Some(a.value),
        "contact" => contact = Some(a.value)
    }

    consume_current_element(ctx);

    unwrap_attribute!(ctx, tag, name);
    unwrap_attribute!(ctx, tag, author);
    unwrap_attribute!(ctx, tag, contact);

    Some(Tag {
        name,
        author,
        contact,
    })
}

fn parse_type<R: Read>(ctx: &mut ParseCtx<R>, attributes: Vec<XmlAttribute>) -> TypesChild {
    let mut api = None;
    let mut alias = None;
    let mut requires = None;
    let mut name = None;
    let mut category = None;
    let mut parent = None;
    let mut returnedonly = None;
    let mut structextends = None;
    let mut allowduplicate = None;
    let mut objtypeenum = None;
    let mut bitvalues = None;
    let mut comment = None;

    let mut code = String::new();
    let mut markup = Vec::new();
    let mut members = Vec::new();

    match_attributes! {ctx, a in attributes,
        "api"            => api            = Some(a.value),
        "alias"          => alias          = Some(a.value),
        "requires"       => requires       = Some(a.value),
        "name"           => name           = Some(a.value),
        "category"       => category       = Some(a.value),
        "parent"         => parent         = Some(a.value),
        "returnedonly"   => returnedonly   = Some(a.value),
        "structextends"  => structextends  = Some(a.value),
        "allowduplicate" => allowduplicate = Some(a.value),
        "objtypeenum"    => objtypeenum    = Some(a.value),
        "bitvalues"      => bitvalues      = Some(a.value),
        "comment"        => comment        = Some(a.value)
    }

    match_elements_combine_text! {ctx, attributes, code,
        "member" => {
            let mut len = None;
            let mut altlen = None;
            let mut externsync = None;
            let mut optional = None;
            let mut selector = None;
            let mut selection = None;
            let mut noautovalidity = None;
            let mut validextensionstructs = None;
            let mut values = None;
            let mut limittype = None;
            let mut objecttype = None;
            let mut code = String::new();
            let mut markup = Vec::new();
            match_attributes!{ctx, a in attributes,
                "len"                   => len                   = Some(a.value),
                "altlen"                => altlen                = Some(a.value),
                "externsync"            => externsync            = Some(a.value),
                "optional"              => optional              = Some(a.value),
                "selector"              => selector              = Some(a.value),
                "selection"             => selection             = Some(a.value),
                "noautovalidity"        => noautovalidity        = Some(a.value),
                "validextensionstructs" => validextensionstructs = Some(a.value),
                "values"                => values                = Some(a.value),
                "limittype"             => limittype             = Some(a.value),
                "objecttype"            => objecttype            = Some(a.value)
            }
            match_elements_combine_text!{ctx, code,
                "type" => {
                    let text = parse_text_element(ctx);
                    code.push_str(&text);
                    markup.push(TypeMemberMarkup::Type(text));
                },
                "name" => {
                    let text = parse_text_element(ctx);
                    code.push_str(&text);
                    markup.push(TypeMemberMarkup::Name(text));
                },
                "enum" => {
                    let text = parse_text_element(ctx);
                    code.push_str(&text);
                    markup.push(TypeMemberMarkup::Enum(text));
                },
                "comment" => {
                    let text = parse_text_element(ctx);
                    markup.push(TypeMemberMarkup::Comment(text));
                }
            }
            members.push(TypeMember::Definition(TypeMemberDefinition {
                len,
                altlen,
                externsync,
                optional,
                selector,
                selection,
                noautovalidity,
                validextensionstructs,
                values,
                limittype,
                objecttype,
                code,
                markup,
            }))
        },
        "comment" => members.push(TypeMember::Comment(parse_text_element(ctx))),
        "name" => {
            let text = parse_text_element(ctx);
            code.push_str(&text);
            markup.push(TypeCodeMarkup::Name(text));
        },
        "type" => {
            let text = parse_text_element(ctx);
            code.push_str(&text);
            markup.push(TypeCodeMarkup::Type(text));
        },
        "apientry" => {
            let text = parse_text_element(ctx);
            code.push_str(&text);
            markup.push(TypeCodeMarkup::ApiEntry(text));
        }
    }

    TypesChild::Type(Type {
        api,
        alias,
        requires,
        name,
        category,
        parent,
        returnedonly,
        structextends,
        allowduplicate,
        objtypeenum,
        bitvalues,
        comment,
        spec: if members.len() > 0 {
            TypeSpec::Members(members)
        } else if code.len() > 0 {
            TypeSpec::Code(TypeCode { code, markup })
        } else {
            TypeSpec::None
        },
    })
}

fn parse_command<R: Read>(ctx: &mut ParseCtx<R>, attributes: Vec<XmlAttribute>) -> Option<Command> {
    let mut name = None;
    let mut alias = None;
    let mut queues = None;
    let mut successcodes = None;
    let mut errorcodes = None;
    let mut renderpass = None;
    let mut cmdbufferlevel = None;
    let mut pipeline = None;
    let mut comment = None;

    match_attributes! {ctx, a in attributes,
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
        unwrap_attribute!(ctx, command, name);
        consume_current_element(ctx);
        Some(Command::Alias { alias, name })
    } else {
        let mut code = String::new();
        let mut proto = None;
        let mut params = Vec::new();
        let mut description = None;
        let mut implicitexternsyncparams = Vec::new();

        fn parse_name_with_type<R: Read>(
            ctx: &mut ParseCtx<R>,
            buffer: &mut String,
        ) -> Option<NameWithType> {
            let mut name = None;
            let mut type_name = None;
            match_elements_combine_text! {ctx, buffer,
                "type" => {
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

        match_elements! {ctx, attributes,
            "proto" => {
                proto = parse_name_with_type(ctx, &mut code);
                code.push('(');
            },

            "param" => {
                let mut len = None;
                let mut altlen = None;
                let mut externsync = None;
                let mut optional = None;
                let mut noautovalidity = None;
                let mut objecttype = None;

                match_attributes!{ctx, a in attributes,
                    "len"            => len            = Some(a.value),
                    "altlen"         => altlen         = Some(a.value),
                    "externsync"     => externsync     = Some(a.value),
                    "optional"       => optional       = Some(a.value),
                    "noautovalidity" => noautovalidity = Some(a.value),
                    "objecttype"     => objecttype     = Some(a.value)
                }

                if params.len() > 0 {
                    code.push_str(", ");
                }
                if let Some(definition) = parse_name_with_type(ctx, &mut code) {
                    params.push(CommandParam {
                        len,
                        altlen,
                        externsync,
                        optional,
                        noautovalidity,
                        objecttype,
                        definition,
                    });
                }
            },

            "alias" => {
                match_attributes!{ctx, a in attributes,
                    "name" => alias = Some(a.value)
                }
                consume_current_element(ctx);
            },

            "description" => description = Some(parse_text_element(ctx)),
            "implicitexternsyncparams" => {
                match_elements!{ctx,
                    "param" => implicitexternsyncparams.push(parse_text_element(ctx))
                }
            }
        }
        code.push_str(");");

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
            queues,
            successcodes,
            errorcodes,
            renderpass,
            cmdbufferlevel,
            pipeline,
            comment,
            proto,
            params,
            alias,
            description,
            implicitexternsyncparams,
            code,
        }))
    }
}

fn parse_enum<R: Read>(ctx: &mut ParseCtx<R>, attributes: Vec<XmlAttribute>) -> Option<Enum> {
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
    let mut protect = None;
    let mut alias = None;

    match_attributes! {ctx, a in attributes,
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
                ctx.errors.push(Error::UnexpectedAttributeValue {
                    xpath: ctx.xpath.clone(),
                    name: String::from("dir"),
                    value: a.value
                });
            }
        },
        "bitpos" => bitpos = Some(a.value),
        "extnumber" => extnumber = Some(a.value),
        "protect" => protect = Some(a.value),
        "alias" => alias = Some(a.value)
    }

    unwrap_attribute!(ctx, enum, name);

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
        ctx.errors.push(Error::SchemaViolation {
            xpath: ctx.xpath.clone(),
            desc: format!(
                "Unable to determine correct specification of enum: offset={:?}, bitpos={:?}, value={:?}, alias={:?}",
                offset, bitpos, value, alias
            ),
        });
        consume_current_element(ctx);
        return None;
    }

    let spec = if let Some(alias) = alias {
        EnumSpec::Alias { alias, extends }
    } else if let Some(offset) = offset {
        let offset = match parse_integer(ctx, &offset) {
            Some(v) => v,
            None => {
                consume_current_element(ctx);
                return None;
            }
        };
        if let Some(extends) = extends {
            EnumSpec::Offset {
                offset,
                extends,
                extnumber: match extnumber {
                    Some(extnumber) => parse_integer(ctx, &extnumber),
                    None => None,
                },
                dir: positive,
            }
        } else {
            ctx.errors.push(Error::SchemaViolation {
                xpath: ctx.xpath.clone(),
                desc: String::from("Missing extends on enum with offset spec."),
            });
            consume_current_element(ctx);
            return None;
        }
    } else if let Some(bitpos) = bitpos {
        let bitpos = match parse_integer(ctx, &bitpos) {
            Some(v) => v,
            None => {
                consume_current_element(ctx);
                return None;
            }
        };
        EnumSpec::Bitpos { bitpos, extends }
    } else if let Some(value) = value {
        EnumSpec::Value { value, extends }
    } else {
        EnumSpec::None
    };

    consume_current_element(ctx);

    Some(Enum {
        name,
        comment,
        type_suffix,
        api,
        protect,
        spec,
    })
}

fn parse_enums_child_unused<R: Read>(
    ctx: &mut ParseCtx<R>,
    attributes: Vec<XmlAttribute>,
) -> Option<EnumsChild> {
    let mut start = None;
    let mut end = None;
    let mut vendor = None;
    let mut comment = None;
    match_attributes! {ctx, a in attributes,
        "start"   => start   = Some(a.value),
        "end"     => end     = Some(a.value),
        "vendor"  => vendor  = Some(a.value),
        "comment" => comment = Some(a.value)
    }
    consume_current_element(ctx);
    unwrap_attribute!(ctx, unused, start);
    let start = match parse_integer(ctx, &start) {
        Some(v) => v,
        None => return None,
    };
    let end = end.and_then(|val| parse_integer(ctx, &val));
    Some(EnumsChild::Unused(Unused {
        start,
        end,
        vendor,
        comment,
    }))
}

fn parse_feature<R: Read>(
    ctx: &mut ParseCtx<R>,
    attributes: Vec<XmlAttribute>,
) -> Option<RegistryChild> {
    let mut api = None;
    let mut name = None;
    let mut number = None;
    let mut protect = None;
    let mut comment = None;
    let mut children = Vec::new();

    match_attributes! {ctx, a in attributes,
        "api"     => api     = Some(a.value),
        "name"    => name    = Some(a.value),
        "number"  => number  = Some(a.value),
        "protect" => protect = Some(a.value),
        "comment" => comment = Some(a.value)
    }

    match_elements! {ctx, attributes,
        "require" => children.push(parse_extension_item_require(ctx, attributes)),
        "remove"  => children.push(parse_extension_item_remove(ctx, attributes))
    }

    unwrap_attribute!(ctx, feature, api);
    unwrap_attribute!(ctx, feature, name);
    unwrap_attribute!(ctx, feature, number);

    Some(RegistryChild::Feature(Feature {
        api,
        name,
        number,
        protect,
        comment,
        children,
    }))
}

fn parse_extensions<R: Read>(
    ctx: &mut ParseCtx<R>,
    attributes: Vec<XmlAttribute>,
) -> RegistryChild {
    let mut comment = None;
    let mut children = Vec::new();

    match_attributes! {ctx, a in attributes,
        "comment" => comment = Some(a.value)
    }

    match_elements! {ctx, attributes,
        "extension" => if let Some(v) = parse_extension(ctx, attributes) {
            children.push(v);
        }
    }

    RegistryChild::Extensions(Extensions { comment, children })
}

fn parse_extension<R: Read>(
    ctx: &mut ParseCtx<R>,
    attributes: Vec<XmlAttribute>,
) -> Option<Extension> {
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
    let mut deprecatedby = None;
    let mut promotedto = None;
    let mut obsoletedby = None;
    let mut provisional = None;
    let mut specialuse = None;
    let mut sortorder = None;
    let mut children = Vec::new();

    match_attributes! {ctx, a in attributes,
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
        "supported"    => supported     = Some(a.value),
        "deprecatedby" => deprecatedby  = Some(a.value),
        "promotedto"   => promotedto    = Some(a.value),
        "provisional"  => provisional   = Some(a.value),
        "obsoletedby"  => obsoletedby   = Some(a.value),
        "specialuse"   => specialuse    = Some(a.value),
        "sortorder"    => sortorder     = Some(a.value)
    }

    let number = match number {
        Some(text) => parse_integer(ctx, &text),
        None => None,
    };

    let provisional = match provisional {
        Some(value) => {
            if value == "true" {
                true
            } else {
                ctx.errors.push(Error::SchemaViolation {
                    xpath: ctx.xpath.clone(),
                    desc: format!("Unexpected value of 'provisional' attribute: {}", value),
                });
                false
            }
        }
        None => false,
    };

    let sortorder = match sortorder {
        Some(text) => parse_integer(ctx, &text),
        None => None,
    };

    unwrap_attribute!(ctx, extension, name);

    match_elements! {ctx, attributes,
        "require" => children.push(parse_extension_item_require(ctx, attributes)),
        "remove" => children.push(parse_extension_item_remove(ctx, attributes))
    }

    Some(Extension {
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
        deprecatedby,
        promotedto,
        obsoletedby,
        provisional,
        specialuse,
        sortorder,
        children,
    })
}

fn parse_extension_item_require<R: Read>(
    ctx: &mut ParseCtx<R>,
    attributes: Vec<XmlAttribute>,
) -> ExtensionChild {
    let mut api = None;
    let mut profile = None;
    let mut extension = None;
    let mut feature = None;
    let mut comment = None;
    let mut items = Vec::new();

    match_attributes! {ctx, a in attributes,
        "api"       => api       = Some(a.value),
        "profile"   => profile   = Some(a.value),
        "extension" => extension = Some(a.value),
        "feature"   => feature   = Some(a.value),
        "comment"   => comment   = Some(a.value)
    }

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

    ExtensionChild::Require {
        api,
        profile,
        extension,
        feature,
        comment,
        items,
    }
}

fn parse_extension_item_remove<R: Read>(
    ctx: &mut ParseCtx<R>,
    attributes: Vec<XmlAttribute>,
) -> ExtensionChild {
    let mut api = None;
    let mut profile = None;
    let mut comment = None;
    let mut items = Vec::new();

    match_attributes! {ctx, a in attributes,
        "api"     => api     = Some(a.value),
        "profile" => profile = Some(a.value),
        "comment" => comment = Some(a.value)
    }

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

    ExtensionChild::Remove {
        api,
        profile,
        comment,
        items,
    }
}

fn parse_interface_item<R: Read>(
    ctx: &mut ParseCtx<R>,
    name: &str,
    attributes: Vec<XmlAttribute>,
) -> Option<InterfaceItem> {
    match name {
        "comment" => Some(InterfaceItem::Comment(parse_text_element(ctx))),
        "type" => {
            let mut name = None;
            let mut comment = None;
            match_attributes! {ctx, a in attributes,
                "name"    => name    = Some(a.value),
                "comment" => comment = Some(a.value)
            }
            unwrap_attribute!(ctx, type, name);
            consume_current_element(ctx);
            Some(InterfaceItem::Type { name, comment })
        }
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

fn parse_formats<R: Read>(ctx: &mut ParseCtx<R>) -> RegistryChild {
    let mut children = Vec::new();

    match_elements! {ctx, attributes,
        "format" => if let Some(v) = parse_format(ctx, attributes) {
            children.push(v);
        }
    }

    RegistryChild::Formats(Formats {
        comment: None,
        children,
    })
}

#[allow(non_snake_case)]
fn parse_format<R: Read>(ctx: &mut ParseCtx<R>, attributes: Vec<XmlAttribute>) -> Option<Format> {
    let mut name = None;
    let mut class = None;
    let mut blockSize = None;
    let mut texelsPerBlock = None;
    let mut blockExtent = None;
    let mut packed = None;
    let mut compressed = None;
    let mut chroma = None;
    let mut children = Vec::new();

    match_attributes! {ctx, a in attributes,
        "name"           => name           = Some(a.value),
        "class"          => class          = Some(a.value),
        "blockSize"      => blockSize      = Some(a.value),
        "texelsPerBlock" => texelsPerBlock = Some(a.value),
        "blockExtent"    => blockExtent    = Some(a.value),
        "packed"         => packed         = Some(a.value),
        "compressed"     => compressed     = Some(a.value),
        "chroma"         => chroma         = Some(a.value)
    }

    unwrap_attribute!(ctx, extension, name);
    unwrap_attribute!(ctx, extension, class);
    unwrap_attribute!(ctx, extension, blockSize);
    unwrap_attribute!(ctx, extension, texelsPerBlock);

    match_elements! {ctx, attributes,
        "component"        => if let Some(v) = parse_format_component(ctx, attributes) { children.push(v); },
        "plane"            => if let Some(v) = parse_format_plane(ctx, attributes) { children.push(v); },
        "spirvimageformat" => if let Some(v) = parse_format_spirvimageformat(ctx, attributes) { children.push(v); }
    }

    let blockSize: Option<u8> = parse_int_attribute(ctx, blockSize, "blockSize");
    let texelsPerBlock: Option<u8> = parse_int_attribute(ctx, texelsPerBlock, "texelsPerBlock");
    let packed = packed.map(|v| -> Option<u8> { parse_int_attribute(ctx, v, "packed") });

    let blockSize = match blockSize {
        Some(v) => v,
        None => return None,
    };

    let texelsPerBlock = match texelsPerBlock {
        Some(v) => v,
        None => return None,
    };

    let packed = match packed {
        Some(Some(v)) => Some(v),
        Some(None) => return None, // Attribute present, but parse error occurred.
        None => None,
    };

    Some(Format {
        name,
        class,
        blockSize,
        texelsPerBlock,
        blockExtent,
        packed,
        compressed,
        chroma,
        children,
    })
}

#[allow(non_snake_case)]
fn parse_format_component<R: Read>(
    ctx: &mut ParseCtx<R>,
    attributes: Vec<XmlAttribute>,
) -> Option<FormatChild> {
    let mut name = None;
    let mut bits = None;
    let mut numericFormat = None;
    let mut planeIndex = None;

    match_attributes! {ctx, a in attributes,
        "name"          => name          = Some(a.value),
        "bits"          => bits          = Some(a.value),
        "numericFormat" => numericFormat = Some(a.value),
        "planeIndex"    => planeIndex    = Some(a.value)
    }

    unwrap_attribute!(ctx, extension, name);
    unwrap_attribute!(ctx, extension, bits);
    unwrap_attribute!(ctx, extension, numericFormat);

    consume_current_element(ctx);

    let planeIndex =
        planeIndex.map(|v| -> Option<u8> { parse_int_attribute(ctx, v, "planeIndex") });

    let planeIndex = match planeIndex {
        Some(Some(v)) => Some(v),
        Some(None) => return None, // Attribute present, but parse error occurred.
        None => None,
    };

    Some(FormatChild::Component {
        name,
        bits,
        numericFormat,
        planeIndex,
    })
}

#[allow(non_snake_case)]
fn parse_format_plane<R: Read>(
    ctx: &mut ParseCtx<R>,
    attributes: Vec<XmlAttribute>,
) -> Option<FormatChild> {
    let mut index = None;
    let mut widthDivisor = None;
    let mut heightDivisor = None;
    let mut compatible = None;

    match_attributes! {ctx, a in attributes,
        "index"         => index         = Some(a.value),
        "widthDivisor"  => widthDivisor  = Some(a.value),
        "heightDivisor" => heightDivisor = Some(a.value),
        "compatible"    => compatible    = Some(a.value)
    }

    unwrap_attribute!(ctx, extension, index);
    unwrap_attribute!(ctx, extension, widthDivisor);
    unwrap_attribute!(ctx, extension, heightDivisor);
    unwrap_attribute!(ctx, extension, compatible);

    consume_current_element(ctx);

    let index: Option<u8> = parse_int_attribute(ctx, index, "index");
    let widthDivisor: Option<u8> = parse_int_attribute(ctx, widthDivisor, "widthDivisor");
    let heightDivisor: Option<u8> = parse_int_attribute(ctx, heightDivisor, "heightDivisor");

    let index = match index {
        Some(v) => v,
        None => return None,
    };

    let widthDivisor = match widthDivisor {
        Some(v) => v,
        None => return None,
    };

    let heightDivisor = match heightDivisor {
        Some(v) => v,
        None => return None,
    };

    Some(FormatChild::Plane {
        index,
        widthDivisor,
        heightDivisor,
        compatible,
    })
}

fn parse_format_spirvimageformat<R: Read>(
    ctx: &mut ParseCtx<R>,
    attributes: Vec<XmlAttribute>,
) -> Option<FormatChild> {
    let mut name = None;

    match_attributes! {ctx, a in attributes,
        "name" => name = Some(a.value)
    }

    unwrap_attribute!(ctx, extension, name);

    consume_current_element(ctx);

    Some(FormatChild::SpirvImageFormat { name })
}

fn parse_spirvextensions<R: Read>(
    ctx: &mut ParseCtx<R>,
    attributes: Vec<XmlAttribute>,
) -> RegistryChild {
    let mut comment = None;
    let mut children = Vec::new();

    match_attributes! {ctx, a in attributes,
        "comment" => comment = Some(a.value)
    }

    match_elements! {ctx, attributes,
        "spirvextension" => if let Some(v) = parse_spirvextension(ctx, attributes) {
            children.push(v);
        }
    }

    RegistryChild::SpirvExtensions(SpirvExtensions { comment, children })
}

fn parse_spirvextension<R: Read>(
    ctx: &mut ParseCtx<R>,
    attributes: Vec<XmlAttribute>,
) -> Option<SpirvExtension> {
    let mut name = None;
    let mut enables = Vec::new();

    match_attributes! {ctx, a in attributes,
        "name" => name = Some(a.value)
    }

    match_elements! {ctx, attributes,
        "enable" => if let Some(v) = parse_enable(ctx, attributes) {
            enables.push(v);
        }
    }

    unwrap_attribute!(ctx, spirvextension, name);

    Some(SpirvExtension { name, enables })
}

fn parse_spirvcapabilities<R: Read>(
    ctx: &mut ParseCtx<R>,
    attributes: Vec<XmlAttribute>,
) -> RegistryChild {
    let mut comment = None;
    let mut children = Vec::new();

    match_attributes! {ctx, a in attributes,
        "comment" => comment = Some(a.value)
    }

    match_elements! {ctx, attributes,
        "spirvcapability" => if let Some(v) = parse_spirvcapability(ctx, attributes) {
            children.push(v);
        }
    }

    RegistryChild::SpirvCapabilities(SpirvCapabilities { comment, children })
}

fn parse_spirvcapability<R: Read>(
    ctx: &mut ParseCtx<R>,
    attributes: Vec<XmlAttribute>,
) -> Option<SpirvCapability> {
    let mut name = None;
    let mut enables = Vec::new();

    match_attributes! {ctx, a in attributes,
        "name" => name = Some(a.value)
    }

    match_elements! {ctx, attributes,
        "enable" => if let Some(v) = parse_enable(ctx, attributes) {
            enables.push(v);
        }
    }

    unwrap_attribute!(ctx, spirvcapability, name);

    Some(SpirvCapability { name, enables })
}

fn parse_enable<R: Read>(ctx: &mut ParseCtx<R>, attributes: Vec<XmlAttribute>) -> Option<Enable> {
    let mut version = None;
    let mut extension = None;
    let mut struct_ = None;
    let mut feature = None;
    let mut requires = None;
    let mut alias = None;
    let mut property = None;
    let mut member = None;
    let mut value = None;

    match_attributes! {ctx, a in attributes,
        "version" => version = Some(a.value),
        "extension" => extension = Some(a.value),
        "struct" => struct_ = Some(a.value),
        "feature" => feature = Some(a.value),
        "requires" => requires = Some(a.value),
        "alias" => alias = Some(a.value),
        "property" => property = Some(a.value),
        "member" => member = Some(a.value),
        "value" => value = Some(a.value)
    }

    consume_current_element(ctx);

    if let Some(version) = version {
        Some(Enable::Version(version))
    } else if let Some(extension) = extension {
        Some(Enable::Extension(extension))
    } else if let Some(struct_) = struct_ {
        unwrap_attribute!(ctx, enable, feature);
        Some(Enable::Feature(FeatureEnable {
            struct_,
            feature,
            requires,
            alias,
        }))
    } else if let Some(property) = property {
        unwrap_attribute!(ctx, enable, member);
        unwrap_attribute!(ctx, enable, value);
        Some(Enable::Property(PropertyEnable {
            property,
            member,
            value,
            requires,
        }))
    } else {
        unimplemented!();
    }
}
