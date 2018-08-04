#![allow(dead_code)]
extern crate vkxml;
extern crate xml;

type XmlEvents<R> = xml::reader::Events<R>;
type XmlAttribute = xml::attribute::OwnedAttribute;

use c;
use parse::*;
use std;
use std::io::Read;
use types::*;
use xml::reader::XmlEvent;

//--------------------------------------------------------------------------------------------------
fn new_field() -> vkxml::Field {
    vkxml::Field {
        array: None,
        auto_validity: true,
        basetype: vkxml::Identifier::new(),
        c_size: None,
        errorcodes: None,
        is_const: false,
        is_struct: false,
        name: None,
        notation: None,
        null_terminate: false,
        optional: None,
        reference: None,
        size: None,
        size_enumref: None,
        successcodes: None,
        sync: None,
        type_enums: None,
    }
}

/// Parses an file which must be the Vulkan registry XML in its standard format.
///
/// Returns a Rust representation of the registry.
pub fn parse_file_as_vkxml(path: &std::path::Path) -> vkxml::Registry {
    let file = std::io::BufReader::new(std::fs::File::open(path).unwrap());
    let parser = xml::reader::ParserConfig::new().create_reader(file);

    let mut events = parser.into_iter();
    match_elements!{events,
        "registry" => return parse_registry_as_vkxml(&mut events)
    }

    panic!("Couldn't find 'registry' element in file {:?}", path);
}

fn parse_registry_as_vkxml<R: Read>(events: &mut XmlEvents<R>) -> vkxml::Registry {
    fn flush_enums(
        enums: &mut Option<vkxml::Enums>,
        registry_elements: &mut Vec<vkxml::RegistryElement>,
    ) {
        if let Some(value) = enums.take() {
            registry_elements.push(vkxml::RegistryElement::Enums(value));
        }
    }

    let mut registry = vkxml::Registry {
        elements: Vec::new(),
    };

    let mut enums: Option<vkxml::Enums> = None;

    match_elements!{attributes in events,
        "comment" => {
            let notation = parse_text_element(events);
            if let Some(ref mut enums) = enums {
                enums.elements.push(vkxml::EnumsElement::Notation(notation));
            } else {
                registry.elements.push(vkxml::RegistryElement::Notation(notation));
            }
        },

        "vendorids" => {
            flush_enums(&mut enums, &mut registry.elements);
            registry.elements.push(parse_vendorids(attributes, events).into());
        },

        "tags" => {
            flush_enums(&mut enums, &mut registry.elements);
            registry.elements.push(parse_tags(attributes, events).into());
        },

        "types" => {
            flush_enums(&mut enums, &mut registry.elements);
            registry.elements.push(vkxml::RegistryElement::Definitions(parse_types_vkxml(
                attributes, events,
            )));
        },

        "enums" => {
            let mut is_constant = true;
            for a in attributes.iter() {
                if a.name.local_name.as_str() == "type" {
                    is_constant = false;
                    break;
                }
            }

            if is_constant {
                flush_enums(&mut enums, &mut registry.elements);
                registry.elements.push(vkxml::RegistryElement::Constants(parse_constants(
                    attributes, events,
                )));
            } else {
                let enumeration = parse_enumeration(attributes, events);
                if let Some(ref mut enums) = enums {
                    enums
                        .elements
                        .push(vkxml::EnumsElement::Enumeration(enumeration));
                } else {
                    enums = Some(vkxml::Enums {
                        notation: None,
                        elements: vec![vkxml::EnumsElement::Enumeration(enumeration)],
                    });
                }
            }
        },

        "commands" => {
            flush_enums(&mut enums, &mut registry.elements);
            let mut r = vkxml::Commands {
                notation: None,
                elements: Vec::new(),
            };

            match_attributes!{a in attributes,
                "comment" => r.notation = Some(a.value)
            }

            match_elements!{attributes in events,
                "command" => {
                    if let Some(cmd) = parse_command(attributes, events).into() {
                        r.elements.push(cmd);
                    }
                }
            }

            registry.elements.push(vkxml::RegistryElement::Commands(r));
        },

        "feature" => {
            flush_enums(&mut enums, &mut registry.elements);
            registry.elements.push(vkxml::RegistryElement::Features(vkxml::Features {
                elements: vec![parse_feature_vkxml(attributes, events)],
            }));
        },

        "extensions" => {
            flush_enums(&mut enums, &mut registry.elements);
            registry.elements.push(vkxml::RegistryElement::Extensions(parse_extensions_vkxml(
                attributes, events,
            )));
        },

        "platforms" => consume_current_element(events) // mk:TODO Not supported by vkxml.
    }

    registry
}

fn parse_types_vkxml<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> vkxml::Definitions {
    let mut notation = None;
    let mut elements = Vec::new();

    match_attributes!{a in attributes,
        "comment" => notation = Some(a.value)
    }

    match_elements!{attributes in events,
        "comment" => elements.push(vkxml::DefinitionsElement::Notation(parse_text_element(events))),
        "type" => {
            use parse::parse_type;
            let t = parse_type(attributes, events);
            if let Some(t) = t.into() {
                elements.push(t);
            }
        }
    }

    vkxml::Definitions { notation, elements }
}

impl From<TypeItem> for Option<vkxml::DefinitionsElement> {
    fn from(orig: TypeItem) -> Self {
        match orig {
            TypeItem::Comment(text) => Some(vkxml::DefinitionsElement::Notation(text)),
            TypeItem::Type {
                comment,
                category,
                name,
                requires,
                contents,
                alias,
                parent,
                returnedonly,
                structextends,
                ..
            } => {
                let category = match category {
                    Some(c) => c,
                    None => {
                        let name = name.unwrap_or(String::new());
                        return Some(vkxml::DefinitionsElement::Reference(vkxml::Reference {
                            name,
                            notation: comment,
                            include: requires,
                        }));
                    }
                };

                match category.as_str() {
                    "include" => {
                        let mut include = vkxml::Include {
                            name: name.unwrap_or(String::new()),
                            notation: comment,
                            style: vkxml::IncludeStyle::Quote,
                            need_ext: false,
                        };

                        match contents {
                            TypeContents::Code { code, markup } => {
                                let mut iter = code.split_whitespace();
                                let token = iter.next().unwrap();
                                if token != "#include" {
                                    panic!("Unexpected token {:?}", token);
                                }
                                let token = iter.next().unwrap();
                                if token.starts_with('<') {
                                    include.style = vkxml::IncludeStyle::Bracket;
                                }
                                for tag in markup {
                                    match tag {
                                        TypeCodeMarkup::Name(name) => include.name = name,
                                        _ => (),
                                    }
                                }
                            }
                            _ => (),
                        }

                        include.need_ext = !include.name.ends_with(".h");
                        return Some(vkxml::DefinitionsElement::Include(include));
                    }

                    "define" => {
                        let mut define = vkxml::Define {
                            name: name.unwrap_or(String::new()),
                            notation: comment,
                            is_disabled: true,
                            comment: None,
                            replace: false,
                            defref: Vec::new(),
                            parameters: Vec::new(),
                            c_expression: None,
                            value: None,
                        };
                        match contents {
                            TypeContents::Code { code, markup } => {
                                for tag in markup {
                                    match tag {
                                        TypeCodeMarkup::Type(val) => define.defref.push(val),
                                        TypeCodeMarkup::Name(val) => define.name = val,
                                        _ => panic!("Unexpected tag in define {:?}", tag),
                                    }
                                }
                                process_define_code(&mut define, code);
                            }
                            _ => panic!("Unexpected contents of define {:?}", contents),
                        }
                        return Some(vkxml::DefinitionsElement::Define(define));
                    }

                    "basetype" => {
                        let mut typedef = vkxml::Typedef {
                            name: String::new(),
                            notation: comment,
                            basetype: String::new(),
                        };
                        let markup = match contents {
                            TypeContents::Code { markup, .. } => markup,
                            _ => panic!("Unexpected contents of typedef {:?}", contents),
                        };
                        for tag in markup {
                            match tag {
                                TypeCodeMarkup::Type(val) => typedef.basetype = val,
                                TypeCodeMarkup::Name(val) => typedef.name = val,
                                _ => panic!("Unexpected tag in typedef {:?}", tag),
                            }
                        }
                        return Some(vkxml::DefinitionsElement::Typedef(typedef));
                    }

                    "bitmask" => {
                        if name.is_some() || alias.is_some() {
                            return None;
                        }
                        let mut bitmask = vkxml::Bitmask {
                            name: vkxml::Identifier::new(),
                            notation: comment,
                            basetype: vkxml::Identifier::new(),
                            enumref: requires,
                        };
                        let markup = match contents {
                            TypeContents::Code { markup, .. } => markup,
                            _ => panic!("Unexpected contents of bitmaks {:?}", contents),
                        };
                        for tag in markup {
                            match tag {
                                TypeCodeMarkup::Type(val) => bitmask.basetype = val,
                                TypeCodeMarkup::Name(val) => bitmask.name = val,
                                _ => panic!("Unexpected tag in typedef {:?}", tag),
                            }
                        }
                        return Some(vkxml::DefinitionsElement::Bitmask(bitmask));
                    }

                    "handle" => {
                        if name.is_some() || alias.is_some() {
                            return None;
                        }
                        let mut handle = vkxml::Handle {
                            name: String::new(),
                            notation: comment,
                            parent,
                            ty: vkxml::HandleType::Dispatch,
                        };
                        let markup = match contents {
                            TypeContents::Code { markup, .. } => markup,
                            _ => panic!("Unexpected contents of handle {:?}", contents),
                        };
                        for tag in markup {
                            match tag {
                                TypeCodeMarkup::Name(val) => handle.name = val,
                                TypeCodeMarkup::Type(val) => {
                                    handle.ty = match val.as_str() {
                                        "VK_DEFINE_HANDLE" => vkxml::HandleType::Dispatch,
                                        "VK_DEFINE_NON_DISPATCHABLE_HANDLE" => {
                                            vkxml::HandleType::NoDispatch
                                        }
                                        _ => panic!("Unexpected handle type: {}", val),
                                    }
                                }
                                _ => panic!("Unexpected tag in typedef {:?}", tag),
                            }
                        }
                        return Some(vkxml::DefinitionsElement::Handle(handle));
                    }

                    "enum" => {
                        // if alias.is_some() {
                        //     return None;
                        // }
                        return Some(vkxml::DefinitionsElement::Enumeration(
                            vkxml::EnumerationDeclaration {
                                name: name.unwrap_or(String::new()),
                                notation: comment,
                            },
                        ));
                    }

                    "funcpointer" => {
                        let mut fnptr = vkxml::FunctionPointer {
                            name: vkxml::Identifier::new(),
                            notation: comment,
                            return_type: new_field(),
                            param: Vec::new(),
                        };
                        let code = match contents {
                            TypeContents::Code { code, .. } => code,
                            _ => panic!("Unexpected contents of handle {:?}", contents),
                        };

                        parse_type_funcptr(&mut fnptr, &code);
                        return Some(vkxml::DefinitionsElement::FuncPtr(fnptr));
                    }

                    "struct" => {
                        if alias.is_some() {
                            return None;
                        }
                        let mut s = vkxml::Struct {
                            name: name.unwrap_or(String::new()),
                            notation: comment,
                            is_return: returnedonly.unwrap_or(String::new()).as_str() == "true",
                            extends: structextends,
                            elements: Vec::new(),
                        };
                        match contents {
                            TypeContents::Members(members) => for member in members {
                                s.elements.push(member.into());
                            },
                            _ => {
                                panic!("Unexpected contents of struct {:?}: {:?}", s.name, contents)
                            }
                        }

                        return Some(vkxml::DefinitionsElement::Struct(s));
                    }

                    "union" => {
                        let mut u = vkxml::Union {
                            name: name.unwrap_or(String::new()),
                            notation: comment,
                            elements: Vec::new(),
                        };
                        match contents {
                            TypeContents::Members(members) => for member in members {
                                match member {
                                    TypeMember::Comment(..) => (),
                                    TypeMember::Definition { code, .. } => {
                                        let mut iter = code
                                            .split_whitespace()
                                            .flat_map(|s| c::TokenIter::new(s))
                                            .peekable();

                                        let mut field = parse_c_field(&mut iter).unwrap();
                                        u.elements.push(field);
                                    }
                                }
                            },
                            _ => {
                                panic!("Unexpected contents of union {:?}: {:?}", u.name, contents)
                            }
                        }

                        return Some(vkxml::DefinitionsElement::Union(u));
                    }

                    _ => panic!("Unexpected category of type {:?}", category),
                }
            }
        }
    }
}

impl From<TypeMember> for vkxml::StructElement {
    fn from(orig: TypeMember) -> Self {
        match orig {
            TypeMember::Comment(comment) => vkxml::StructElement::Notation(comment),
            TypeMember::Definition {
                len,
                altlen,
                externsync,
                optional,
                values,
                code,
                markup,
                ..
            } => {
                let mut iter = code
                    .split_whitespace()
                    .flat_map(|s| c::TokenIter::new(s))
                    .peekable();

                let mut field = parse_c_field(&mut iter).unwrap();
                field.c_size = altlen;
                field.sync = externsync;
                field.optional = optional;
                field.type_enums = values;
                match len {
                    Some(mut value) => {
                        let null_terminated_part = ",null-terminated";
                        if value.as_str().ends_with(null_terminated_part) {
                            field.null_terminate = true;
                            let start = value.len() - null_terminated_part.len();
                            value.drain(start..);
                        }

                        if value.as_str() == "null-terminated" {
                            field.null_terminate = true;
                        } else {
                            field.size = Some(value);
                        }
                        field.array = Some(vkxml::ArrayType::Dynamic);
                    }
                    None => (),
                }
                for tag in markup {
                    match tag {
                        TypeMemberMarkup::Enum(value) => field.size_enumref = Some(value),
                        TypeMemberMarkup::Comment(comment) => field.notation = Some(comment),
                        _ => (),
                    }
                }

                vkxml::StructElement::Member(field)
            }
        }
    }
}

fn process_define_code(r: &mut vkxml::Define, code: String) {
    fn consume_whitespace(chars: &mut std::str::Chars, mut current: Option<char>) -> Option<char> {
        while let Some(c) = current {
            if !c.is_whitespace() {
                break;
            }
            current = chars.next();
        }
        current
    }

    {
        enum State {
            Initial,
            LineComment,
            BlockComment,
            DefineName,
            DefineArgs,
            DefineExpression,
            DefineValue,
        }
        let mut state = State::Initial;
        let mut chars = code.chars();
        loop {
            match state {
                State::Initial => {
                    let mut current = chars.next();
                    current = consume_whitespace(&mut chars, current);

                    match current {
                        Some('/') => {
                            current = chars.next();
                            match current {
                                Some('/') => state = State::LineComment,
                                Some('*') => state = State::BlockComment,
                                Some(c) => panic!("Unexpected symbol {:?}", c),
                                None => panic!("Unexpected end of code."),
                            }
                        }

                        Some('#') => {
                            let text = chars.as_str();
                            let mut directive_len = 0;
                            while let Some(c) = chars.next() {
                                if c.is_whitespace() {
                                    break;
                                }
                                if 'a' <= c && c <= 'z' {
                                    directive_len += 1;
                                } else {
                                    panic!("Unexpected symbol in preprocessor directive: {:?}", c);
                                }
                            }

                            let directive = &text[..directive_len];
                            match directive {
                                "define" => state = State::DefineName,
                                _ => {
                                    // Different directive. Whole text treated as c expression and replace set to true.
                                    r.replace = true;
                                    r.is_disabled = false;
                                    break;
                                }
                            }
                        }

                        Some('s') => {
                            let expected = "truct ";

                            let text = chars.as_str();
                            if text.starts_with(expected) {
                                // mk:TODO Less hacky handling of define which is actually forward declaration.
                                r.replace = true;
                                break;
                            } else {
                                println!("Unexpected code segment {:?}", code);
                            }
                        }

                        Some(c) => panic!("Unexpected symbol {:?}", c),
                        None => panic!("Unexpected end of code."),
                    }
                }

                State::LineComment => {
                    let text = chars.as_str();
                    if let Some(idx) = text.find('\n') {
                        let comment = text[..idx].trim();
                        if r.comment.is_none() {
                            r.comment = Some(String::from(comment));
                        }
                        chars = text[idx + 1..].chars();
                        state = State::Initial;
                    } else {
                        if r.comment.is_none() {
                            r.comment = Some(String::from(text.trim()));
                        }

                        break;
                    }
                }

                State::BlockComment => {
                    let text = chars.as_str();
                    if let Some(idx) = text.find("*/") {
                        let comment = &text[..idx];
                        if r.comment.is_none() {
                            r.comment = Some(String::from(comment));
                        }
                        chars = text[idx + 2..].chars();
                        state = State::Initial;
                    } else {
                        panic!("Unterminated block comment {:?}", text);
                    }
                }

                State::DefineName => {
                    r.is_disabled = false;
                    let text = chars.as_str();
                    let mut current = chars.next();
                    let mut whitespace_len = 0;
                    while let Some(c) = current {
                        if !c.is_whitespace() {
                            break;
                        }
                        current = chars.next();
                        whitespace_len += 1;
                    }

                    let mut name_len = 0;
                    while let Some(c) = current {
                        if !c::is_c_identifier_char(c) {
                            break;
                        }
                        name_len += 1;
                        current = chars.next();
                    }

                    let name = &text[whitespace_len..whitespace_len + name_len];
                    if name != r.name.as_str() {
                        panic!("#define name mismatch. {:?} vs. {:?}", name, r.name);
                    }

                    match current {
                        Some('(') => state = State::DefineArgs,
                        Some(c) => if c.is_whitespace() {
                            state = State::DefineValue;
                        } else {
                            panic!("Unexpected char after #define name: {:?}", c);
                        },
                        None => break,
                    }
                }

                State::DefineArgs => {
                    let mut text = chars.as_str();
                    let mut current = chars.next();
                    loop {
                        let mut whitespace_len = 0;
                        while let Some(c) = current {
                            if !c.is_whitespace() {
                                break;
                            }
                            whitespace_len += 1;
                            current = chars.next();
                        }

                        let mut name_len = 0;
                        while let Some(c) = current {
                            if !c::is_c_identifier_char(c) {
                                break;
                            }
                            current = chars.next();
                            name_len += 1;
                        }
                        let name = &text[whitespace_len..whitespace_len + name_len];
                        r.parameters.push(String::from(name));

                        current = consume_whitespace(&mut chars, current);
                        match current {
                            Some(',') => {
                                text = chars.as_str();
                                current = chars.next();
                            }
                            Some(')') => {
                                chars.next();
                                break;
                            }
                            Some(c) => {
                                panic!("Unexpected character in #define argument list: {:?}", c)
                            }
                            None => {
                                panic!("End of text while in the middle of #define argument list.")
                            }
                        }
                    }
                    state = State::DefineExpression;
                }

                State::DefineExpression => {
                    r.c_expression = Some(String::from(chars.as_str().trim()));
                    break;
                }

                State::DefineValue => {
                    let v = Some(String::from(chars.as_str().trim()));
                    if r.defref.len() > 0 {
                        r.c_expression = v;
                    } else {
                        r.value = v;
                    }
                    break;
                }
            }
        }
    }

    if r.replace {
        r.c_expression = Some(code);
    }
}

fn parse_type_funcptr(r: &mut vkxml::FunctionPointer, code: &str) {
    let mut iter = code
        .split_whitespace()
        .flat_map(|s| c::TokenIter::new(s))
        .peekable();
    let token = iter.next().unwrap();
    if token != "typedef" {
        panic!("Unexpected token {:?}", token);
    }

    r.return_type = parse_c_field(&mut iter).unwrap();

    let token = iter.next().unwrap();
    if token != "(" {
        panic!("Unexpected token {:?}", token);
    }

    let token = iter.next().unwrap();
    if token != "VKAPI_PTR" {
        panic!("Unexpected token {:?}", token);
    }

    let token = iter.next().unwrap();
    if token != "*" {
        panic!("Unexpected token {:?}", token);
    }

    r.name.push_str(iter.next().unwrap());

    let token = iter.next().unwrap();
    if token != ")" {
        panic!("Unexpected token {:?}", token);
    }

    while let Some(token) = iter.next() {
        match token {
            "(" | "," => (),
            ")" => break,
            _ => panic!("Unexpected token {:?}", token),
        }

        let field = if let Some(field) = parse_c_field(&mut iter) {
            field
        } else {
            continue;
        };

        if field.basetype == "void" && field.reference.is_none() && field.name.is_none() {
            continue;
        }

        r.param.push(field);
    }
}

fn parse_constants<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> vkxml::Constants {
    let mut r = vkxml::Constants {
        notation: None,
        elements: Vec::new(),
    };

    match_attributes!{a in attributes,
        "name"    => (),
        "comment" => r.notation = Some(a.value)
    }

    while let Some(Ok(e)) = events.next() {
        match e {
            XmlEvent::StartElement { attributes, .. } => {
                if let Some(c) = parse_constant(attributes, events) {
                    r.elements.push(c);
                }
            }

            XmlEvent::EndElement { .. } => break,
            _ => (),
        }
    }

    r
}

fn parse_enumeration<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> vkxml::Enumeration {
    let mut r = vkxml::Enumeration {
        name: String::new(),
        notation: None,
        purpose: None,
        elements: Vec::new(),
    };

    match_attributes!{a in attributes,
        "name" => r.name = a.value,
        "type" => if a.value.as_str() == "bitmask" {
            r.purpose = Some(vkxml::EnumerationPurpose::Bitmask);
        } else {
            assert_eq!(a.value.as_str(), "enum");
        },
        "comment" => r.notation = Some(a.value)
    }

    match_elements!{attributes in events,
        "enum" => {
            let constant = parse_constant(attributes, events).unwrap();
            r.elements.push(vkxml::EnumerationElement::Enum(constant));
        },
        "comment" => {
            let text = parse_text_element(events);
            r.elements.push(vkxml::EnumerationElement::Notation(text));
        },
        "unused" => {
            let unused_range = parse_enum_unused(attributes, events);
            r.elements.push(vkxml::EnumerationElement::UnusedRange(unused_range));
        }
    }

    r
}

fn parse_constant<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> Option<vkxml::Constant> {
    let mut r = vkxml::Constant {
        name: String::new(),
        notation: None,
        number: None,
        hex: None,
        bitpos: None,
        c_expression: None,
    };

    match_attributes!{a in attributes,
        "name" => r.name = a.value,
        "value" => {
            if let Ok(value) = i32::from_str_radix(&a.value, 10) {
                r.number = Some(value);
            } else if a.value.starts_with("0x") {
                r.hex = Some(String::from(a.value.split_at(2).1))
            } else {
                r.c_expression = Some(a.value)
            }
        },
        "bitpos" => r.bitpos = Some(u32::from_str_radix(&a.value, 10).unwrap()),
        "comment" => r.notation = Some(a.value),
        "alias" => {
            // mk:TODO Not supported by vkxml.
            consume_current_element(events);
            return None;
        }
    }

    consume_current_element(events);
    Some(r)
}

fn parse_enum_unused<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> vkxml::Range {
    let mut r = vkxml::Range {
        range_start: 0,
        range_end: None,
    };

    println!("{:?}", attributes);
    match_attributes!{a in attributes,
        "start" => r.range_start = parse_integer(&a.value) as i32,
        "end" => r.range_end = Some(parse_integer(&a.value) as i32),
        "comment" => () // not supported by vkxml
    }

    consume_current_element(events);
    r
}

fn parse_feature_vkxml<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> vkxml::Feature {
    let feature = parse_feature(attributes, events);
    match feature {
        RegistryItem::Feature {
            api,
            name,
            number,
            protect,
            comment,
            items,
        } => vkxml::Feature {
            name,
            notation: comment,
            api,
            version: number,
            define: protect,
            elements: items
                .into_iter()
                .filter_map(|i| match i.into() {
                    Some(v) => Some(vkxml::FeatureElement::Require(v)),
                    None => None,
                })
                .collect(),
        },
        _ => panic!("Unexpected return value from parse_feature()."),
    }
}

fn parse_extensions_vkxml<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> vkxml::Extensions {
    let mut r = vkxml::Extensions {
        notation: None,
        elements: Vec::new(),
    };

    match parse_extensions(attributes, events) {
        RegistryItem::Extensions { comment, items } => {
            r.notation = comment;
            r.elements.reserve(items.len());
            for item in items {
                r.elements.push(item.into());
            }
        }
        _ => panic!("Unexpected return value from parse_extensions()."),
    }

    r
}

fn parse_c_field<'a, I: Iterator<Item = &'a str>>(
    iter: &mut std::iter::Peekable<I>,
) -> Option<vkxml::Field> {
    match iter.peek() {
        Some(&")") => return None,
        None => return None,
        _ => (),
    }

    let mut r = new_field();

    let mut token = iter.next().unwrap();
    loop {
        match token {
            "const" => {
                r.is_const = true;
                token = iter.next().unwrap();
            }
            "struct" => {
                r.is_struct = true;
                token = iter.next().unwrap();
            }
            _ => break,
        }
    }

    r.basetype = String::from(token);

    while let Some(&token) = iter.peek() {
        match token {
            "," | ")" | "(" | ";" => break,
            "*" => match r.reference {
                None => r.reference = Some(vkxml::ReferenceType::Pointer),
                _ => (),
            },
            "const" => r.reference = Some(vkxml::ReferenceType::PointerToConstPointer),
            "[" => {
                r.array = Some(vkxml::ArrayType::Static);
            }
            "]" => {
                break;
            }
            t => if r.array.is_some() {
                let mut is_number = true;
                for c in t.chars() {
                    if c < '0' || '9' < c {
                        is_number = false;
                        break;
                    }
                }
                if is_number {
                    r.size = Some(String::from(t));
                }
            } else {
                r.name = Some(String::from(token))
            },
        }
        iter.next().unwrap();
    }

    Some(r)
}

/*
//--------------------------------------------------------------------------------------------------
struct ChildrenDataIter<'a, R: Read + 'a> {
    events: &'a mut XmlEvents<R>,
    depth: usize,
}

impl<'a, R: Read> ChildrenDataIter<'a, R> {
    fn new(events: &'a mut XmlEvents<R>) -> Self {
        Self { events, depth: 1 }
    }
}

impl<'a, R: Read> Iterator for ChildrenDataIter<'a, R> {
    type Item = String;
    fn next(&mut self) -> Option<Self::Item> {
        while let Some(Ok(e)) = self.events.next() {
            match e {
                XmlEvent::StartElement { .. } => self.depth += 1,
                XmlEvent::EndElement { .. } => {
                    self.depth -= 1;
                    if self.depth == 0 {
                        break;
                    }
                }

                XmlEvent::Characters(text) => return Some(text),
                XmlEvent::Whitespace(..) => (),

                _ => panic!("Unexpected xml event {:?}", e),
            }
        }

        None
    }
}
*/

//--------------------------------------------------------------------------------------------------
impl From<RegistryItem> for vkxml::RegistryElement {
    fn from(orig: RegistryItem) -> Self {
        match orig {
            RegistryItem::Comment(..) => {
                panic!("Cannot convert using from as it affects enums state.")
            }

            RegistryItem::VendorIds { comment, mut items } => {
                vkxml::RegistryElement::VendorIds(vkxml::VendorIds {
                    notation: comment,
                    elements: items.drain(..).map(|i| i.into()).collect(),
                })
            }

            RegistryItem::Platforms { .. } => {
                panic!("Not supported by vkxml (cannot be converted 1:1).")
            }

            RegistryItem::Tags { comment, mut items } => {
                vkxml::RegistryElement::Tags(vkxml::Tags {
                    notation: comment,
                    elements: items.drain(..).map(|i| i.into()).collect(),
                })
            }

            _ => panic!("Missing implementation"),
        }
    }
}

impl From<VendorId> for vkxml::VendorId {
    fn from(orig: VendorId) -> Self {
        Self {
            name: orig.name,
            notation: orig.comment,
            id: format!("0x{:X}", orig.id),
        }
    }
}

impl From<Tag> for vkxml::Tag {
    fn from(orig: Tag) -> Self {
        Self {
            name: orig.name,
            notation: None,
            author: orig.author,
            contact: orig.contact,
        }
    }
}

impl From<Extension> for vkxml::Extension {
    fn from(mut orig: Extension) -> Self {
        let mut disabled = false;
        let mut match_api = None;
        let supported = orig.supported.take();
        match supported {
            Some(text) => {
                if text == "disabled" {
                    disabled = true;
                } else {
                    match_api = Some(text);
                }
            }
            None => (),
        }

        let mut elements = Vec::new();
        for item in orig.items {
            elements.push(item.into());
        }

        vkxml::Extension {
            name: orig.name,
            notation: orig.comment,
            number: match orig.number {
                Some(val) => val as i32,
                None => 0,
            },
            disabled,
            match_api,
            ty: match orig.ext_type {
                Some(text) => match text.as_str() {
                    "instance" => Some(vkxml::ExtensionType::Instance),
                    "device" => Some(vkxml::ExtensionType::Device),
                    _ => panic!(
                        "Unexpected value of type attribute on extension: {:?}",
                        text
                    ),
                },
                None => None,
            },
            define: orig.protect,
            requires: orig.requires,
            author: orig.author,
            contact: orig.contact,
            elements,
        }
    }
}

impl From<ExtensionItem> for vkxml::ExtensionElement {
    fn from(orig: ExtensionItem) -> Self {
        match orig {
            ExtensionItem::Remove {
                api,
                profile,
                comment,
                items,
            } => vkxml::ExtensionElement::Remove(vkxml::ExtensionSpecification {
                profile,
                notation: comment,
                extension: None,
                api,
                elements: items.into_iter().filter_map(|i| i.into()).collect(),
            }),

            ExtensionItem::Require {
                api,
                profile,
                extension,
                comment,
                items,
                ..
            } => vkxml::ExtensionElement::Require(vkxml::ExtensionSpecification {
                profile,
                notation: comment,
                extension,
                api,
                elements: items.into_iter().filter_map(|i| i.into()).collect(),
            }),
        }
    }
}

impl From<ExtensionItem> for Option<vkxml::FeatureSpecification> {
    fn from(orig: ExtensionItem) -> Self {
        match orig {
            ExtensionItem::Require {
                profile,
                comment,
                extension,
                items,
                ..
            } => Some(vkxml::FeatureSpecification {
                profile,
                notation: comment,
                extension,
                elements: items.into_iter().map(|i| i.into()).collect(),
            }),
            ExtensionItem::Remove { .. } => None,
        }
    }
}

impl From<InterfaceItem> for vkxml::FeatureReference {
    fn from(orig: InterfaceItem) -> Self {
        match orig {
            InterfaceItem::Comment(v) => vkxml::FeatureReference::Notation(v),
            InterfaceItem::Type { name, comment } => {
                vkxml::FeatureReference::DefinitionReference(vkxml::NamedIdentifier {
                    name,
                    notation: comment,
                })
            }
            InterfaceItem::Enum(e) => {
                vkxml::FeatureReference::EnumeratorReference(vkxml::NamedIdentifier {
                    name: e.name,
                    notation: e.comment,
                })
            }
            InterfaceItem::Command { name, comment } => {
                vkxml::FeatureReference::CommandReference(vkxml::NamedIdentifier {
                    name,
                    notation: comment,
                })
            }
        }
    }
}

impl From<InterfaceItem> for Option<vkxml::ExtensionSpecificationElement> {
    fn from(orig: InterfaceItem) -> Self {
        match orig {
            InterfaceItem::Comment(text) => {
                Some(vkxml::ExtensionSpecificationElement::Notation(text))
            }

            InterfaceItem::Enum(e) => match e.spec {
                EnumSpec::Alias { .. } => None,

                EnumSpec::Offset {
                    offset,
                    extends,
                    dir,
                    ..
                } => Some(vkxml::ExtensionSpecificationElement::Enum(
                    vkxml::ExtensionEnum {
                        name: e.name,
                        number: None,
                        notation: e.comment,
                        offset: Some(offset as usize),
                        negate: !dir,
                        extends,
                        hex: None,
                        bitpos: None,
                        c_expression: None,
                    },
                )),

                EnumSpec::Bitpos { bitpos, extends } => {
                    if let Some(extends) = extends {
                        Some(vkxml::ExtensionSpecificationElement::Enum(
                            vkxml::ExtensionEnum {
                                name: e.name,
                                number: None,
                                notation: e.comment,
                                offset: None,
                                negate: false,
                                extends,
                                hex: None,
                                bitpos: Some(bitpos as u32),
                                c_expression: None,
                            },
                        ))
                    } else {
                        Some(vkxml::ExtensionSpecificationElement::Constant(
                            vkxml::ExtensionConstant {
                                name: e.name,
                                notation: e.comment,
                                text: None,
                                enumref: None,
                                number: None,
                                hex: None,
                                bitpos: Some(bitpos as u32),
                                c_expression: None,
                            },
                        ))
                    }
                }

                EnumSpec::Value { mut value, extends } => {
                    let mut text = None;
                    let mut number = None;
                    let mut enumref = None;
                    if let Ok(val) = i32::from_str_radix(&value, 10) {
                        number = Some(val);
                    } else if value.starts_with('"') && value.ends_with('"') {
                        let end = value.len() - 1;
                        value.remove(end);
                        value.remove(0);
                        text = Some(value);
                    } else {
                        enumref = Some(value);
                    }

                    if let Some(extends) = extends {
                        Some(vkxml::ExtensionSpecificationElement::Enum(
                            vkxml::ExtensionEnum {
                                name: e.name,
                                number,
                                notation: e.comment,
                                offset: None,
                                negate: false,
                                extends,
                                hex: None,
                                bitpos: None,
                                c_expression: None,
                            },
                        ))
                    } else {
                        Some(vkxml::ExtensionSpecificationElement::Constant(
                            vkxml::ExtensionConstant {
                                name: e.name,
                                notation: e.comment,
                                text,
                                enumref,
                                number,
                                hex: None,
                                bitpos: None,
                                c_expression: None,
                            },
                        ))
                    }
                }

                EnumSpec::None => Some(vkxml::ExtensionSpecificationElement::EnumeratorReference(
                    vkxml::NamedIdentifier {
                        name: e.name,
                        notation: e.comment,
                    },
                )),
            },

            InterfaceItem::Command { name, comment } => Some(
                vkxml::ExtensionSpecificationElement::CommandReference(vkxml::NamedIdentifier {
                    name,
                    notation: comment,
                }),
            ),

            InterfaceItem::Type { name, comment } => Some(
                vkxml::ExtensionSpecificationElement::DefinitionReference(vkxml::NamedIdentifier {
                    name,
                    notation: comment,
                }),
            ),
        }
    }
}

impl From<Command> for Option<vkxml::Command> {
    fn from(orig: Command) -> Self {
        match orig {
            Command::Alias { .. } => None,
            Command::Definition {
                comment,
                proto,
                successcodes,
                errorcodes,
                implicitexternsyncparams,
                queues,
                renderpass,
                cmdbufferlevel,
                pipeline,
                params,
                code,
                ..
            } => {
                let mut r = vkxml::Command {
                    name: proto.name,
                    notation: comment,
                    return_type: new_field(),
                    param: Vec::new(),
                    external_sync: None,
                    cmdbufferlevel,
                    pipeline: None,
                    queues,
                    renderpass: None,
                };
                match proto.type_name {
                    Some(type_name) => r.return_type.basetype = type_name,
                    None => (),
                }
                r.return_type.successcodes = successcodes;
                r.return_type.errorcodes = errorcodes;
                for text in implicitexternsyncparams {
                    r.external_sync = Some(vkxml::ExternalSync { sync: text })
                }
                if let Some(renderpass) = renderpass {
                    r.renderpass = match renderpass.as_str() {
                        "both" => Some(vkxml::Renderpass::Both),
                        "inside" => Some(vkxml::Renderpass::Inside),
                        "outside" => Some(vkxml::Renderpass::Outside),
                        _ => panic!("Unexpected renderpass value {:?}", renderpass),
                    };
                }
                if let Some(pipeline) = pipeline {
                    r.pipeline = match pipeline.as_str() {
                        "graphics" => Some(vkxml::Pipeline::Graphics),
                        "compute" => Some(vkxml::Pipeline::Compute),
                        "transfer" => Some(vkxml::Pipeline::Transfer),
                        _ => panic!("Unexpected pipeline value {:?}", pipeline),
                    };
                }

                r.param.reserve(params.len());
                for param in params {
                    let mut p = new_field();
                    p.name = Some(param.definition.name);
                    if let Some(v) = param.definition.type_name {
                        p.basetype = v;
                    }
                    p.optional = param.optional;
                    p.sync = param.externsync;
                    if let Some(mut value) = param.len {
                        let null_terminated_part = ",null-terminated";
                        if value.as_str().ends_with(null_terminated_part) {
                            p.null_terminate = true;
                            let start = value.len() - null_terminated_part.len();
                            value.drain(start..);
                        }

                        if value.as_str() == "null-terminated" {
                            p.null_terminate = true;
                        } else {
                            p.size = Some(value);
                        }
                        p.array = Some(vkxml::ArrayType::Dynamic);
                    }
                    r.param.push(p);
                }

                let mut tokens = c::TokenIter::new(&code);
                while let Some(token) = tokens.next() {
                    if token == "(" {
                        break;
                    }
                }

                let mut is_array = false;
                let mut array_size = String::new();
                let mut param_idx = 0;
                let mut ptr_count = 0;
                let mut const_count = 0;
                while let Some(token) = tokens.next() {
                    match token {
                        "," | ")" => {
                            let p = &mut r.param[param_idx];
                            if const_count > 0 {
                                p.is_const = true;
                            }
                            if ptr_count == 2 {
                                if const_count >= 1 {
                                    p.reference = Some(vkxml::ReferenceType::PointerToConstPointer);
                                } else {
                                    p.reference = Some(vkxml::ReferenceType::PointerToPointer);
                                }
                            } else if ptr_count == 1 {
                                p.reference = Some(vkxml::ReferenceType::Pointer);
                            }
                            param_idx += 1;
                            ptr_count = 0;
                            const_count = 0;
                        }
                        "*" => ptr_count += 1,
                        "const" => const_count += 1,
                        "struct" => r.param[param_idx].is_struct = true,
                        "[" => is_array = true,
                        "]" => {
                            is_array = false;
                            let p = &mut r.param[param_idx];
                            p.array = Some(vkxml::ArrayType::Static);
                            p.size = Some(array_size);
                            array_size = String::new();
                        }
                        t => if is_array {
                            array_size.push_str(t);
                        },
                    }
                }

                Some(r)
            }
        }
    }
}
