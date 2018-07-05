extern crate vkxml;
extern crate xml;

type XmlEvents<R> = xml::reader::Events<R>;
type XmlAttribute = xml::attribute::OwnedAttribute;

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
            registry.elements.push(vkxml::RegistryElement::Commands(parse_commands_vkxml(
                attributes, events,
            )));
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
        "type" => {
            if let Some(t) = parse_type_vkxml(attributes, events) {
                elements.push(t);
            }
        },
        "comment" => elements.push(vkxml::DefinitionsElement::Notation(parse_text_element(events)))
    }

    vkxml::Definitions { notation, elements }
}

type ParseTypeFn<R> = for<'r> std::ops::Fn(Vec<XmlAttribute>, &'r mut XmlEvents<R>)
    -> Option<vkxml::DefinitionsElement>;

fn parse_type_vkxml<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> Option<vkxml::DefinitionsElement> {
    let fn_reference = |a, e: &mut XmlEvents<R>| {
        return Some(vkxml::DefinitionsElement::Reference(parse_type_reference(
            a, e,
        )));
    };
    let fn_include = |a, e: &mut XmlEvents<R>| {
        return Some(vkxml::DefinitionsElement::Include(parse_type_include(a, e)));
    };
    let fn_typedef = |_a, e: &mut XmlEvents<R>| {
        return Some(vkxml::DefinitionsElement::Typedef(parse_type_typedef(e)));
    };
    let fn_bitmask = |a, e: &mut XmlEvents<R>| {
        if let Some(bitmask) = parse_type_bitmask(a, e) {
            return Some(vkxml::DefinitionsElement::Bitmask(bitmask));
        } else {
            return None;
        }
    };
    let fn_struct = |a, e: &mut XmlEvents<R>| {
        return Some(vkxml::DefinitionsElement::Struct(parse_type_struct(a, e)));
    };
    let fn_union = |a, e: &mut XmlEvents<R>| {
        return Some(vkxml::DefinitionsElement::Union(parse_type_union(a, e)));
    };
    let fn_define = |a, e: &mut XmlEvents<R>| {
        return Some(vkxml::DefinitionsElement::Define(parse_type_define(a, e)));
    };
    let fn_handle = |a, e: &mut XmlEvents<R>| {
        return Some(vkxml::DefinitionsElement::Handle(parse_type_handle(a, e)));
    };
    let fn_enumeration = |a, e: &mut XmlEvents<R>| {
        return Some(vkxml::DefinitionsElement::Enumeration(
            parse_type_enumeration(a, e),
        ));
    };
    let fn_funcptr = |_a, e: &mut XmlEvents<R>| {
        return Some(vkxml::DefinitionsElement::FuncPtr(parse_type_funcptr(e)));
    };

    let mut parse_fn: &ParseTypeFn<R> = &fn_reference;

    for a in attributes.iter() {
        let name = a.name.local_name.as_str();
        let value = a.value.as_str();

        match (name, value) {
            ("category", "include") => {
                parse_fn = &fn_include;
                break;
            }

            ("category", "basetype") => {
                parse_fn = &fn_typedef;
                break;
            }

            ("category", "bitmask") => {
                parse_fn = &fn_bitmask;
                break;
            }

            ("category", "struct") => {
                parse_fn = &fn_struct;
                break;
            }

            ("category", "union") => {
                parse_fn = &fn_union;
                break;
            }

            ("category", "define") => {
                parse_fn = &fn_define;
                break;
            }

            ("category", "handle") => {
                parse_fn = &fn_handle;
                break;
            }

            ("category", "enum") => {
                parse_fn = &fn_enumeration;
                break;
            }

            ("category", "funcpointer") => {
                parse_fn = &fn_funcptr;
                break;
            }

            _ => (),
        }
    }

    parse_fn(attributes, events)
}

fn parse_type_include<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> vkxml::Include {
    let mut r = vkxml::Include {
        name: String::new(),
        notation: None,
        style: vkxml::IncludeStyle::Quote,
        need_ext: false,
    };

    match_attributes!{a in attributes,
        "name" => r.name = a.value,
        "category" => () // handled when deciding what type this is
    }

    while let Some(Ok(e)) = events.next() {
        match e {
            XmlEvent::Characters(text) => {
                r.style = if text.ends_with('"') {
                    vkxml::IncludeStyle::Quote
                } else {
                    vkxml::IncludeStyle::Bracket
                };
            }

            XmlEvent::StartElement { name, .. } => {
                if name.local_name.as_str() == "name" {
                    if let XmlEvent::Characters(text) = events.next().unwrap().unwrap() {
                        r.name.clear();
                        r.name.push_str(text.as_str());
                    } else {
                        panic!("Missing name of include.");
                    }
                } else {
                    consume_current_element(events);
                }
            }

            XmlEvent::EndElement { ref name } if name.local_name.as_str() == "type" => break,

            _ => (),
        }
    }

    r.need_ext = !r.name.contains('.');
    r
}

fn parse_type_reference<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> vkxml::Reference {
    let mut r = vkxml::Reference {
        name: vkxml::Identifier::new(),
        notation: None,
        include: None,
    };

    match_attributes!{a in attributes,
        "name"     => r.name    = a.value,
        "requires" => r.include = Some(a.value)
    }

    consume_current_element(events);

    r
}

fn parse_type_typedef<R: Read>(events: &mut XmlEvents<R>) -> vkxml::Typedef {
    let mut r = vkxml::Typedef {
        name: vkxml::Identifier::new(),
        notation: None,
        basetype: vkxml::Identifier::new(),
    };

    match_elements!{events,
        "type" => r.basetype = parse_text_element(events),
        "name" => r.name = parse_text_element(events)
    }

    r
}

fn parse_type_bitmask<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> Option<vkxml::Bitmask> {
    let mut r = vkxml::Bitmask {
        name: vkxml::Identifier::new(),
        notation: None,
        basetype: vkxml::Identifier::new(),
        enumref: None,
    };

    match_attributes!{a in attributes,
        "requires" => r.enumref = Some(a.value),
        "category" => (), // handled when deciding what type this is
        "name" => {
            // mk:TODO Not supported by vkxml.
            consume_current_element(events);
            return None;
        },
        "alias" => {
            // mk:TODO Not supported by vkxml.
            consume_current_element(events);
            return None;
        }
    }

    match_elements!{events,
        "type" => r.basetype = parse_text_element(events),
        "name" => r.name = parse_text_element(events)
    }

    Some(r)
}

fn parse_type_handle<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> vkxml::Handle {
    let mut r = vkxml::Handle {
        name: vkxml::Identifier::new(),
        notation: None,
        parent: None,
        ty: vkxml::HandleType::Dispatch,
    };

    match_attributes!{a in attributes,
        "parent"   => r.parent = Some(a.value),
        "name"     => (),
        "alias"    => (),
        "category" => () // handled when deciding what type this is
    }

    match_elements!{events,
        "type" => {
            let text = parse_text_element(events);
            r.ty = match text.as_str() {
                "VK_DEFINE_HANDLE" => vkxml::HandleType::Dispatch,
                "VK_DEFINE_NON_DISPATCHABLE_HANDLE" => vkxml::HandleType::NoDispatch,
                _ => panic!("Unexpected handle type: {}", text),
            };
        },
        "name" => r.name = parse_text_element(events)
    }

    r
}

fn parse_type_define<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> vkxml::Define {
    let mut r = vkxml::Define {
        name: vkxml::Identifier::new(),
        notation: None,
        is_disabled: true,
        comment: None,
        replace: false,
        defref: Vec::new(),
        parameters: Vec::new(),
        c_expression: None,
        value: None,
    };

    // mk:TODO Handle all macro types.
    match_attributes!{a in attributes,
        "name"     => r.name = a.value,
        "category" => ()
    }

    let mut code = String::new();
    while let Some(Ok(e)) = events.next() {
        match e {
            XmlEvent::StartElement { name, .. } => {
                let name = name.local_name.as_str();
                if name == "name" {
                    r.name = parse_text_element(events);
                    code.push_str(&r.name);
                } else if name == "type" {
                    let text = parse_text_element(events);
                    code.push_str(&text);
                    r.defref.push(text);
                } else {
                    panic!("Unexpected element {:?}", name);
                }
            }

            XmlEvent::Characters(text) => code.push_str(&text),
            XmlEvent::Whitespace(text) => code.push_str(&text),
            XmlEvent::CData(text) => code.push_str(&text),

            XmlEvent::EndElement { .. } => break,

            _ => (),
        }
    }

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
                        if !CTokenIter::is_c_identifier_char(c) {
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
                            if !CTokenIter::is_c_identifier_char(c) {
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

    r
}

fn parse_type_enumeration<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> vkxml::EnumerationDeclaration {
    let mut r = vkxml::EnumerationDeclaration {
        name: vkxml::Identifier::new(),
        notation: None,
    };

    match_attributes!{a in attributes,
        "name"     => r.name = a.value,
        "alias"    => (),
        "category" => ()
    }

    consume_current_element(events);

    r
}

fn parse_type_funcptr<R: Read>(events: &mut XmlEvents<R>) -> vkxml::FunctionPointer {
    // mk:TODO Full parsing.

    let mut r = vkxml::FunctionPointer {
        name: vkxml::Identifier::new(),
        notation: None,
        return_type: new_field(),
        param: Vec::new(),
    };

    let mut buffer = String::new();
    for text in ChildrenDataIter::new(events) {
        buffer.push_str(&text);
    }

    let mut iter = buffer
        .split_whitespace()
        .flat_map(|s| CTokenIter::new(s))
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

    r
}

fn parse_type_struct<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> vkxml::Struct {
    let mut r = vkxml::Struct {
        name: vkxml::Identifier::new(),
        notation: None,
        is_return: false,
        extends: None,
        elements: Vec::new(),
    };

    match_attributes!{a in attributes,
        "name"          => r.name = a.value,
        "category"      => (),
        "alias"         => (),
        "comment"       => r.notation = Some(a.value),
        "returnedonly"  => r.is_return = a.value.as_str() == "true",
        "structextends" => r.extends = Some(a.value)
    }

    match_elements!{attributes in events,
        "member" => {
            let member = parse_type_struct_member(attributes, events);
            r.elements.push(vkxml::StructElement::Member(member));
        },
        "comment" => r.elements.push(vkxml::StructElement::Notation(parse_text_element(events)))
    }

    r
}

fn parse_type_union<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> vkxml::Union {
    let mut r = vkxml::Union {
        name: vkxml::Identifier::new(),
        notation: None,
        elements: Vec::new(),
    };

    match_attributes!{a in attributes,
        "name"     => r.name     = a.value,
        "comment"  => r.notation = Some(a.value),
        "category" => ()
    }

    match_elements!{attributes in events,
        "member" => {
            let member = parse_type_struct_member(attributes, events);
            r.elements.push(member);
        }
    }

    r
}

fn parse_type_struct_member<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> vkxml::Field {
    let mut r = new_field();

    match_attributes!{a in attributes,
        "len" => {
            let mut value = a.value;
            let null_terminated_part = ",null-terminated";
            if value.as_str().ends_with(null_terminated_part) {
                r.null_terminate = true;
                let start = value.len() - null_terminated_part.len();
                value.drain(start..);
            }

            if value.as_str() == "null-terminated" {
                r.null_terminate = true;
            } else {
                r.size = Some(value);
            }
            r.array = Some(vkxml::ArrayType::Dynamic);
        },
        "altlen"                => r.c_size = Some(a.value),
        "externsync"            => r.sync = Some(a.value),
        "optional"              => r.optional = Some(a.value),
        "noautovalidity"        => (),
        "values"                => r.type_enums = Some(a.value),
        "validextensionstructs" => () // mk:TODO Not supported by vkxml.
    }

    // mk:TODO Full parsing. (const, reference/pointer, array, ...)

    let mut panic_cause = None;
    while let Some(Ok(e)) = events.next() {
        match e {
            XmlEvent::StartElement { name, .. } => {
                let name = name.local_name.as_str();
                if name == "type" {
                    r.basetype = parse_text_element(events);
                } else if name == "name" {
                    r.name = Some(parse_text_element(events));
                } else if name == "enum" {
                    r.size_enumref = Some(parse_text_element(events));
                } else if name == "comment" {
                    r.notation = Some(parse_text_element(events));
                } else {
                    panic!("Unexpected element {:?}", name);
                }
            }

            XmlEvent::Characters(mut text) => {
                let mut iter = text.split_whitespace().flat_map(|s| CTokenIter::new(s));

                let mut array_start_curr = false;
                for token in iter {
                    let array_start_prev = array_start_curr;
                    array_start_curr = false;
                    match token {
                        "struct" => r.is_struct = true,
                        "*" => match r.reference {
                            None => r.reference = Some(vkxml::ReferenceType::Pointer),
                            Some(vkxml::ReferenceType::Pointer) => {
                                r.reference = Some(vkxml::ReferenceType::PointerToPointer)
                            }
                            // PointerToPointer should not encounter * token
                            // PointerToConstPointer is created by encountering const and assumes there will be one more * following it.
                            _ => (),
                        },
                        "const" => match r.reference {
                            None => r.is_const = true,
                            Some(vkxml::ReferenceType::Pointer) => {
                                r.reference = Some(vkxml::ReferenceType::PointerToConstPointer)
                            }
                            _ => (),
                        },
                        "[" => {
                            r.array = Some(vkxml::ArrayType::Static);
                            array_start_curr = true;
                        }
                        "]" => match r.array {
                            Some(vkxml::ArrayType::Static) => (),
                            _ => {
                                panic!("Found ']' with no corresponding '[' for array declaration.")
                            }
                        },
                        t => {
                            if array_start_prev && t.len() > 0 {
                                r.size = Some(String::from(t));
                            } else if panic_cause.is_none() {
                                panic_cause = Some(String::from(t));
                            }
                        }
                    }
                }
            }

            XmlEvent::EndElement { .. } => break,
            _ => (),
        }
    }

    if let Some(text) = panic_cause {
        panic!("Unexpected text {:?} when parsing field {:?}", text, r);
    }
    r
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

fn parse_commands_vkxml<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> vkxml::Commands {
    let mut r = vkxml::Commands {
        notation: None,
        elements: Vec::new(),
    };

    match_attributes!{a in attributes,
        "comment" => r.notation = Some(a.value)
    }

    match_elements!{attributes in events,
        "command" => {
            if let Some(cmd) = parse_command_vkxml(attributes, events) {
                r.elements.push(cmd);
            }
        }
    }

    r
}

fn parse_command_vkxml<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> Option<vkxml::Command> {
    let mut r = vkxml::Command {
        name: vkxml::Identifier::new(),
        notation: None,
        return_type: new_field(),
        param: Vec::new(),
        external_sync: None,
        renderpass: None,
        cmdbufferlevel: None,
        pipeline: None,
        queues: None,
    };

    match_elements!{attributes in events,
        "proto" => {
            let mut proto = parse_type_struct_member(attributes, events);
            r.name = proto.name.take().unwrap();
            r.return_type = proto;
        },
        "param" => r.param.push(parse_type_struct_member(attributes, events)),
        "implicitexternsyncparams" => {
            for text in ChildrenDataIter::new(events) {
                r.external_sync = Some(vkxml::ExternalSync { sync: text })
            }
        }
    }

    match_attributes!{a in attributes,
        "successcodes" => r.return_type.successcodes = Some(a.value),
        "errorcodes" => r.return_type.errorcodes = Some(a.value),
        "queues" => r.queues = Some(a.value),
        "cmdbufferlevel" => r.cmdbufferlevel = Some(a.value),
        "comment" => r.notation = Some(a.value),
        "pipeline" => match a.value.as_str() {
            "graphics" => r.pipeline = Some(vkxml::Pipeline::Graphics),
            "compute" => r.pipeline = Some(vkxml::Pipeline::Compute),
            "transfer" => r.pipeline = Some(vkxml::Pipeline::Transfer),
            _ => panic!("Unexpected attribute value {:?}", a.value),
        },
        "renderpass" => match a.value.as_str() {
            "both" => r.renderpass = Some(vkxml::Renderpass::Both),
            "inside" => r.renderpass = Some(vkxml::Renderpass::Inside),
            "outside" => r.renderpass = Some(vkxml::Renderpass::Outside),
            _ => panic!("Unexpected attribute value {:?}", a.value),
        },
        "name" => return None, // mk:TODO Not supported by vkxml.
        "alias" => return None // mk:TODO Not supported by vkxml.
    }

    Some(r)
}

fn parse_feature_vkxml<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> vkxml::Feature {
    let mut r = vkxml::Feature {
        name: vkxml::Identifier::new(),
        notation: None,
        api: String::new(),
        version: 0.0,
        define: None,
        elements: Vec::new(),
    };

    match_attributes!{a in attributes,
        "api" => r.api = a.value,
        "name" => r.name = a.value,
        "comment" => r.notation = Some(a.value),
        "number" => {
            use std::str::FromStr;
            r.version = f32::from_str(&a.value).unwrap();
        }
    }

    match_elements!{attributes in events,
        "require" => r.elements.push(vkxml::FeatureElement::Require(
            parse_feature_require(attributes, events),
        ))
    }

    r
}

fn parse_feature_require<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> vkxml::FeatureSpecification {
    let mut r = vkxml::FeatureSpecification {
        profile: None,
        notation: None,
        extension: None,
        elements: Vec::new(),
    };

    match_attributes!{a in attributes,
        "comment" => r.notation = Some(a.value)
    }

    match_elements!{attributes in events,
        "type" => r.elements.push(vkxml::FeatureReference::DefinitionReference(
            parse_feature_require_ref(attributes, events),
        )),
        "enum" => r.elements.push(vkxml::FeatureReference::EnumeratorReference(
            parse_feature_require_ref(attributes, events),
        )),
        "command" => r.elements.push(vkxml::FeatureReference::CommandReference(
            parse_feature_require_ref(attributes, events),
        )),
        "comment" => r.elements.push(vkxml::FeatureReference::Notation(
            parse_text_element(events),
        ))
    }

    r
}

fn parse_feature_require_ref<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> vkxml::NamedIdentifier {
    let mut r = vkxml::NamedIdentifier {
        name: vkxml::Identifier::new(),
        notation: None,
    };

    match_attributes!{a in attributes,
        "name"      => r.name     = a.value,
        "comment"   => r.notation = Some(a.value),
        "extends"   => (),   // mk:TODO Not supported by vkxml.
        "extnumber" => (), // mk:TODO Not supported by vkxml.
        "offset"    => (),    // mk:TODO Not supported by vkxml.
        "bitpos"    => (),    // mk:TODO Not supported by vkxml.
        "dir"       => ()        // mk:TODO Not supported by vkxml.
    }

    consume_current_element(events);
    r
}

fn parse_extensions_vkxml<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> vkxml::Extensions {
    let mut r = vkxml::Extensions {
        notation: None,
        elements: Vec::new(),
    };

    match_attributes!{a in attributes,
        "comment" => r.notation = Some(a.value)
    }

    match_elements!{attributes in events,
        "extension" => r.elements.push(parse_extension_vkxml(attributes, events))
    }

    r
}

fn parse_extension_vkxml<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> vkxml::Extension {
    let mut r = vkxml::Extension {
        name: vkxml::Identifier::new(),
        notation: None,
        number: 0,
        disabled: false,
        match_api: None,
        ty: None,
        define: None,
        requires: None,
        author: None,
        contact: None,
        elements: Vec::new(),
    };

    match_attributes!{a in attributes,
        "name" => r.name = a.value,
        "comment" => r.notation = Some(a.value),
        "number" => {
            use std::str::FromStr;
            r.number = i32::from_str(&a.value).unwrap();
        },
        "type" => {
            let ty = a.value.as_str();
            r.ty = Some(match ty {
                "instance" => vkxml::ExtensionType::Instance,
                "device" => vkxml::ExtensionType::Device,
                _ => panic!("Unexpected attribute value {:?}", ty),
            });
        },
        "author" => r.author = Some(a.value),
        "contact" => r.contact = Some(a.value),
        "supported" => if a.value.as_str() == "disabled" {
            r.disabled = true;
        } else {
            r.match_api = Some(a.value);
        },
        "requires" => r.requires = Some(a.value),
        "protect" => r.define = Some(a.value),
        "platform" => (),     // mk:TODO Not supported by vkxml.
        "requiresCore" => ()  // mk:TODO Not supported by vkxml.
    }

    match_elements!{attributes in events,
        "require" => r.elements.push(vkxml::ExtensionElement::Require(
            parse_extension_require(attributes, events),
        ))
    }

    r
}

fn parse_extension_require<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> vkxml::ExtensionSpecification {
    let mut r = vkxml::ExtensionSpecification {
        profile: None,
        notation: None,
        extension: None,
        api: None,
        elements: Vec::new(),
    };

    match_attributes!{a in attributes,
        "extension" => r.extension = Some(a.value),
        "feature"   => () // mk:TODO Not supported by vkxml.
    }

    match_elements!{attributes in events,
        "comment" => r.elements.push(vkxml::ExtensionSpecificationElement::Notation(
            parse_text_element(events),
        )),
        "enum" => r.elements.push(parse_extension_require_enum(attributes, events)),
        "command" => r.elements.push(vkxml::ExtensionSpecificationElement::CommandReference(
            parse_extension_require_ref(attributes, events),
        )),
        "type" => r.elements.push(vkxml::ExtensionSpecificationElement::DefinitionReference(
            parse_extension_require_ref(attributes, events),
        ))
    }

    r
}

fn parse_extension_require_enum<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> vkxml::ExtensionSpecificationElement {
    let mut name = vkxml::Identifier::new();
    let mut notation = None;
    let mut offset = None;
    let mut negate = false;
    let mut extends = None;
    let mut number = None;
    let hex = None;
    let mut bitpos = None;
    let c_expression = None;
    let mut text = None;
    let mut enumref = None;
    let mut name_only = true;

    for mut a in attributes {
        let n = a.name.local_name.as_str();
        if n != "name" {
            name_only = false;
        }
        match n {
            "name" => name = a.value,
            "value" => {
                if let Ok(val) = i32::from_str_radix(&a.value, 10) {
                    number = Some(val);
                } else if a.value.starts_with('"') && a.value.ends_with('"') {
                    let end = a.value.len() - 1;
                    a.value.remove(end);
                    a.value.remove(0);
                    text = Some(a.value);
                } else {
                    enumref = Some(a.value);
                }
            }
            "offset" => offset = Some(usize::from_str_radix(&a.value, 10).unwrap()),
            "dir" => {
                if a.value.as_str() != "-" {
                    panic!(
                        "Unexpected value of attribute {:?}, expected \"-\", found {:?}",
                        name, a.value
                    );
                }
                negate = a.value.as_str() == "-";
            }
            "extends" => extends = Some(a.value),
            "comment" => notation = Some(a.value),
            "bitpos" => bitpos = Some(u32::from_str_radix(&a.value, 10).unwrap()),

            "extnumber" => (), // mk:TODO Not supported by vkxml.
            "alias" => (),     // mk:TODO Not supported by vkxml.

            _ => panic!("Unexpected attributes {:?}", n),
        }
    }

    consume_current_element(events);

    if name_only {
        vkxml::ExtensionSpecificationElement::EnumeratorReference(vkxml::NamedIdentifier {
            name,
            notation,
        })
    } else if let Some(extends) = extends {
        vkxml::ExtensionSpecificationElement::Enum(vkxml::ExtensionEnum {
            name,
            number,
            notation,
            offset,
            negate,
            extends,
            hex,
            bitpos,
            c_expression,
        })
    } else {
        vkxml::ExtensionSpecificationElement::Constant(vkxml::ExtensionConstant {
            name,
            notation,
            text,
            enumref,
            number,
            hex,
            bitpos,
            c_expression,
        })
    }
}

fn parse_extension_require_ref<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> vkxml::NamedIdentifier {
    let mut r = vkxml::NamedIdentifier {
        name: vkxml::Identifier::new(),
        notation: None,
    };

    match_attributes!{a in attributes,
        "name" => r.name = a.value
    }

    consume_current_element(events);
    r
}

fn parse_c_field<'a, I: Iterator<Item = &'a str>>(
    iter: &mut std::iter::Peekable<I>,
) -> Option<vkxml::Field> {
    match iter.peek() {
        Some(&")") => return None,
        _ => (),
    }

    let mut r = new_field();

    let mut token = iter.next().unwrap();
    if token == "const" {
        r.is_const = true;
        token = iter.next().unwrap();
    }

    r.basetype = String::from(token);

    while let Some(&token) = iter.peek() {
        match token {
            "," | ")" | "(" | ";" => break,
            "*" => r.reference = Some(vkxml::ReferenceType::Pointer),
            _ => r.name = Some(String::from(token)),
        }
        iter.next().unwrap();
    }

    Some(r)
}

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

//--------------------------------------------------------------------------------------------------
struct CTokenIter<'a> {
    src: &'a str,
}

impl<'a> CTokenIter<'a> {
    fn new(src: &'a str) -> Self {
        Self { src }
    }

    fn is_c_identifier_char(c: char) -> bool {
        if '0' <= c && c <= '9' {
            true
        } else if 'a' <= c && c <= 'z' {
            true
        } else if 'A' <= c && c <= 'Z' {
            true
        } else if c == '_' {
            true
        } else {
            false
        }
    }

    #[allow(dead_code)]
    fn is_c_identifier(s: &str) -> bool {
        for c in s.chars() {
            if !CTokenIter::is_c_identifier_char(c) {
                return false;
            }
        }
        true
    }
}

impl<'a> Iterator for CTokenIter<'a> {
    type Item = &'a str;
    fn next(&mut self) -> Option<&'a str> {
        let mut iter = self.src.char_indices();
        if let Some((_, c)) = iter.next() {
            if CTokenIter::is_c_identifier_char(c) {
                for (end_idx, c) in iter {
                    if !CTokenIter::is_c_identifier_char(c) {
                        let split = self.src.split_at(end_idx);
                        self.src = split.1;
                        return Some(split.0);
                    }
                }

                let res = self.src;
                self.src = "";
                return Some(res);
            } else {
                let split = self.src.split_at(1);
                self.src = split.1;
                return Some(split.0);
            }
        }

        None
    }
}

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
