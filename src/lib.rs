extern crate vkxml;
extern crate xml;

use std::io::Read;

type XmlEvents<R> = xml::reader::Events<R>;
type XmlAttribute = xml::attribute::OwnedAttribute;
use xml::reader::XmlEvent;

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

//--------------------------------------------------------------------------------------------------
pub fn parse_registry<R: Read>(events: &mut XmlEvents<R>) -> vkxml::Registry {
    let mut registry = vkxml::Registry {
        elements: Vec::new(),
    };

    let mut enums: Option<vkxml::Enums> = None;

    while let Some(Ok(e)) = events.next() {
        match e {
            XmlEvent::StartElement {
                name, attributes, ..
            } => {
                parse_registry_element(
                    name.local_name.as_str(),
                    attributes,
                    events,
                    &mut enums,
                    &mut registry.elements,
                );
            }

            XmlEvent::EndElement { .. } => break,
            _ => {}
        }
    }

    registry
}

fn parse_registry_element<R: Read>(
    name: &str,
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
    enums: &mut Option<vkxml::Enums>,
    registry_elements: &mut Vec<vkxml::RegistryElement>,
) {
    fn flush_enums(
        enums: &mut Option<vkxml::Enums>,
        registry_elements: &mut Vec<vkxml::RegistryElement>,
    ) {
        if let Some(value) = enums.take() {
            registry_elements.push(vkxml::RegistryElement::Enums(value));
        }
    }

    match name {
        "comment" => {
            let notation = parse_text_element(events);
            if let &mut Some(ref mut enums) = enums {
                enums.elements.push(vkxml::EnumsElement::Notation(notation));
            } else {
                registry_elements.push(vkxml::RegistryElement::Notation(notation));
            }
        }

        "vendorids" => {
            flush_enums(enums, registry_elements);
            registry_elements.push(vkxml::RegistryElement::VendorIds(parse_vendorids(
                attributes,
                events,
            )));
        }

        "tags" => {
            flush_enums(enums, registry_elements);
            registry_elements.push(vkxml::RegistryElement::Tags(parse_tags(attributes, events)));
        }

        "types" => {
            flush_enums(enums, registry_elements);
            registry_elements.push(vkxml::RegistryElement::Definitions(parse_types(
                attributes,
                events,
            )));
        }

        "enums" => {
            let mut is_constant = true;
            for a in attributes.iter() {
                if a.name.local_name.as_str() == "type" {
                    is_constant = false;
                    break;
                }
            }

            if is_constant {
                flush_enums(enums, registry_elements);
                registry_elements.push(vkxml::RegistryElement::Constants(parse_constants(
                    attributes,
                    events,
                )));
            } else {
                let enumeration = parse_enumeration(attributes, events);
                if let &mut Some(ref mut enums) = enums {
                    enums
                        .elements
                        .push(vkxml::EnumsElement::Enumeration(enumeration));
                } else {
                    *enums = Some(vkxml::Enums {
                        notation: None,
                        elements: vec![vkxml::EnumsElement::Enumeration(enumeration)],
                    });
                }
            }
        }

        "commands" => {
            flush_enums(enums, registry_elements);
            registry_elements.push(vkxml::RegistryElement::Commands(parse_commands(
                attributes,
                events,
            )));
        }

        "feature" => {
            flush_enums(enums, registry_elements);
            registry_elements.push(vkxml::RegistryElement::Features(vkxml::Features {
                elements: vec![parse_feature(attributes, events)],
            }));
        }

        "extensions" => {
            flush_enums(enums, registry_elements);
            registry_elements.push(vkxml::RegistryElement::Extensions(parse_extensions(
                attributes,
                events,
            )));
        }

        _ => {
            panic!("Unexpected element {:?}", name);
        }
    }
}

//--------------------------------------------------------------------------------------------------
fn parse_vendorids<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> vkxml::VendorIds {
    let mut notation = None;
    for a in attributes {
        if a.name.local_name == "comment" {
            notation = Some(a.value);
            break;
        }
    }

    let mut elements = Vec::new();
    while let Some(Ok(e)) = events.next() {
        match e {
            XmlEvent::StartElement {
                name, attributes, ..
            } => {
                if name.local_name == "vendorid" {
                    let vendorid = parse_vendorid(attributes, events);
                    elements.push(vendorid);
                } else {
                    consume_current_element(events)
                }
            }

            XmlEvent::EndElement { ref name } if name.local_name == "vendorids" => break,
            _ => (),
        }
    }

    vkxml::VendorIds { notation, elements }
}

fn parse_vendorid<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> vkxml::VendorId {
    let mut result = vkxml::VendorId {
        name: String::new(),
        notation: None,
        id: vkxml::HexadecimalNumber::new(),
    };

    for a in attributes {
        match a.name.local_name.as_str() {
            "name" => result.name = a.value,
            "id" => result.id = a.value,
            "comment" => result.notation = Some(a.value),
            _ => (),
        }
    }

    consume_current_element(events);
    result
}

//--------------------------------------------------------------------------------------------------
fn parse_tags<R: Read>(attributes: Vec<XmlAttribute>, events: &mut XmlEvents<R>) -> vkxml::Tags {
    let mut notation = None;
    for a in attributes {
        if a.name.local_name == "comment" {
            notation = Some(a.value);
            break;
        }
    }

    let mut elements = Vec::new();
    while let Some(Ok(e)) = events.next() {
        match e {
            XmlEvent::StartElement {
                name, attributes, ..
            } => {
                if name.local_name == "tag" {
                    let tag = parse_tag(attributes, events);
                    elements.push(tag);
                } else {
                    consume_current_element(events);
                }
            }

            XmlEvent::EndElement { ref name } if name.local_name == "tags" => break,
            _ => (),
        }
    }

    vkxml::Tags { notation, elements }
}

fn parse_tag<R: Read>(attributes: Vec<XmlAttribute>, events: &mut XmlEvents<R>) -> vkxml::Tag {
    let mut result = vkxml::Tag {
        name: String::new(),
        notation: None,
        author: String::new(),
        contact: String::new(),
    };

    for a in attributes {
        match a.name.local_name.as_str() {
            "name" => result.name = a.value,
            "author" => result.author = a.value,
            "contact" => result.contact = a.value,
            "comment" => result.notation = Some(a.value),
            _ => (),
        }
    }

    consume_current_element(events);
    result
}

//--------------------------------------------------------------------------------------------------
fn parse_types<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> vkxml::Definitions {
    let mut notation = None;
    for a in attributes {
        if a.name.local_name == "comment" {
            notation = Some(a.value);
            break;
        }
    }

    let mut elements = Vec::new();
    while let Some(Ok(e)) = events.next() {
        match e {
            XmlEvent::StartElement {
                name, attributes, ..
            } => {
                let name = name.local_name.as_str();
                match name {
                    "type" => elements.push(parse_type(attributes, events)),
                    "comment" => elements.push(vkxml::DefinitionsElement::Notation(
                        parse_text_element(events),
                    )),
                    _ => panic!("Unexpected element '{:?}'", name),
                };
            }

            XmlEvent::EndElement { .. } => break,
            _ => (),
        }
    }

    vkxml::Definitions { notation, elements }
}

type ParseTypeFn<R> =
    for<'r> std::ops::Fn(Vec<XmlAttribute>, &'r mut XmlEvents<R>) -> vkxml::DefinitionsElement;

fn parse_type<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> vkxml::DefinitionsElement {
    let fn_reference = |a, e: &mut XmlEvents<R>| {
        return vkxml::DefinitionsElement::Reference(parse_type_reference(a, e));
    };
    let fn_include = |a, e: &mut XmlEvents<R>| {
        return vkxml::DefinitionsElement::Include(parse_type_include(a, e));
    };
    let fn_typedef = |_a, e: &mut XmlEvents<R>| {
        return vkxml::DefinitionsElement::Typedef(parse_type_typedef(e));
    };
    let fn_bitmask = |a, e: &mut XmlEvents<R>| {
        return vkxml::DefinitionsElement::Bitmask(parse_type_bitmask(a, e));
    };
    let fn_struct = |a, e: &mut XmlEvents<R>| {
        return vkxml::DefinitionsElement::Struct(parse_type_struct(a, e));
    };
    let fn_union = |a, e: &mut XmlEvents<R>| {
        return vkxml::DefinitionsElement::Union(parse_type_union(a, e));
    };
    let fn_define = |a, e: &mut XmlEvents<R>| {
        return vkxml::DefinitionsElement::Define(parse_type_define(a, e));
    };
    let fn_handle = |a, e: &mut XmlEvents<R>| {
        return vkxml::DefinitionsElement::Handle(parse_type_handle(a, e));
    };
    let fn_enumeration = |a, e: &mut XmlEvents<R>| {
        return vkxml::DefinitionsElement::Enumeration(parse_type_enumeration(a, e));
    };
    let fn_funcptr = |_a, e: &mut XmlEvents<R>| {
        return vkxml::DefinitionsElement::FuncPtr(parse_type_funcptr(e));
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

    for a in attributes {
        if a.name.local_name.as_str() == "name" {
            r.name.clear();
            r.name.push_str(a.value.as_str());
        }
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

    for a in attributes {
        let name = a.name.local_name;
        match name.as_str() {
            "name" => {
                r.name = a.value;
            }
            "requires" => {
                r.include = Some(a.value);
            }
            _ => (),
        }
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

    while let Some(Ok(e)) = events.next() {
        match e {
            XmlEvent::StartElement { ref name, .. } => {
                if name.local_name.as_str() == "type" {
                    r.basetype = parse_text_element(events);
                } else if name.local_name.as_str() == "name" {
                    r.name = parse_text_element(events);
                } else {
                    consume_current_element(events);
                }
            }

            XmlEvent::EndElement { .. } => break,

            _ => (),
        }
    }

    r
}

fn parse_type_bitmask<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> vkxml::Bitmask {
    let mut r = vkxml::Bitmask {
        name: vkxml::Identifier::new(),
        notation: None,
        basetype: vkxml::Identifier::new(),
        enumref: None,
    };

    for a in attributes {
        if a.name.local_name.as_str() == "requires" {
            r.enumref = Some(a.value);
            break;
        }
    }

    while let Some(Ok(e)) = events.next() {
        match e {
            XmlEvent::StartElement { ref name, .. } => {
                if name.local_name.as_str() == "type" {
                    r.basetype = parse_text_element(events);
                } else if name.local_name.as_str() == "name" {
                    r.name = parse_text_element(events);
                } else {
                    consume_current_element(events);
                }
            }

            XmlEvent::EndElement { .. } => break,

            _ => (),
        }
    }

    r
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

    for a in attributes {
        if a.name.local_name.as_str() == "parent" {
            r.parent = Some(a.value);
            break;
        }
    }

    while let Some(Ok(e)) = events.next() {
        match e {
            XmlEvent::StartElement { ref name, .. } => {
                if name.local_name.as_str() == "type" {
                    let text = parse_text_element(events);
                    r.ty = match text.as_str() {
                        "VK_DEFINE_HANDLE" => vkxml::HandleType::Dispatch,
                        "VK_DEFINE_NON_DISPATCHABLE_HANDLE" => vkxml::HandleType::NoDispatch,
                        _ => panic!("Unexpected handle type: {}", text),
                    };
                } else if name.local_name.as_str() == "name" {
                    r.name = parse_text_element(events);
                } else {
                    consume_current_element(events);
                }
            }

            XmlEvent::EndElement { .. } => break,

            _ => (),
        }
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
        is_disabled: false,
        comment: None,
        replace: false,
        defref: Vec::new(),
        parameters: Vec::new(),
        c_expression: None,
        value: None,
    };

    // mk:TODO Handle all macro types.
    for a in attributes {
        if a.name.local_name.as_str() == "name" {
            r.name = a.value;
            break;
        }
    }

    while let Some(Ok(e)) = events.next() {
        match e {
            XmlEvent::StartElement { name, .. } => {
                let name = name.local_name.as_str();
                if name == "name" {
                    r.name = parse_text_element(events);
                } else if name == "type" {
                    r.defref.push(parse_text_element(events));
                } else {
                    consume_current_element(events);
                }
            }

            XmlEvent::EndElement { .. } => break,

            _ => (),
        }
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

    for a in attributes {
        let name = a.name.local_name.as_str();
        let value = a.value.as_str();
        match name {
            "name" => {
                r.name.clear();
                r.name.push_str(value);
                break;
            }
            _ => (),
        }
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

    for a in attributes {
        let name = a.name.local_name.as_str();
        match name {
            "name" => r.name = a.value,
            "comment" => r.notation = Some(a.value),
            "returnedonly" => r.is_return = a.value.as_str() == "true",
            "structextends" => r.extends = Some(a.value),
            _ => (),
        }
    }

    while let Some(Ok(e)) = events.next() {
        match e {
            XmlEvent::StartElement {
                name, attributes, ..
            } => {
                let name = name.local_name.as_str();
                if name == "member" {
                    let member = parse_type_struct_member(attributes, events);
                    r.elements.push(vkxml::StructElement::Member(member));
                } else if name == "comment" {
                    let comment = parse_text_element(events);
                    r.elements.push(vkxml::StructElement::Notation(comment));
                } else {
                    panic!("Unexpected element {:?}", name);
                }
            }

            XmlEvent::EndElement { .. } => break,
            _ => (),
        }
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

    for a in attributes {
        let name = a.name.local_name.as_str();
        match name {
            "name" => r.name = a.value,
            "comment" => r.notation = Some(a.value),
            _ => (),
        }
    }

    while let Some(Ok(e)) = events.next() {
        match e {
            XmlEvent::StartElement {
                name, attributes, ..
            } => {
                let name = name.local_name.as_str();
                if name == "member" {
                    let member = parse_type_struct_member(attributes, events);
                    r.elements.push(member);
                } else {
                    panic!("Unexpected element {:?}", name);
                }
            }

            XmlEvent::EndElement { .. } => break,
            _ => (),
        }
    }

    r
}

fn parse_type_struct_member<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> vkxml::Field {
    let mut r = new_field();

    for a in attributes {
        let name = a.name.local_name;
        let value = a.value;
        match name.as_str() {
            "len" => {
                let mut value = value;
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
            }
            "altlen" => r.c_size = Some(value),
            "externsync" => r.sync = Some(value),
            "optional" => r.optional = Some(value),
            "noautovalidity" => (),
            "values" => r.type_enums = Some(value),
            _ => panic!("Unexpected attribute {:?}", name),
        }
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
                text.retain(|c| !c.is_ascii_whitespace());
                match text.as_str() {
                    "const" => r.is_const = true,
                    "*" => r.reference = Some(vkxml::ReferenceType::Pointer),
                    "**" => r.reference = Some(vkxml::ReferenceType::PointerToPointer),
                    "*const*" => r.reference = Some(vkxml::ReferenceType::PointerToConstPointer),
                    "struct" => r.is_struct = true,
                    "]" => match r.array {
                        Some(vkxml::ArrayType::Static) => (),
                        _ => panic!("Found ']' with no corresponding '[' for array declaration."),
                    },

                    // Can be "[" but also "[2]"
                    _ => {
                        if text.starts_with('[') {
                            r.array = Some(vkxml::ArrayType::Static);
                            if text.len() > 1 && text.ends_with(']') {
                                text.pop().unwrap();
                                text.remove(0);
                                r.size = Some(text);
                            }
                        } else if panic_cause.is_none() {
                            panic_cause = Some(text);
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

//--------------------------------------------------------------------------------------------------
fn parse_constants<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> vkxml::Constants {
    let mut r = vkxml::Constants {
        notation: None,
        elements: Vec::new(),
    };

    for a in attributes {
        if a.name.local_name.as_str() == "comment" {
            r.notation = Some(a.value);
        }
    }

    while let Some(Ok(e)) = events.next() {
        match e {
            XmlEvent::StartElement { attributes, .. } => {
                r.elements.push(parse_constant(attributes, events))
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

    for a in attributes {
        let name = a.name.local_name.as_str();
        match name {
            "name" => r.name = a.value,
            "type" => if a.value.as_str() == "bitmask" {
                r.purpose = Some(vkxml::EnumerationPurpose::Bitmask);
            } else {
                assert_eq!(a.value.as_str(), "enum");
            },
            "comment" => r.notation = Some(a.value),
            _ => panic!("Unexpected attribute {:?}", name),
        }
    }

    while let Some(Ok(e)) = events.next() {
        match e {
            XmlEvent::StartElement {
                name, attributes, ..
            } => {
                let name = name.local_name.as_str();
                let element = match name {
                    "enum" => vkxml::EnumerationElement::Enum(parse_constant(attributes, events)),
                    "comment" => vkxml::EnumerationElement::Notation(parse_text_element(events)),
                    "unused" => vkxml::EnumerationElement::UnusedRange(parse_enum_unused(
                        attributes,
                        events,
                    )),
                    _ => panic!("Unexpected element {:?}", name),
                };
                r.elements.push(element)
            }

            XmlEvent::EndElement { .. } => break,
            _ => (),
        }
    }

    r
}

fn parse_constant<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> vkxml::Constant {
    let mut r = vkxml::Constant {
        name: String::new(),
        notation: None,
        number: None,
        hex: None,
        bitpos: None,
        c_expression: None,
    };

    for a in attributes {
        let name = a.name.local_name.as_str();
        match name {
            "name" => r.name = a.value,
            "value" => {
                if let Ok(value) = i32::from_str_radix(&a.value, 10) {
                    r.number = Some(value);
                } else if a.value.starts_with("0x") {
                    r.hex = Some(String::from(a.value.split_at(2).1))
                } else {
                    r.c_expression = Some(a.value)
                }
            }

            "bitpos" => r.bitpos = Some(u32::from_str_radix(&a.value, 10).unwrap()),
            "comment" => r.notation = Some(a.value),
            _ => panic!("Unexpected attribute {:?}", name),
        }
    }

    consume_current_element(events);
    r
}

fn parse_enum_unused<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> vkxml::Range {
    let mut r = vkxml::Range {
        range_start: 0,
        range_end: None,
    };

    for a in attributes {
        let name = a.name.local_name.as_str();
        match name {
            "start" => r.range_start = i32::from_str_radix(&a.value, 10).unwrap(),
            "end" => r.range_end = Some(i32::from_str_radix(&a.value, 10).unwrap()),
            _ => panic!("Unexpected attribute {:?}", name),
        }
    }

    consume_current_element(events);
    r
}

//--------------------------------------------------------------------------------------------------
fn parse_commands<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> vkxml::Commands {
    let mut r = vkxml::Commands {
        notation: None,
        elements: Vec::new(),
    };

    for a in attributes {
        if a.name.local_name.as_str() == "comment" {
            r.notation = Some(a.value);
        }
    }

    while let Some(Ok(e)) = events.next() {
        match e {
            XmlEvent::StartElement {
                name, attributes, ..
            } => {
                if name.local_name.as_str() == "command" {
                    r.elements.push(parse_command(attributes, events));
                } else {
                    panic!("Unexpected element {:?}", name.local_name.as_str());
                }
            }
            XmlEvent::EndElement { .. } => break,
            _ => (),
        }
    }

    r
}

fn parse_command<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> vkxml::Command {
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

    while let Some(Ok(e)) = events.next() {
        match e {
            XmlEvent::StartElement {
                name, attributes, ..
            } => {
                let name = name.local_name.as_str();
                match name {
                    "proto" => {
                        let mut proto = parse_type_struct_member(attributes, events);
                        r.name = proto.name.take().unwrap();
                        r.return_type = proto;
                    }
                    "param" => r.param.push(parse_type_struct_member(attributes, events)),
                    "implicitexternsyncparams" => consume_current_element(events),
                    _ => panic!("Unexpected element {:?}", name),
                }
            }
            XmlEvent::EndElement { .. } => break,
            _ => (),
        }
    }

    for a in attributes {
        let name = a.name.local_name.as_str();
        match name {
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
            _ => panic!("Unexpected attribute {:?}", name),
        }
    }

    r
}

//--------------------------------------------------------------------------------------------------
fn parse_feature<R: Read>(
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

    for a in attributes {
        let name = a.name.local_name.as_str();
        match name {
            "api" => r.api = a.value,
            "name" => r.name = a.value,
            "comment" => r.notation = Some(a.value),
            "number" => {
                use std::str::FromStr;
                r.version = f32::from_str(&a.value).unwrap();
            }
            _ => panic!("Unexpected attribute {:?}", name),
        }
    }

    while let Some(Ok(e)) = events.next() {
        match e {
            XmlEvent::StartElement {
                name, attributes, ..
            } => {
                let name = name.local_name.as_str();
                match name {
                    "require" => r.elements.push(vkxml::FeatureElement::Require(
                        parse_feature_require(attributes, events),
                    )),
                    _ => panic!("Unexpected element {:?}", name),
                }
            }

            XmlEvent::EndElement { .. } => break,
            _ => (),
        }
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

    for a in attributes {
        let name = a.name.local_name.as_str();
        match name {
            "comment" => r.notation = Some(a.value),
            _ => panic!("Unexpected attribute {:?}", name),
        }
    }

    while let Some(Ok(e)) = events.next() {
        match e {
            XmlEvent::StartElement {
                name, attributes, ..
            } => {
                let name = name.local_name.as_str();
                match name {
                    "type" => r.elements
                        .push(vkxml::FeatureReference::DefinitionReference(
                            parse_feature_require_ref(attributes, events),
                        )),
                    "enum" => r.elements
                        .push(vkxml::FeatureReference::EnumeratorReference(
                            parse_feature_require_ref(attributes, events),
                        )),
                    "command" => r.elements.push(vkxml::FeatureReference::CommandReference(
                        parse_feature_require_ref(attributes, events),
                    )),
                    _ => panic!("Unexpected element {:?}", name),
                }
            }
            XmlEvent::EndElement { .. } => break,
            _ => (),
        }
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

    for a in attributes {
        let name = a.name.local_name.as_str();
        match name {
            "name" => r.name = a.value,
            _ => panic!("Unexpected attribute {:?}", name),
        }
    }

    consume_current_element(events);
    r
}

//--------------------------------------------------------------------------------------------------
fn parse_extensions<R: Read>(
    attributes: Vec<XmlAttribute>,
    events: &mut XmlEvents<R>,
) -> vkxml::Extensions {
    let mut r = vkxml::Extensions {
        notation: None,
        elements: Vec::new(),
    };

    for a in attributes {
        let name = a.name.local_name.as_str();
        match name {
            "comment" => r.notation = Some(a.value),
            _ => panic!("Unexpected attribute {:?}", name),
        }
    }

    while let Some(Ok(e)) = events.next() {
        match e {
            XmlEvent::StartElement {
                name, attributes, ..
            } => {
                let name = name.local_name.as_str();
                match name {
                    "extension" => r.elements.push(parse_extension(attributes, events)),
                    _ => panic!("Unexpected element {:?}", name),
                }
            }
            XmlEvent::EndElement { .. } => break,
            _ => (),
        }
    }

    r
}

fn parse_extension<R: Read>(
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

    for a in attributes {
        let name = a.name.local_name.as_str();
        match name {
            "name" => r.name = a.value,
            "number" => {
                use std::str::FromStr;
                r.number = i32::from_str(&a.value).unwrap();
            }
            "type" => {
                let ty = a.value.as_str();
                r.ty = Some(match ty {
                    "instance" => vkxml::ExtensionType::Instance,
                    "device" => vkxml::ExtensionType::Device,
                    _ => panic!("Unexpected attribute value {:?}", ty),
                });
            }
            "author" => r.author = Some(a.value),
            "contact" => r.contact = Some(a.value),
            "supported" => if a.value.as_str() == "disabled" {
                r.disabled = true;
            } else {
                r.match_api = Some(a.value);
            },
            "requires" => r.requires = Some(a.value),
            "protect" => r.define = Some(a.value),
            _ => panic!("Unexpected attribute {:?}", name),
        }
    }

    while let Some(Ok(e)) = events.next() {
        match e {
            XmlEvent::StartElement {
                name, attributes, ..
            } => {
                let name = name.local_name.as_str();
                match name {
                    "require" => r.elements.push(vkxml::ExtensionElement::Require(
                        parse_extension_require(attributes, events),
                    )),
                    _ => panic!("Unexpected element {:?}", name),
                }
            }
            XmlEvent::EndElement { .. } => break,
            _ => (),
        }
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

    for a in attributes {
        let name = a.name.local_name.as_str();
        match name {
            "extension" => r.extension = Some(a.value),
            _ => panic!("Unexpected attribute {:?}", name),
        }
    }

    while let Some(Ok(e)) = events.next() {
        match e {
            XmlEvent::StartElement {
                name, attributes, ..
            } => {
                let name = name.local_name.as_str();
                match name {
                    "comment" => {
                        r.elements
                            .push(vkxml::ExtensionSpecificationElement::Notation(
                                parse_text_element(events),
                            ));
                    }

                    "enum" => {
                        r.elements
                            .push(parse_extension_require_enum(attributes, events));
                    }

                    "command" => {
                        r.elements
                            .push(vkxml::ExtensionSpecificationElement::CommandReference(
                                parse_extension_require_ref(attributes, events),
                            ));
                    }

                    "type" => {
                        r.elements
                            .push(vkxml::ExtensionSpecificationElement::DefinitionReference(
                                parse_extension_require_ref(attributes, events),
                            ));
                    }

                    _ => panic!("Unexpected element {:?}", name),
                }
            }
            XmlEvent::EndElement { .. } => break,
            _ => (),
        }
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

    for a in attributes {
        let name = a.name.local_name.as_str();
        match name {
            "name" => r.name = a.value,
            _ => panic!("Unexpected attributes {:?}", name),
        }
    }

    consume_current_element(events);
    r
}

//--------------------------------------------------------------------------------------------------
fn consume_current_element<R: Read>(events: &mut XmlEvents<R>) {
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

fn parse_text_element<R: Read>(events: &mut XmlEvents<R>) -> String {
    let result = if let Some(Ok(XmlEvent::Characters(text))) = events.next() {
        text
    } else {
        String::new()
    };

    consume_current_element(events);
    result
}

//--------------------------------------------------------------------------------------------------
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
