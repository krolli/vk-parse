extern crate vkxml;

use c;
use std;
use parse::*;
use types::*;
use vk::parse::*;
use vk::types::*;

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
pub fn parse_file_as_vkxml(path: &std::path::Path) -> Result<vkxml::Registry, FatalError> {
    parse_file(path).map(|(reg, _)| reg.into())
}

/// Parses data from stream which must be the Vulkan registry XML in its standard format.
///
/// Returns a Rust representation of the registry.
pub fn parse_stream_as_vkxml<T: std::io::Read>(stream: T) -> Result<vkxml::Registry, FatalError> {
    parse_stream(stream).map(|(reg, _)| reg.into())
}

impl From<Registry> for vkxml::Registry {
    fn from(orig: Registry) -> vkxml::Registry {
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

        for item in orig.0 {
            match item {
                RegistryChild::Comment(comment) => {
                    if let Some(ref mut enums) = enums {
                        enums.elements.push(vkxml::EnumsElement::Notation(comment));
                    } else {
                        registry
                            .elements
                            .push(vkxml::RegistryElement::Notation(comment));
                    }
                }

                RegistryChild::VendorIds(ids) => {
                    flush_enums(&mut enums, &mut registry.elements);
                    registry
                        .elements
                        .push(vkxml::RegistryElement::VendorIds(ids.into()));
                }

                RegistryChild::Tags(tags) => {
                    flush_enums(&mut enums, &mut registry.elements);
                    registry
                        .elements
                        .push(vkxml::RegistryElement::Tags(tags.into()));
                }

                RegistryChild::Types(types) => {
                    flush_enums(&mut enums, &mut registry.elements);
                    registry
                        .elements
                        .push(vkxml::RegistryElement::Definitions(types.into()));
                }

                RegistryChild::Enums(e) => match e.kind {
                    None => {
                        flush_enums(&mut enums, &mut registry.elements);
                        let mut constants = vkxml::Constants {
                            notation: e.comment,
                            elements: Vec::with_capacity(e.children.len()),
                        };
                        for item in e.children {
                            if let Some(e) = item.into() {
                                constants.elements.push(e);
                            }
                        }
                        registry
                            .elements
                            .push(vkxml::RegistryElement::Constants(constants));
                    }
                    Some(kind) => {
                        let enumeration = vkxml::Enumeration {
                            name: e.name.unwrap_or(String::new()),
                            notation: e.comment,
                            purpose: if kind.as_str() == "bitmask" {
                                Some(vkxml::EnumerationPurpose::Bitmask)
                            } else {
                                None
                            },
                            elements: {
                                let mut elements = Vec::with_capacity(e.children.len());
                                for item in e.children {
                                    if let Some(val) = item.into() {
                                        elements.push(val);
                                    }
                                }
                                elements
                            },
                        };
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

                RegistryChild::Commands(commands) => {
                    flush_enums(&mut enums, &mut registry.elements);
                    registry
                        .elements
                        .push(vkxml::RegistryElement::Commands(commands.into()));
                }

                RegistryChild::Feature(f) => {
                    flush_enums(&mut enums, &mut registry.elements);
                    registry
                        .elements
                        .push(vkxml::RegistryElement::Features(vkxml::Features {
                            elements: vec![f.into()],
                        }));
                }

                RegistryChild::Extensions(e) => {
                    flush_enums(&mut enums, &mut registry.elements);
                    registry
                        .elements
                        .push(vkxml::RegistryElement::Extensions(e.into()));
                }

                RegistryChild::Platforms { .. } => (),
                RegistryChild::Formats { .. } => (),
                RegistryChild::SpirvExtensions { .. } => (),
                RegistryChild::SpirvCapabilities { .. } => (),
            }
        }

        registry
    }
}

impl From<TypesChild> for Option<vkxml::DefinitionsElement> {
    fn from(orig: TypesChild) -> Self {
        match orig {
            TypesChild::Comment(text) => Some(vkxml::DefinitionsElement::Notation(text)),
            TypesChild::Type(t) => {
                let category = match t.category {
                    Some(c) => c,
                    None => {
                        let name = t.name.unwrap_or(String::new());
                        return Some(vkxml::DefinitionsElement::Reference(vkxml::Reference {
                            name,
                            notation: t.comment,
                            include: t.requires,
                        }));
                    }
                };

                match category.as_str() {
                    "include" => {
                        let mut include = vkxml::Include {
                            name: t.name.unwrap_or(String::new()),
                            notation: t.comment,
                            style: vkxml::IncludeStyle::Quote,
                            need_ext: false,
                        };

                        match t.spec {
                            TypeSpec::Code(TypeCode { code, markup }) => {
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
                            name: t.name.unwrap_or(String::new()),
                            notation: t.comment,
                            is_disabled: true,
                            comment: None,
                            replace: false,
                            defref: Vec::new(),
                            parameters: Vec::new(),
                            c_expression: None,
                            value: None,
                        };
                        match t.spec {
                            TypeSpec::Code(TypeCode { code, markup }) => {
                                for tag in markup {
                                    match tag {
                                        TypeCodeMarkup::Type(val) => define.defref.push(val),
                                        TypeCodeMarkup::Name(val) => define.name = val,
                                        _ => panic!("Unexpected tag in define {:?}", tag),
                                    }
                                }
                                process_define_code(&mut define, code);
                            }
                            _ => panic!("Unexpected contents of define {:?}", t.spec),
                        }
                        return Some(vkxml::DefinitionsElement::Define(define));
                    }

                    "basetype" => {
                        let mut typedef = vkxml::Typedef {
                            name: String::new(),
                            notation: t.comment,
                            basetype: String::new(),
                        };
                        let markup = match t.spec {
                            TypeSpec::Code(TypeCode { markup, .. }) => markup,
                            _ => panic!("Unexpected contents of typedef {:?}", t.spec),
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
                        if t.name.is_some() || t.alias.is_some() {
                            return None;
                        }
                        let mut bitmask = vkxml::Bitmask {
                            name: vkxml::Identifier::new(),
                            notation: t.comment,
                            basetype: vkxml::Identifier::new(),
                            enumref: t.requires,
                        };
                        let markup = match t.spec {
                            TypeSpec::Code(TypeCode { markup, .. }) => markup,
                            _ => panic!("Unexpected contents of bitmaks {:?}", t.spec),
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
                        if t.name.is_some() || t.alias.is_some() {
                            return None;
                        }
                        let mut handle = vkxml::Handle {
                            name: String::new(),
                            notation: t.comment,
                            parent: t.parent,
                            ty: vkxml::HandleType::Dispatch,
                        };
                        let markup = match t.spec {
                            TypeSpec::Code(TypeCode { markup, .. }) => markup,
                            _ => panic!("Unexpected contents of handle {:?}", t.spec),
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
                                name: t.name.unwrap_or(String::new()),
                                notation: t.comment,
                            },
                        ));
                    }

                    "funcpointer" => {
                        let mut fnptr = vkxml::FunctionPointer {
                            name: vkxml::Identifier::new(),
                            notation: t.comment,
                            return_type: new_field(),
                            param: Vec::new(),
                        };
                        let code = match t.spec {
                            TypeSpec::Code(TypeCode { code, .. }) => code,
                            _ => panic!("Unexpected contents of handle {:?}", t.spec),
                        };

                        parse_type_funcptr(&mut fnptr, &code);
                        return Some(vkxml::DefinitionsElement::FuncPtr(fnptr));
                    }

                    "struct" => {
                        if t.alias.is_some() {
                            return None;
                        }
                        let mut s = vkxml::Struct {
                            name: t.name.unwrap_or(String::new()),
                            notation: t.comment,
                            is_return: t.returnedonly.unwrap_or(String::new()).as_str() == "true",
                            extends: t.structextends,
                            elements: Vec::new(),
                        };
                        match t.spec {
                            TypeSpec::Members(members) => {
                                for member in members {
                                    s.elements.push(member.into());
                                }
                            }
                            _ => panic!("Unexpected contents of struct {:?}: {:?}", s.name, t.spec),
                        }

                        return Some(vkxml::DefinitionsElement::Struct(s));
                    }

                    "union" => {
                        let mut u = vkxml::Union {
                            name: t.name.unwrap_or(String::new()),
                            notation: t.comment,
                            elements: Vec::new(),
                        };
                        match t.spec {
                            TypeSpec::Members(members) => {
                                for member in members {
                                    match member {
                                        TypeMember::Comment(..) => (),
                                        TypeMember::Definition(def) => {
                                            let mut iter = def
                                                .code
                                                .split_whitespace()
                                                .flat_map(|s| c::TokenIter::new(s))
                                                .peekable();

                                            let field = parse_c_field(&mut iter).unwrap();
                                            u.elements.push(field);
                                        }
                                    }
                                }
                            }
                            _ => panic!("Unexpected contents of union {:?}: {:?}", u.name, t.spec),
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
            TypeMember::Definition(def) => {
                let mut iter = def
                    .code
                    .split_whitespace()
                    .flat_map(|s| c::TokenIter::new(s))
                    .peekable();

                let mut field = parse_c_field(&mut iter).unwrap();
                field.c_size = def.altlen;
                field.sync = def.externsync;
                field.optional = def.optional;
                field.type_enums = def.values;
                match def.len {
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
                for tag in def.markup {
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
                        Some(c) => {
                            if c.is_whitespace() {
                                state = State::DefineValue;
                            } else {
                                panic!("Unexpected char after #define name: {:?}", c);
                            }
                        }
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

impl From<EnumsChild> for Option<vkxml::Constant> {
    fn from(orig: EnumsChild) -> Self {
        match orig {
            EnumsChild::Enum(e) => e.into(),
            _ => None,
        }
    }
}

impl From<EnumsChild> for Option<vkxml::EnumerationElement> {
    fn from(orig: EnumsChild) -> Self {
        match orig {
            EnumsChild::Enum(e) => {
                if let Some(constant) = e.into() {
                    Some(vkxml::EnumerationElement::Enum(constant))
                } else {
                    None
                }
            }
            EnumsChild::Unused(unused) => {
                Some(vkxml::EnumerationElement::UnusedRange(vkxml::Range {
                    range_start: unused.start as i32,
                    range_end: unused.end.map(|v| v as i32),
                }))
            }
            EnumsChild::Comment(comment) => Some(vkxml::EnumerationElement::Notation(comment)),
        }
    }
}

impl From<Enum> for Option<vkxml::Constant> {
    fn from(orig: Enum) -> Self {
        match orig.spec {
            EnumSpec::Bitpos { bitpos, .. } => Some(vkxml::Constant {
                name: orig.name,
                notation: orig.comment,
                number: None,
                hex: None,
                bitpos: Some(bitpos as u32),
                c_expression: None,
            }),
            EnumSpec::Value { value, .. } => {
                let mut r = vkxml::Constant {
                    name: orig.name,
                    notation: orig.comment,
                    number: None,
                    hex: None,
                    bitpos: None,
                    c_expression: None,
                };
                if let Ok(value) = i32::from_str_radix(&value, 10) {
                    r.number = Some(value);
                } else if value.starts_with("0x") {
                    r.hex = Some(String::from(value.split_at(2).1))
                } else {
                    r.c_expression = Some(value)
                }
                Some(r)
            }
            _ => None,
        }
    }
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
            t => {
                if r.array.is_some() {
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
                }
            }
        }
        iter.next().unwrap();
    }

    Some(r)
}

//--------------------------------------------------------------------------------------------------
fn items_to_elements<T, U: From<T>>(items: Vec<T>) -> Vec<U> {
    let mut elements = Vec::with_capacity(items.len());
    for item in items {
        elements.push(item.into());
    }
    elements
}

impl From<VendorIds> for vkxml::VendorIds {
    fn from(orig: VendorIds) -> Self {
        Self {
            notation: orig.comment,
            elements: items_to_elements(orig.children),
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

impl From<Tags> for vkxml::Tags {
    fn from(orig: Tags) -> Self {
        Self {
            notation: orig.comment,
            elements: items_to_elements(orig.children),
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

impl From<Types> for vkxml::Definitions {
    fn from(orig: Types) -> Self {
        Self {
            notation: orig.comment,
            elements: {
                let mut elements = Vec::with_capacity(orig.children.len());
                for item in orig.children {
                    if let Some(t) = item.into() {
                        elements.push(t);
                    }
                }
                elements
            },
        }
    }
}

impl From<Extensions> for vkxml::Extensions {
    fn from(orig: Extensions) -> Self {
        Self {
            notation: orig.comment,
            elements: items_to_elements(orig.children),
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
        for item in orig.children {
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

impl From<ExtensionChild> for vkxml::ExtensionElement {
    fn from(orig: ExtensionChild) -> Self {
        match orig {
            ExtensionChild::Remove {
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

            ExtensionChild::Require {
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

impl From<ExtensionChild> for Option<vkxml::FeatureSpecification> {
    fn from(orig: ExtensionChild) -> Self {
        match orig {
            ExtensionChild::Require {
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
            ExtensionChild::Remove { .. } => None,
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

impl From<Commands> for vkxml::Commands {
    fn from(orig: Commands) -> Self {
        Self {
            notation: orig.comment,
            elements: {
                let mut elements = Vec::with_capacity(orig.children.len());
                for item in orig.children {
                    if let Some(cmd) = item.into() {
                        elements.push(cmd);
                    }
                }
                elements
            },
        }
    }
}

impl From<Command> for Option<vkxml::Command> {
    fn from(orig: Command) -> Self {
        match orig {
            Command::Alias { .. } => None,
            Command::Definition(def) => {
                let mut r = vkxml::Command {
                    name: def.proto.name,
                    notation: def.comment,
                    return_type: new_field(),
                    param: Vec::new(),
                    external_sync: None,
                    cmdbufferlevel: def.cmdbufferlevel,
                    pipeline: None,
                    queues: def.queues,
                    renderpass: None,
                };
                match def.proto.type_name {
                    Some(type_name) => r.return_type.basetype = type_name,
                    None => (),
                }
                r.return_type.successcodes = def.successcodes;
                r.return_type.errorcodes = def.errorcodes;
                for text in def.implicitexternsyncparams {
                    r.external_sync = Some(vkxml::ExternalSync { sync: text })
                }
                if let Some(renderpass) = def.renderpass {
                    r.renderpass = match renderpass.as_str() {
                        "both" => Some(vkxml::Renderpass::Both),
                        "inside" => Some(vkxml::Renderpass::Inside),
                        "outside" => Some(vkxml::Renderpass::Outside),
                        _ => panic!("Unexpected renderpass value {:?}", renderpass),
                    };
                }
                if let Some(pipeline) = def.pipeline {
                    r.pipeline = match pipeline.as_str() {
                        "graphics" => Some(vkxml::Pipeline::Graphics),
                        "compute" => Some(vkxml::Pipeline::Compute),
                        "transfer" => Some(vkxml::Pipeline::Transfer),
                        _ => panic!("Unexpected pipeline value {:?}", pipeline),
                    };
                }

                r.param.reserve(def.params.len());
                for param in def.params {
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

                let mut tokens = c::TokenIter::new(&def.code);
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
                        t => {
                            if is_array {
                                array_size.push_str(t);
                            }
                        }
                    }
                }

                Some(r)
            }
        }
    }
}

impl From<Feature> for vkxml::Feature {
    fn from(orig: Feature) -> Self {
        use std::str::FromStr;
        Self {
            name: orig.name,
            notation: orig.comment,
            api: orig.api,
            version: f32::from_str(&orig.number).unwrap(),
            define: orig.protect,
            elements: {
                let mut elements = Vec::with_capacity(orig.children.len());
                for item in orig.children {
                    if let Some(v) = item.into() {
                        elements.push(vkxml::FeatureElement::Require(v));
                    }
                }
                elements
            },
        }
    }
}
