extern crate vkxml;

use parse::*;
use std;
use types::*;

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
                            name: e.name.unwrap_or_default(),
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
            TypesChild::Type {
                definition,
                comment: notation,
            } => match *definition {
                TypeDefinition::None {
                    name,
                    requires: include,
                } => Some(vkxml::DefinitionsElement::Reference(vkxml::Reference {
                    name,
                    notation,
                    include,
                })),
                TypeDefinition::Include { name, quoted } => {
                    let need_ext = !name.ends_with(".h");
                    let include = vkxml::Include {
                        name,
                        notation,
                        style: match quoted {
                            Some(true) | None => vkxml::IncludeStyle::Quote,
                            Some(false) => vkxml::IncludeStyle::Bracket,
                        },
                        need_ext,
                    };

                    Some(vkxml::DefinitionsElement::Include(include))
                }

                TypeDefinition::Define(TypeDefine {
                    name,
                    value,
                    comment,
                    defref,
                    is_disabled,
                    replace,
                    ..
                }) => {
                    let mut define = vkxml::Define {
                        name,
                        notation,
                        is_disabled,
                        comment,
                        replace,
                        defref: defref.into_iter().collect(),
                        parameters: Vec::new(),
                        c_expression: None,
                        value: None,
                    };
                    match value {
                        TypeDefineValue::Empty => {}
                        TypeDefineValue::Value(v) => {
                            define.value = Some(v);
                        }
                        TypeDefineValue::Expression(e) => {
                            define.c_expression = Some(e);
                        }
                        TypeDefineValue::Function { params, expression } => {
                            define.parameters = params;
                            define.c_expression = Some(expression);
                        }
                    }
                    Some(vkxml::DefinitionsElement::Define(define))
                }

                TypeDefinition::Typedef { name, basetype } => {
                    let typedef = vkxml::Typedef {
                        name,
                        notation,
                        basetype: basetype.unwrap_or_default(),
                    };
                    Some(vkxml::DefinitionsElement::Typedef(typedef))
                }

                TypeDefinition::Bitmask(TypeBitmask::Alias { .. }) => None,
                TypeDefinition::Bitmask(TypeBitmask::Definition {
                    definition,
                    has_bitvalues,
                }) => {
                    let NameWithType {
                        type_name, name, ..
                    } = *definition;
                    let enumref = if has_bitvalues {
                        Some(name.replacen("Flags", "FlagBits", 1))
                    } else {
                        None
                    };
                    let bitmask = vkxml::Bitmask {
                        name,
                        notation,
                        basetype: type_name,
                        enumref,
                    };
                    Some(vkxml::DefinitionsElement::Bitmask(bitmask))
                }
                TypeDefinition::Handle(TypeHandle::Alias { .. }) => None,
                TypeDefinition::Handle(TypeHandle::Definition {
                    name,
                    handle_type,
                    parent,
                    ..
                }) => {
                    let handle = vkxml::Handle {
                        name,
                        notation,
                        parent,
                        ty: match handle_type {
                            HandleType::Dispatch => vkxml::HandleType::Dispatch,
                            HandleType::NoDispatch => vkxml::HandleType::NoDispatch,
                        },
                    };
                    Some(vkxml::DefinitionsElement::Handle(handle))
                }

                TypeDefinition::Enumeration { alias: Some(_), .. } => None,
                TypeDefinition::Enumeration { name, alias: None } => {
                    Some(vkxml::DefinitionsElement::Enumeration(
                        vkxml::EnumerationDeclaration { name, notation },
                    ))
                }
                TypeDefinition::FunctionPointer(TypeFunctionPointer {
                    proto: defn,
                    params,
                    ..
                }) => Some(vkxml::DefinitionsElement::FuncPtr(vkxml::FunctionPointer {
                    name: defn.name.clone(),
                    notation,
                    return_type: defn.into(),
                    param: params.into_iter().map(|p| p.into()).collect(),
                })),
                TypeDefinition::Struct(TypeStruct::Alias { .. }) => None,
                TypeDefinition::Struct(TypeStruct::Definition {
                    name,
                    members,
                    returned_only,
                    struct_extends,
                    ..
                }) => {
                    let mut s = vkxml::Struct {
                        name,
                        notation,
                        is_return: returned_only.is_some(),
                        extends: if struct_extends.is_empty() {
                            None
                        } else {
                            Some(struct_extends.join(","))
                        },
                        elements: Vec::new(),
                    };
                    for member in members {
                        s.elements.push(member.into());
                    }

                    Some(vkxml::DefinitionsElement::Struct(s))
                }

                TypeDefinition::Union(TypeUnion { name, members, .. }) => {
                    let mut u = vkxml::Union {
                        name,
                        notation,
                        elements: Vec::new(),
                    };
                    for member in members {
                        match member {
                            TypeMember::Comment(..) => (),
                            TypeMember::Definition(def) => {
                                u.elements.push(def.definition.into());
                            }
                        }
                    }

                    Some(vkxml::DefinitionsElement::Union(u))
                }
            },
        }
    }
}

impl From<TypeMember> for vkxml::StructElement {
    fn from(orig: TypeMember) -> Self {
        match orig {
            TypeMember::Comment(comment) => vkxml::StructElement::Notation(comment),
            TypeMember::Definition(def) => {
                let mut field: vkxml::Field = def.definition.into();
                field.type_enums = def.values;

                vkxml::StructElement::Member(field)
            }
        }
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
                Option::<vkxml::Constant>::from(e).map(vkxml::EnumerationElement::Enum)
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
                if let Ok(value) = value.parse::<i32>() {
                    r.number = Some(value);
                } else if let Some(value) = value.strip_prefix("0x") {
                    r.hex = Some(String::from(value))
                } else {
                    r.c_expression = Some(value)
                }
                Some(r)
            }
            _ => None,
        }
    }
}

impl From<NameWithType> for vkxml::Field {
    fn from(nt: NameWithType) -> Self {
        let NameWithType {
            type_name,
            pointer_kind,
            is_struct,
            bitfield_size: _,
            array_shape,
            name,
            dynamic_shape,
            externsync,
            optional,
            noautovalidity,
            objecttype: _,
            comment,
        } = nt;
        vkxml::Field {
            array: if dynamic_shape.is_some() {
                Some(vkxml::ArrayType::Dynamic)
            } else if array_shape.is_some() {
                Some(vkxml::ArrayType::Static)
            } else {
                None
            },
            auto_validity: noautovalidity.is_none(),
            basetype: type_name,
            c_size: match &dynamic_shape {
                Some(DynamicShapeKind::Expression {
                    latex_expr: _,
                    c_expr,
                }) => Some(c_expr.to_string()),
                _ => None,
            },
            errorcodes: None,
            is_const: matches!(
                pointer_kind,
                Some(
                    PointerKind::Single { is_const: true }
                        | PointerKind::Double { is_const: true, .. }
                )
            ),
            is_struct,
            sync: externsync.map(|es| match es {
                ExternSyncKind::Value => "true".to_string(),
                ExternSyncKind::Fields(fs) if dynamic_shape.is_some() => fs
                    .iter()
                    .map(|s| format!("{}[].{}", name, s))
                    .collect::<Vec<_>>()
                    .join(","),
                ExternSyncKind::Fields(fs) => fs
                    .iter()
                    .map(|s| format!("{}->{}", name, s))
                    .collect::<Vec<_>>()
                    .join(","),
            }),
            name: Some(name),
            notation: comment,
            null_terminate: matches!(
                dynamic_shape,
                Some(
                    DynamicShapeKind::Single(DynamicLength::NullTerminated)
                        | DynamicShapeKind::Double(DynamicLength::NullTerminated, _)
                        | DynamicShapeKind::Double(_, DynamicLength::NullTerminated)
                )
            ),
            optional: optional.map(|opt| match opt {
                OptionalKind::Single(outer) => outer.to_string(),
                OptionalKind::Double(outer, inner) => format!("{},{}", outer, inner),
            }),
            reference: pointer_kind.map(|kind| match kind {
                PointerKind::Single { .. } => vkxml::ReferenceType::Pointer,
                PointerKind::Double {
                    inner_is_const: true,
                    ..
                } => vkxml::ReferenceType::PointerToConstPointer,
                PointerKind::Double {
                    inner_is_const: false,
                    ..
                } => vkxml::ReferenceType::PointerToPointer,
            }),
            size: match (&dynamic_shape, array_shape.as_deref()) {
                (
                    Some(
                        DynamicShapeKind::Single(DynamicLength::Parameterized(p))
                        | DynamicShapeKind::Double(DynamicLength::Parameterized(p), _),
                    ),
                    _,
                ) => Some(p.to_string()),
                (None, Some([ArrayLength::Static(n), ..])) => Some(n.to_string()),
                _ => None,
            },
            size_enumref: match array_shape.as_deref() {
                Some([ArrayLength::Constant(c), ..]) => Some(c.to_string()),
                _ => None,
            },
            successcodes: None,
            type_enums: None,
        }
    }
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
    fn from(orig: Extension) -> Self {
        let Extension {
            name,
            comment,
            number,
            protect,
            platform: _,
            author,
            contact,
            ext_type,
            requires,
            requires_core: _,
            supported,
            deprecatedby: _,
            promotedto: _,
            obsoletedby: _,
            provisional: _,
            specialuse: _,
            sortorder: _,
            children,
        } = orig;
        let mut disabled = false;
        let mut match_api = None;

        if let Some(text) = supported {
            if text == "disabled" {
                disabled = true;
            } else {
                match_api = Some(text);
            }
        }

        let mut elements = Vec::new();
        for item in children {
            elements.push(item.into());
        }

        vkxml::Extension {
            name,
            notation: comment,
            number: match number {
                Some(val) => val as i32,
                None => 0,
            },
            disabled,
            match_api,
            ty: match ext_type {
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
            define: protect,
            requires,
            author,
            contact,
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
                    if let Ok(val) = value.parse::<i32>() {
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
                r.return_type.basetype = def.proto.type_name;
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
                    r.param.push(param.definition.into());
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
