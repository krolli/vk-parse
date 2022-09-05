#![allow(non_snake_case)]

/// Errors from which parser cannot recover.
#[derive(Debug)]
#[non_exhaustive]
pub enum FatalError {
    MissingRegistryElement,
    IoError(std::io::Error),
}

impl From<std::io::Error> for FatalError {
    fn from(v: std::io::Error) -> FatalError {
        FatalError::IoError(v)
    }
}

/// Errors from which parser can recover. How much information will be missing
/// in the resulting Registry depends on the type of error and situation in
/// which it occurs. For example, unrecognized attribute will simply be skipped
/// without affecting anything around it, while unrecognized element will have
/// all of its contents skipped.
#[derive(Debug, PartialEq, Eq)]
#[non_exhaustive]
pub enum Error {
    UnexpectedElement {
        xpath: String,
        name: String,
    },
    UnexpectedAttribute {
        xpath: String,
        name: String,
    }, // "Missing attribute '{}' on element '{}'."
    UnexpectedAttributeValue {
        xpath: String,
        name: String,
        value: String,
    },
    MissingElement {
        xpath: String,
        name: String,
    },
    MissingCharacters {
        xpath: String,
    },
    MissingAttribute {
        xpath: String,
        name: String,
    },
    SchemaViolation {
        xpath: String,
        desc: String,
    },
    ParseIntError {
        xpath: String,
        text: String,
        error: std::num::ParseIntError,
    },
    Internal {
        desc: &'static str,
    },
}

/// Rust structure representing the Vulkan registry.
///
/// The registry contains all the information contained in a certain version
/// of the Vulkan specification, stored within a programmer-accessible format.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
pub struct Registry(pub Vec<RegistryChild>);

/// An element of the Vulkan registry.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub enum RegistryChild {
    /// Comments are human-readable strings which contain registry meta-data.
    Comment(String),

    /// IDs of all known Vulkan vendors.
    VendorIds(VendorIds),

    /// List of supported Vulkan platforms.
    Platforms(Platforms),

    /// Known extension tags.
    Tags(Tags),

    /// Type definitions.
    ///
    /// Unlike OpenGL, Vulkan is a strongly-typed API.
    Types(Types),

    /// Enum definitions.
    Enums(Enums),

    /// Commands are the Vulkan API's name for functions.
    Commands(Commands),

    /// Feature level of the API, such as Vulkan 1.0 or 1.1
    Feature(Feature),

    /// Container for all published Vulkan specification extensions.
    Extensions(Extensions),

    Formats(Formats),

    SpirvExtensions(SpirvExtensions),

    SpirvCapabilities(SpirvCapabilities),
}

pub type VendorIds = CommentedChildren<VendorId>;

/// Unique identifier for a Vulkan vendor.
///
/// Note: in newer versions of the Vulkan spec (1.1.79 and later), this tag is
/// not used, instead it has been replaced by the `VKVendorId` enum.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub struct VendorId {
    /// Name of the vendor.
    pub name: String,

    /// The unique ID.
    pub id: u32,

    /// Human-readable description.
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub comment: Option<String>,
}

pub type Platforms = CommentedChildren<Platform>;

/// A platform refers to a windowing system which Vulkan can use.
///
/// Most operating systems will have only one corresponding platform,
/// but Linux has multiple (XCB, Wayland, etc.)
///
/// Used in versions 1.1.70 and later.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub struct Platform {
    /// Short identifier.
    pub name: String,

    /// C macro name which is used to guard platform-specific definitions.
    pub protect: String,

    /// Human readable description of the platform.
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub comment: Option<String>,
}

pub type Tags = CommentedChildren<Tag>;

/// Tags are the little suffixes attached to extension names or items, indicating the author.
///
/// Some examples:
/// - KHR for Khronos extensions
/// - EXT for multi-vendor extensions
#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub struct Tag {
    /// The name of the tag, e.g. "KHR".
    pub name: String,
    /// Author of the extensions associated with the tag, e.g. "Khronos".
    pub author: String,
    /// Contact information for the extension author(s).
    pub contact: String,
}

pub type Types = CommentedChildren<TypesChild>;

/// An item making up a type definition.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub enum TypesChild {
    Type {
        definition: Box<TypeDefinition>,
        comment: Option<String>,
    },
    Comment(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub enum TypeDefinition {
    /// When the `category` attribute is missing
    None {
        name: String,
        #[cfg_attr(
            feature = "serialize",
            serde(default, skip_serializing_if = "is_default")
        )]
        requires: Option<String>,
    },
    Include {
        name: String,
        quoted: Option<bool>,
    },
    Define(TypeDefine),
    Typedef {
        name: String,
        // FIXME doesn't include all of the necessary information to recreate
        basetype: Option<String>,
    },
    Bitmask(TypeBitmask),
    Handle(TypeHandle),
    Enumeration {
        name: String,
        #[cfg_attr(
            feature = "serialize",
            serde(default, skip_serializing_if = "is_default")
        )]
        alias: Option<String>,
    },
    FunctionPointer(TypeFunctionPointer),
    Struct(TypeStruct),
    Union(TypeUnion),
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub enum TypeBitmask {
    Alias {
        name: String,
        alias: String,
    },

    Definition {
        definition: Box<NameWithType>,
        has_bitvalues: bool,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub(crate) enum TypeCodeMarkup {
    Name(String),
    Type(String),
    ApiEntry(String),
}

/// A member of a type definition, i.e. a struct member.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub enum TypeMember {
    /// Human-readable comment.
    Comment(String),

    /// A structure field definition.
    Definition(Box<TypeMemberDefinition>),
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub struct TypeMemberDefinition {
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub selector: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub selection: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub validextensionstructs: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub values: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub limittype: Option<String>,

    /// The definition of this member.
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub definition: NameWithType,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub struct TypeFunctionPointer {
    pub proto: NameWithType,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub params: Vec<NameWithType>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub requires: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub enum TypeHandle {
    Alias {
        name: String,
        alias: String,
    },

    Definition {
        name: String,
        handle_type: HandleType,
        objtypeenum: String,
        #[cfg_attr(
            feature = "serialize",
            serde(default, skip_serializing_if = "is_default")
        )]
        parent: Option<String>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub enum HandleType {
    Dispatch,
    NoDispatch,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub struct TypeDefine {
    pub name: String,
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub comment: Option<String>,
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub defref: Option<String>,
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub requires: Option<String>,
    pub is_disabled: bool,
    pub replace: bool,
    pub value: TypeDefineValue,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub enum TypeDefineValue {
    Empty,
    Value(String),
    Expression(String),
    Function {
        params: Vec<String>,
        expression: String,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub enum TypeStruct {
    Alias {
        name: String,
        alias: String,
    },

    Definition {
        name: String,
        members: Vec<TypeMember>,
        #[cfg_attr(
            feature = "serialize",
            serde(default, skip_serializing_if = "is_default")
        )]
        returned_only: Option<()>,
        #[cfg_attr(
            feature = "serialize",
            serde(default, skip_serializing_if = "is_default")
        )]
        struct_extends: Vec<String>,
        #[cfg_attr(
            feature = "serialize",
            serde(default, skip_serializing_if = "is_default")
        )]
        allow_duplicate: Option<bool>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub struct TypeUnion {
    pub name: String,
    pub members: Vec<TypeMember>,
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub returned_only: Option<()>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub struct Enums {
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub name: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub kind: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub start: Option<i64>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub end: Option<i64>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub vendor: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub comment: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub children: Vec<EnumsChild>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub bitwidth: Option<u32>,
}

/// An item which forms an enum.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub enum EnumsChild {
    /// Actual named enum.
    Enum(Enum),

    /// An unused range of enum values.
    Unused(Unused),

    /// Human-readable comment.
    Comment(String),
}

/// An unused range of enum values.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub struct Unused {
    /// Beginning of the range.
    pub start: i64,

    /// Ending value of the range, if any.
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub end: Option<i64>,

    /// Vendor who reserved this range.
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub vendor: Option<String>,

    /// Human-readable description.
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub comment: Option<String>,
}

/// An item of an enumeration type.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub struct Enum {
    /// Name of this enum.
    pub name: String,

    /// Human-readable description.
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub comment: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub type_suffix: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub api: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub protect: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub spec: EnumSpec,
}

/// An enum specifier, which assigns a value to the enum.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub enum EnumSpec {
    None,

    Alias {
        alias: String,

        #[cfg_attr(
            feature = "serialize",
            serde(default, skip_serializing_if = "is_default")
        )]
        extends: Option<String>,
    },

    Offset {
        offset: i64,

        #[cfg_attr(
            feature = "serialize",
            serde(default, skip_serializing_if = "is_default")
        )]
        extends: String,

        #[cfg_attr(
            feature = "serialize",
            serde(default, skip_serializing_if = "is_default")
        )]
        extnumber: Option<i64>,

        #[cfg_attr(
            feature = "serialize",
            serde(default, skip_serializing_if = "is_default")
        )]
        dir: bool,
    },

    /// Indicates an enum which is a bit flag.
    Bitpos {
        /// The bit to be set.
        bitpos: i64,

        /// Which structure this enum extends.
        #[cfg_attr(
            feature = "serialize",
            serde(default, skip_serializing_if = "is_default")
        )]
        extends: Option<String>,
    },

    /// An enum value.
    Value {
        /// Hard coded value for an enum.
        value: String, // rnc says this is an Integer, but validates it as text, and that's what it sometimes really is.

        /// Which structure this enum extends.
        #[cfg_attr(
            feature = "serialize",
            serde(default, skip_serializing_if = "is_default")
        )]
        extends: Option<String>,
    },
}

impl Default for EnumSpec {
    fn default() -> Self {
        EnumSpec::None
    }
}

pub type Commands = CommentedChildren<Command>;

/// A command is just a Vulkan function.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub enum Command {
    /// Indicates this function is an alias for another one.
    Alias { name: String, alias: String },

    /// Defines a new Vulkan function.
    Definition(Box<CommandDefinition>),
}

bitflags::bitflags! {
    #[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
    pub struct CommandQueue: u32 {
        const GRAPHICS = 1 << 0;
        const COMPUTE = 1 << 1;
        const TRANSFER = 1 << 2;
        const SPARSE_BINDING = 1 << 3;
        const PROTECTED = 1 << 4;
        const VIDEO_DECODE = 1 << 5;
        const VIDEO_ENCODE = 1 << 6;
    }
}

impl core::fmt::Display for CommandQueue {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let mut prepend_comma = false;
        if self.contains(CommandQueue::GRAPHICS) {
            f.write_str("graphics")?;
            prepend_comma = true;
        }
        if self.contains(CommandQueue::COMPUTE) {
            if prepend_comma {
                f.write_str(",")?;
            }
            f.write_str("compute")?;
            prepend_comma = true;
        }
        if self.contains(CommandQueue::TRANSFER) {
            if prepend_comma {
                f.write_str(",")?;
            }
            f.write_str("transfer")?;
            prepend_comma = true;
        }
        if self.contains(CommandQueue::SPARSE_BINDING) {
            if prepend_comma {
                f.write_str(",")?;
            }
            f.write_str("sparse_binding")?;
            prepend_comma = true;
        }
        // this isn't found in the
        if self.contains(CommandQueue::PROTECTED) {
            if prepend_comma {
                f.write_str(",")?;
            }
            f.write_str("protected")?;
            prepend_comma = true;
        }
        if self.contains(CommandQueue::VIDEO_DECODE) {
            if prepend_comma {
                f.write_str(",")?;
            }
            f.write_str("decode")?;
            prepend_comma = true;
        }
        if self.contains(CommandQueue::VIDEO_ENCODE) {
            if prepend_comma {
                f.write_str(",")?;
            }
            f.write_str("encode")?;
            // prepend_comma = true;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub enum CommandSuccessCodes {
    /// `successcodes="VK_SUCCESS"`
    DefaultSuccess,
    Codes(Vec<String>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub enum CommandRenderpass {
    Inside,
    Outside,
    Both,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub enum CommandVideoCoding {
    Inside,
    Outside,
    Both,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub enum CommandBufferLevel {
    PrimaryOnly,
    Both,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub struct CommandDefinition {
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub queues: Option<CommandQueue>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub successcodes: Option<CommandSuccessCodes>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub errorcodes: Option<Vec<String>>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub renderpass: Option<CommandRenderpass>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub videocoding: Option<CommandVideoCoding>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub cmdbufferlevel: Option<CommandBufferLevel>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub comment: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub proto: NameWithType,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub params: Vec<CommandParam>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub alias: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub description: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub implicitexternsyncparams: Vec<String>,
}

/// Parameter for this Vulkan function.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub struct CommandParam {
    /// The definition of this parameter.
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub definition: NameWithType,

    /// only applicable for parameters which are pointers to `VkBaseInStructure` or
    /// `VkBaseOutStructure` types, used as abstract placeholders. Specifies a list of structures
    /// which may be passed in place of the parameter, or anywhere in the `pNext` chain of the
    /// parameter.
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub validstructs: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub struct Feature {
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub api: String,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub name: String,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub number: String,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub protect: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub comment: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub children: Vec<FeatureChild>,
}

pub type FeatureChild = ExtensionChild;

pub type Extensions = CommentedChildren<Extension>;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub struct Extension {
    /// Name of the extension.
    pub name: String,

    /// Human-readable description.
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub comment: Option<String>,

    /// The unique index of this extension.
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub number: Option<i64>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub protect: Option<String>,

    /// Which platform it works with, if any.
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub platform: Option<String>,

    /// Tag name of the author.
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub author: Option<String>,

    /// Contact information for extension author(s).
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub contact: Option<String>,

    /// The level at which the extension applies (instance / device).
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub ext_type: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub requires: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub requires_core: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub supported: Option<String>, // mk:TODO StringGroup?

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub deprecatedby: Option<String>,

    /// Whether this extension was promoted to core, and in which version.
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub promotedto: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub obsoletedby: Option<String>,

    /// 'true' if this extension is released provisionally
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub provisional: bool,

    /// The items which make up this extension.
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub specialuse: Option<String>,

    /// Relative sortorder
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub sortorder: Option<i64>,

    /// The items which make up this extension.
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub children: Vec<ExtensionChild>,
}

/// A part of an extension declaration.
///
/// Extensions either include functionality from the spec, or remove some functionality.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub enum ExtensionChild {
    /// Indicates the items which this extension requires to work.
    Require {
        #[cfg_attr(
            feature = "serialize",
            serde(default, skip_serializing_if = "is_default")
        )]
        api: Option<String>,

        #[cfg_attr(
            feature = "serialize",
            serde(default, skip_serializing_if = "is_default")
        )]
        profile: Option<String>,

        /// The extension which provides these required items, if any.
        #[cfg_attr(
            feature = "serialize",
            serde(default, skip_serializing_if = "is_default")
        )]
        extension: Option<String>,

        #[cfg_attr(
            feature = "serialize",
            serde(default, skip_serializing_if = "is_default")
        )]
        feature: Option<String>,

        #[cfg_attr(
            feature = "serialize",
            serde(default, skip_serializing_if = "is_default")
        )]
        comment: Option<String>,

        /// The items which form this require block.
        items: Vec<InterfaceItem>,
    },

    /// Indicates the items this extension removes.
    Remove {
        #[cfg_attr(
            feature = "serialize",
            serde(default, skip_serializing_if = "is_default")
        )]
        api: Option<String>,

        #[cfg_attr(
            feature = "serialize",
            serde(default, skip_serializing_if = "is_default")
        )]
        profile: Option<String>,

        #[cfg_attr(
            feature = "serialize",
            serde(default, skip_serializing_if = "is_default")
        )]
        comment: Option<String>,

        #[cfg_attr(
            feature = "serialize",
            serde(default, skip_serializing_if = "is_default")
        )]
        items: Vec<InterfaceItem>,
    },
}

/// An interface item is a function or an enum which makes up a Vulkan interface.
///
/// This structure is used by extensions to express dependencies or include functionality.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub enum InterfaceItem {
    Comment(String),

    Type {
        name: String,

        #[cfg_attr(
            feature = "serialize",
            serde(default, skip_serializing_if = "is_default")
        )]
        comment: Option<String>,
    },

    Enum(Enum),

    Command {
        name: String,

        #[cfg_attr(
            feature = "serialize",
            serde(default, skip_serializing_if = "is_default")
        )]
        comment: Option<String>,
    },
}

pub type Formats = CommentedChildren<Format>;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub struct Format {
    pub name: String,
    pub class: String,
    pub blockSize: u8,
    pub texelsPerBlock: u8,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub blockExtent: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub packed: Option<u8>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub compressed: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub chroma: Option<String>,

    pub children: Vec<FormatChild>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub enum FormatChild {
    #[non_exhaustive]
    Component {
        name: String,
        bits: String,
        numericFormat: String,
        #[cfg_attr(
            feature = "serialize",
            serde(default, skip_serializing_if = "is_default")
        )]
        planeIndex: Option<u8>,
    },

    #[non_exhaustive]
    Plane {
        index: u8,
        widthDivisor: u8,
        heightDivisor: u8,
        compatible: String,
    },

    #[non_exhaustive]
    SpirvImageFormat { name: String },
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub enum PointerKind {
    Single {
        is_const: bool,
    },
    Double {
        is_const: bool,
        inner_is_const: bool,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub enum ArrayLength {
    Static(core::num::NonZeroUsize),
    Constant(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub enum DynamicLength {
    NullTerminated,
    // FIXME only found in VkAccelerationStructureBuildGeometryInfoKHR->ppGeometries, is this a mistake?
    Static(core::num::NonZeroUsize),
    Parameterized(String),
    ParameterizedField { parameter: String, field: String },
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub enum DynamicShapeKind {
    Expression {
        latex_expr: Option<String>,
        c_expr: String,
    },
    Single(DynamicLength),
    Double(DynamicLength, DynamicLength),
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub enum OptionalKind {
    Single(bool),
    Double(bool, bool),
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub enum ExternSyncKind {
    /// externsync="true"
    Value,
    Fields(Vec<String>),
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub struct NameWithType {
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub type_name: String,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub pointer_kind: Option<PointerKind>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub is_struct: bool,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub bitfield_size: Option<core::num::NonZeroU8>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub array_shape: Option<Vec<ArrayLength>>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub name: String,

    /// combination of `len` and `altlen` attributes
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub dynamic_shape: Option<DynamicShapeKind>,

    /// Whether this parameter must be externally synchronised by the app.
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub externsync: Option<ExternSyncKind>,

    /// Whether this parameter must have a non-null value.
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub optional: Option<OptionalKind>,

    /// Disables automatic validity language being generated for this item.
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub noautovalidity: Option<()>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub objecttype: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub comment: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub struct CommentedChildren<T> {
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub comment: Option<String>,

    pub children: Vec<T>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub struct SpirvExtOrCap {
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub name: String,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub enables: Vec<Enable>,
}

pub type SpirvExtension = SpirvExtOrCap;
pub type SpirvExtensions = CommentedChildren<SpirvExtension>;

pub type SpirvCapability = SpirvExtOrCap;
pub type SpirvCapabilities = CommentedChildren<SpirvCapability>;

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub enum Enable {
    Version(String),
    Extension(String),
    Feature(FeatureEnable),
    Property(PropertyEnable),
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub struct FeatureEnable {
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub struct_: String,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub feature: String,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub requires: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub alias: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub struct PropertyEnable {
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub property: String,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub member: String,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub value: String,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub requires: Option<String>,
}

#[cfg(feature = "serialize")]
fn is_default<T: Default + Eq>(v: &T) -> bool {
    v.eq(&T::default())
}
