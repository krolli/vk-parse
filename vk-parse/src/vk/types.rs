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
    Type(Type),
    Comment(String),
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub struct Type {
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub name: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub alias: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub api: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub requires: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub category: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub comment: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub parent: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub returnedonly: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub structextends: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub allowduplicate: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub objtypeenum: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub bitvalues: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub spec: TypeSpec,
}

/// The contents of a type definition.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub enum TypeSpec {
    None,
    Code(TypeCode),
    Members(Vec<TypeMember>),
}

impl Default for TypeSpec {
    fn default() -> Self {
        TypeSpec::None
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub struct TypeCode {
    pub code: String,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub markup: Vec<TypeCodeMarkup>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub enum TypeCodeMarkup {
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
    Definition(TypeMemberDefinition),
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub struct TypeMemberDefinition {
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub len: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub altlen: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub externsync: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub optional: Option<String>,

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
    pub noautovalidity: Option<String>,

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

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub objecttype: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub code: String,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub markup: Vec<TypeMemberMarkup>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub enum TypeMemberMarkup {
    Name(String),
    Type(String),
    Enum(String),
    Comment(String),
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
    Definition(CommandDefinition),
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub struct CommandDefinition {
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub queues: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub successcodes: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub errorcodes: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub renderpass: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub cmdbufferlevel: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub pipeline: Option<String>,

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

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub code: String,
}

/// Parameter for this Vulkan function.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub struct CommandParam {
    /// The expression which indicates the length of this array.
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub len: Option<String>,

    /// Alternate description of the length of this parameter.
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub altlen: Option<String>,

    /// Whether this parameter must be externally synchronised by the app.
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub externsync: Option<String>,

    /// Whether this parameter must have a non-null value.
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub optional: Option<String>,

    /// Disables automatic validity language being generated for this item.
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub noautovalidity: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub objecttype: Option<String>,

    /// The definition of this parameter.
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub definition: NameWithType,
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

#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub struct NameWithType {
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub type_name: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub name: String,
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
