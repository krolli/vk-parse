/// Rust structure representing the Vulkan registry.
///
/// The registry contains all the information contained in a certain version
/// of the Vulkan specification, stored within a programmer-accessible format.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Default)]
pub struct Registry(pub Vec<RegistryChild>);

/// An element of the Vulkan registry.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
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
}

pub type VendorIds = CommentedChildren<VendorId>;

/// Unique identifier for a Vulkan vendor.
///
/// Note: in newer versions of the Vulkan spec (1.1.79 and later), this tag is
/// not used, instead it has been replaced by the `VKVendorId` enum.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Default)]
pub struct VendorId {
    /// Name of the vendor.
    pub name: String,

    /// The unique ID.
    pub id: u32,

    /// Human-readable description.
    #[serde(default, skip_serializing_if = "is_default")]
    pub comment: Option<String>,
}

pub type Platforms = CommentedChildren<Platform>;

/// A platform refers to a windowing system which Vulkan can use.
///
/// Most operating systems will have only one corresponding platform,
/// but Linux has multiple (XCB, Wayland, etc.)
///
/// Used in versions 1.1.70 and later.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Default)]
pub struct Platform {
    /// Short identifier.
    pub name: String,

    /// C macro name which is used to guard platform-specific definitions.
    pub protect: String,

    /// Human readable description of the platform.
    #[serde(default, skip_serializing_if = "is_default")]
    pub comment: Option<String>,
}

pub type Tags = CommentedChildren<Tag>;

/// Tags are the little suffixes attached to extension names or items, indicating the author.
///
/// Some examples:
/// - KHR for Khronos extensions
/// - EXT for multi-vendor extensions
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Default)]
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
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum TypesChild {
    Type(Type),
    Comment(String),
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Default)]
pub struct Type {
    #[serde(default, skip_serializing_if = "is_default")]
    pub name: Option<String>,

    #[serde(default, skip_serializing_if = "is_default")]
    pub alias: Option<String>,

    #[serde(default, skip_serializing_if = "is_default")]
    pub api: Option<String>,

    #[serde(default, skip_serializing_if = "is_default")]
    pub requires: Option<String>,

    #[serde(default, skip_serializing_if = "is_default")]
    pub category: Option<String>,

    #[serde(default, skip_serializing_if = "is_default")]
    pub comment: Option<String>,

    #[serde(default, skip_serializing_if = "is_default")]
    pub parent: Option<String>,

    #[serde(default, skip_serializing_if = "is_default")]
    pub returnedonly: Option<String>,

    #[serde(default, skip_serializing_if = "is_default")]
    pub structextends: Option<String>,

    #[serde(default, skip_serializing_if = "is_default")]
    pub spec: TypeSpec,
}

/// The contents of a type definition.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
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

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Default)]
pub struct TypeCode {
    pub code: String,

    #[serde(default, skip_serializing_if = "is_default")]
    pub markup: Vec<TypeCodeMarkup>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum TypeCodeMarkup {
    Name(String),
    Type(String),
    ApiEntry(String),
}

/// A member of a type definition, i.e. a struct member.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum TypeMember {
    /// Human-readable comment.
    Comment(String),

    /// A structure field definition.
    Definition(TypeMemberDefinition),
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Default)]
pub struct TypeMemberDefinition {
    #[serde(default, skip_serializing_if = "is_default")]
    pub len: Option<String>,

    #[serde(default, skip_serializing_if = "is_default")]
    pub altlen: Option<String>,

    #[serde(default, skip_serializing_if = "is_default")]
    pub externsync: Option<String>,

    #[serde(default, skip_serializing_if = "is_default")]
    pub optional: Option<String>,

    #[serde(default, skip_serializing_if = "is_default")]
    pub noautovalidity: Option<String>,

    #[serde(default, skip_serializing_if = "is_default")]
    pub validextensionstructs: Option<String>,

    #[serde(default, skip_serializing_if = "is_default")]
    pub values: Option<String>,

    #[serde(default, skip_serializing_if = "is_default")]
    pub code: String,

    #[serde(default, skip_serializing_if = "is_default")]
    pub markup: Vec<TypeMemberMarkup>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum TypeMemberMarkup {
    Name(String),
    Type(String),
    Enum(String),
    Comment(String),
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Default)]
pub struct Enums {
    #[serde(default, skip_serializing_if = "is_default")]
    pub name: Option<String>,

    #[serde(default, skip_serializing_if = "is_default")]
    pub kind: Option<String>,

    #[serde(default, skip_serializing_if = "is_default")]
    pub start: Option<i64>,

    #[serde(default, skip_serializing_if = "is_default")]
    pub end: Option<i64>,

    #[serde(default, skip_serializing_if = "is_default")]
    pub vendor: Option<String>,

    #[serde(default, skip_serializing_if = "is_default")]
    pub comment: Option<String>,

    #[serde(default, skip_serializing_if = "is_default")]
    pub children: Vec<EnumsChild>,
}

/// An item which forms an enum.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum EnumsChild {
    /// Actual named enum.
    Enum(Enum),

    /// An unused range of enum values.
    Unused(Unused),

    /// Human-readable comment.
    Comment(String),
}

/// An unused range of enum values.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Default)]
pub struct Unused {
    /// Beginning of the range.
    pub start: i64,

    /// Ending value of the range, if any.
    #[serde(default, skip_serializing_if = "is_default")]
    pub end: Option<i64>,

    /// Vendor who reserved this range.
    #[serde(default, skip_serializing_if = "is_default")]
    pub vendor: Option<String>,

    /// Human-readable description.
    #[serde(default, skip_serializing_if = "is_default")]
    pub comment: Option<String>,
}

/// An item of an enumeration type.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Default)]
pub struct Enum {
    /// Name of this enum.
    pub name: String,

    /// Human-readable description.
    #[serde(default, skip_serializing_if = "is_default")]
    pub comment: Option<String>,

    #[serde(default, skip_serializing_if = "is_default")]
    pub type_suffix: Option<String>,

    #[serde(default, skip_serializing_if = "is_default")]
    pub api: Option<String>,

    #[serde(default, skip_serializing_if = "is_default")]
    pub spec: EnumSpec,
}

/// An enum specifier, which assigns a value to the enum.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum EnumSpec {
    None,

    Alias {
        alias: String,

        #[serde(default, skip_serializing_if = "is_default")]
        extends: Option<String>,
    },

    Offset {
        offset: i64,

        #[serde(default, skip_serializing_if = "is_default")]
        extends: String,

        #[serde(default, skip_serializing_if = "is_default")]
        extnumber: Option<i64>,

        #[serde(default, skip_serializing_if = "is_default")]
        dir: bool,
    },

    /// Indicates an enum which is a bit flag.
    Bitpos {
        /// The bit to be set.
        bitpos: i64,

        /// Which structure this enum extends.
        #[serde(default, skip_serializing_if = "is_default")]
        extends: Option<String>,
    },

    /// An enum value.
    Value {
        /// Hard coded value for an enum.
        value: String, // rnc says this is an Integer, but validates it as text, and that's what it sometimes really is.

        /// Which structure this enum extends.
        #[serde(default, skip_serializing_if = "is_default")]
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
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum Command {
    /// Indicates this function is an alias for another one.
    Alias { name: String, alias: String },

    /// Defines a new Vulkan function.
    Definition(CommandDefinition),
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Default)]
pub struct CommandDefinition {
    #[serde(default, skip_serializing_if = "is_default")]
    pub queues: Option<String>,

    #[serde(default, skip_serializing_if = "is_default")]
    pub successcodes: Option<String>,

    #[serde(default, skip_serializing_if = "is_default")]
    pub errorcodes: Option<String>,

    #[serde(default, skip_serializing_if = "is_default")]
    pub renderpass: Option<String>,

    #[serde(default, skip_serializing_if = "is_default")]
    pub cmdbufferlevel: Option<String>,

    #[serde(default, skip_serializing_if = "is_default")]
    pub pipeline: Option<String>,

    #[serde(default, skip_serializing_if = "is_default")]
    pub comment: Option<String>,

    #[serde(default, skip_serializing_if = "is_default")]
    pub proto: NameWithType,

    #[serde(default, skip_serializing_if = "is_default")]
    pub params: Vec<CommandParam>,

    #[serde(default, skip_serializing_if = "is_default")]
    pub alias: Option<String>,

    #[serde(default, skip_serializing_if = "is_default")]
    pub description: Option<String>,

    #[serde(default, skip_serializing_if = "is_default")]
    pub implicitexternsyncparams: Vec<String>,

    #[serde(default, skip_serializing_if = "is_default")]
    pub code: String,
}

/// Parameter for this Vulkan function.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Default)]
pub struct CommandParam {
    /// The expression which indicates the length of this array.
    #[serde(default, skip_serializing_if = "is_default")]
    pub len: Option<String>,

    /// Alternate description of the length of this parameter.
    #[serde(default, skip_serializing_if = "is_default")]
    pub altlen: Option<String>,

    /// Whether this parameter must be externally synchronised by the app.
    #[serde(default, skip_serializing_if = "is_default")]
    pub externsync: Option<String>,

    /// Whether this parameter must have a non-null value.
    #[serde(default, skip_serializing_if = "is_default")]
    pub optional: Option<String>,

    /// Disables automatic validity language being generated for this item.
    #[serde(default, skip_serializing_if = "is_default")]
    pub noautovalidity: Option<String>,

    /// The definition of this parameter.
    #[serde(default, skip_serializing_if = "is_default")]
    pub definition: NameWithType,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Default)]
pub struct Feature {
    #[serde(default, skip_serializing_if = "is_default")]
    pub api: String,

    #[serde(default, skip_serializing_if = "is_default")]
    pub name: String,

    #[serde(default, skip_serializing_if = "is_default")]
    pub number: String,

    #[serde(default, skip_serializing_if = "is_default")]
    pub protect: Option<String>,

    #[serde(default, skip_serializing_if = "is_default")]
    pub comment: Option<String>,

    #[serde(default, skip_serializing_if = "is_default")]
    pub children: Vec<FeatureChild>,
}

pub type FeatureChild = ExtensionChild;

pub type Extensions = CommentedChildren<Extension>;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Default)]
pub struct Extension {
    /// Name of the extension.
    pub name: String,

    /// Human-readable description.
    #[serde(default, skip_serializing_if = "is_default")]
    pub comment: Option<String>,

    /// The unique index of this extension.
    #[serde(default, skip_serializing_if = "is_default")]
    pub number: Option<i64>,

    #[serde(default, skip_serializing_if = "is_default")]
    pub protect: Option<String>,

    /// Which platform it works with, if any.
    #[serde(default, skip_serializing_if = "is_default")]
    pub platform: Option<String>,

    /// Tag name of the author.
    #[serde(default, skip_serializing_if = "is_default")]
    pub author: Option<String>,

    /// Contact information for extension author(s).
    #[serde(default, skip_serializing_if = "is_default")]
    pub contact: Option<String>,

    /// The level at which the extension applies (instance / device).
    #[serde(default, skip_serializing_if = "is_default")]
    pub ext_type: Option<String>,

    #[serde(default, skip_serializing_if = "is_default")]
    pub requires: Option<String>,

    #[serde(default, skip_serializing_if = "is_default")]
    pub requires_core: Option<String>,

    #[serde(default, skip_serializing_if = "is_default")]
    pub supported: Option<String>, // mk:TODO StringGroup?

    #[serde(default, skip_serializing_if = "is_default")]
    pub deprecatedby: Option<String>,

    /// Whether this extension was promoted to core, and in which version.
    #[serde(default, skip_serializing_if = "is_default")]
    pub promotedto: Option<String>,

    #[serde(default, skip_serializing_if = "is_default")]
    pub obsoletedby: Option<String>,

    /// 'true' if this extension is released provisionally
    #[serde(default, skip_serializing_if = "is_default")]
    pub provisional: bool,

    /// The items which make up this extension.
    #[serde(default, skip_serializing_if = "is_default")]
    pub children: Vec<ExtensionChild>,
}

/// A part of an extension declaration.
///
/// Extensions either include functionality from the spec, or remove some functionality.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ExtensionChild {
    /// Indicates the items which this extension requires to work.
    Require {
        #[serde(default, skip_serializing_if = "is_default")]
        api: Option<String>,

        #[serde(default, skip_serializing_if = "is_default")]
        profile: Option<String>,

        /// The extension which provides these required items, if any.
        #[serde(default, skip_serializing_if = "is_default")]
        extension: Option<String>,

        #[serde(default, skip_serializing_if = "is_default")]
        feature: Option<String>,

        #[serde(default, skip_serializing_if = "is_default")]
        comment: Option<String>,

        /// The items which form this require block.
        items: Vec<InterfaceItem>,
    },

    /// Indicates the items this extension removes.
    Remove {
        #[serde(default, skip_serializing_if = "is_default")]
        api: Option<String>,

        #[serde(default, skip_serializing_if = "is_default")]
        profile: Option<String>,

        #[serde(default, skip_serializing_if = "is_default")]
        comment: Option<String>,

        #[serde(default, skip_serializing_if = "is_default")]
        items: Vec<InterfaceItem>,
    },
}

/// An interface item is a function or an enum which makes up a Vulkan interface.
///
/// This structure is used by extensions to express dependencies or include functionality.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum InterfaceItem {
    Comment(String),

    Type {
        name: String,

        #[serde(default, skip_serializing_if = "is_default")]
        comment: Option<String>,
    },

    Enum(Enum),

    Command {
        name: String,

        #[serde(default, skip_serializing_if = "is_default")]
        comment: Option<String>,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Default)]
pub struct NameWithType {
    #[serde(default, skip_serializing_if = "is_default")]
    pub type_name: Option<String>,

    #[serde(default, skip_serializing_if = "is_default")]
    pub name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Default)]
pub struct CommentedChildren<T> {
    #[serde(default, skip_serializing_if = "is_default")]
    pub comment: Option<String>,

    pub children: Vec<T>,
}

fn is_default<T: Default + Eq>(v: &T) -> bool {
    v.eq(&T::default())
}
