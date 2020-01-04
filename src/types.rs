/// Rust structure representing the Vulkan registry.
///
/// The registry contains all the information contained in a certain version
/// of the Vulkan specification, stored within a programmer-accessible format.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
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
/// Note: in newer versions of the Vulkan spec, this tag is not used,
/// instead it has been replaced by the `VKVendorId` enum.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct VendorId {
    /// Name of the vendor.
    pub name: String,
    /// Human-readable description.
    pub comment: Option<String>,
    /// The unique ID.
    pub id: u32,
}

pub type Platforms = CommentedChildren<Platform>;

/// A platform refers to a windowing system which Vulkan can use.
///
/// Most operating systems will have only one corresponding platform,
/// but Linux has multiple (XCB, Wayland, etc.)
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Platform {
    /// Short identifier.
    pub name: String,
    /// Human readable description of the platform.
    pub comment: Option<String>,
    /// C macro name which is used to guard platform-specific definitions.
    pub protect: String,
}

pub type Tags = CommentedChildren<Tag>;

/// Tags are the little suffixes attached to extension names or items, indicating the author.
///
/// Some examples:
/// - KHR for Khronos extensions
/// - EXT for multi-vendor extensions
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
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

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Type {
    pub name: Option<String>,
    pub alias: Option<String>,
    pub api: Option<String>,
    pub requires: Option<String>,
    pub category: Option<String>,
    pub comment: Option<String>,
    pub parent: Option<String>,
    pub returnedonly: Option<String>,
    pub structextends: Option<String>,
    pub spec: TypeSpec,
}

/// The contents of a type definition.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum TypeSpec {
    Code(TypeCode),
    Members(Vec<TypeMember>),
    None,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct TypeCode {
    pub code: String,
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

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct TypeMemberDefinition {
    pub len: Option<String>,
    pub altlen: Option<String>,
    pub externsync: Option<String>,
    pub optional: Option<String>,
    pub noautovalidity: Option<String>,
    pub validextensionstructs: Option<String>,
    pub values: Option<String>,
    pub code: String,
    pub markup: Vec<TypeMemberMarkup>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum TypeMemberMarkup {
    Name(String),
    Type(String),
    Enum(String),
    Comment(String),
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Enums {
    pub name: Option<String>,
    pub kind: Option<String>,
    pub start: Option<i64>,
    pub end: Option<i64>,
    pub vendor: Option<String>,
    pub comment: Option<String>,
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
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Unused {
    /// Beginning of the range.
    pub start: i64,
    /// Ending value of the range, if any.
    pub end: Option<i64>,
    /// Vendor who reserved this range.
    pub vendor: Option<String>,
    /// Human-readable description.
    pub comment: Option<String>,
}

/// An item of an enumeration type.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Enum {
    /// Name of this enum.
    pub name: String,
    /// Human-readable description.
    pub comment: Option<String>,
    pub type_suffix: Option<String>,
    pub api: Option<String>,
    pub spec: EnumSpec,
}

/// An enum specifier, which assigns a value to the enum.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum EnumSpec {
    Alias {
        alias: String,
        extends: Option<String>,
    },
    Offset {
        offset: i64,
        extends: String,
        extnumber: Option<i64>,
        dir: bool,
    },
    /// Indicates an enum which is a bit flag.
    Bitpos {
        /// The bit to be set.
        bitpos: i64,
        /// Which structure this enum extends.
        extends: Option<String>,
    },
    /// An enum value.
    Value {
        /// Hard coded value for an enum.
        value: String, // rnc says this is an Integer, but validates it as text, and that's what it sometimes really is.
        /// Which structure this enum extends.
        extends: Option<String>,
    },
    None,
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

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct CommandDefinition {
    pub queues: Option<String>,
    pub successcodes: Option<String>,
    pub errorcodes: Option<String>,
    pub renderpass: Option<String>,
    pub cmdbufferlevel: Option<String>,
    pub pipeline: Option<String>,
    pub comment: Option<String>,
    pub proto: NameWithType,
    pub params: Vec<CommandParam>,
    pub alias: Option<String>,
    pub description: Option<String>,
    pub implicitexternsyncparams: Vec<String>,

    pub code: String,
}

/// Parameter for this Vulkan function.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct CommandParam {
    /// The expression which indicates the length of this array.
    pub len: Option<String>,
    /// Alternate description of the length of this parameter.
    pub altlen: Option<String>,
    /// Whether this parameter must be externally synchronised by the app.
    pub externsync: Option<String>,
    /// Whether this parameter must have a non-null value.
    pub optional: Option<String>,
    /// Disables automatic validity language being generated for this item.
    pub noautovalidity: Option<String>,

    /// The definition of this parameter.
    pub definition: NameWithType,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Feature {
    pub api: String,
    pub name: String,
    pub number: f32,
    pub protect: Option<String>,
    pub comment: Option<String>,
    pub children: Vec<FeatureChild>,
}

pub type FeatureChild = ExtensionChild;

pub type Extensions = CommentedChildren<Extension>;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Extension {
    /// Name of the extension.
    pub name: String,
    /// Human-readable description.
    pub comment: Option<String>,
    /// The unique index of this extension.
    pub number: Option<i64>,
    pub protect: Option<String>,
    /// Which platform it works with, if any.
    pub platform: Option<String>,
    /// Tag name of the author.
    pub author: Option<String>,
    /// Contact information for extension author(s).
    pub contact: Option<String>,
    /// The level at which the extension applies (instance / device).
    pub ext_type: Option<String>,
    pub requires: Option<String>,
    pub requires_core: Option<String>,
    pub supported: Option<String>, // mk:TODO StringGroup?
    pub deprecatedby: Option<String>,
    /// Whether this extension was promoted to core, and in which version.
    pub promotedto: Option<String>,
    pub obsoletedby: Option<String>,

    /// 'true' if this extension is released provisionally
    #[serde(default, skip_serializing_if = "is_default")]
    pub provisional: bool,
    /// The items which make up this extension.
    pub children: Vec<ExtensionChild>,
}

/// A part of an extension declaration.
///
/// Extensions either include functionality from the spec, or remove some functionality.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ExtensionChild {
    /// Indicates the items which this extension requires to work.
    Require {
        api: Option<String>,
        profile: Option<String>,
        /// The extension which provides these required items, if any.
        extension: Option<String>,
        feature: Option<String>,
        comment: Option<String>,
        /// The items which form this require block.
        items: Vec<InterfaceItem>,
    },
    /// Indicates the items this extension removes.
    Remove {
        api: Option<String>,
        profile: Option<String>,
        comment: Option<String>,
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
        comment: Option<String>,
    },
    Enum(Enum),
    Command {
        name: String,
        comment: Option<String>,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct NameWithType {
    pub type_name: Option<String>,
    pub name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct CommentedChildren<T> {
    pub comment: Option<String>,
    pub children: Vec<T>,
}

fn is_default<T: Default + Eq>(v: &T) -> bool {
    v.eq(&T::default())
}
