/// Rust structure representing the Vulkan registry.
///
/// The registry contains all the information contained in a certain version
/// of the Vulkan specification, stored within a programmer-accessible format.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Registry(pub Vec<RegistryItem>);

/// An element of the Vulkan registry.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RegistryItem {
    /// Comments are human-readable strings which contain registry meta-data.
    Comment(String),
    /// IDs of all known Vulkan vendors.
    VendorIds {
        comment: Option<String>,
        items: Vec<VendorId>,
    },
    /// List of supported Vulkan platforms.
    Platforms {
        comment: Option<String>,
        items: Vec<Platform>,
    },
    /// Known extension tags.
    Tags {
        comment: Option<String>,
        items: Vec<Tag>,
    },
    /// Type definitions.
    ///
    /// Unlike OpenGL, Vulkan is a strongly-typed API.
    Types {
        comment: Option<String>,
        items: Vec<TypeItem>,
    },
    /// Enum definitions.
    Enums {
        name: Option<String>,
        kind: Option<String>,
        start: Option<i64>,
        end: Option<i64>,
        vendor: Option<String>,
        comment: Option<String>,
        items: Vec<EnumsItem>,
    },
    /// Commands are the Vulkan API's name for functions.
    Commands {
        comment: Option<String>,
        items: Vec<Command>,
    },
    /// Feature level of the API, such as Vulkan 1.0 or 1.1
    Feature {
        api: String,
        name: String,
        number: f32,
        protect: Option<String>,
        comment: Option<String>,
        items: Vec<ExtensionItem>,
    },
    /// Container for all published Vulkan specification extensions.
    Extensions {
        comment: Option<String>,
        items: Vec<Extension>,
    },
}

/// Unique identifier for a Vulkan vendor.
///
/// Note: in newer versions of the Vulkan spec, this tag is not used,
/// instead it has been replaced by the `VKVendorId` enum.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VendorId {
    /// Name of the vendor.
    pub name: String,
    /// Human-readable description.
    pub comment: Option<String>,
    /// The unique ID.
    pub id: u32,
}

/// A platform refers to a windowing system which Vulkan can use.
///
/// Most operating systems will have only one corresponding platform,
/// but Linux has multiple (XCB, Wayland, etc.)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Platform {
    /// Short identifier.
    pub name: String,
    /// Human readable description of the platform.
    pub comment: Option<String>,
    /// C macro name which is used to guard platform-specific definitions.
    pub protect: String,
}

/// Tags are the little suffixes attached to extension names or items, indicating the author.
///
/// Some examples:
/// - KHR for Khronos extensions
/// - EXT for multi-vendor extensions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Tag {
    /// The name of the tag, e.g. "KHR".
    pub name: String,
    /// Author of the extensions associated with the tag, e.g. "Khronos".
    pub author: String,
    /// Contact information for the extension author(s).
    pub contact: String,
}

/// An item making up a type definition.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TypeItem {
    Type {
        api: Option<String>,
        alias: Option<String>,
        requires: Option<String>,
        name: Option<String>,
        category: Option<String>,
        parent: Option<String>,
        returnedonly: Option<String>,
        structextends: Option<String>,
        comment: Option<String>,
        contents: TypeContents,
    },
    Comment(String),
}

/// The contents of a type definition.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TypeContents {
    Code {
        code: String,
        markup: Vec<TypeCodeMarkup>,
    },
    Members(Vec<TypeMember>),
    None,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TypeCodeMarkup {
    Name(String),
    Type(String),
    ApiEntry(String),
}

/// A member of a type definition, i.e. a struct member.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TypeMember {
    /// Human-readable comment.
    Comment(String),
    /// A structure field definition.
    Definition {
        len: Option<String>,
        altlen: Option<String>,
        externsync: Option<String>,
        optional: Option<String>,
        noautovalidity: Option<String>,
        validextensionstructs: Option<String>,
        values: Option<String>,
        code: String,
        markup: Vec<TypeMemberMarkup>,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TypeMemberMarkup {
    Name(String),
    Type(String),
    Enum(String),
    Comment(String),
}

/// A command is just a Vulkan function.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Command {
    /// Indicates this function is an alias for another one.
    Alias { name: String, alias: String },
    /// Defines a new Vulkan function.
    Definition {
        queues: Option<String>,
        successcodes: Option<String>,
        errorcodes: Option<String>,
        renderpass: Option<String>,
        cmdbufferlevel: Option<String>,
        pipeline: Option<String>,
        comment: Option<String>,
        proto: NameWithType,
        params: Vec<CommandParam>,
        alias: Option<String>,
        description: Option<String>,
        implicitexternsyncparams: Vec<String>,

        code: String,
    },
}

/// Parameter for this Vulkan function.
#[derive(Debug, Clone, Serialize, Deserialize)]
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

#[derive(Debug, Clone, Serialize, Deserialize)]
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
    /// The items which make up this extension.
    pub items: Vec<ExtensionItem>,
}

/// A part of an extension declaration.
///
/// Extensions either include functionality from the spec, or remove some functionality.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ExtensionItem {
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
#[derive(Debug, Clone, Serialize, Deserialize)]
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

/// An item which forms an enum.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum EnumsItem {
    /// Actual named enum.
    Enum(Enum),
    /// An unused range of enum values.
    Unused {
        /// Beginning of the range.
        start: i64,
        /// Ending value of the range, if any.
        end: Option<i64>,
        /// Vendor who reserved this range.
        vendor: Option<String>,
        /// Human-readable description.
        comment: Option<String>,
    },
    /// Human-readable comment.
    Comment(String),
}

/// An enum specifier, which assigns a value to the enum.
#[derive(Debug, Clone, Serialize, Deserialize)]
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

/// An item of an enumeration type.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Enum {
    /// Name of this enum.
    pub name: String,
    /// Human-readable description.
    pub comment: Option<String>,
    pub type_suffix: Option<String>,
    pub api: Option<String>,
    pub spec: EnumSpec,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NameWithType {
    pub type_name: Option<String>,
    pub name: String,
}
