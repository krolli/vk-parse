#![allow(non_snake_case)]

/// Rust structure representing the opengl registry.
///
/// The registry contains all the information contained in a certain version
/// of the Vulkan specification, stored within a programmer-accessible format.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
pub struct Registry(pub Vec<RegistryChild>);

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
pub struct CommandParam {
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub group: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub class: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub len: Option<String>,

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
pub struct CommentedChildren<T> {
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub namespace: Option<String>,

    pub children: Vec<T>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub struct CommandDefinition {
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
    pub code: String,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub vec_equiv: Option<String>,
}

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

pub type Commands = CommentedChildren<Command>;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub struct Extensions {
    pub children: Vec<Extension>,
}

/// An interface item is a function or an enum which makes up a Vulkan interface.
///
/// This structure is used by extensions to express dependencies or include functionality.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub enum InterfaceItem {
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
        items: Vec<InterfaceItem>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub struct Extension {
    pub name: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub supported: Option<String>,

    /// The items which make up this extension.
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub children: Vec<ExtensionChild>,
}


#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub struct Type {
    #[cfg_attr(
    feature = "serialize",
    serde(default, skip_serializing_if = "is_default")
    )]
    pub requires: Option<String>,

    #[cfg_attr(
    feature = "serialize",
    serde(default, skip_serializing_if = "is_default")
    )]
    pub comment: Option<String>,

    #[cfg_attr(
    feature = "serialize",
    serde(default, skip_serializing_if = "is_default")
    )]
    pub name: Option<String>,

    #[cfg_attr(
    feature = "serialize",
    serde(default, skip_serializing_if = "is_default")
    )]
    pub code: String,

}
#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub struct Types {
    pub children: Vec<Type>,
}


/// An element of the Vulkan registry.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub enum RegistryChild {
    /// Comments are human-readable strings which contain registry meta-data.
    Comment(String),

    /// Unlike OpenGL, Vulkan is a strongly-typed API.
    Types(Types),

    /// Enum definitions.
    Enums(Enums),

    /// Commands are the Vulkan API's name for functions.
    Commands(Commands),

    /// Container for all published Vulkan specification extensions.
    Extensions(Extensions),
}


#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub struct Enums {
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub namespace: Option<String>,

    #[cfg_attr(
    feature = "serialize",
    serde(default, skip_serializing_if = "is_default")
    )]
    pub group: Option<String>,

    #[cfg_attr(
    feature = "serialize",
    serde(default, skip_serializing_if = "is_default")
    )]
    pub enum_type: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub start: Option<String>,

    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub end: Option<String>,

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
    pub value: Option<String>,

    /// Human-readable description.
    #[cfg_attr(
        feature = "serialize",
        serde(default, skip_serializing_if = "is_default")
    )]
    pub group: Option<String>,
}

/// An item which forms an enum.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub enum EnumsChild {
    /// Actual named enum.
    Enum(Enum),
    //
    // /// An unused range of enum values.
    // Unused(Unused),
    /// Human-readable comment.
    Comment(String),
}

#[cfg(feature = "serialize")]
fn is_default<T: Default + Eq>(v: &T) -> bool {
    v.eq(&T::default())
}
