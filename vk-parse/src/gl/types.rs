
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
    //
    // /// IDs of all known Vulkan vendors.
    // VendorIds(VendorIds),
    //
    // /// List of supported Vulkan platforms.
    // Platforms(Platforms),
    //
    // /// Known extension tags.
    // Tags(Tags),
    //
    // /// Type definitions.
    // ///
    // /// Unlike OpenGL, Vulkan is a strongly-typed API.
    // Types(Types),
    //
    // /// Enum definitions.
    // Enums(Enums),
    //
    // /// Commands are the Vulkan API's name for functions.
    // Commands(Commands),
    //
    // /// Feature level of the API, such as Vulkan 1.0 or 1.1
    // Feature(Feature),
    //
    // /// Container for all published Vulkan specification extensions.
    // Extensions(Extensions),
    //
    // Formats(Formats),
    //
    // SpirvExtensions(SpirvExtensions),
    //
    // SpirvCapabilities(SpirvCapabilities),
}
