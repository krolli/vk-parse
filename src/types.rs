#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Registry(pub Vec<RegistryItem>);

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RegistryItem {
    Comment(String),
    VendorIds {
        comment: Option<String>,
        items: Vec<VendorId>,
    },
    Platforms {
        comment: Option<String>,
        items: Vec<Platform>,
    },
    Tags {
        comment: Option<String>,
        items: Vec<Tag>,
    },
    Types {
        comment: Option<String>,
        items: Vec<TypeItem>,
    },
    Enums {
        name: Option<String>,
        kind: Option<String>,
        start: Option<i64>,
        end: Option<i64>,
        vendor: Option<String>,
        comment: Option<String>,
        items: Vec<EnumsItem>,
    },
    Commands {
        comment: Option<String>,
        items: Vec<Command>,
    },
    Feature {
        api: String,
        name: String,
        number: f32,
        protect: Option<String>,
        comment: Option<String>,
        items: Vec<ExtensionItem>,
    },
    Extensions {
        comment: Option<String>,
        items: Vec<Extension>,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VendorId {
    pub name: String,
    pub comment: Option<String>,
    pub id: u32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Platform {
    pub name: String,
    pub comment: Option<String>,
    pub protect: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Tag {
    pub name: String,
    pub author: String,
    pub contact: String,
}

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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TypeMember {
    Comment(String),
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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Command {
    Alias {
        name: String,
        alias: String,
    },
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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CommandParam {
    pub len: Option<String>,
    pub altlen: Option<String>,
    pub externsync: Option<String>,
    pub optional: Option<String>,
    pub noautovalidity: Option<String>,

    pub definition: NameWithType,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Extension {
    pub name: String,
    pub comment: Option<String>,
    pub number: Option<i64>,
    pub protect: Option<String>,
    pub platform: Option<String>,
    pub author: Option<String>,
    pub contact: Option<String>,
    pub ext_type: Option<String>,
    pub requires: Option<String>,
    pub requires_core: Option<String>,
    pub supported: Option<String>, // mk:TODO StringGroup?
    pub deprecatedby: Option<String>,
    pub promotedto: Option<String>,
    pub obsoletedby: Option<String>,
    pub items: Vec<ExtensionItem>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ExtensionItem {
    Require {
        api: Option<String>,
        profile: Option<String>,
        extension: Option<String>,
        feature: Option<String>,
        comment: Option<String>,
        items: Vec<InterfaceItem>,
    },
    Remove {
        api: Option<String>,
        profile: Option<String>,
        comment: Option<String>,
        items: Vec<InterfaceItem>,
    },
}

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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum EnumsItem {
    Enum(Enum),
    Unused {
        start: i64,
        end: Option<i64>,
        vendor: Option<String>,
        comment: Option<String>,
    },
    Comment(String),
}

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
    Bitpos {
        bitpos: i64,
        extends: Option<String>,
    },
    Value {
        value: String, // rnc says this is an Integer, but validates it as text, and that's what it sometimes really is.
        extends: Option<String>,
    },
    None,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Enum {
    pub name: String,
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
