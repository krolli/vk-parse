
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
