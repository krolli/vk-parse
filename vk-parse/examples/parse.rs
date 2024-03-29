use std::{
    error::Error,
    path::PathBuf,
    fmt,
};

fn main() -> Result<(), Box<dyn Error>> {
    let path = std::env::args()
        .skip(1)
        .map(PathBuf::from)
        .next()
        .ok_or(MissingArgumentError("XML_PATH"))?;
    let registry = vk_parse::parse_file(path.as_ref())
        .map(|(ret, errors)| {
            errors.into_iter().for_each(|e| {
                eprintln!("non-fatal error while parsing registry: {:?}", &e);
            });
            ret
        });
    println!("{:#?}", &registry);
    Ok(())
}

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
struct MissingArgumentError<'a>(&'a str);

impl<'a> fmt::Display for MissingArgumentError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "missing required argument: {}", self.0)
    }
}

impl<'a> Error for MissingArgumentError<'a> {}
