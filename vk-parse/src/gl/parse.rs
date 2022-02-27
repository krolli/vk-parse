use std::io::Read;

use util::*;
use types::*;
use gl::types::*;

//--------------------------------------------------------------------------------------------------
/// Parses the Vulkan XML file into a Rust object.
pub fn parse_file(path: &std::path::Path) -> Result<(Registry, Vec<Error>), FatalError> {
    let file = std::io::BufReader::new(std::fs::File::open(path)?);
    let parser = xml::reader::ParserConfig::new().create_reader(file);
    parse_xml(parser.into_iter())
}

/// Parses the Vulkan XML file from stream into a Rust object.
pub fn parse_stream<T: std::io::Read>(stream: T) -> Result<(Registry, Vec<Error>), FatalError> {
    let parser = xml::reader::ParserConfig::new().create_reader(stream);
    parse_xml(parser.into_iter())
}

fn parse_xml<R: Read>(events: XmlEvents<R>) -> Result<(Registry, Vec<Error>), FatalError> {
    let mut ctx = ParseCtx {
        events,
        xpath: String::from(""),
        errors: Vec::new(),
    };

    let mut result = Err(FatalError::MissingRegistryElement);

    {
        let ctx = &mut ctx;
        match_elements! {ctx,
            "registry" => result = parse_registry(ctx)
        }
    }

    result.map(|r| (r, ctx.errors))
}


fn parse_registry<R: Read>(ctx: &mut ParseCtx<R>) -> Result<Registry, FatalError> {
    let mut registry = Registry(Vec::new());

    match_elements! {ctx, attributes,
        "comment" => registry.0.push(RegistryChild::Comment(parse_text_element(ctx)))
    }
    Ok(registry)
}
