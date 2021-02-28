#[macro_use]
extern crate pest_derive;

mod error;
mod parser;

pub use error::{EBNFError, EBNFResult};
pub use parser::{
    MetaIdentifier, SingleDefinition, SyntacticException, SyntacticFactor, SyntacticPrimary,
    SyntacticTerm, Syntax, SyntaxRule,
};

pub fn parse(raw: &str) -> error::EBNFResult<parser::Syntax> {
    parser::InnerParser::new(raw)
}
