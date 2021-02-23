#[macro_use]
extern crate pest_derive;

mod parser;

pub use parser::{
    MetaIdentifier, SingleDefinition, SyntacticException, SyntacticFactor, SyntacticPrimary,
    SyntacticTerm, Syntax, SyntaxRule,
};

pub fn parse(raw: &str) -> parser::EBNFResult<parser::Syntax> {
    parser::InnerParser::new(raw)
}
