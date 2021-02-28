use crate::parser::Rule;
use std::{
    convert::From,
    error::Error,
    fmt::{self, Display, Formatter},
};

#[derive(Debug)]
pub enum EBNFError {
    UnexpectedRules(Vec<(Rule, String)>),
    InsufficientTokens(Vec<Option<Rule>>),
    NoTokens,
    Pest(Box<dyn Error>),
}

impl Error for EBNFError {}

impl Display for EBNFError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:#?}", self)
    }
}

impl From<pest::error::Error<Rule>> for EBNFError {
    fn from(error: pest::error::Error<Rule>) -> Self {
        EBNFError::Pest(Box::new(error))
    }
}

pub type EBNFResult<T> = Result<T, EBNFError>;
