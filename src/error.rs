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

macro_rules! impl_error {
    ($ty:ty) => {
        impl From<$ty> for EBNFError {
            fn from(error: $ty) -> Self {
                EBNFError::Pest(Box::new(error))
            }
        }
    };
}

impl_error!(pest::error::Error<Rule>);
impl_error!(std::num::ParseIntError);

pub type EBNFResult<T> = Result<T, EBNFError>;
