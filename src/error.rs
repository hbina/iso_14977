use std::{
    convert::From,
    error::Error,
    fmt::{self, Display, Formatter},
};

#[derive(Debug)]
pub enum EBNFError {
    NoTokens,
    Pest(Box<dyn Error>),
}

impl Error for EBNFError {}

impl Display for EBNFError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:#?}", self)
    }
}

pub type EBNFResult<T> = Result<T, EBNFError>;
