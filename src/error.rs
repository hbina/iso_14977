use std::{
    error::Error,
    fmt::{self, Display, Formatter},
};

#[derive(Debug)]
pub enum EBNFError {
    ExtraToken(String),
    NomError(Box<dyn Error>),
}

impl Error for EBNFError {}

impl Display for EBNFError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:#?}", self)
    }
}

pub type EBNFResult<T> = Result<T, EBNFError>;

impl From<nom::Err<nom::error::Error<&str>>> for EBNFError {
    fn from(err: nom::Err<nom::error::Error<&str>>) -> Self {
        EBNFError::NomError(Box::new(err.to_owned()))
    }
}
