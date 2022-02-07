use std::{
    error::Error,
    fmt::{self, Display, Formatter},
};

#[derive(Debug)]
pub enum EbnfError {
    ExtraToken(String),
    NomError(Box<dyn Error>),
}

impl Error for EbnfError {}

impl Display for EbnfError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:#?}", self)
    }
}

pub type EbnfResult<T> = Result<T, EbnfError>;

impl From<nom::Err<nom::error::Error<&str>>> for EbnfError {
    fn from(err: nom::Err<nom::error::Error<&str>>) -> Self {
        EbnfError::NomError(Box::new(err.to_owned()))
    }
}
