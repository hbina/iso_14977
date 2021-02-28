use crate::parser::Rrule;

#[derive(Debug)]
pub enum EBNFError {
    UnexpectedRules(Vec<(Rrule, String)>),
    InsufficientTokens(Vec<Option<Rrule>>),
    NoTokens,
    Pest(Box<dyn std::error::Error>),
}

impl std::error::Error for EBNFError {}

impl std::fmt::Display for EBNFError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#?}", self)
    }
}

macro_rules! impl_error {
    ($ty:ty) => {
        impl std::convert::From<$ty> for EBNFError {
            fn from(error: $ty) -> Self {
                EBNFError::Pest(Box::new(error))
            }
        }
    };
}

impl_error!(pest::error::Error<Rrule>);
impl_error!(std::num::ParseIntError);

pub type EBNFResult<T> = std::result::Result<T, EBNFError>;
