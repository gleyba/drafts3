pub mod parser;
pub mod spec;
pub mod fields;

#[derive(Debug)]
pub struct YError {
    description: String,
}

impl YError {
    pub fn new_raw(source: &str) -> YError {
        YError {
            description: source.to_owned(),
        }
    }
}

pub type YResult<T> = Result<T, YError>;