pub mod gen;
pub mod out;
mod out_file_builder;

use crate::writer::scheme as pattern;

#[derive(Debug)]
pub enum GenError {
    NonUnique(pattern::TypeUnwrapper),
    CantParsePath(String),
}

pub type GenResult<T> = Result<T, GenError>;