use serde::{Serialize, Deserialize};
use std::collections::HashMap;

use crate::scheme::scheme as meta;

#[derive(Serialize, Deserialize, Clone, Eq, PartialEq, Hash, Debug)]
pub enum TypeMeta {
    Primitive(Option<meta::Primitive>),
    Predefined(Option<meta::Predefined>),
    Optional(Option<Box<TypeMeta>>),
    List(Option<Box<TypeMeta>>),
    Tuple(Option<Vec<TypeMeta>>),
    Variant(Option<Vec<TypeMeta>>),
    Dictionary(Option<Box<(TypeMeta,TypeMeta)>>),
    Closure(Option<(Vec<TypeMeta>, Option<Box<TypeMeta>>)>),
    Enum,
    Record,
    Interface,
}

#[derive(Serialize, Deserialize, Clone)]
pub enum MetaArg {
    Number(u8),
    All,
}

#[derive(Serialize, Deserialize)]
pub enum UnwrapSeq {
    Str(String),
    Arg(MetaArg),
    SelfProp(String),
    Formatted(String, Vec<UnwrapSeq>),
}

#[derive(Serialize, Deserialize)]
pub struct Unique {
    id: Vec<UnwrapSeq>,
    out: String,
    pattern: Vec<UnwrapSeq>,
}

#[derive(Serialize, Deserialize)]
pub enum TypeUnwrapper {
    Plain(String),
    Matcher(HashMap<TypeMeta, Box<TypeUnwrapper>>),
    Unwrap(Vec<UnwrapSeq>),
    Unique(Unique),
}

#[derive(Serialize, Deserialize, Clone)]
pub enum FormatterArg {
    Str(String),
    MetaArg,
}

#[derive(Serialize, Deserialize)]
pub struct Formatter {
    pub args: Vec<FormatterArg>,
    pub unwrap: Vec<UnwrapSeq>,
}

#[derive(Serialize, Deserialize)]
pub struct Scheme {
    pub consts: HashMap<String, String>,
    pub formatters: HashMap<String, Formatter>,
    pub unwrappers: HashMap<TypeMeta, TypeUnwrapper>,
}