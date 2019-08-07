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

#[derive(Serialize, Deserialize, Clone, Eq, PartialEq, Hash, Debug)]
pub enum UnwrapSeq {
    Str(String),
    MetaArg(u8),
}

#[derive(Serialize, Deserialize)]
pub enum TypeUnwrapper {
    Plain(String),
    Matcher(HashMap<TypeMeta, Box<TypeUnwrapper>>),
    Unwrap(Vec<UnwrapSeq>),
}

#[derive(Serialize, Deserialize)]
pub struct Scheme {
    pub consts: HashMap<String, String>,
    pub unwrappers: HashMap<TypeMeta, TypeUnwrapper>,
}