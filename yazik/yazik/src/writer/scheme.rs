use serde::{Serialize, Deserialize};
use std::collections::HashMap;

use crate::scheme::scheme as meta;

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum PathParts {
    Str(String),
    Nest(String),
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum Const {
    Path(Vec<PathParts>),
}

#[derive(Serialize, Deserialize, Clone, Eq, PartialEq, Hash, Debug)]
pub enum TypeMeta {
    NumberRef(u8),
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

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum MetaArg {
    Meta(TypeMeta),
    All,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum UnwrapSeq {
    NL,
    WS(u16),
    TAB,
    Str(String),
    Arg(MetaArg),
    SelfProp(String),
    Formatted(String, Vec<UnwrapSeq>),
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum TypeUnwrapper {
    Plain(String),
    Matcher(HashMap<TypeMeta, Box<TypeUnwrapper>>),
    Unwrap(Vec<UnwrapSeq>),
    Unique(Unique),
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum FormatterArg {
    Str(String),
    MetaArg,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Formatter {
    pub args: Vec<FormatterArg>,
    pub unwrap: Vec<UnwrapSeq>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Unique {
    pub id: Vec<UnwrapSeq>,
    pub out: Vec<PathParts>,
    pub pattern: Vec<UnwrapSeq>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Scheme {
    pub consts: HashMap<String, Const>,
    pub formatters: HashMap<String, Formatter>,
    pub unwrappers: HashMap<TypeMeta, TypeUnwrapper>,
}