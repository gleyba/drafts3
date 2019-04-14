use std::collections::HashMap;
use std::slice::Iter;
use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Clone, Eq, PartialEq, Hash, Debug)]
pub enum Primitive {
    Bool,
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
}

impl Primitive {
    pub fn iter() -> Iter<'static, Primitive> {
        use Primitive::*;
        static PRIMITIVES: [Primitive;  7] = [Bool, I8, I16, I32, I64, F32, F64];
        PRIMITIVES.into_iter()
    }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Enum {
    pub name: String,
    pub options: Vec<String>,
    pub hash: String,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Source {
    pub description: String,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct CustomTypeRef {
    pub ident: String,
}

#[derive(Serialize, Deserialize, Clone, Eq, PartialEq, Hash, Debug)]
pub enum Predefined {
    String,
    Date,
    Binary
}

impl Predefined {
    pub fn iter() -> Iter<'static, Predefined> {
        use Predefined::*;
        static PREDEFINED: [Predefined;  3] = [String, Date, Binary];
        PREDEFINED.into_iter()
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub enum DictionaryKeyType {
    Primitive(Primitive),
    Predefined(Predefined),
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Record {
    pub name: String,
    pub fields: Vec<(String,DataType)>,
    pub hash: String,
}

pub trait TypeTrait{}

#[derive(Serialize, Deserialize, Debug)]
pub enum DataType {
    Primitive(Primitive),
    Predefined(Predefined),
    Optional(Box<DataType>),
    List(Box<DataType>),
    Tuple(Vec<DataType>),
    Variant(Vec<DataType>),
    Dictionary((DictionaryKeyType, Box<DataType>)),
    Enum(String),
    Record(String),
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Method {
    pub name: String,
    pub params: Vec<(String, CrossType)>,
    pub return_type: Option<CrossType>,
    pub is_static: bool,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Property {
    pub name: String,
    pub yazik_type: CrossType,
    pub has_setter: bool,
    pub is_static: bool,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Interface {
    pub name: String,
    pub properties: Vec<(String,Property)>,
    pub methods: Vec<(String,Method)>,
    pub hash: String,
}

#[derive(Serialize, Deserialize, Debug)]
pub enum CrossType {
    Primitive(Primitive),
    Predefined(Predefined),
    Optional(Box<CrossType>),
    List(Box<CrossType>),
    Tuple(Vec<CrossType>),
    Variant(Vec<CrossType>),
    Dictionary((DictionaryKeyType, Box<CrossType>)),
    Closure((Vec<(String, CrossType)>, Option<Box<CrossType>>)),
    Enum(String),
    Record(String),
    Interface(String),
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Scheme {
    pub enums: HashMap<String, Enum>,
    pub records: HashMap<String, Record>,
    pub interfaces: HashMap<String, Interface>,
}