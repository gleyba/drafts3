use serde::{Serialize, Deserialize};
use std::collections::HashMap;

use crate::scheme::scheme;

pub type PrimitiveMatcher = HashMap<scheme::Primitive, String>;
pub type PredefinedMatcher = HashMap<scheme::Predefined, String>;

#[derive(Serialize, Deserialize)]
pub struct Scheme {
    pub consts: HashMap<String, String>,
    pub primitives: PrimitiveMatcher,
    pub predefined: PredefinedMatcher,
}