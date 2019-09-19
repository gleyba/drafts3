use serde::{Serialize, Deserialize};
use std::collections::HashMap;

pub struct Out {
    pub files: HashMap<String, String>,
}