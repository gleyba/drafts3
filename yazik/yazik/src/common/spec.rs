use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize)]
pub struct Spec {
    pub ns: String,
}
