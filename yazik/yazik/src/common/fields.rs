use serde::ser::{
    Serialize,
    Serializer,
    SerializeMap,
};
use serde::de::{
    Deserialize,
    Deserializer,
    DeserializeOwned,
    Visitor,
    MapAccess,
};
use serde_value::Value;

use std::fmt;
use std::marker::PhantomData;
use std::collections::HashMap;
use std::cell::RefCell;
use std::any::Any;

use super::{YError, YResult};
use serde::export::fmt::Debug;


pub struct FieldMap<T> where T: Serialize {
    data: HashMap<String, T>,
    values: RefCell<HashMap<* const dyn Any, Value>>,
}

impl<T> FieldMap<T> where T: Serialize {
    pub fn new(data: HashMap<String, T>) -> FieldMap<T> {
        FieldMap {
            data: data,
            values: RefCell::new(HashMap::new()),
        }
    }
}

impl<T> Serialize for FieldMap<T> where T: Serialize {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(self.data.len()))?;
        for (k, v) in &self.data {
            map.serialize_entry(&k.to_string(), &v)?;
        }
        map.end()
    }
}

struct FieldMapVisitor<V> where V: Serialize {
    marker: PhantomData<fn() -> FieldMap<V>>
}

impl<V> FieldMapVisitor<V> where V: Serialize {
    fn new() -> Self {
        FieldMapVisitor {
            marker: PhantomData
        }
    }
}

impl<'de, V> Visitor<'de> for FieldMapVisitor<V>
where
    V: Serialize + Deserialize<'de>,
{
    type Value = FieldMap<V>;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("FieldMap")
    }

    fn visit_map<M>(self, mut access: M) -> Result<Self::Value, M::Error>
    where
        M: MapAccess<'de>,
    {

        let mut map = HashMap::with_capacity(access.size_hint().unwrap_or(0));
        while let Some((key, value)) = access.next_entry()? {
            map.insert(key, value);
        }

        Ok(FieldMap::new(map))
    }
}

impl<'de, V> Deserialize<'de> for FieldMap<V>
where
    V: Serialize + Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_map(FieldMapVisitor::new())
    }
}

impl<V> fmt::Debug for FieldMap<V> where V: Serialize + Debug {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.data)
    }
}

pub fn get_field_by_name<T, R>(data: T, field: &str) -> YResult<R>
where
    T: Serialize,
    R: DeserializeOwned,
{
    guard!(let Ok(Value::Map(mut map)) = serde_value::to_value(data)
        else { return Err(YError::new_raw("expected a struct")) });

    let key = Value::String(field.to_owned());

    guard!(let Some(value) = map.remove(&key)
        else { return Err(YError::new_raw("no such field")) });

    match R::deserialize(value) {
        Ok(r) => Ok(r),
        Err(_) => Err(YError::new_raw("wrong type?")),
    }
}