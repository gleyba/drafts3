use serde::Serialize;
use serde::de::DeserializeOwned;
use serde_value::Value;

use pest::iterators::{Pair, Pairs};
use pest::RuleType;
use pest::error::Error as PestError;

use crypto::md5::Md5;
use crypto::digest::Digest;

#[derive(Debug)]
pub struct ParseError {
    pub source: String,
}

pub type ParserResult<T> = Result<T, ParseError>;

impl ParseError {

    pub fn new_raw(source: &str) -> ParseError {
        ParseError {
            source: source.to_string(),
        }
    }

    pub fn new<R: RuleType>(rule: &Pair<R>) -> ParseError {
        ParseError {
            source: format!("{:?}", rule.as_span()),
        }
    }

    pub fn new_from_pest_error<R: RuleType>(pest_error: PestError<R>) -> ParseError {
        ParseError {
            source: format!("{:?}", pest_error),
        }
    }
}

pub trait ForPair <'a> {
    fn as_error<T>(&'a self) -> Result<T, ParseError>;

    fn md5_hash(&'a self) -> String;
}

impl <'i, R: RuleType>ForPair<'i> for Pair<'i, R> {
    fn as_error<T>(&'i self) -> Result<T, ParseError> {
        Err(ParseError::new(&self))
    }

    fn md5_hash(&'i self) -> String {
        let rule_str = self.as_str();
        let mut hasher = Md5::new();
        hasher.input_str(rule_str);
        hasher.result_str()
    }
}

pub trait ForPairs <'a, R: RuleType> {
    fn as_error<T>(&'a self) -> Result<T, ParseError>;
    fn next_unwrap(&'a mut self) -> ParserResult<Pair<'a, R>>;
}

impl <'i, R: RuleType>ForPairs<'i, R> for Pairs<'i, R> {
    fn as_error<T>(&'i self) -> Result<T, ParseError> {
        Err(ParseError::new_raw(self.as_str()))
    }
    fn next_unwrap(&'i mut self) -> ParserResult<Pair<'i, R>> {
        guard!(let Some(rule) = self.next()
            else { return self.as_error() });

        Ok(rule)
    }
}

pub fn get_field_by_name<T, R>(data: T, field: &str) -> ParserResult<R>
where
    T: Serialize,
    R: DeserializeOwned,
{
    guard!(let Ok(Value::Map(mut map)) = serde_value::to_value(data)
        else { return Err(ParseError::new_raw("expected a struct")) });

    let key = Value::String(field.to_owned());

    guard!(let Some(value) = map.remove(&key)
        else { return Err(ParseError::new_raw("no such field")) });

    match R::deserialize(value) {
        Ok(r) => Ok(r),
        Err(_) => Err(ParseError::new_raw("wrong type?")),
    }
}