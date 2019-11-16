use pest::iterators::{Pair, Pairs};
use pest::RuleType;
use pest::error::Error as PestError;

use crypto::md5::Md5;
use crypto::digest::Digest;

use super::{YError, YResult};

impl YError {
    pub fn new<R: RuleType>(rule: &Pair<R>) -> YError {
        YError {
            description: format!("{:?}", rule.as_span()),
        }
    }

    pub fn new_from_pest_error<R: RuleType>(pest_error: PestError<R>) -> YError {
        YError {
            description: format!("{:?}", pest_error),
        }
    }
}

pub trait ForPair <'a> {
    fn as_error<T>(&'a self) -> YResult<T>;
    fn md5_hash(&'a self) -> String;
}

impl <'i, R: RuleType>ForPair<'i> for Pair<'i, R> {
    fn as_error<T>(&'i self) -> YResult<T> {
        Err(YError::new(&self))
    }

    fn md5_hash(&'i self) -> String {
        let rule_str = self.as_str();
        let mut hasher = Md5::new();
        hasher.input_str(rule_str);
        hasher.result_str()
    }
}

pub trait ForPairs <'a, R: RuleType> {
    fn as_error<T>(&'a self) -> YResult<T>;
    fn next_unwrap(&'a mut self) -> YResult<Pair<'a, R>>;
}

impl <'i, R: RuleType>ForPairs<'i, R> for Pairs<'i, R> {
    fn as_error<T>(&'i self) -> YResult<T> {
        Err(YError::new_raw(self.as_str()))
    }
    fn next_unwrap(&'i mut self) -> YResult<Pair<'i, R>> {
        guard!(let Some(rule) = self.next()
            else { return self.as_error() });

        Ok(rule)
    }
}

