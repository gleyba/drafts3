use crate::scheme::scheme as data;
use crate::writer::scheme as pattern;
use crate::common::spec::*;
use crate::common::parser::*;
use crate::common::fields::FieldMap;
use std::collections::HashMap;

use super::out::*;
use super::{GenError, GenResult};
use super::out_file_builder::{OutFileBuilder, OutFilesBuilder};
use std::cell::RefCell;

struct Generator {
    spec: Spec,
    pattern: pattern::Scheme,
    out_files: OutFilesBuilder,
}

static ENUM_META: pattern::TypeMeta = pattern::TypeMeta::Enum;

impl Generator {

    fn new(spec: Spec, pattern: pattern::Scheme) -> Generator {
        Generator {
            spec: spec,
            pattern: pattern,
            out_files: OutFilesBuilder::new(),
        }
    }

    fn unwrap_path_nest(&self, nest: &str, to: &mut String) -> GenResult<()> {
        if nest.starts_with("consts.") {
            let slice = &nest[7..];
            println!("{:?}", slice);

        }
        Err(GenError::CantParsePath(nest.to_owned()))
    }

    fn file(&self, path: &Vec<pattern::PathParts>, closure: &dyn Fn(&OutFileBuilder) -> ())
     -> GenResult<()> {
        let mut path_str = String::new();
        for part in path.iter() {
            match part {
                pattern::PathParts::Str(val) => path_str += val,
                pattern::PathParts::Nest(val) => self.unwrap_path_nest(val, &mut path_str)?,
            }
        }
        self.out_files.file(&path_str, closure);
        Ok(())
    }

    fn process_enum(&self, name: &str, enum_v: &data::Enum) -> GenResult<()> {
        guard!(let Some(enum_pattern) = self.pattern.unwrappers.get(&ENUM_META)
            else { return Ok(()) });

        guard!(let pattern::TypeUnwrapper::Unique(enum_u) = enum_pattern
            else { return Err(GenError::NonUnique(enum_pattern.clone())) });

        self.file(&enum_u.out, &|fb| {

        })?;

        println!("{:?}", enum_pattern);
        Ok(())
    }

}

pub fn resolve(spec: Spec, data: data::Scheme, pattern: pattern::Scheme) -> GenResult<Out> {
    let generator = Generator::new(spec,pattern);

    for pair in data.enums.iter() {
        generator.process_enum(pair.0, pair.1)?
    }

    Ok(Out{ files: HashMap::new()})
}