use std::collections::HashMap;
use std::cell::RefCell;

use pest::Parser;
use pest::iterators::{Pair, Pairs};

use crate::common::parser::*;
use crate::common::spec::*;

use super::scheme::*;
use crate::scheme::scheme;
use crate::scheme::scheme::DataType::Primitive;
use core::borrow::Borrow;

#[derive(Parser)]
#[grammar = "writer/parser.pest"]
struct SchemeParser;

trait ResolveHelper {
    fn get_str_from_spec(&self, def: &str) -> ParserResult<String>;
}

trait Resolver<T> {
    fn resolve(&self, helper: &ResolveHelper) -> ParserResult<T>;
}

struct NestCallResolver<'a> {
    rule: Pair<'a, Rule>,
}

impl<'i> Resolver<String> for NestCallResolver<'i> {
    fn resolve(&self, helper: &ResolveHelper) -> ParserResult<String> {
        let rule_str = self.rule.as_str();
        if (rule_str.starts_with("spec.")) {
            return helper.get_str_from_spec(&rule_str[5..]);
        }
        self.rule.as_error()
    }
}

fn parse_nest_call<'a>(rule: &Pair<'a, Rule>) -> ParserResult<NestCallResolver<'a>> {
    let mut inner_rules = rule.clone().into_inner();

    guard!(let Some(def_rule) = inner_rules.next()
        else { return rule.as_error() });

    Ok(NestCallResolver { rule: def_rule })
}

enum PathResolverParts<'a> {
    Str(String),
    Nest(NestCallResolver<'a>),
}

struct PathResolver<'a> {
    rule: Pair<'a, Rule>,
    parts: Vec<PathResolverParts<'a>>,
}

impl<'i> Resolver<String> for PathResolver<'i> {
    fn resolve(&self, helper: &ResolveHelper) -> ParserResult<String> {
        use PathResolverParts::*;
        let mut result = String::new();
        for part in self.parts.iter() {
            match part {
                Str(val) => result+= val,
                Nest(resolver) => result += resolver.resolve(helper)?.as_str(),
            }
        }
        Ok(result)
    }
}

fn parse_path<'a>(rule: &Pair<'a, Rule>) -> ParserResult<PathResolver<'a>> {
    let mut parts: Vec<PathResolverParts> = Vec::new();

    let mut cur_plain_part = String::new();

    let mut inner_rules = rule.clone().into_inner();
    while let Some(cur) = inner_rules.next() {
       match cur.as_rule() {
           Rule::path_str => cur_plain_part += cur.as_str(),
           Rule::nest_call => {
               if !cur_plain_part.is_empty() {
                   parts.push(PathResolverParts::Str(cur_plain_part));
                   cur_plain_part = String::new();
               }
               parts.push(PathResolverParts::Nest(parse_nest_call(&cur)?));
           },
           _ =>  return rule.as_error()
       }
    }
    if !cur_plain_part.is_empty() {
        parts.push(PathResolverParts::Str(cur_plain_part));
    }
    Ok(PathResolver{ rule: rule.clone(), parts: parts })
}

enum ConstResolver<'a> {
    Path(PathResolver<'a>)
}

impl<'i> Resolver<String> for ConstResolver<'i> {
    fn resolve(&self, helper: &ResolveHelper) -> ParserResult<String> {
        match self {
            ConstResolver::Path(r) => r.resolve(helper)
        }
    }
}

fn parse_const_value<'a>(rule: &Pair<'a, Rule>) -> ParserResult<ConstResolver<'a>> {
    match rule.as_rule() {
        Rule::path => Ok(ConstResolver::Path(parse_path(rule)?)),
        _ =>  rule.as_error()
    }
}

fn parse_const<'a>(rule: &Pair<'a, Rule>) -> ParserResult<(String, ConstResolver<'a>)> {
    let mut inner_rules = rule.clone().into_inner();

    guard!(let Some(name_rule) = inner_rules.next()
        else { return rule.as_error() });

    guard!(let Some(value_rule) = inner_rules.next()
        else { return rule.as_error() });

    Ok((
        name_rule.as_str().to_string(),
        parse_const_value(&value_rule)?,
    ))
}

fn parse_consts<'a>(rule: &Pair<'a, Rule>) -> ParserResult<HashMap<String, ConstResolver<'a>>> {
    let mut defs: HashMap<String, ConstResolver<'a>> = HashMap::new();
    let mut inner_rules = rule.clone().into_inner();
    while let Some(cur) = inner_rules.next() {
        let const_def = parse_const(&cur)?;
        defs.insert(const_def.0, const_def.1);
    }
    Ok(defs)
}


fn parse_primitive_matcher(rule: &Pair<Rule>) -> ParserResult<PrimitiveMatcher> {
    use scheme::Primitive;
    let mut matcher = HashMap::new();
    let mut inner_rules = rule.clone().into_inner();
    while let Some(primitive_type) = inner_rules.next() {
        guard!(let Some(matcher_rule)  = inner_rules.next()
            else { return primitive_type.as_error() });

        let primitive = match primitive_type.as_rule() {
            Rule::bool => Primitive::Bool,
            Rule::i8 => Primitive::I8,
            Rule::i16 => Primitive::I16,
            Rule::i32 => Primitive::I32,
            Rule::i64 => Primitive::I64,
            Rule::f32 => Primitive::F32,
            Rule::f64 => Primitive::F64,
            _ => return primitive_type.as_error()
        };

        matcher.insert(primitive, matcher_rule.as_str().to_string());
    }
    for primitive in Primitive::iter() {
        if (!matcher.contains_key(primitive)) {
            return rule.as_error()
        }
    }
    Ok(matcher)
}

fn parse_predefined_matcher(rule: &Pair<Rule>) -> ParserResult<PredefinedMatcher> {
    use scheme::Predefined;
    let mut matcher = HashMap::new();
    let mut inner_rules = rule.clone().into_inner();
    while let Some(predefined_type) = inner_rules.next() {
        guard!(let Some(matcher_rule)  = inner_rules.next()
            else { return predefined_type.as_error() });

        let predefined = match predefined_type.as_rule() {
            Rule::string => Predefined::String,
            Rule::date => Predefined::Date,
            Rule::binary => Predefined::Binary,
            _ => return predefined_type.as_error()
        };

        matcher.insert(predefined, matcher_rule.as_str().to_string());
    }
    for predefined in Predefined::iter() {
        if (!matcher.contains_key(predefined)) {
            return rule.as_error()
        }
    }
    Ok(matcher)
}

struct SchemeBuilder<'a> {
    spec: Spec,
    consts: RefCell<HashMap<String, ConstResolver<'a>>>,
    primitive_matcher:  RefCell<Option<PrimitiveMatcher>>,
    predefined_matcher: RefCell<Option<PredefinedMatcher>>,
}

impl<'i> SchemeBuilder<'i> {

    fn new(spec: Spec) -> SchemeBuilder<'i> {
        SchemeBuilder {
            spec: spec,
            consts: RefCell::new(HashMap::new()),
            primitive_matcher: RefCell::new(None),
            predefined_matcher: RefCell::new(None),
        }
    }

    fn process_consts(&'i self, rule: Pair<'i, Rule>) -> ParserResult<()> {
        let consts = parse_consts(&rule)?;
        self.consts.borrow_mut().extend(consts);
        Ok(())
    }

    fn process_primitive_matcher_rule(&'i self, rule: Pair<'i, Rule>) -> ParserResult<()> {
        self.primitive_matcher.replace(Some(parse_primitive_matcher(&rule)?));
        Ok(())
    }

    fn process_predefined_matcher_rule(&'i self, rule: Pair<'i, Rule>) -> ParserResult<()> {
        self.predefined_matcher.replace(Some(parse_predefined_matcher(&rule)?));
        Ok(())
    }
}

impl<'i> ResolveHelper for SchemeBuilder<'i> {
    fn get_str_from_spec(&self, def: &str) -> ParserResult<String> {
        get_field_by_name(&self.spec, def)
    }
}

fn build_scheme(mut root_rules: Pairs<Rule>, spec: Spec) -> ParserResult<Scheme> {
    guard!(let Some(root) = root_rules.next()
        else { return root_rules.as_error() });

    let mut scheme_builder = SchemeBuilder::new(spec);
    for rule in root.clone().into_inner() {
        match rule.as_rule() {
            Rule::consts => scheme_builder.process_consts(rule)?,
            Rule::matcher_primitive => scheme_builder.process_primitive_matcher_rule(rule)?,
            Rule::matcher_predefined => scheme_builder.process_predefined_matcher_rule(rule)?,
            Rule::EOI => (),
            _ => return rule.as_error()
        }
    }

    let mut consts = HashMap::new();
    for (name, rr) in scheme_builder.consts.borrow_mut().drain() {
        consts.insert(name, rr.resolve(&scheme_builder)?);
    }

    guard!(let Some(primitives) = scheme_builder.primitive_matcher.borrow().clone()
        else { return Err(ParseError::new_raw("No primitives")) });

    guard!(let Some(predefined) = scheme_builder.predefined_matcher.borrow().clone()
        else { return Err(ParseError::new_raw("No predefined")) });

    let result = Scheme {
        consts: consts,
        primitives: primitives,
        predefined: predefined,
    };

    Ok(result)
}

pub fn parse(data: &str, spec: Spec) -> ParserResult<Scheme>  {
    let parse_result = SchemeParser::parse(Rule::scheme, data);
    let root_rules = match parse_result {
        Ok(rules) => rules,
        Err(e) => return Err(ParseError::new_from_pest_error(e))
    };

    build_scheme(root_rules, spec)
}