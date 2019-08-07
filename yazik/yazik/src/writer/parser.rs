use std::collections::HashMap;
use std::cell::RefCell;

use pest::Parser;
use pest::iterators::{Pair, Pairs};

use crate::common::parser::*;
use crate::common::spec::*;

use super::scheme::*;
use crate::scheme::scheme;
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

type BoxedTypeUnwrapperResolver = Box<Resolver<TypeUnwrapper>>;

struct MetaAndUnwrapper {
    meta: TypeMeta,
    resolver: BoxedTypeUnwrapperResolver,
}

type MetaAndUnwrapperResult = ParserResult<MetaAndUnwrapper>;

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

fn parse_primitive(rule: &Pair<Rule>) -> Option<scheme::Primitive> {
    let result = match rule.as_rule() {
        Rule::bool => scheme::Primitive::Bool,
        Rule::i8 => scheme::Primitive::I8,
        Rule::i16 => scheme::Primitive::I16,
        Rule::i32 => scheme::Primitive::I32,
        Rule::i64 => scheme::Primitive::I64,
        Rule::f32 => scheme::Primitive::F32,
        Rule::f64 => scheme::Primitive::F64,
        _ => return None,
    };
    Some(result)
}

fn parse_predefined(rule: &Pair<Rule>) -> Option<scheme::Predefined> {
    let result = match rule.as_rule() {
        Rule::string => scheme::Predefined::String,
        Rule::date => scheme::Predefined::Date,
        Rule::binary => scheme::Predefined::Binary,
        _ => return None,
    };
    Some(result)
}

fn parse_privitive_or_predefined_meta(rule: &Pair<Rule>) -> ParserResult<TypeMeta> {
    if let Some(primitive) = parse_primitive(rule) {
        return Ok(TypeMeta::Primitive(Some(primitive)))
    } else if let Some(predefined) = parse_predefined(rule) {
        return Ok(TypeMeta::Predefined(Some(predefined)))
    }

    return rule.as_error()
}

fn parse_type_meta_non_opt(rule: &Pair<Rule>) -> ParserResult<TypeMeta> {
    let mut inner_rules = rule.clone().into_inner();
    let result = match rule.as_rule() {
        Rule::optional
            => TypeMeta::Optional(parse_type_meta_from_rules(&mut inner_rules)?.map(Box::new)),
        Rule::list
            => TypeMeta::List(parse_type_meta_from_rules(&mut inner_rules)?.map(Box::new)),
        Rule::tuple
            => TypeMeta::Tuple(parse_type_metas_from_rules(&mut inner_rules)?),
        Rule::variant
            => TypeMeta::Variant(parse_type_metas_from_rules(&mut inner_rules)?),
        Rule::dictionary
            => TypeMeta::Dictionary(parse_dictionary_meta_from_rules(&mut inner_rules)?),
        Rule::closure
            => TypeMeta::Closure(parse_closure_meta_from_rules(&mut inner_rules)?),
        Rule::enumt => TypeMeta::Enum,
        Rule::recordt => TypeMeta::Record,
        Rule::interfacet => TypeMeta::Interface,
        _ => return parse_privitive_or_predefined_meta(rule),
    };
    Ok(result)
}

fn parse_type_meta_from_rule(rule: Pair<Rule>) -> ParserResult<Option<TypeMeta>> {
    let result = match rule.as_rule() {
        Rule::any_meta => None,
        _ => Some(parse_type_meta_non_opt(&rule)?),
    };

    Ok(result)
}

fn parse_type_metas_from_rules(rules: &mut Pairs<Rule>) -> ParserResult<Option<Vec<TypeMeta>>> {
    guard!(let Some(first_rule) = rules.next()
    else { return rules.as_error() });

    let first_meta = match first_rule.as_rule() {
        Rule::any_meta => return Ok(None),
        _ => parse_type_meta_non_opt(&first_rule)?,
    };

    let mut result = vec![first_meta];

    while let Some(cur) = rules.next() {
        result.push(parse_type_meta_non_opt(&cur)?);
    }

    Ok(Some(result))
}

fn parse_type_meta_from_rules(rules: &mut Pairs<Rule>) -> ParserResult<Option<TypeMeta>> {
    guard!(let Some(meta_rule) = rules.next()
        else { return rules.as_error() });

    let result = match meta_rule.as_rule() {
        Rule::any_meta => None,
        _ => Some(parse_type_meta_non_opt(&meta_rule)?),
    };

    Ok(result)
}

fn parse_dictionary_meta_from_rules(rules: &mut Pairs<Rule>) -> ParserResult<Option<Box<(TypeMeta,TypeMeta)>>> {
    guard!(let Some(key_meta_rule) = rules.next()
        else { return rules.as_error() });

    let result = match key_meta_rule.as_rule() {
        Rule::any_meta => None,
        _ => {
            let key_meta = parse_privitive_or_predefined_meta(&key_meta_rule)?;
            guard!(let Some(value_meta_rule) = rules.next()
                else { return key_meta_rule.as_error() });
            let value_meta = parse_type_meta_non_opt(&value_meta_rule)?;
            Some(Box::new((key_meta,value_meta)))
        },
    };
    Ok(result)
}

fn parse_closure_params(rule: &Pair<Rule>) -> ParserResult<Vec<TypeMeta>> {
    let mut inner_rules = rule.clone().into_inner();
    let mut params = Vec::new();

    while let Some(cur) = inner_rules.next() {
        params.push(parse_type_meta_non_opt(&cur)?);
    }
    Ok(params)
}

fn parse_closure_return(rule: &Pair<Rule>) -> ParserResult<Option<Box<TypeMeta>>> {
    let mut inner_rules = rule.clone().into_inner();
    guard!(let Some(return_rule) = rule.clone().into_inner().next()
            else { return Ok(None) });

    parse_type_meta_non_opt(&return_rule).map(Box::new).map(Some)
}

fn parse_closure_meta_from_rules(rules: &mut Pairs<Rule>)
    -> ParserResult<Option<(Vec<TypeMeta>, Option<Box<TypeMeta>>)>> {

    guard!(let Some(params_rule) = rules.next()
        else { return rules.as_error() });

    let result = match params_rule.as_rule() {
        Rule::any_meta => None,
        Rule::closure_params => {
            guard!(let Some(return_rule) = rules.next()
                else { return rules.as_error() });
            Some((
                parse_closure_params(&params_rule)?,
                parse_closure_return(&return_rule)?
            ))
        },
        _ => return rules.as_error()
    };
    Ok(result)
}

struct PrimitiveMatcherResolver {
    primitive_map: HashMap<scheme::Primitive, String>,
}

impl Resolver<TypeUnwrapper> for PrimitiveMatcherResolver {
    fn resolve(&self, helper: &ResolveHelper) -> ParserResult<TypeUnwrapper> {
        let result: HashMap<TypeMeta, Box<TypeUnwrapper>> = self.primitive_map.clone()
            .drain()
            .map(|(k,v)| (
                TypeMeta::Primitive(Some(k)),
                Box::new(TypeUnwrapper::Plain(v))
            ))
            .collect();
        Ok(TypeUnwrapper::Matcher(result))
    }
}

fn parse_primitive_matcher(rule: &Pair<Rule>) -> MetaAndUnwrapperResult {
    use scheme::Primitive;
    let mut matcher = HashMap::new();
    let mut inner_rules = rule.clone().into_inner();
    while let Some(primitive_type) = inner_rules.next() {
        guard!(let Some(matcher_rule)  = inner_rules.next()
            else { return primitive_type.as_error() });

        guard!(let Some(primitive) = parse_primitive(&primitive_type)
            else { return primitive_type.as_error()});

        matcher.insert(primitive, matcher_rule.as_str().to_string());
    }
    for primitive in Primitive::iter() {
        if (!matcher.contains_key(primitive)) {
            return rule.as_error()
        }
    }

    Ok(MetaAndUnwrapper {
        meta: TypeMeta::Primitive(None),
        resolver: Box::new(PrimitiveMatcherResolver{ primitive_map: matcher})
    })
}

struct PredefinedMatcherResolver {
    predefined_map: HashMap<scheme::Predefined, String>,
}

impl Resolver<TypeUnwrapper> for PredefinedMatcherResolver {
    fn resolve(&self, helper: &ResolveHelper) -> ParserResult<TypeUnwrapper> {
        let result: HashMap<TypeMeta, Box<TypeUnwrapper>> = self.predefined_map.clone()
            .drain()
            .map(|(k,v)| (
                TypeMeta::Predefined(Some(k)),
                Box::new(TypeUnwrapper::Plain(v))
            ))
            .collect();
        Ok(TypeUnwrapper::Matcher(result))
    }
}

fn parse_predefined_matcher(rule: &Pair<Rule>) -> MetaAndUnwrapperResult {
    use scheme::Predefined;
    let mut matcher = HashMap::new();
    let mut inner_rules = rule.clone().into_inner();
    while let Some(predefined_type) = inner_rules.next() {
        guard!(let Some(matcher_rule)  = inner_rules.next()
            else { return predefined_type.as_error() });

        guard!(let Some(predefined) = parse_predefined(&predefined_type)
            else { return predefined_type.as_error()});

        matcher.insert(predefined, matcher_rule.as_str().to_string());
    }
    for predefined in Predefined::iter() {
        if (!matcher.contains_key(predefined)) {
            return rule.as_error()
        }
    }

    Ok(MetaAndUnwrapper {
        meta: TypeMeta::Predefined(None),
        resolver: Box::new(PredefinedMatcherResolver{ predefined_map: matcher})
    })
}

fn parse_unwrap_rule(rule: &Pair<Rule>) -> MetaAndUnwrapperResult {
    let mut inner_rules = rule.clone().into_inner();

    guard!(let Some(meta) = parse_type_meta_from_rules(&mut inner_rules)?
        else { return rule.as_error() });

    Err(ParseError::new_raw("unimplemented"))
}

struct SchemeBuilder<'a> {
    spec: Spec,
    consts: RefCell<HashMap<String, ConstResolver<'a>>>,
    unwrapper_resolvers: RefCell<HashMap<TypeMeta, BoxedTypeUnwrapperResolver>>,
}

impl<'i> SchemeBuilder<'i> {

    fn new(spec: Spec) -> SchemeBuilder<'i> {
        SchemeBuilder {
            spec: spec,
            consts: RefCell::new(HashMap::new()),
            unwrapper_resolvers: RefCell::new(HashMap::new()),
        }
    }

    fn process_consts(&'i self, rule: Pair<'i, Rule>) -> ParserResult<()> {
        let consts = parse_consts(&rule)?;
        self.consts.borrow_mut().extend(consts);
        Ok(())
    }

   fn add_parsed(&'i self, mr_pair: MetaAndUnwrapper) {
        self.unwrapper_resolvers.borrow_mut().insert(
            mr_pair.meta,
            mr_pair.resolver
        );
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
            Rule::consts
                => scheme_builder.process_consts(rule)?,
            Rule::matcher_primitive
                => scheme_builder.add_parsed(parse_primitive_matcher(&rule)?),
            Rule::matcher_predefined
                => scheme_builder.add_parsed(parse_predefined_matcher(&rule)?),
            Rule::unwrap
                => scheme_builder.add_parsed(parse_unwrap_rule(&rule)?),
            Rule::EOI => (),
            _ => return rule.as_error()
        }
    }

    let mut consts = HashMap::new();
    for (name, rr) in scheme_builder.consts.borrow().iter() {
        consts.insert(name.clone(), rr.resolve(&scheme_builder)?);
    }

    let mut unwrappers = HashMap::new();
    for (meta, rr) in scheme_builder.unwrapper_resolvers.borrow().iter()  {
        unwrappers.insert(meta.clone(), rr.resolve(&scheme_builder)?);
    }

    let result = Scheme {
        consts: consts,
        unwrappers: unwrappers,
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