use std::collections::HashMap;
use std::cell::RefCell;

use pest::Parser;
use pest::iterators::{Pair, Pairs};

use crate::common::parser::*;

use super::scheme::*;
use crate::scheme::scheme;

#[derive(Parser)]
#[grammar = "writer/parser.pest"]
struct SchemeParser;

trait Resolver<T> {
    fn resolve(&self) -> ParserResult<T>;
}

type BoxedTypeUnwrapperResolver = Box<Resolver<TypeUnwrapper>>;

struct MetaAndUnwrapper {
    meta: TypeMeta,
    resolver: BoxedTypeUnwrapperResolver,
}

type MetaAndUnwrapperResult = ParserResult<MetaAndUnwrapper>;

fn parse_path_nest_call(rule: &Pair<Rule>) -> ParserResult<String> {
    let mut inner_rules = rule.clone().into_inner();

    guard!(let Some(def_rule) = inner_rules.next()
        else { return rule.as_error() });

    Ok(def_rule.as_str().to_owned())
}

struct PathResolver {
    parts: Vec<PathParts>,
}

impl Resolver<Vec<PathParts>> for PathResolver {
    fn resolve(&self) -> ParserResult<Vec<PathParts>> {
        Ok(self.parts.clone())
    }
}

fn parse_path(rule: &Pair<Rule>) -> ParserResult<PathResolver> {
    let mut parts: Vec<PathParts> = Vec::new();

    let mut cur_plain_part = String::new();

    let mut inner_rules = rule.clone().into_inner();
    while let Some(cur) = inner_rules.next() {
       match cur.as_rule() {
           Rule::path_str => cur_plain_part += cur.as_str(),
           Rule::path_nest_call => {
               if !cur_plain_part.is_empty() {
                   parts.push(PathParts::Str(cur_plain_part));
                   cur_plain_part = String::new();
               }
               parts.push(PathParts::Nest(parse_path_nest_call(&cur)?));
           },
           _ =>  return rule.as_error()
       }
    }
    if !cur_plain_part.is_empty() {
        parts.push(PathParts::Str(cur_plain_part));
    }
    Ok(PathResolver{ parts: parts })
}

enum ConstResolver {
    Path(PathResolver)
}

impl Resolver<Const> for ConstResolver {
    fn resolve(&self) -> ParserResult<Const> {
        let result = match self {
            ConstResolver::Path(r) => Const::Path(r.resolve()?)
        };
        Ok(result)
    }
}

fn parse_const_value(rule: &Pair<Rule>) -> ParserResult<ConstResolver> {
    match rule.as_rule() {
        Rule::path => Ok(ConstResolver::Path(parse_path(rule)?)),
        _ =>  rule.as_error()
    }
}

fn parse_const(rule: &Pair<Rule>) -> ParserResult<(String, ConstResolver)> {
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

fn parse_consts(rule: &Pair<Rule>) -> ParserResult<HashMap<String, ConstResolver>> {
    let mut defs: HashMap<String, ConstResolver> = HashMap::new();
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
        Rule::number_ref => {
            guard!(let Ok(value) = rule.as_str().parse::<u8>()
                else { return rule.as_error() });

            TypeMeta::NumberRef(value)
        },
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
    fn resolve(&self) -> ParserResult<TypeUnwrapper> {
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
        if !matcher.contains_key(primitive) {
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
    fn resolve(&self) -> ParserResult<TypeUnwrapper> {
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
        if !matcher.contains_key(predefined) {
            return rule.as_error()
        }
    }

    Ok(MetaAndUnwrapper {
        meta: TypeMeta::Predefined(None),
        resolver: Box::new(PredefinedMatcherResolver{ predefined_map: matcher})
    })
}

struct MetaArgResolver {
    value: MetaArg,
}

impl MetaArgResolver {
    fn new_boxed(value: MetaArg) -> Box<MetaArgResolver> {
        Box::new(MetaArgResolver{ value: value })
    }
}

impl Resolver<UnwrapSeq> for MetaArgResolver {
    fn resolve(&self) -> ParserResult<UnwrapSeq> {
        Ok(UnwrapSeq::Arg(self.value.clone()))
    }
}

fn parse_meta_arg(rule: Pair<Rule>) -> ParserResult<Box<dyn Resolver<UnwrapSeq>>> {
    guard!(let Some(meta_rule) = rule.clone().into_inner().next()
        else { return rule.as_error() });

    let result = match meta_rule.as_rule() {
        Rule::any_meta => MetaArgResolver::new_boxed(MetaArg::All),
        _ => MetaArgResolver::new_boxed(MetaArg::Meta(parse_type_meta_non_opt(&meta_rule)?)),
    };

    Ok(result)
}

struct SeqResolver {
    value: UnwrapSeq,
}

impl SeqResolver {
    fn new_boxed(value: UnwrapSeq) -> Box<SeqResolver> {
        Box::new(SeqResolver{ value: value })
    }
    fn new_boxed_nl() -> Box<SeqResolver> {
        Self::new_boxed(UnwrapSeq::NL)
    }
    fn new_boxed_ws(count: u16) -> Box<SeqResolver> {
        Self::new_boxed(UnwrapSeq::WS(count))
    }
    fn new_boxed_tab() -> Box<SeqResolver> {
        Self::new_boxed(UnwrapSeq::TAB)
    }
    fn new_boxed_str(value: &str) -> Box<SeqResolver> {
        Self::new_boxed(UnwrapSeq::Str(value.to_owned()))
    }
    fn new_boxed_self_prop(value: &str) -> Box<SeqResolver> {
        Self::new_boxed(UnwrapSeq::SelfProp(value.to_owned()))
    }
}

impl Resolver<UnwrapSeq> for SeqResolver {
    fn resolve(&self) -> ParserResult<UnwrapSeq> {
        Ok(self.value.clone())
    }
}

fn parse_formatted_args(rule: Pair<Rule>)
-> ParserResult<Vec<Box<dyn Resolver<UnwrapSeq>>>> {
    let mut inner_rules = rule.clone().into_inner();
    let mut result = Vec::new();
    while let Some(rule) = inner_rules.next() {
        let resolver = match rule.as_rule() {
            Rule::nest_call_formatter => parse_formatted_seq(rule)?,
            Rule::nest_call_meta_or_any => parse_meta_arg(rule)?,
            Rule::ident => SeqResolver::new_boxed_self_prop(rule.as_str()),
            Rule::nest_call_formatter_arg_str => SeqResolver::new_boxed_str(rule.as_str()),
            _ => return rule.as_error(),
        };
        result.push(resolver);
    }
    Ok(result)
}

struct FormattedResolver {
    name: String,
    args_resolver: Vec<Box<dyn Resolver<UnwrapSeq>>>,
}

impl Resolver<UnwrapSeq> for FormattedResolver {
    fn resolve(&self) -> ParserResult<UnwrapSeq> {
        let mut args = Vec::new();
        for rr in self.args_resolver.iter() {
            args.push(rr.resolve()?);
        };

        Ok(UnwrapSeq::Formatted(
            self.name.clone(),
            args
        ))
    }
}

fn parse_formatted_seq(rule: Pair<Rule>)
-> ParserResult<Box<dyn Resolver<UnwrapSeq>>> {
    let mut inner_rules = rule.clone().into_inner();

    guard!(let Some(name) = inner_rules.next()
        else { return rule.as_error() });

    guard!(let Some(args) = inner_rules.next()
        else { return rule.as_error() });

    Ok(Box::new(FormattedResolver{
        name: name.as_str().to_owned(),
        args_resolver: parse_formatted_args(args)?
    }))
}

fn parse_unwrap_seq_resolver_from_nest_call(rule: Pair<Rule>)
-> ParserResult<Box<dyn Resolver<UnwrapSeq>>> {
    let mut inner_rules = rule.clone().into_inner();
    guard!(let Some(nest_inner) = inner_rules.next()
        else { return rule.as_error() });


    let result: Box<dyn Resolver<UnwrapSeq>> = match nest_inner.as_rule() {
        Rule::nest_call_formatter => parse_formatted_seq(nest_inner)?,
        Rule::ident => SeqResolver::new_boxed_self_prop(nest_inner.as_str()),
        _ => return nest_inner.as_error(),
    };
    Ok(result)
}

struct UnwrapSeqResolver {
    inner: Vec<Box<dyn Resolver<UnwrapSeq>>>,
}

impl Resolver<TypeUnwrapper> for UnwrapSeqResolver {
    fn resolve(&self) -> ParserResult<TypeUnwrapper> {
        Ok(TypeUnwrapper::Unwrap(self::Resolver::<Vec<UnwrapSeq>>::resolve(self)?))
    }
}

impl Resolver<Vec<UnwrapSeq>> for UnwrapSeqResolver {
    fn resolve(&self) -> ParserResult<Vec<UnwrapSeq>> {
        let mut result = Vec::new();
        for rr in self.inner.iter() {
            result.push(rr.resolve()?);
        }
        Ok(result)
    }
}

fn parse_unwrap_seq(rule: Pair<Rule>) -> ParserResult<UnwrapSeqResolver> {
    let mut inner_rules = rule.clone().into_inner();
    let mut inner: Vec<Box<dyn Resolver<UnwrapSeq>>> = Vec::new();

    let mut ws_row_count = 0;
    while let Some(rule) = inner_rules.next() {
        let rule_type = rule.as_rule();
        match rule_type {
            Rule::pattern_ws => ws_row_count += 1,
            _ => {
                if ws_row_count > 0 {
                    inner.push(SeqResolver::new_boxed_ws(ws_row_count));
                    ws_row_count = 0;
                }
                match rule_type {
                    Rule::pattern_any_seq_ch
                        => inner.push(SeqResolver::new_boxed_str(rule.as_str())),
                    Rule::nest_call_any_seq
                        => inner.push(SeqResolver::new_boxed_str(rule.as_str())),
                    Rule::pattern_nl
                        => inner.push(SeqResolver::new_boxed_nl()),
                    Rule::pattern_tab
                        => inner.push(SeqResolver::new_boxed_tab()),
                    Rule::nest_call
                        => inner.push(parse_unwrap_seq_resolver_from_nest_call(rule)?),
                    _ => return rule.as_error(),
                }
            }
        }
    }

//    check_str_set();

    Ok(UnwrapSeqResolver {inner: inner} )
}


fn parse_unwrap_rule(rule: &Pair<Rule>) -> MetaAndUnwrapperResult {
    let mut inner_rules = rule.clone().into_inner();

    guard!(let Some(meta) = parse_type_meta_from_rules(&mut inner_rules)?
        else { return rule.as_error() });

    guard!(let Some(unwrap) = inner_rules.next()
        else { return rule.as_error() });

    Ok(MetaAndUnwrapper{
        meta: meta,
        resolver: parse_unwrap_seq(unwrap).map(Box::new)?,
    })
}

fn parse_formatter_args(rule: Pair<Rule>) -> ParserResult<Vec<FormatterArg>> {
    let mut inner_rules = rule.clone().into_inner();
    let mut result = Vec::new();

    while let Some(rule) = inner_rules.next() {
        let arg_str = rule.as_str().to_owned();
        if arg_str == "_" {
            result.push(FormatterArg::MetaArg);
        } else {
            result.push(FormatterArg::Str(arg_str));
        }
    }

    Ok(result)
}

struct FormatterResolver {
    args: Vec<FormatterArg>,
    unwrap: UnwrapSeqResolver,
}

impl Resolver<Formatter> for FormatterResolver {
    fn resolve(&self) -> ParserResult<Formatter> {
        Ok(Formatter{
            args: self.args.clone(),
            unwrap: self.unwrap.resolve()?,
        })
    }
}

struct UniqueResolver {
    id: UnwrapSeqResolver,
    out: PathResolver,
    pattern: UnwrapSeqResolver,
}

impl Resolver<TypeUnwrapper> for UniqueResolver {
    fn resolve(&self) -> ParserResult<TypeUnwrapper> {
        Ok(TypeUnwrapper::Unique(Unique {
            id: self.id.resolve()?,
            out: self.out.resolve()?,
            pattern: self.pattern.resolve()?,
        }))
    }
}

fn parse_unique_rule(rule: &Pair<Rule>) -> MetaAndUnwrapperResult {
    let mut inner_rules = rule.clone().into_inner();

    guard!(let Some(meta) = parse_type_meta_from_rules(&mut inner_rules)?
        else { return rule.as_error() });

    guard!(let Some(unwrap_id) = inner_rules.next()
        else { return rule.as_error() });

    guard!(let Some(path_out) = inner_rules.next()
        else { return rule.as_error() });

    guard!(let Some(pattern) = inner_rules.next()
        else { return rule.as_error() });

    Ok(MetaAndUnwrapper{
        meta: meta,
        resolver: Box::new(UniqueResolver{
            id: parse_unwrap_seq(unwrap_id)?,
            out: parse_path(&path_out)?,
            pattern: parse_unwrap_seq(pattern)?,
        }),
    })
}


struct SchemeBuilder {
    consts: RefCell<HashMap<String, ConstResolver>>,
    formatters: RefCell<HashMap<String, FormatterResolver>>,
    unwrapper_resolvers: RefCell<HashMap<TypeMeta, BoxedTypeUnwrapperResolver>>,
}

impl SchemeBuilder {

    fn new() -> SchemeBuilder {
        SchemeBuilder {
            consts: RefCell::new(HashMap::new()),
            formatters: RefCell::new(HashMap::new()),
            unwrapper_resolvers: RefCell::new(HashMap::new()),
        }
    }

    fn process_consts(&self, rule: Pair<Rule>) -> ParserResult<()> {
        let consts = parse_consts(&rule)?;
        self.consts.borrow_mut().extend(consts);
        Ok(())
    }

    fn process_formatter(&self, rule: Pair<Rule>) -> ParserResult<()> {
        let mut inner_rules = rule.clone().into_inner();

        guard!(let Some(name) = inner_rules.next()
            else { return rule.as_error() });
        guard!(let Some(args) = inner_rules.next()
            else { return rule.as_error() });
        guard!(let Some(unwrap) = inner_rules.next()
            else { return rule.as_error() });

        self.formatters.borrow_mut().insert(
            name.as_str().to_owned(),
            FormatterResolver {
                args: parse_formatter_args(args)?,
                unwrap: parse_unwrap_seq(unwrap)?,
            }
        );
        Ok(())
    }

    fn add_parsed(&self, mr_pair: MetaAndUnwrapper) {
        self.unwrapper_resolvers.borrow_mut().insert(
            mr_pair.meta,
            mr_pair.resolver
        );
    }

}

fn build_scheme(mut root_rules: Pairs<Rule>) -> ParserResult<Scheme> {
    guard!(let Some(root) = root_rules.next()
        else { return root_rules.as_error() });

    let scheme_builder = SchemeBuilder::new();
    for rule in root.clone().into_inner() {
        match rule.as_rule() {
            Rule::consts
                => scheme_builder.process_consts(rule)?,
            Rule::formatter
                => scheme_builder.process_formatter(rule)?,
            Rule::matcher_primitive
                => scheme_builder.add_parsed(parse_primitive_matcher(&rule)?),
            Rule::matcher_predefined
                => scheme_builder.add_parsed(parse_predefined_matcher(&rule)?),
            Rule::unwrap
                => scheme_builder.add_parsed(parse_unwrap_rule(&rule)?),
            Rule::unique
                => scheme_builder.add_parsed(parse_unique_rule(&rule)?),
            Rule::EOI => (),
            _ => return rule.as_error()
        }
    }

    let mut consts = HashMap::new();
    for (name, rr) in scheme_builder.consts.borrow().iter() {
        consts.insert(name.clone(), rr.resolve()?);
    }

    let mut formatters = HashMap::new();
    for (name, rr) in scheme_builder.formatters.borrow().iter() {
        formatters.insert(name.clone(), rr.resolve()?);
    }

    let mut unwrappers = HashMap::new();
    for (meta, rr) in scheme_builder.unwrapper_resolvers.borrow().iter()  {
        unwrappers.insert(meta.clone(), rr.resolve()?);
    }

    let result = Scheme {
        consts: consts,
        formatters: formatters,
        unwrappers: unwrappers,
    };

    Ok(result)
}

pub fn parse(data: &str) -> ParserResult<Scheme>  {
    let parse_result = SchemeParser::parse(Rule::scheme, data);
    let root_rules = match parse_result {
        Ok(rules) => rules,
        Err(e) => return Err(ParseError::new_from_pest_error(e))
    };

    build_scheme(root_rules)
}