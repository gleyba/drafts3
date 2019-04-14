use std::collections::HashMap;
use std::marker::PhantomData;
use std::cell::RefCell;

use pest::Parser;
use pest::iterators::{Pair, Pairs};

#[derive(Parser)]
#[grammar = "scheme/parser.pest"]
struct SchemeParser;

use crate::common::parser::*;
use super::scheme::*;

fn parse_primitive(rule: &Pair<Rule>) -> ParserResult<Primitive> {
    use Primitive::*;
    let result = match rule.as_str() {
        "bool" => Bool,
        "i8" => I8,
        "i16" => I16,
        "i32" => I32,
        "i64" => I64,
        "f32" => F32,
        "f64" => F64,
        _ => return rule.as_error(),
    };
    Ok(result)
}

fn parse_predefined(rule: &Pair<Rule>) -> ParserResult<Predefined> {
    use Predefined::*;
    let result = match rule.as_str() {
        "string" => String,
        "date" => Date,
        "binary" => Binary,
        _ => return rule.as_error(),
    };
    Ok(result)
}

fn parse_enum(rule: &Pair<Rule>) -> ParserResult<Enum> {
    let mut inner_rules = rule.clone().into_inner();

    guard!(let Some(name_rule) = inner_rules.next()
        else { return rule.as_error() });

    let options: Vec<String> = inner_rules
        .map(|p| p.as_str().to_string() )
        .collect();

    if options.is_empty() {
        return rule.as_error()
    }

    Ok(Enum {
        name: name_rule.as_str().to_string(),
        options: options,
        hash: rule.md5_hash(),
    })
}

trait Resolver<T> {
    fn resolve(&self) -> ParserResult<T>;
}

enum CustomRef {
    Enum(String),
    Record(String),
    Interface(String),
}

trait CustomRefsRepo {
    fn get_custom_ref(&self, rule: &Pair<Rule>) ->  ParserResult<CustomRef>;
}

type DictionaryType<T> = (DictionaryKeyType, Box<T>);
type ParamsType<T> = Vec<(String,T)>;
type ReturnType<T> = Option<Box<T>>;
type ClosureType<T> = (Vec<(String,T)>, ReturnType<T>);

trait TypeResolverTyped<T> {
    fn get_rule(&self) -> Pair<Rule>;
    fn resolve_impl(&self, rule: &Pair<Rule>) -> ParserResult<T>;

    fn parse_inner(&self, rule: &Pair<Rule>) -> ParserResult<T> {
        guard!(let Some(inner) = rule.clone().into_inner().next()
            else { return rule.as_error() });

        self.resolve_impl(&inner)
    }

    fn parse_inner_boxed(&self, rule: &Pair<Rule>) -> ParserResult<Box<T>> {
        self.parse_inner(rule).map(Box::new)
    }

    fn parse_vec(&self, rule: &Pair<Rule>) -> ParserResult<Vec<T>> {
        rule.clone()
            .into_inner()
            .map(|r| self.resolve_impl(&r))
            .collect()
    }

    fn parse_dictionary(&self, rule: &Pair<Rule>) -> ParserResult<DictionaryType<T>> {
        use DictionaryKeyType::*;
        let mut inner_rules = rule.clone().into_inner();

        guard!(let Some(key_rule) = inner_rules.next()
            else { return rule.as_error() });

        let key = match key_rule.as_rule() {
            Rule::primitive => Primitive(parse_primitive(&key_rule)?),
            Rule::predefined => Predefined(parse_predefined(&key_rule)?),
            _ => return key_rule.as_error()
        };

        guard!(let Some(value_rule) = inner_rules.next()
            else { return rule.as_error() });

        let value = self.resolve_impl(&value_rule)?;
        Ok((key, Box::new(value)))
    }

    fn parse_params(&self, rule: &Pair<Rule>) -> ParserResult<ParamsType<T>> {
        let mut params = vec![];
        let mut params_rules = rule.clone().into_inner();
        while let Some(cur) = params_rules.next() {
            guard!(let Some(param_type_rule)  = params_rules.next()
                else { return cur.as_error() });

            params.push((
                cur.as_str().to_string(),
                self.resolve_impl(&param_type_rule)?
            ));
        }
        Ok(params)
    }


    fn parse_return_type(&self, rule: &Pair<Rule>) -> ParserResult<ReturnType<T>> {
        guard!(let Some(return_type_rule) = rule.clone().into_inner().next()
            else { return Ok(None) });

        self.resolve_impl(&return_type_rule)
            .map(Box::new)
            .map(Some)
    }


    fn parse_closure(&self, rule: &Pair<Rule>) -> ParserResult<ClosureType<T>> {
        let mut inner_rules = rule.clone().into_inner();

        let params = match inner_rules.next() {
            Some(rule) => self.parse_params(&rule)?,
            None => return rule.as_error()
        };
        let return_type = match inner_rules.next() {
            Some(rule) => self.parse_return_type(&rule)?,
            None => return rule.as_error()
        };

        Ok((params, return_type))
    }
}

struct TypeResolver<'a, T> {
    rule: Pair<'a, Rule>,
    repo: &'a (CustomRefsRepo + 'a),
    phantom: PhantomData<T>,
}

impl <'i,T>TypeResolver<'i, T> {
    fn new(rule: Pair<'i, Rule>, repo: &'i (CustomRefsRepo + 'i)) -> TypeResolver<'i, T> {
        TypeResolver::<'i, T>{
            rule: rule,
            repo: repo,
            phantom: PhantomData
        }
    }
}

impl <'i>TypeResolverTyped<DataType> for TypeResolver<'i, DataType> {

    fn get_rule(&self) -> Pair<Rule> {
        self.rule.clone()
    }

    fn resolve_impl(&self, rule: &Pair<Rule>) -> ParserResult<DataType> {
        use DataType::*;
        let result = match rule.as_rule() {
            Rule::primitive => Primitive(parse_primitive(rule)?),
            Rule::predefined => Predefined(parse_predefined(rule)?),
            Rule::optional => Optional(self.parse_inner_boxed(rule)?),
            Rule::list => List(self.parse_inner_boxed(rule)?),
            Rule::tuple => Tuple(self.parse_vec(rule)?),
            Rule::variant => Variant(self.parse_vec(rule)?),
            Rule::dictionary => Dictionary(self.parse_dictionary(rule)?),
            Rule::ident => match self.repo.get_custom_ref(rule)? {
                CustomRef::Enum(id) => Enum(id),
                CustomRef::Record(id) => Record(id),
                _ => return rule.as_error()
            }
            _ => return rule.as_error()
        };
        Ok(result)
    }
}

type ParamType<'a, T> = (String, TypeResolver<'a, T>);
type ParamsResolversVec<'a, T> = Vec<ParamType<'a, T>>;

fn parse_params_resolvers<'i, T>(
    rule: &Pair<'i, Rule>,
    repo: &'i CustomRefsRepo
) -> ParserResult<ParamsResolversVec<'i, T>> {
    let mut inner_rules = rule.clone().into_inner();

    let mut fields_resolvers = vec![];
    while let Some(cur) = inner_rules.next() {
        guard!(let Some(next) = inner_rules.next()
            else { return rule.as_error() });

        fields_resolvers.push((
            cur.as_str().to_string(),
            TypeResolver::<'i, T>::new(
                next,
                repo
            )
        ));
    }

    Ok(fields_resolvers)
}


impl <'i>Resolver<DataType> for TypeResolver<'i, DataType> {
    fn resolve(&self) -> ParserResult<DataType> {
        self.resolve_impl(&self.get_rule())
    }
}

struct RecordResolver<'a> {
    name: String,
    hash: String,
    fields_resolvers: ParamsResolversVec<'a, DataType>,
}


impl <'i>RecordResolver<'i> {

    fn create(rule: Pair<'i, Rule>, repo: &'i CustomRefsRepo) -> ParserResult<RecordResolver<'i>> {
        let mut inner_rules = rule.clone().into_inner();

        guard!(let Some(name_rule) = inner_rules.next()
            else { return rule.as_error() });

        guard!(let Some(fields_rule) = inner_rules.next()
            else { return rule.as_error() });


        Ok(RecordResolver {
            name: name_rule.as_str().to_string(),
            hash: rule.md5_hash(),
            fields_resolvers:parse_params_resolvers::<DataType>(
                &fields_rule,
                repo
            )?,
        })
    }
}

impl <'i>Resolver<Record> for RecordResolver<'i> {
    fn resolve(&self) -> ParserResult<Record> {
        let fields: ParserResult<Vec<(String, DataType)>> = self.fields_resolvers
            .iter()
            .map (|(name, resolver)| -> ParserResult<(String, DataType)> {
                let dt = resolver.resolve()?;
                Ok((name.clone(), dt))
            })
            .collect();
        let result = Record {
            name: self.name.clone(),
            hash: self.hash.clone(),
            fields:fields?,
        };
        Ok(result)
    }
}

impl <'i>TypeResolverTyped<CrossType> for TypeResolver<'i, CrossType> {

    fn get_rule(&self) -> Pair<Rule> {
        self.rule.clone()
    }

    fn resolve_impl(&self, rule: &Pair<Rule>) -> ParserResult<CrossType> {
        use CrossType::*;
        let result = match rule.as_rule() {
            Rule::primitive => Primitive(parse_primitive(rule)?),
            Rule::predefined => Predefined(parse_predefined(rule)?),
            Rule::optional => Optional(self.parse_inner_boxed(rule)?),
            Rule::list => List(self.parse_inner_boxed(rule)?),
            Rule::tuple => Tuple(self.parse_vec(rule)?),
            Rule::variant => Variant(self.parse_vec(rule)?),
            Rule::dictionary => Dictionary(self.parse_dictionary(rule)?),
            Rule::closure => Closure(self.parse_closure(rule)?),
            Rule::ident => match self.repo.get_custom_ref(rule)? {
                CustomRef::Enum(id) => Enum(id),
                CustomRef::Record(id) => Record(id),
                CustomRef::Interface(id) => Interface(id),
            }
            _ => return rule.as_error()
        };
        Ok(result)
    }
}

impl <'i>Resolver<CrossType> for TypeResolver<'i, CrossType> {
    fn resolve(&self) -> ParserResult<CrossType> {
        self.resolve_impl(&self.get_rule())
    }
}

impl <'i>Resolver<Property> for TypeResolver<'i, Property> {
    fn resolve(&self) -> ParserResult<Property> {
        let mut inner_rules = self.rule.clone().into_inner();
        guard!(let Some(is_static_rule) = inner_rules.next()
            else { return self.rule.as_error() });

        guard!(let Some(name_rule) = inner_rules.next()
            else { return self.rule.as_error() });

        guard!(let Some(type_rule) = inner_rules.next()
            else { return self.rule.as_error() });

        guard!(let Some(mods_rule) = inner_rules.next()
            else { return self.rule.as_error() });

        let mut has_setter = false;
        let mut mods_rules = mods_rule.into_inner();

        while let Some(cur) = mods_rules.next() {
            match cur.as_rule() {
                Rule::has_setter => has_setter = cur.as_str() != "",
                _ => return cur.as_error()
            }
        }

        let yt = TypeResolver::<'i, CrossType>::new(
            type_rule,
            self.repo
        ).resolve()?;

        let result = Property {
            name: name_rule.as_str().to_string(),
            yazik_type: yt,
            has_setter: has_setter,
            is_static: !is_static_rule.as_str().is_empty(),
        };

        Ok(result)
    }
}

impl <'i> TypeResolver<'i, Method> {
    fn parse_return_type(&self, rule: &Pair<'i, Rule>) -> ParserResult<Option<CrossType>> {
        guard!(let Some(return_type_rule) = rule.clone().into_inner().next()
            else { return Ok(None) });

        let yt = TypeResolver::<'i, CrossType>::new(
            return_type_rule,
            self.repo
        ).resolve().map(Some);

        Ok(yt?)
    }
}

impl <'i>Resolver<Method> for TypeResolver<'i, Method> {
    fn resolve(&self) -> ParserResult<Method> {
        let mut inner_rules = self.rule.clone().into_inner();

        guard!(let Some(is_static_rule) = inner_rules.next()
            else { return self.rule.as_error() });

        guard!(let Some(name_rule) = inner_rules.next()
            else { return self.rule.as_error() });

        guard!(let Some(params_rule) = inner_rules.next()
            else { return self.rule.as_error() });

        guard!(let Some(return_type_rule) = inner_rules.next()
            else { return self.rule.as_error() });

        let params_resolvers = parse_params_resolvers::<CrossType>(
            &params_rule,
            self.repo
        )?;
        let mut params =  vec![];
        for (name, resolver) in params_resolvers {
            let yt = resolver.resolve()?;
            params.push((name.clone(), yt));
        }

        let return_type = self.parse_return_type(&return_type_rule)?;

        let result = Method {
            name: name_rule.as_str().to_string(),
            params: params,
            return_type: return_type,
            is_static: !is_static_rule.as_str().is_empty(),
        };

        Ok(result)
    }
}

struct InterfaceResolver<'a> {
    name: String,
    hash: String,
    protperties_resolvers: Vec<TypeResolver<'a, Property>>,
    methods_resolvers: Vec<TypeResolver<'a, Method>>,
}

impl <'i>InterfaceResolver<'i> {
    fn create(rule: Pair<'i, Rule>, repo: &'i CustomRefsRepo) -> ParserResult<InterfaceResolver<'i>> {
        let mut inner_rules = rule.clone().into_inner();

        guard!(let Some(name_rule) = inner_rules.next()
            else { return rule.as_error() });

        let mut protperties_resolvers = vec![];
        let mut methods_resolvers = vec![];

        while let Some(cur) = inner_rules.next() {
            match cur.as_rule() {
                Rule::property => protperties_resolvers.push(
                    TypeResolver::<'i, Property>::new(
                        cur,
                        repo
                    )
                ),
                Rule::method => methods_resolvers.push(
                    TypeResolver::<'i, Method>::new(
                        cur,
                        repo
                    )
                ),
                _ => return cur.as_error()
            }
        }

        if protperties_resolvers.is_empty() && methods_resolvers.is_empty() {
            return rule.as_error()
        }

        let result = InterfaceResolver {
            name: name_rule.as_str().to_string(),
            hash: rule.md5_hash(),
            protperties_resolvers: protperties_resolvers,
            methods_resolvers: methods_resolvers,
        };
        Ok(result)
    }
}

impl <'i>Resolver<Interface> for InterfaceResolver<'i> {
    fn resolve(&self) -> ParserResult<Interface> {
        let mut properties = vec![];
        for pr in self.protperties_resolvers.iter() {
            let property = pr.resolve()?;
            properties.push((property.name.clone(), property));
        }

        let mut methods = vec![];
        for mr in self.methods_resolvers.iter() {
            let method = mr.resolve()?;
            methods.push((method.name.clone(), method));
        }

        let result = Interface {
            name: self.name.clone(),
            hash: self.hash.clone(),
            properties: properties,
            methods: methods,
        };
        Ok(result)
    }
}


#[derive(Default)]
struct SchemeBuilder<'a> {
    enums: RefCell<HashMap<String, Enum>>,
    records_resolvers: RefCell<HashMap<String, RecordResolver<'a>>>,
    interface_resolvers: RefCell<HashMap<String, InterfaceResolver<'a>>>,
}

impl <'i>CustomRefsRepo for SchemeBuilder<'i> {
    fn get_custom_ref(&self, rule: &Pair<Rule>) -> ParserResult<CustomRef> {
        use CustomRef::*;
        let ident = rule.as_str().to_string();
        let result = if self.enums.borrow().contains_key(&ident) {
            Enum(ident)
        } else if self.records_resolvers.borrow().contains_key(&ident) {
            Record(ident)
        } else if self.interface_resolvers.borrow().contains_key(&ident) {
            Interface(ident)
        } else {
            return rule.as_error()
        };
        Ok(result)
    }
}

impl <'i>SchemeBuilder<'i> {

    fn new() -> SchemeBuilder<'i> {
        SchemeBuilder {
            enums: RefCell::new(HashMap::new()),
            records_resolvers: RefCell::new(HashMap::new()),
            interface_resolvers: RefCell::new(HashMap::new()),
        }
    }

    fn process_enum_rule(&'i self, rule: Pair<'i, Rule>) -> ParserResult<()> {
        let enum_val = parse_enum(&rule)?;
        self.enums.borrow_mut().insert(
            enum_val.name.clone(),
            enum_val
        );
        Ok(())
    }

    fn process_record_rule(&'i self, rule: Pair<'i, Rule>) -> ParserResult<()> {
        let record_resolver = RecordResolver::create(rule, self)?;
        self.records_resolvers.borrow_mut().insert(
            record_resolver.name.clone(),
            record_resolver
        );
        Ok(())
    }

    fn process_interface_rule(&'i self, rule: Pair<'i, Rule>) -> ParserResult<()> {
        let interface_resolver = InterfaceResolver::create(rule, self)?;
        self.interface_resolvers.borrow_mut().insert(
            interface_resolver.name.clone(),
            interface_resolver
        );
        Ok(())
    }

}

fn build_scheme(mut root_rules: Pairs<Rule>)  -> ParserResult<Scheme> {
    guard!(let Some(yazik) = root_rules.next()
        else { return root_rules.as_error() });

    let scheme_builder = SchemeBuilder::new();
    for yazik_rule in yazik.clone().into_inner() {
        match yazik_rule.as_rule() {
            Rule::yazik_enum => scheme_builder.process_enum_rule(yazik_rule)?,
            Rule::yazik_record => scheme_builder.process_record_rule(yazik_rule)?,
            Rule::yazik_interface => scheme_builder.process_interface_rule(yazik_rule)?,
            Rule::EOI => (),
            _ => return yazik.as_error()
        }
    };

    let enums: HashMap<String, Enum> =  scheme_builder.enums.borrow().clone();

    let mut records = HashMap::new();
    for (name, rr) in scheme_builder.records_resolvers.borrow().iter() {
        records.insert(name.clone(), rr.resolve()?);
    }

    let mut interfaces= HashMap::new();
    for (name, rr) in scheme_builder.interface_resolvers.borrow().iter() {
        interfaces.insert(name.clone(), rr.resolve()?);
    }

    let result = Scheme {
        enums: enums,
        records: records,
        interfaces: interfaces,
    };

    Ok(result)
}

pub fn parse(data: &str) -> ParserResult<Scheme>  {
    let parse_result = SchemeParser::parse(Rule::yazik, data);
    let root_rules = match parse_result {
        Ok(rules) => rules,
        Err(e) => return Err(ParseError::new_from_pest_error(e))
    };

    build_scheme(root_rules)
}