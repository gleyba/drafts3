extern crate pest;
#[macro_use]
extern crate pest_derive;

#[macro_use]
extern crate guard;

extern crate crypto;

#[macro_use]
extern crate serde_derive;

extern crate serde;
extern crate serde_value;

mod common;
pub mod scheme;
pub mod writer;
pub mod generator;

static DATA: &str = r#"
Color = enum {
    Red;
    Green;
    Blue;
}

Vec3 = record {
    x: f32;
    y: f32;
    z: f32;
}

Weapon = record {
    name: string;
    damage: i16;
}

Money = record {
    count: i32;
}

Expo = record {
    count: i32;
}

Monster = record {
    id: string;
    mana: optional<i16>;
    equipped: list<string>;
    path: list<Vec3>;
    inventory: binary;
    special: tuple<i16,i16,i16,i16,i16,i16,i16>;
    reward: variant<Money,Expo>;
    weapons: dictionary<string, Weapon>;
    color: Color;
}

Director = interface {
    static instance: Director;

    playerPosition: Vec3;
    level: i32 (set);

    static start();
    static stop();

    spawnMonster(monster: Monster): bool;

    killMonster(id: string): optional<variant<Money,Expo>>;

    doSomeThing(p1: i8, p2: string, p3: binary, p4: list<Color>);

    takeSomeVoidClosure(cl: () -> ());
    takeSomeNonVoidClosure(cl: (p1: i8, p2: string, p3: binary, p4: list<Color>) -> string);
    doReturnVoidClosure(): () -> ();
    doReturnNonVoidClosure(): (p1: i8, p2: string, p3: binary, p4: list<Color>) -> string;
}
"#;

static PATTERN: &str =
r#"
Consts = {
    fb_scheme_file = scheme/${spec.ns}.fbs
}

Matcher(Primitive) = {
    Bool => bool
    I8 => byte
    I16 => short
    I32 => int
    I64 => long
    F32 => float
    F64 => double
}

Matcher(Predefined) = {
    String => string
    Date => long
    Binary => [byte]
}

Unwrap(Optional<_>) = ${type_of(0)}

Unwrap(List<_>) = [${type_of(0)}]

fb_record_field: Formatter(name,_) = ${name}: ${type_of(0)}

Unique(Tuple<_>) = {
    id = ${join("",name_of(_))}Tuple
    out = ${consts.fb_scheme_file}
    pattern:
    <%
    table ${id} {
        ${join(";\n",fb_record_field(number(_),_))}
    }
    %>
}

Unique(Variant<_>) = {
    id = ${join("",name_of(_))}Variant
    out = ${consts.fb_scheme_file}
    pattern:
    <%
    union ${id} {
        ${join_not_last(",\n",type_of(_))}
    }
    %>
}

Unique(Dictionary<_>) = {
    id = ${join("",name_of(_))}Dictionary
    out = ${consts.fb_scheme_file}
    pattern:
    <%
    struct ${id} {
        kv_pairs: [${type_of(Tuple<0,1>)}];
    }
    %>
}

Unique(Enum) = {
    id = ${name}
    out = ${consts.fb_scheme_file}
    pattern:
    <%
    enum ${id} {
        ${join_not_last(",\n",options)}
    }
    %>
}

Unique(Record) = {
    id = ${name}
    out = ${consts.fb_scheme_file}
    pattern:
    <%
    table ${id} {
        ${join(";\n",fb_record_field(fields_names,fields_types))}
    }
    %>
}

"#;



fn main() {
    let data = match scheme::parser::parse(DATA) {
        Ok(data) => data,
        Err(e) => panic!("{:?}", e)
    };
    let pattern = match writer::parser::parse(PATTERN) {
        Ok(pattern) => pattern,
        Err(e) => panic!("{:?}", e)
    };
    let spec = common::spec::Spec {
        ns: String::from("test"),
    };

    let out = match generator::gen::resolve(spec, data, pattern) {
        Ok(out) => out,
        Err(e) => panic!("{:?}", e)
    };

//    let toml_scheme = match scheme {
//        Ok(scheme) => serde_yaml::to_string(&scheme),
//        Err(e) => panic!("{:?}", e)
//    };

//    println!("{}", toml_scheme.unwrap());



//    let writer_spec = writer::parser::parse(FBS_YAZIKSPEC);
//    let toml_writer = match writer_spec {
//        Ok(writer) => serde_yaml::to_string(&writer),
//        Err(e) => panic!("{:?}", e)
//    };
//    println!("{}", toml_writer.unwrap());
}