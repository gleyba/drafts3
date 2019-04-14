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

use crate::common::spec::Spec;

mod common;
pub mod scheme;
pub mod writer;

static TEST_SCHEME: &str = r#"
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

static FBS_YAZIKSPEC: &str = r#"
Consts:
fb_scheme_file = scheme/${spec.ns}.fbs
---
Matcher(Primitive):
bool => bool
i8 => byte
i16 => short
i32 => int
i64 => long
f32 => float
f64 => double
---
Matcher(Predefined):
string => string
date => I64
binary => [byte]
---
"#;

fn main() {
//    let scheme = yazik::parser::parse(TEST_SCHEME);
//
//    let toml_scheme = match scheme {
//        Ok(scheme) => serde_yaml::to_string(&scheme),
//        Err(e) => panic!("{:?}", e)
//    };
//
//    println!("{}", toml_scheme.unwrap())

    let spec = Spec {
        ns: String::from("test"),
    };

    let writer_spec = writer::parser::parse(FBS_YAZIKSPEC, spec);
    let toml_writer = match writer_spec {
        Ok(writer) => serde_yaml::to_string(&writer),
        Err(e) => panic!("{:?}", e)
    };
    println!("{}", toml_writer.unwrap())
}