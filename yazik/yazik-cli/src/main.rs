extern crate serde_yaml;
extern crate toml;

extern crate yazik;

static TEST_SCHEME: &str = r#"
"#;

fn main() {
    let scheme = yazik::parser::parse(TEST_SCHEME);

    let toml_scheme = match scheme {
        Ok(scheme) => serde_yaml::to_string(&scheme),
        Err(e) => panic!("{:?}", e)
    };

    println!("{}", toml_scheme.unwrap())
}