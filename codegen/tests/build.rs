#[cfg(feature = "with-syntex")]
pub fn main() {
    extern crate syntex;
    extern crate rustlex_codegen;
    use std::env;
    use std::path::Path;

    let mut registry = syntex::Registry::new();
    rustlex_codegen::plugin_registrar(&mut registry);
    let src = Path::new("src/test.in.rs");
    let dst = Path::new(&env::var_os("OUT_DIR").unwrap()).join("test.rs");
    registry.expand("", &src, &dst).unwrap();
}

#[cfg(not(feature = "with-syntex"))]
pub fn main() {}
