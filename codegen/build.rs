#[cfg(feature = "with-syntex")]
pub fn main() {
    extern crate syntex;
    extern crate quasi_codegen;
    use std::env;
    use std::path::Path;

    let mut registry = syntex::Registry::new();
    quasi_codegen::register(&mut registry);
    let src = Path::new("src/codegen.in.rs");
    let dst = Path::new(&env::var_os("OUT_DIR").unwrap()).join("codegen.rs");
    registry.expand("", &src, &dst).unwrap();
}

#[cfg(not(feature = "with-syntex"))]
pub fn main() {}
