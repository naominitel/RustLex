#[cfg(feature = "with-syntex")]
pub fn main() {
    extern crate quasi_codegen;
    use std::env;
    use std::path::Path;

    let src = Path::new("codegen.in.rs");
    let dst = Path::new(&env::var_os("OUT_DIR").unwrap()).join("codegen.rs");
    quasi_codegen::expand(&src, &dst).unwrap();
}

#[cfg(not(feature = "with-syntex"))]
pub fn main() {}
