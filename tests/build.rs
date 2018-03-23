pub fn main() {
    extern crate syntex;
    extern crate rustlex_codegen;
    use std::env;
    use std::path::Path;

    let out_dir = env::var_os("OUT_DIR").unwrap();
    for file in [ "complex", "condition", "module", "properties", "simple" ].iter() {
        let src = format!("{}.in.rs", file);
        let dst = Path::new(&out_dir).join(format!("{}.rs", file));
        let mut registry = syntex::Registry::new();
        rustlex_codegen::plugin_registrar(&mut registry);
        registry.expand("", &Path::new(&src), &dst).unwrap();
    }
}
