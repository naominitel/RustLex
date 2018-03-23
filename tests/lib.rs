extern crate rustlex;

mod complex    { include!(concat!(env!("OUT_DIR"), "/complex.rs")); }
mod condition  { include!(concat!(env!("OUT_DIR"), "/condition.rs")); }
mod module     { include!(concat!(env!("OUT_DIR"), "/module.rs")); }
mod properties { include!(concat!(env!("OUT_DIR"), "/properties.rs")); }
mod simple     { include!(concat!(env!("OUT_DIR"), "/simple.rs")); }
