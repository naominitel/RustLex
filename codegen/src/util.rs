// A vector type optimized for cases where the size is almost always 0 or 1
// Code inspired my Mozilla's SmallVector for libsyntax

pub mod svec {
    use regex;
    pub use self::SVec::{Zero, One, Many, ManyBut, Any};
    pub enum SVec {
        Zero,
        One(u8),
        Many(regex::CharSet),
        ManyBut(regex::CharSet),
        Any
    }
}
