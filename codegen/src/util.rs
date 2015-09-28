// A vector type optimized for cases where the size is almost always 0 or 1
// Code inspired my Mozilla's SmallVector for libsyntax

pub mod svec {
    use bit_set::BitSet;
    pub use self::SVec::{Zero, One, Many, ManyBut, Any};
    pub enum SVec {
        Zero,
        One(u8),
        Many(Box<BitSet>),
        ManyBut(Box<BitSet>),
        Any
    }
}
