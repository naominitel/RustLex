// A vector type optimized for cases where the size is almost always 0 or 1
// Code inspired my Mozilla's SmallVector for libsyntax

pub mod svec {
    pub enum SVec<T> {
        Zero,
        One(T),
        Many(Vec<T>)
    }
}
