use std::iter;
use std::slice::Iter;

// A vector type optimized for cases where the size is almost always 0 or 1
// Code inspired my Mozilla's SmallVector for libsyntax

pub mod svec {
    use super::BinSetu8;
    pub use self::SVec::{Zero, One, Many, ManyBut, Any};
    pub enum SVec {
        Zero,
        One(u8),
        Many(Box<BinSetu8>),
        ManyBut(Box<BinSetu8>),
        Any
    }
}

// efficient data structure for representing a set of states in an NFA
// * data is a binary bit array that uses one bit per state in the NFA
//   that may be either 0 or 1 (for respectively not included or included in
//   the set). It allows for fast lookup/insertion. It is represented as an
//   array of (N/64)+1 64 bit integers
// * states is a redundant array that stores directly the numbers of the
//   states included. It allows for fast iteration over the contents of the
//   states
// * action is set whenever a final set is inserted into the set.
//   If several final states are inserted, the higher action is taken. It
//   should correspond to the first action defined in the lexer file.
// also used to represent a character class in the parser before its converted
// to a vec
pub struct BinSet {
    data: Vec<u64>,
    states: Vec<uint>,
    pub action: uint
}

// FIXME: make generic
// currently we can't write something like impl<T: Bound> where Bound
// would allow T to be shifted or bitor/and'd by any numeric type
#[derive(Clone)]
pub struct BinSetu8 {
    pub data: Vec<u64>,
    pub states: Vec<u8>
}

impl BinSet {
    // checks or sets the presence of a given state in the set
    // bits 0-5 of the state num index the state in the chunk
    // higher bits give the index of the chunk in the data array

    #[inline(always)]
    pub fn contains(&self, state: uint) -> bool {
        let chunk = state >> 6;
        let idx = state & 0x3F;
        ((self.data[chunk] >> idx) & 1) != 0
    }

    #[inline(always)]
    pub fn insert(&mut self, state: uint) {
        let chunk = state >> 6;
        let idx = state & 0x3F;
        self.data[chunk] |= 1 << idx;
        self.states.push(state);
    }

    #[inline(always)]
    pub fn new(state_count: uint) -> BinSet {
        // (state_count / 64) + 1
        let chunks = (state_count >> 6) + 1;
        BinSet {
            data: iter::repeat(0).take(chunks).collect(),
            states: Vec::with_capacity(state_count),
            action: 0
        }
    }

    #[inline(always)]
    pub fn iter<'a>(&'a self) -> Iter<'a, uint> {
        self.states.iter()
    }

    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.states.is_empty()
    }

    #[inline(always)]
    pub fn action(&self) -> uint {
        self.action
    }
}

impl BinSetu8 {
    // checks or sets the presence of a given state in the set
    // bits 0-5 of the state num index the state in the chunk
    // higher bits give the index of the chunk in the data array

    #[inline(always)]
    #[allow(dead_code)]
    pub fn contains(&self, state: u8) -> bool {
        let chunk = (state >> 6u) as uint;
        let idx = (state & 0x3Fu8) as uint;
        ((self.data[chunk] >> idx) & 1) != 0
    }

    #[inline(always)]
    pub fn insert(&mut self, state: u8) {
        let chunk = (state >> 6u) as uint;
        let idx = (state & 0x3Fu8) as uint;
        self.data[chunk] |= 1 << idx;
        self.states.push(state);
    }

    #[inline(always)]
    pub fn new(state_count: uint) -> BinSetu8 {
        // (state_count / 64) + 1
        let chunks = (state_count >> 6) + 1;
        BinSetu8 {
            data: iter::repeat(0).take(chunks).collect(),
            states: Vec::with_capacity(state_count)
        }
    }
}

impl PartialEq for BinSet {
    fn eq(&self, other: &BinSet) -> bool {
        // assumes both vectors have the same length
        let len = self.data.len();
        let mut i = 0;
        while i < len {
            if self.data[i] != other.data[i] {
                return false;
            }
            i += 1;
        }
        true
    }
}
