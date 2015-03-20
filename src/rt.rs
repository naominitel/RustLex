use std::io::Read;
use std::ops::IndexMut;

const RUSTLEX_BUFSIZE: usize = 4096;

pub struct RustLexBuffer {
    d: Vec<u8>,
    valid: bool
}

impl RustLexBuffer {
    #[inline(always)]
    pub fn len(&self) -> usize {
        self.d.len()
    }

    #[inline(always)]
    pub fn get(&self, idx: usize) -> &u8 {
        &self.d[idx]
    }

    #[inline(always)]
    pub fn as_slice(&self) -> &[u8] {
        self.d.as_slice()
    }

    #[inline(always)]
    pub fn slice(&self, from: usize, to: usize) -> &[u8] {
        &self.d[from..to]
    }

    #[inline(always)]
    pub fn slice_from(&self, from: usize) -> &[u8] {
        &self.d[from..]
    }
}

#[derive(Copy)]
pub struct RustLexPos {
    pub buf: usize,
    pub off: usize
}

impl PartialEq for RustLexPos {
    fn eq(&self, other: &RustLexPos) -> bool {
        self.buf == other.buf &&
        self.off == other.off
    }
}

pub struct RustLexLexer<R : Read> {
    pub stream: R,
    pub inp: Vec<RustLexBuffer>,
    pub advance: RustLexPos,
    pub pos: RustLexPos,
    pub tok: RustLexPos
}

impl<R: ::std::io::Read> RustLexLexer<R> {
    fn fill_buf(&mut self) {
        let &mut RustLexBuffer {
            ref mut d,
            ref mut valid
        } = self.inp.index_mut(&self.pos.buf);
        *valid = true;
        // Grow to the correct bufsize
        if d.len() < RUSTLEX_BUFSIZE {
            d.resize(RUSTLEX_BUFSIZE, 0);
        }
        // Shrink back down to however much was used.
        match self.stream.read(&mut ** d) {
            Ok(l) => d.resize(l, 0),
            Err(_) => d.clear()
        };
        self.pos.off = 0;
    }

    pub fn getchar(&mut self) -> Option<u8> {
        if self.pos.off == RUSTLEX_BUFSIZE {
            let npos = self.pos.buf + 1;
            if self.inp.len() > npos && self.inp[npos].valid {
                self.pos.buf = npos;
                self.pos.off = 0;
            } else {
                // we reached the end of the current buffer. We must get
                // more input. First, see if we can get rid of buffers
                // that won't be used anymore. Shifting the array can be
                // done cheaply because most analysers won't need more
                // than a couple of buffers
                let unused_buffers_count = self.tok.buf;
                for i in (0 .. unused_buffers_count) {
                    self.inp[i].valid = false;
                    self.inp[i].d = Vec::new();
                    self.inp.as_mut_slice().swap(i + unused_buffers_count, i);
                }
                self.tok.buf -= unused_buffers_count;
                self.pos.buf += 1;
                self.pos.buf -= unused_buffers_count;
                self.advance.buf -= unused_buffers_count;

                while self.pos.buf >= self.inp.len() {
                    // we couldn't free some space, we have to create a
                    // new buffer and add it to our vector
                    self.inp.push(RustLexBuffer {
                        d: Vec::new(),
                        valid: false
                    });
                }

                self.fill_buf();
            }
        } else if self.pos.off >= self.inp[self.pos.buf].len() {
            // the current buffer wasn't full, this mean this is
            // actually EOF
            return None
        }

        let &ch = self.inp[self.pos.buf].get(self.pos.off);
        self.pos.off += 1;
        Some(ch)
    }

    pub fn new(stream: R) -> RustLexLexer<R> {
        let mut lex = RustLexLexer {
            stream: stream,
            inp: vec!(RustLexBuffer{
                d: Vec::new(),
                valid: false
            }),
            advance: RustLexPos {
                off: 0,
                buf: 0
            },
            pos: RustLexPos {
                off: 0,
                buf: 0
            },
            tok: RustLexPos {
                off: 0,
                buf: 0
            }
        };
        lex.fill_buf();
        lex
    }
}
