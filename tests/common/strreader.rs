// utility module for test functions
// test functions test lexical analysers that take strings as input instead
// of files, to avoid useless files on the repository, and to assert easily
// that the results of the analysis are correct
// they thus need this structure to run an analyser on a plain string:
use std::io;

struct StrReader<'a> {
    str: &'a str,
    pos: uint
}

impl<'a> io::Reader for StrReader<'a> {
    fn read(&mut self, buf: &mut [u8]) -> io::IoResult<uint> {
        let mut ret = 0;
        while self.pos < self.str.len() && ret < buf.len() {
            buf[ret] = self.str.as_bytes()[self.pos];
            self.pos += 1;
            ret += 1;
        }
        if ret != 0 { Ok(ret) } else { Err(io::IoError {
            kind: io::EndOfFile,
            desc: "end of file",
            detail: None
        })}
    }
}

pub fn reader<'a>(s: &'a str) -> Box<StrReader<'a>> {
    box StrReader { str: s, pos: 0 }
}
