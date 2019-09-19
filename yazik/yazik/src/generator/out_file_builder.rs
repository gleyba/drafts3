use std::collections::HashMap;
use std::cell::RefCell;
use std::cell::Ref;
use std::io::BufWriter;

use crate::writer::scheme as pattern;

struct OutFileLayout {
    writer: RefCell<BufWriter<Vec<u8>>>,
}

impl OutFileLayout {
    fn new() -> Self {
        Self {
            writer: RefCell::new(BufWriter::new(Vec::new())),
        }
    }
}

pub struct OutFileBuilder {
    layouts: RefCell<HashMap<String, OutFileLayout>>,
}

impl OutFileBuilder {
    pub fn new() -> Self {
        Self {
            layouts: RefCell::new(HashMap::new()),
        }
    }
}

pub struct OutFilesBuilder {
     out_files: RefCell<HashMap<String, OutFileBuilder>>,
}

impl OutFilesBuilder {
    pub fn new() -> Self {
        Self {
            out_files: RefCell::new(HashMap::new()),
        }
    }

    pub fn file(&self, path: &str, closure: &dyn Fn(&OutFileBuilder) -> ()) {
        if let Some(exist) = self.out_files.borrow().get(path) {
            closure(exist);
            return
        }

        self.out_files.borrow_mut().insert(path.to_owned(), OutFileBuilder::new());
        self.file(path, closure)
    }
}