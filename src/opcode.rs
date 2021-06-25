use std::collections::HashMap;

const OP_CONSTANT: u8 = 0;

// definitions maps the opcode into their respective opranrd widths.
pub struct Definitions {
    pub defs: HashMap<u8, u8>,
}

impl Definitions {
    pub fn default() -> Self {
        let mut table = Self {
            defs: HashMap::new(),
        };
        // insert the definitions for the different op codes.
        table.defs.insert(OP_CONSTANT, 2);

        table
    }

    pub fn look_up(&self, opcode: &u8) -> Option<&u8> {
        self.defs.get(opcode)
    }
}
