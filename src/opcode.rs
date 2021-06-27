use std::collections::HashMap;

pub const OP_CONSTANT: u8 = 0;
pub const OP_ADD: u8 = 1;

pub struct Inst(pub Vec<u8>);

// definitions maps the opcode into their respective opranrd widths.
pub struct Definitions {
    pub defs: HashMap<u8, usize>,
}

impl Definitions {
    pub fn default() -> Self {
        let mut table = Self {
            defs: HashMap::new(),
        };
        // insert the definitions for the different op codes.
        table.defs.insert(OP_CONSTANT, 2);
        table.defs.insert(OP_CONSTANT, 0);

        table
    }

    pub fn look_up(&self, opcode: &u8) -> Option<&usize> {
        self.defs.get(opcode)
    }
}

pub struct InstMaker {
    pub table: Definitions,
}

impl InstMaker {
    pub fn default() -> Self {
        Self {
            table: Definitions::default(),
        }
    }

    pub fn make(&self, opcode: u8, operand: usize) -> Option<Inst> {
        let wanted_width = self.table.look_up(&opcode)?;
        let mut inst: Vec<u8> = vec![0_u8; 1];
        inst[0] = opcode;

        match wanted_width {
            2 => inst.extend_from_slice(&(operand as u16).to_be_bytes()),
            _ => return None,
        };

        Some(Inst(inst))
    }

    pub fn read_operand(&self, operand_width: usize, insts: &[u8]) -> (usize, usize) {
        let mut offset = 0;

        let res = match operand_width {
            2 => {
                // this is so fucking bad
                (((insts[0] as u16) << 8) | insts[1] as u16) as usize
            }
            _ => 0,
        };
        offset += operand_width;

        (res, offset)
    }

    // takes in a slice starting from a given position thats why it
    // just looks over the first and second indices.
    pub fn read_u16(insts: &[u8]) -> u16 {
        ((insts[0] as u16) << 8) | insts[1] as u16
    }
}

// takes in a slice starting from a given position thats why it
// just looks over the first and second indices.
pub fn read_u16(insts: &[u8]) -> u16 {
    ((insts[0] as u16) << 8) | insts[1] as u16
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_make() {
        struct MakeTestcase {
            pub op: u8,
            pub operand: usize,
            pub expected_insts: Vec<u8>,
        }

        let expected = vec![MakeTestcase {
            op: OP_CONSTANT,
            operand: 65534,
            expected_insts: vec![OP_CONSTANT, 255, 254],
        }];

        let inst_maker = InstMaker::default();
        for tt in expected.iter() {
            let inst = inst_maker.make(tt.op, tt.operand);
            assert!(!inst.is_none());

            let inst = inst.unwrap();
            assert_eq!(inst.0.len(), tt.expected_insts.len());

            for (i, byte) in tt.expected_insts.iter().enumerate() {
                assert_eq!(inst.0[i], byte.to_owned());
            }
        }
    }

    #[test]
    fn test_read_operand() {
        struct ReadOperandTestcase {
            pub op: u8,
            pub operand: usize,
            pub bytes_read: usize,
        }

        let tests = vec![ReadOperandTestcase {
            op: OP_CONSTANT,
            operand: 65535,
            bytes_read: 2,
        }];

        let inst_maker = InstMaker::default();
        for tt in tests.iter() {
            let inst = inst_maker.make(tt.op, tt.operand);
            assert!(!inst.is_none());

            let inst = inst.unwrap();

            let def = inst_maker.table.look_up(&tt.op);
            assert!(!def.is_none());
            let def = def.unwrap();

            let res = inst_maker.read_operand(def.to_owned(), &inst.0[1..]);
            assert_eq!(tt.bytes_read, res.1);
            assert_eq!(tt.operand, res.0);
        }
    }
}
