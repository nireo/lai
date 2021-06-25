use std::collections::HashMap;

const OP_CONSTANT: u8 = 0;

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

    pub fn read_operand(operand_width: usize, insts: &Inst) -> usize {
        let mut offset = 0;

        match operand_width {
            2 => {
                // this is so fucking bad
                (((insts.0[0] as u16) << 8) | insts.0[1] as u16) as usize
            }
            _ => 0,
        }
    }
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
}
