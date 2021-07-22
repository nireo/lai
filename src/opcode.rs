// We use constants, but then we would need to do some converting when adding
// instructions or reading them.
pub const OP_CONSTANT: u8 = 0;
pub const OP_ADD: u8 = 1;
pub const OP_POP: u8 = 2;
pub const OP_SUB: u8 = 3;
pub const OP_MUL: u8 = 4;
pub const OP_DIV: u8 = 5;
pub const OP_TRUE: u8 = 6;
pub const OP_FALSE: u8 = 7;
pub const OP_EQ: u8 = 8;
pub const OP_NE: u8 = 9;
pub const OP_GT: u8 = 10;
pub const OP_MINUS: u8 = 11;
pub const OP_BANG: u8 = 12;
pub const OP_JMPNT: u8 = 13;
pub const OP_JMP: u8 = 14;
pub const OP_NULL: u8 = 15;
pub const OP_GET_GLOBAL: u8 = 16;
pub const OP_SET_GLOBAL: u8 = 17;
pub const OP_ARRAY: u8 = 18;
pub const OP_INDEX: u8 = 19;
pub const OP_CALL: u8 = 20;
pub const OP_RETURN_VALUE: u8 = 21;
pub const OP_RETURN: u8 = 22;
pub const OP_GET_LOCAL: u8 = 23;
pub const OP_SET_LOCAL: u8 = 24;

#[derive(Clone, Debug)]
pub struct Inst(pub Vec<u8>);

// we could use a hashmap but this allows us not to have global variables, and different
// crates to allow construction of hashmaps before runtime.
fn get_definition(opcode: u8) -> usize {
    match opcode {
        OP_CONSTANT => 2,
        OP_JMPNT => 2,
        OP_JMP => 2,
        OP_GET_GLOBAL => 2,
        OP_SET_GLOBAL => 2,
        OP_ARRAY => 2,
        OP_GET_LOCAL => 1,
        OP_SET_LOCAL => 1,
        OP_CALL => 1,
        _ => 0,
    }
}

pub fn make(opcode: u8, operand: usize) -> Option<Inst> {
    let wanted_width = get_definition(opcode);
    let mut inst: Vec<u8> = vec![0_u8; 1];
    inst[0] = opcode;

    match wanted_width {
        2 => inst.extend_from_slice(&(operand as u16).to_be_bytes()),
        1 => inst.push(operand as u8),
        0 => return Some(Inst(inst)),
        _ => return None,
    };

    Some(Inst(inst))
}

pub fn make_simple(opcode: u8) -> Inst {
    let inst = vec![opcode; 1];
    Inst(inst)
}

pub fn read_operand(operand_width: usize, insts: &[u8]) -> (usize, usize) {
    let mut offset = 0;

    let res = match operand_width {
        2 => {
            // this is so fucking bad
            (((insts[0] as u16) << 8) | insts[1] as u16) as usize
        }
        1 => {
            insts[0] as usize
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

        let expected = vec![
            MakeTestcase {
                op: OP_CONSTANT,
                operand: 65534,
                expected_insts: vec![OP_CONSTANT, 255, 254],
            },
            MakeTestcase {
                op: OP_GET_LOCAL,
                operand: 255,
                expected_insts: vec![OP_GET_LOCAL, 255],
            },
        ];

        for tt in expected.iter() {
            let inst = make(tt.op, tt.operand);
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

        let tests = vec![
            ReadOperandTestcase {
                op: OP_CONSTANT,
                operand: 65535,
                bytes_read: 2,
            },
            ReadOperandTestcase {
                op: OP_GET_LOCAL,
                operand: 255,
                bytes_read: 1,
            },
        ];

        for tt in tests.iter() {
            let inst = make(tt.op, tt.operand);
            assert!(!inst.is_none());

            let inst = inst.unwrap();

            let def = get_definition(tt.op);

            let res = read_operand(def.to_owned(), &inst.0[1..]);
            assert_eq!(tt.bytes_read, res.1);
            assert_eq!(tt.operand, res.0);
        }
    }
}
