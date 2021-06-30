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

pub struct Inst(pub Vec<u8>);

// we could use a hashmap but this allows us not to have global variables, and different
// crates to allow construction of hashmaps before runtime.
fn get_definition(opcode: u8) -> usize {
    match opcode {
        OP_CONSTANT => 2,
        OP_ADD => 0,
        OP_POP => 0,
        OP_DIV => 0,
        OP_MUL => 0,
        OP_SUB => 0,
        _ => u8::max_value as usize,
    }
}

pub fn make(opcode: u8, operand: usize) -> Option<Inst> {
    let wanted_width = get_definition(opcode);
    let mut inst: Vec<u8> = vec![0_u8; 1];
    inst[0] = opcode;

    match wanted_width {
        2 => inst.extend_from_slice(&(operand as u16).to_be_bytes()),
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

        let expected = vec![MakeTestcase {
            op: OP_CONSTANT,
            operand: 65534,
            expected_insts: vec![OP_CONSTANT, 255, 254],
        }];

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

        let tests = vec![ReadOperandTestcase {
            op: OP_CONSTANT,
            operand: 65535,
            bytes_read: 2,
        }];

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
