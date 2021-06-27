use crate::{
    object,
    opcode::{Inst, OP_CONSTANT},
};

const STACK_SIZE: usize = 2048;

pub struct VM {
    pub constants: Vec<object::Object>,
    pub insts: Inst,
    pub stack: Vec<object::Object>,
    pub sp: usize,
}

impl VM {
    pub fn new(constants: Vec<object::Object>, insts: Inst) -> Self {
        Self {
            sp: 0,
            stack: vec![object::Object::Null; STACK_SIZE],
            constants,
            insts,
        }
    }

    pub fn stack_top(&self) -> Option<&object::Object> {
        if self.sp == 0 {
            None
        } else {
            Some(&self.stack[self.sp - 1])
        }
    }

    pub fn run(&mut self) -> Option<()> {
        let mut ip: usize = 0;
        while ip < self.insts.0.len() {
            match self.insts.0[ip] {
                OP_CONSTANT => {
                    let const_index = (((self.insts.0[ip + 1] as u16) << 8)
                        | self.insts.0[ip + 2] as u16)
                        as usize;
                    ip += 2;

                    self.push(self.constants[const_index].clone())?;
                }
                _ => return None,
            };
        }

        Some(())
    }

    fn push(&mut self, obj: object::Object) -> Option<()> {
        if self.sp >= STACK_SIZE {
            None
        } else {
            self.stack[self.sp] = obj;
            self.sp += 1;

            Some(())
        }
    }
}

#[cfg(test)]
mod test {
    struct VmTestcase<T> {
        input: String,
        expected: T,
    }
}
