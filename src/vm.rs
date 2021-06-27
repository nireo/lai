use crate::{
    object,
    opcode::{self, Inst, OP_CONSTANT},
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
                    let const_index = opcode::read_u16(&self.insts.0[1..]) as usize;
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
    use super::*;
    use crate::{ast, compiler, parser, scanner};
    use std::any::Any;

    fn parse_program(input: &str) -> ast::Node {
        let lexer = scanner::Scanner::new(input);
        let mut parser = parser::Parser::new(lexer);

        let root_node = parser.parse_root_node();

        ast::Node::Root(Box::new(root_node))
    }

    struct VmTestcase<T> {
        input: String,
        expected: T,
    }

    fn run_vm_tests<T: 'static + Clone>(tests: Vec<VmTestcase<T>>) {
        for tt in tests.iter() {
            let root_node = parse_program(&tt.input);

            let mut compiler = compiler::Compiler::new();
            let res = compiler.compile(root_node);
            assert!(!res.is_none());

            let mut vm = VM::new(compiler.consts, compiler.insts);
            let res = vm.run();
            assert!(!res.is_none());

            let res = vm.stack_top();
            assert!(!res.is_none());
            let top = res.unwrap();

            test_expected_object(top, tt.expected.clone());
        }
    }

    fn test_expected_object<T: Any>(actual: &object::Object, expected: T) {
        match &actual {
            object::Object::Integer(val) => {
                let value_any = &expected as &dyn Any;

                match value_any.downcast_ref::<i32>() {
                    Some(as_i32) => {
                        println!("{}", as_i32.to_owned());
                        println!("{}", val.value);
                        assert!(as_i32.to_owned() == val.value);
                    }
                    _ => assert!(false),
                }
            }
            _ => assert!(false), // XD
        };
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests = vec![
            VmTestcase {
                input: "1".to_owned(),
                expected: 1,
            },
            VmTestcase {
                input: "2".to_owned(),
                expected: 2,
            },
            VmTestcase {
                input: "1 + 2;".to_owned(),
                expected: 2,
            },
        ];

        run_vm_tests(tests);
    }
}
