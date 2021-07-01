use crate::{
    object,
    opcode::{
        Inst, OP_ADD, OP_BANG, OP_CONSTANT, OP_DIV, OP_EQ, OP_FALSE, OP_GT, OP_MINUS, OP_MUL,
        OP_NE, OP_POP, OP_SUB, OP_TRUE, OP_JMP, OP_JMPNT, OP_NULL,
    },
};

static STACK_SIZE: usize = 2048;

pub struct VM {
    constants: Vec<object::Object>,
    insts: Inst,
    stack: Vec<object::Object>,
    last: object::Object,
}

impl VM {
    pub fn new(constants: Vec<object::Object>, insts: Inst) -> Self {
        Self {
            stack: Vec::new(),
            constants,
            insts,
            last: object::Object::Null,
        }
    }

    pub fn stack_top(&self) -> Option<object::Object> {
        if self.stack.len() == 0 {
            None
        } else {
            Some(self.stack[self.stack.len() - 1].clone())
        }
    }

    pub fn get_last(&self) -> object::Object {
        self.last.clone()
    }

    pub fn run(&mut self) -> Option<()> {
        let mut ip: usize = 0;
        while ip < self.insts.0.len() {
            match self.insts.0[ip] {
                OP_CONSTANT => {
                    let const_index =
                        u16::from_be_bytes([self.insts.0[ip + 1], self.insts.0[ip + 2]]) as usize;
                    self.push(self.constants[const_index].clone())?;
                    ip += 2;
                }
                OP_ADD | OP_MUL | OP_SUB | OP_DIV => {
                    self.bin_operation(self.insts.0[ip].clone())?;
                }
                OP_POP => {
                    self.last = self.pop()?;
                }
                OP_TRUE | OP_FALSE => {
                    self.push(object::Object::Bool(object::ValueObj::new(
                        self.insts.0[ip] == OP_TRUE,
                    )))?;
                }
                OP_EQ | OP_NE | OP_GT => {
                    self.comparison(self.insts.0[ip].clone())?;
                }
                OP_BANG => {
                    self.bang_operation()?;
                }
                OP_MINUS => {
                    self.minus_operation()?;
                }
                OP_JMP =>  {
                    let pos =
                        u16::from_be_bytes([self.insts.0[ip + 1], self.insts.0[ip + 2]]) as usize;
                    ip = pos - 1;

                }
                OP_JMPNT => {
                    let pos =
                        u16::from_be_bytes([self.insts.0[ip + 1], self.insts.0[ip + 2]]) as usize;
                    ip += 2;

                    let condition = self.pop()?;
                    if !VM::is_truthy(&condition) {
                        ip = pos - 1;
                    }
                }
                OP_NULL => {
                    self.push(object::Object::Null)?;
                }
                _ => return None,
            };

            ip += 1;
        }

        Some(())
    }

    fn is_truthy(obj: &object::Object) -> bool {
        match obj {
            object::Object::Bool(val) => {
                val.value
            }
            _ => true,
        }
    }

    fn bin_operation(&mut self, op: u8) -> Option<()> {
        let right_obj = self.pop()?;
        let left_obj = self.pop()?;

        let left_value = match &left_obj {
            object::Object::Integer(value) => value.value,
            _ => return None,
        };

        let right_value = match &right_obj {
            object::Object::Integer(value) => value.value,
            _ => return None,
        };

        let value = match op {
            OP_MUL => left_value * right_value,
            OP_SUB => left_value - right_value,
            OP_DIV => left_value / right_value,
            OP_ADD => left_value + right_value,
            _ => return None,
        };

        self.push(object::Object::Integer(object::ValueObj::new(value)))?;

        Some(())
    }

    fn bang_operation(&mut self) -> Option<()> {
        let operand = self.pop()?;

        match &operand {
            object::Object::Bool(val) => {
                self.push(object::Object::Bool(object::ValueObj::new(!val.value)))
            }
            _ => self.push(object::Object::Bool(object::ValueObj::new(false))),
        }
    }

    fn minus_operation(&mut self) -> Option<()> {
        let operand = self.pop()?;

        match &operand {
            object::Object::Integer(val) => {
                self.push(object::Object::Integer(object::ValueObj::new(-val.value)))
            }
            _ => None, // minus is not supported for this type.
        }
    }

    fn comparison(&mut self, op: u8) -> Option<()> {
        let right_obj = self.pop()?;
        let left_obj = self.pop()?;

        match &right_obj {
            object::Object::Bool(rval) => match &left_obj {
                object::Object::Bool(lval) => {
                    self.push(object::Object::Bool(object::ValueObj::new(match op {
                        OP_EQ => rval.value == lval.value,
                        OP_NE => rval.value != lval.value,
                        _ => return None,
                    })))
                }
                _ => None,
            },
            object::Object::Integer(rval) => match &left_obj {
                object::Object::Integer(lval) => {
                    self.push(object::Object::Bool(object::ValueObj::new(match op {
                        OP_EQ => rval.value == lval.value,
                        OP_NE => rval.value != lval.value,
                        OP_GT => lval.value > rval.value,
                        _ => return None,
                    })))
                }
                _ => None,
            },
            _ => Some(()),
        }
    }

    fn push(&mut self, obj: object::Object) -> Option<()> {
        if self.stack.len() >= STACK_SIZE {
            // stack overflow
            None
        } else {
            self.stack.push(obj);
            Some(())
        }
    }

    fn pop(&mut self) -> Option<object::Object> {
        if self.stack.len() == 0 {
            // stack empty
            None
        } else {
            Some(self.stack.pop().unwrap())
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
            println!("{}", tt.input);

            let mut compiler = compiler::Compiler::new();
            let res = compiler.compile(root_node);
            assert!(!res.is_none());

            let mut vm = VM::new(compiler.consts, compiler.insts);

            let res = vm.run();
            assert!(!res.is_none());

            let top = vm.get_last();
            test_expected_object(&top, tt.expected.clone());
        }
    }

    fn test_expected_object<T: Any>(actual: &object::Object, expected: T) {
        match &actual {
            object::Object::Integer(val) => {
                let value_any = &expected as &dyn Any;

                match value_any.downcast_ref::<i32>() {
                    Some(as_i32) => {
                        assert!(as_i32.to_owned() == val.value);
                    }
                    _ => assert!(false),
                }
            }
            object::Object::Bool(val) => {
                let value_any = &expected as &dyn Any;

                match value_any.downcast_ref::<bool>() {
                    Some(as_bool) => {
                        assert!(as_bool.to_owned() == val.value);
                    }
                    _ => assert!(false),
                }
            }
            object::Object::Null => {
                let value = &expected as &dyn Any;

                match value.downcast_ref::<object::Object>() {
                    Some(as_obj) => {
                        match as_obj {
                            object::Object::Null => assert!(true),
                            _ => assert!(false),
                        }
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
                input: "1 + 2".to_owned(),
                expected: 3,
            },
            VmTestcase {
                input: "1 - 2".to_owned(),
                expected: -1,
            },
            VmTestcase {
                input: "1 * 2".to_owned(),
                expected: 2,
            },
            VmTestcase {
                input: "4 / 2".to_owned(),
                expected: 2,
            },
            VmTestcase {
                input: "5 + 5 + 5 + 5 - 10".to_owned(),
                expected: 10,
            },
            VmTestcase {
                input: "50 / 2 * 2 + 10 - 5".to_owned(),
                expected: 55,
            },
            VmTestcase {
                input: "-5".to_owned(),
                expected: -5,
            },
            VmTestcase {
                input: "-10".to_owned(),
                expected: -10,
            },
            VmTestcase {
                input: "-50 + 100 + -50".to_owned(),
                expected: 0,
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_boolean_arithmetic() {
        let tests = vec![
            VmTestcase {
                input: "true".to_owned(),
                expected: true,
            },
            VmTestcase {
                input: "false".to_owned(),
                expected: false,
            },
            VmTestcase {
                input: "1 < 2".to_owned(),
                expected: true,
            },
            VmTestcase {
                input: "1 > 2".to_owned(),
                expected: false,
            },
            VmTestcase {
                input: "1 < 1".to_owned(),
                expected: false,
            },
            VmTestcase {
                input: "1 > 1".to_owned(),
                expected: false,
            },
            VmTestcase {
                input: "1 == 1".to_owned(),
                expected: true,
            },
            VmTestcase {
                input: "1 != 1".to_owned(),
                expected: false,
            },
            VmTestcase {
                input: "true == true".to_owned(),
                expected: true,
            },
            VmTestcase {
                input: "false == false".to_owned(),
                expected: true,
            },
            VmTestcase {
                input: "true == false".to_owned(),
                expected: false,
            },
            VmTestcase {
                input: "true != false".to_owned(),
                expected: true,
            },
            VmTestcase {
                input: "false != true".to_owned(),
                expected: true,
            },
            VmTestcase {
                input: "(1 < 2) == true".to_owned(),
                expected: true,
            },
            VmTestcase {
                input: "(1 < 2) == false".to_owned(),
                expected: false,
            },
            VmTestcase {
                input: "!true".to_owned(),
                expected: false,
            },
            VmTestcase {
                input: "!false".to_owned(),
                expected: true,
            },
            VmTestcase {
                input: "!5".to_owned(),
                expected: false,
            },
            VmTestcase {
                input: "!!true".to_owned(),
                expected: true,
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_conditionals() {
        let tests = vec![
            VmTestcase {
                input: "if (true) { 10 }".to_owned(),
                expected: 10,
            },
            VmTestcase {
                input: "if (true) { 10 } else { 20 }".to_owned(),
                expected: 10,
            },
            VmTestcase {
                input: "if (false) { 10 } else { 20 }".to_owned(),
                expected: 20,
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_null_conditionals() {
        let tests = vec![
            VmTestcase {
                input: "if (1 > 2) { 10 }".to_owned(),
                expected: object::Object::Null,
            },
            VmTestcase {
                input: "if (false) { 10 }".to_owned(),
                expected: object::Object::Null,
            }
        ];

        run_vm_tests(tests);
    }
}
