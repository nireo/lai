use crate::{
    builtin, object,
    opcode::{
        Inst, OP_ADD, OP_ARRAY, OP_BANG, OP_CALL, OP_CONSTANT, OP_DIV, OP_EQ, OP_FALSE,
        OP_GET_BUILTIN, OP_GET_GLOBAL, OP_GET_LOCAL, OP_GT, OP_INDEX, OP_JMP, OP_JMPNT, OP_MINUS,
        OP_MUL, OP_NE, OP_NULL, OP_POP, OP_RETURN, OP_RETURN_VALUE, OP_SET_GLOBAL, OP_SET_LOCAL,
        OP_SUB, OP_TRUE,
    },
};

static STACK_SIZE: usize = 2048;
const GLOBALS_SIZE: usize = 65536;

pub struct VM {
    constants: Vec<object::Object>,
    stack: Vec<object::Object>,
    globals: Vec<object::Object>,
    last: object::Object,
    frames: Vec<Frame>,
}

#[derive(Clone)]
struct Frame {
    ip: usize,
    func: object::CompiledFunction,
    base_pointer: usize,
}

impl Frame {
    fn new(func: object::CompiledFunction, base_pointer: usize) -> Self {
        Self {
            ip: 0,
            func,
            base_pointer,
        }
    }

    fn instructions(&self) -> &Inst {
        &self.func.instructions
    }
}

impl VM {
    pub fn new(constants: Vec<object::Object>, insts: Inst) -> Self {
        let mut globals = Vec::with_capacity(GLOBALS_SIZE);
        globals.resize_with(GLOBALS_SIZE, Default::default);

        let main_frame = Frame::new(object::CompiledFunction::new(insts), 0);
        let mut frames = Vec::with_capacity(1024);
        frames.push(main_frame);

        Self {
            stack: Vec::new(),
            constants,
            last: object::Object::Null,
            globals,
            frames,
        }
    }

    fn current_frame(&mut self) -> &mut Frame {
        let i = self.frames.len() - 1;
        self.frames.get_mut(i).unwrap()
    }

    fn pop_frame(&mut self) -> Option<Frame> {
        self.frames.pop()
    }

    fn push_frame(&mut self, f: Frame) {
        self.frames.push(f);
    }

    pub fn stack_top(&self) -> Option<object::Object> {
        if self.stack.is_empty() {
            None
        } else {
            Some(self.stack[self.stack.len() - 1].clone())
        }
    }

    pub fn get_last(&self) -> object::Object {
        self.last.clone()
    }

    pub fn run(&mut self) -> Result<(), String> {
        while self.current_frame().ip < self.current_frame().instructions().0.len() {
            let ip = self.current_frame().ip;
            let ins = self.current_frame().instructions();
            let op = ins.0[ip];

            match op {
                OP_CONSTANT => {
                    let const_index = u16::from_be_bytes([ins.0[ip + 1], ins.0[ip + 2]]) as usize;
                    self.push(self.constants[const_index].clone())?;
                    self.current_frame().ip += 3;
                }
                OP_ADD | OP_MUL | OP_SUB | OP_DIV => {
                    self.bin_operation(op)?;
                    self.current_frame().ip += 1;
                }
                OP_POP => {
                    self.last = self.pop()?;
                    self.current_frame().ip += 1;
                }
                OP_TRUE | OP_FALSE => {
                    self.push(object::Object::Bool(op == OP_TRUE))?;
                    self.current_frame().ip += 1;
                }
                OP_EQ | OP_NE | OP_GT => {
                    self.comparison(op)?;
                    self.current_frame().ip += 1;
                }
                OP_BANG => {
                    self.bang_operation()?;
                    self.current_frame().ip += 1;
                }
                OP_MINUS => {
                    self.minus_operation()?;
                    self.current_frame().ip += 1;
                }
                OP_JMP => {
                    let pos = u16::from_be_bytes([ins.0[ip + 1], ins.0[ip + 2]]) as usize;
                    self.current_frame().ip = pos;
                }
                OP_SET_GLOBAL => {
                    let index = u16::from_be_bytes([ins.0[ip + 1], ins.0[ip + 2]]) as usize;
                    self.current_frame().ip += 3;

                    self.globals[index] = self.pop()?;
                }
                OP_GET_GLOBAL => {
                    let index = u16::from_be_bytes([ins.0[ip + 1], ins.0[ip + 2]]) as usize;
                    self.current_frame().ip += 3;

                    self.push(self.globals[index].clone())?;
                }
                OP_GET_BUILTIN => {
                    let builtin_index = u8::from_be_bytes([ins.0[ip + 1]]);
                    self.current_frame().ip += 2;

                    let func = builtin::builtin_from_index(builtin_index as usize);
                    if func.is_none() {
                        return Err(String::from("invalid builtin function index"));
                    }

                    let func = func.unwrap();

                    self.push(object::Object::Builtin(func))?;
                }
                OP_JMPNT => {
                    let pos = u16::from_be_bytes([ins.0[ip + 1], ins.0[ip + 2]]) as usize;
                    self.current_frame().ip += 3;

                    let condition = self.pop()?;
                    if !VM::is_truthy(&condition) {
                        self.current_frame().ip = pos;
                    }
                }
                OP_NULL => {
                    self.push(object::Object::Null)?;
                    self.current_frame().ip += 1;
                }
                OP_SET_LOCAL => {
                    let local_index = u8::from_be_bytes([ins.0[ip + 1]]);
                    self.current_frame().ip += 2;

                    let base_pointer = self.current_frame().base_pointer;
                    self.stack[base_pointer + local_index as usize] = self.pop()?;
                }
                OP_GET_LOCAL => {
                    let local_index = u8::from_be_bytes([ins.0[ip + 1]]);
                    self.current_frame().ip += 2;

                    let base_pointer = self.current_frame().base_pointer;
                    self.push(self.stack[base_pointer + local_index as usize].clone())?;
                }
                OP_ARRAY => {
                    let element_count = u16::from_be_bytes([ins.0[ip + 1], ins.0[ip + 2]]) as usize;
                    self.current_frame().ip += 3;

                    let mut array_elements: Vec<object::Object> = Vec::new();
                    array_elements.resize(element_count, object::Object::Null);
                    // loop in reverse, since the last array element is last.
                    for i in (0..element_count).rev() {
                        array_elements[i] = self.pop()?;
                    }

                    self.push(object::Object::Array(array_elements))?;
                }
                OP_CALL => {
                    let num_args = u8::from_be_bytes([ins.0[ip + 1]]) as usize;
                    self.current_frame().ip += 1;
                    let func = self.stack.remove(self.stack.len() - 1 - num_args);

                    match &func {
                        object::Object::CompiledFunction(val) => {
                            if num_args != val.num_params {
                                return Err(format!(
                                    "wrong amount of arguments got: {} want: {}",
                                    num_args, val.num_params
                                ));
                            }

                            let frame =
                                Frame::new(val.clone(), self.stack.len() - num_args);
                            self.push_frame(frame);

                            for _ in 0..val.num_locals {
                                self.push(object::Object::Null)?;
                            }
                        }
                        object::Object::Builtin(val) => {
                            let args = self.stack
                                [(self.stack.len() - num_args)..self.stack.len()]
                                .to_vec();

                            let res = val(args);
                            self.current_frame().ip += 1;

                            self.push(res)?;
                        }
                        _ => {
                            println!(
                                "{:?} stack size: {}, first: {:?}",
                                func,
                                self.stack.len(),
                                self.stack[0]
                            );
                            return Err(String::from("calling a non-function"));
                        }
                    };
                }
                OP_RETURN_VALUE => {
                    let return_value = self.pop()?;
                    let base_pointer = self.pop_frame().unwrap().base_pointer;
                    while base_pointer < self.stack.len() {
                        self.pop()?;
                    }

                    self.push(return_value)?;
                    self.current_frame().ip += 1;
                }
                OP_RETURN => {
                    let base_pointer = self.pop_frame().unwrap().base_pointer;
                    while base_pointer < self.stack.len() {
                        self.pop()?;
                    }

                    self.push(object::Object::Null)?;
                    self.current_frame().ip += 1;
                }
                OP_INDEX => {
                    let idx = self.pop()?;
                    let lhs = self.pop()?;

                    match lhs {
                        object::Object::Array(elements) => match idx {
                            object::Object::Integer(index) => {
                                let mx = elements.len();
                                if index < 0 {
                                    return Err(String::from("indexing with a negative integer"));
                                }

                                let as_usize = index as usize;

                                if as_usize > mx {
                                    self.push(object::Object::Null)?;
                                    return Err(String::from("index is too big"));
                                }

                                self.push(elements[as_usize].clone())?;
                            }
                            _ => return Err(String::from("type doesn't support indexing")),
                        },
                        _ => return Err(String::from("indexing only works on arrays")),
                    };

                    self.current_frame().ip += 1;
                }
                _ => self.current_frame().ip += 1,
            };
        }

        Ok(())
    }

    fn is_truthy(obj: &object::Object) -> bool {
        match obj {
            object::Object::Bool(val) => *val,
            object::Object::Null => false,
            _ => true,
        }
    }

    fn bin_operation(&mut self, op: u8) -> Result<(), String> {
        let right_obj = self.pop()?;
        let left_obj = self.pop()?;

        match &left_obj {
            object::Object::Integer(left_value) => {
                let right_value = match &right_obj {
                    object::Object::Integer(value) => value,
                    _ => return Err(String::from("right object is not an integer")),
                };

                let value = match op {
                    OP_MUL => left_value * right_value,
                    OP_SUB => left_value - right_value,
                    OP_DIV => left_value / right_value,
                    OP_ADD => left_value + right_value,
                    _ => {
                        return Err(String::from(
                            "operator not supported for integer arithmetic",
                        ))
                    }
                };

                self.push(object::Object::Integer(value))?;
            }
            object::Object::String(left_value) => {
                let right_value = match &right_obj {
                    object::Object::String(value) => value,
                    _ => return Err(String::from("cannot add a non-string to a string")),
                };

                let value = match op {
                    OP_ADD => left_value.clone() + right_value,
                    _ => return Err(String::from("operation is not supported for strings")),
                };

                self.push(object::Object::String(value))?;
            }
            _ => {
                return Err(format!(
                    "types don't support arithmetic {:?} and {:?}",
                    left_obj, right_obj
                ))
            }
        };

        Ok(())
    }

    fn bang_operation(&mut self) -> Result<(), String> {
        let operand = self.pop()?;

        match &operand {
            object::Object::Bool(val) => self.push(object::Object::Bool(!val)),
            object::Object::Null => self.push(object::Object::Bool(true)),
            _ => self.push(object::Object::Bool(false)),
        }
    }

    fn minus_operation(&mut self) -> Result<(), String> {
        let operand = self.pop()?;

        match &operand {
            object::Object::Integer(val) => self.push(object::Object::Integer(-val)),
            _ => Ok(()), // minus is not supported for this type.
        }
    }

    fn comparison(&mut self, op: u8) -> Result<(), String> {
        let right_obj = self.pop()?;
        let left_obj = self.pop()?;

        match &right_obj {
            object::Object::Bool(rval) => match &left_obj {
                object::Object::Bool(lval) => self.push(object::Object::Bool(match op {
                    OP_EQ => rval == lval,
                    OP_NE => rval != lval,
                    _ => return Err(String::from("operation is not supported for booleans.")),
                })),
                _ => Err(String::from(
                    "cannot do comparison: unrecognized object on left",
                )),
            },
            object::Object::Integer(rval) => match &left_obj {
                object::Object::Integer(lval) => self.push(object::Object::Bool(match op {
                    OP_EQ => rval == lval,
                    OP_NE => rval != lval,
                    OP_GT => lval > rval,
                    _ => {
                        return Err(String::from(
                            "comparison operator is not supported for integers.",
                        ))
                    }
                })),
                _ => Err(String::from(
                    "cannot do comparison: unrecognized object on left",
                )),
            },
            _ => Err(String::from(
                "cannot do comparsion: unrecognized object on left",
            )),
        }
    }

    fn push(&mut self, obj: object::Object) -> Result<(), String> {
        if self.stack.len() >= STACK_SIZE {
            // stack overflow
            Err(String::from("cannot push: stack overflow"))
        } else {
            self.stack.push(obj);
            Ok(())
        }
    }

    fn pop(&mut self) -> Result<object::Object, String> {
        if self.stack.is_empty() {
            Err(String::from("cannot pop: stack is empty."))
        } else {
            Ok(self.stack.pop().unwrap())
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
        assert!(root_node.is_some());

        ast::Node::Root(Box::new(root_node.unwrap()))
    }

    struct VmTestcase<T> {
        input: String,
        expected: T,
    }

    fn run_vm_tests<T: 'static + Clone>(tests: Vec<VmTestcase<T>>) {
        for (idx, tt) in tests.iter().enumerate() {
            let root_node = parse_program(&tt.input);

            let mut compiler = compiler::Compiler::new();
            let res = compiler.compile(root_node);

            if res.is_err() {
                println!("test {} failed: {}", idx, res.as_ref().err().unwrap());
            }
            assert!(!res.is_err());

            let instructions = compiler.get_insts().clone();
            let mut vm = VM::new(compiler.consts, instructions);

            let res = vm.run();
            if res.is_err() {
                println!("test {} failed: {}", idx, res.as_ref().err().unwrap());
            }
            assert!(!res.is_err());

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
                        assert!(as_i32.to_owned() == val.clone());
                    }
                    _ => assert!(false),
                }
            }
            object::Object::Array(val) => {
                let expected_values = &expected as &dyn Any;
                match expected_values.downcast_ref::<Vec<i32>>() {
                    Some(as_arr) => {
                        for (i, obj) in val.iter().enumerate() {
                            match obj {
                                object::Object::Integer(int_val) => {
                                    assert!(int_val.clone() == as_arr[i])
                                }
                                _ => assert!(false),
                            }
                        }
                    }
                    _ => assert!(false),
                }
            }
            object::Object::Bool(val) => {
                let value_any = &expected as &dyn Any;

                match value_any.downcast_ref::<bool>() {
                    Some(as_bool) => {
                        assert!(as_bool.to_owned() == val.clone());
                    }
                    _ => assert!(false),
                }
            }
            object::Object::Null => {
                let value = &expected as &dyn Any;

                match value.downcast_ref::<object::Object>() {
                    Some(as_obj) => match as_obj {
                        object::Object::Null => assert!(true),
                        _ => assert!(false),
                    },
                    _ => assert!(false),
                }
            }
            object::Object::String(val) => {
                let value_any = &expected as &dyn Any;

                match value_any.downcast_ref::<String>() {
                    Some(as_string) => assert!(as_string.to_owned() == val.clone()),
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
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_global_let_statement() {
        let tests = vec![
            VmTestcase {
                input: "int one = 1; one".to_owned(),
                expected: 1,
            },
            VmTestcase {
                input: "int one = 1; int two = 2; one + two".to_owned(),
                expected: 3,
            },
            VmTestcase {
                input: "int one = 1; int two = one + one; one + two".to_owned(),
                expected: 3,
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_string_expressions() {
        let tests = vec![
            VmTestcase {
                input: "\"test\"".to_owned(),
                expected: "test".to_owned(),
            },
            VmTestcase {
                input: "\"te\" + \"st\"".to_owned(),
                expected: "test".to_owned(),
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_array_literals() {
        let tests = vec![
            VmTestcase {
                input: "[]".to_owned(),
                expected: vec![],
            },
            VmTestcase {
                input: "[1, 2, 3]".to_owned(),
                expected: vec![1, 2, 3],
            },
            VmTestcase {
                input: "[1 + 2, 3 * 4, 5 + 6]".to_owned(),
                expected: vec![3, 12, 11],
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_index_expression() {
        let tests = vec![
            VmTestcase {
                input: "[1, 2, 3][1]".to_owned(),
                expected: 2,
            },
            VmTestcase {
                input: "[1, 2, 3][0 + 2]".to_owned(),
                expected: 3,
            },
            VmTestcase {
                input: "[[1, 1, 1]][0][0]".to_owned(),
                expected: 1,
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_function_wo_arguments() {
        let tests = vec![VmTestcase {
            input: "fn func() -> int { 24 }; func();".to_owned(),
            expected: 24,
        }];

        run_vm_tests(tests);
    }

    #[test]
    fn empty_function_return_null() {
        let tests = vec![VmTestcase {
            input: "fn func() -> void { }; func();".to_owned(),
            expected: object::Object::Null,
        }];

        run_vm_tests(tests);
    }

    #[test]
    fn local_bindings() {
        let tests = vec![
            VmTestcase {
                input: "fn returnsone() -> int { int one = 1; return one; }; returnsone();"
                    .to_owned(),
                expected: 1,
            },
            VmTestcase {
                input: "fn xdxd() -> int { int one = 1; int two = 2; return one + two; }; xdxd();"
                    .to_owned(),
                expected: 3,
            },
            VmTestcase {
                input: "fn first() -> int { int one = 1; return one; };
                            fn second() -> int { int two = 2; return two; };
                            first() + second();"
                    .to_owned(),
                expected: 3,
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn function_with_arguments() {
        let tests = vec![
            VmTestcase {
                input: "fn identity(int a) -> int { return a; }; identity(5);".to_owned(),
                expected: 5,
            },
            VmTestcase {
                input: "fn sum(int a, int b) -> int { return a + b; }; sum(1, 2);".to_owned(),
                expected: 3,
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn builtin_test() {
        let tests = vec![VmTestcase {
            input: "len(\"lai\")".to_owned(),
            expected: 3,
        }];

        run_vm_tests(tests);
    }
}
