use crate::{
    ast,
    object::{self, ValueObj},
    opcode::{make, Inst, OP_CONSTANT},
};

pub struct Compiler {
    pub insts: Inst,
    pub consts: Vec<object::Object>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            consts: Vec::new(),
            insts: Inst(Vec::new()),
        }
    }

    pub fn get_consts(&self) -> &[object::Object] {
        &self.consts
    }

    pub fn get_insts(&self) -> &Inst {
        &self.insts
    }

    pub fn compile(&mut self, node: ast::Node) -> Option<()> {
        match node {
            ast::Node::Root(value) => {
                for st in value.statements.iter() {
                    self.compile(ast::Node::Statement(Box::new(st.clone())))?;
                }
                Some(())
            }
            ast::Node::Statement(st) => match *st {
                ast::Statement::Expression(exp) => {
                    self.compile(ast::Node::Expression(Box::new(exp.value)))?;
                    Some(())
                }
                _ => None,
            },
            ast::Node::Expression(exp) => match *exp {
                ast::Expression::Infix(e) => {
                    self.compile(ast::Node::Expression(e.lhs))?;
                    self.compile(ast::Node::Expression(e.rhs))?;

                    Some(())
                }
                ast::Expression::Integer(e) => {
                    let int_obj = object::Object::Integer(ValueObj::new(e.value));
                    let pos = self.add_constant(int_obj);
                    self.emit(OP_CONSTANT, pos);

                    Some(())
                }
                _ => None,
            },
        }
    }

    fn add_constant(&mut self, obj: object::Object) -> usize {
        self.consts.push(obj);
        self.consts.len() - 1
    }

    fn emit(&mut self, op: u8, operand: usize) -> usize {
        let inst = make(op, operand).unwrap();
        let pos = self.add_inst(&inst.0);

        pos
    }

    fn add_inst(&mut self, ins: &[u8]) -> usize {
        let pos_new_inst = self.insts.0.len();
        self.insts.0.extend_from_slice(ins);

        pos_new_inst
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{parser, scanner};
    use std::any::Any;
    struct CompilerTestcase<T> {
        input: String,
        expected_consts: Vec<T>,
        expected_insts: Vec<Inst>,
    }

    fn parse_program(input: &str) -> ast::Node {
        let lexer = scanner::Scanner::new(input);
        let mut parser = parser::Parser::new(lexer);

        let root_node = parser.parse_root_node();

        ast::Node::Root(Box::new(root_node))
    }

    fn concat_instructions(instructions: &[Inst]) -> Inst {
        let mut out: Vec<u8> = Vec::new();

        for inst in instructions.iter() {
            out.extend_from_slice(&inst.0);
        }

        return Inst(out);
    }

    fn test_instructions(expected: &[Inst], actual: Inst) {
        let concatted = concat_instructions(expected);
        assert_eq!(concatted.0.len(), actual.0.len());

        for (i, ins) in concatted.0.iter().enumerate() {
            assert_eq!(actual.0[i], ins.to_owned());
        }
    }

    fn test_constants<T: Any + 'static>(expected: Vec<T>, actual: &[object::Object]) {
        assert_eq!(expected.len(), actual.len());

        for (i, cnst) in expected.iter().enumerate() {
            match &actual[i] {
                object::Object::Integer(val) => {
                    let value_any = cnst as &dyn Any;

                    match value_any.downcast_ref::<i32>() {
                        Some(as_i32) => assert!(as_i32.to_owned() == val.value),
                        _ => assert!(false),
                    }
                }
                _ => assert!(false), // XD
            };
        }
    }

    fn run_compiler_test<T: 'static + Clone>(tests: Vec<CompilerTestcase<T>>) {
        for tt in tests.iter() {
            let root_node = parse_program(&tt.input);

            let mut compiler = Compiler::new();
            let res = compiler.compile(root_node);
            assert!(!res.is_none());

            let consts = compiler.get_consts().to_owned();
            test_constants(tt.expected_consts.clone(), &consts);

            let insts = compiler.get_insts().to_owned();
            test_instructions(&tt.expected_insts, Inst(insts.0.clone()));
        }
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests = vec![CompilerTestcase {
            input: "1 + 2;".to_owned(),
            expected_consts: vec![1, 2],
            expected_insts: vec![make(OP_CONSTANT, 0).unwrap(), make(OP_CONSTANT, 1).unwrap()],
        }];

        run_compiler_test(tests);
    }
}
