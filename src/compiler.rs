use std::collections::HashMap;

use crate::{
    ast, object,
    opcode::{
        make, make_simple, Inst, OP_ADD, OP_ARRAY, OP_BANG, OP_CONSTANT, OP_DIV, OP_EQ, OP_FALSE,
        OP_GET_GLOBAL, OP_GT, OP_INDEX, OP_JMP, OP_JMPNT, OP_MINUS, OP_MUL, OP_NE, OP_NULL, OP_POP,
        OP_SET_GLOBAL, OP_SUB, OP_TRUE,
    },
    scanner::{self, Token},
};

#[derive(PartialEq, Debug)]
pub enum Scope {
    Global,
}

pub struct Symbol {
    pub name: String,
    pub scope: Scope,
    pub index: usize,
    pub value_type: scanner::Token,
}

struct SymbolTable {
    pub store: HashMap<String, Symbol>,
    pub definition_count: usize,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            definition_count: 0,
            store: HashMap::new(),
        }
    }

    fn resolve(&self, name: String) -> Option<&Symbol> {
        self.store.get(&name)
    }

    fn define(&mut self, name: String, value_type: Token) -> &Symbol {
        let symbol = Symbol {
            name: name.clone(),
            index: self.definition_count,
            scope: Scope::Global,
            value_type,
        };

        self.store.insert(name.clone(), symbol);
        self.definition_count += 1;

        // TODO: a better way to do this?
        self.store.get(&name).unwrap()
    }
}

struct EmittedInstruction {
    opcode: u8,
    pos: usize,
}

pub struct Compiler {
    pub insts: Inst,
    pub consts: Vec<object::Object>,

    last_inst: EmittedInstruction,
    prev_inst: EmittedInstruction,
    symbol_table: SymbolTable,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            consts: Vec::new(),
            insts: Inst(Vec::new()),

            last_inst: EmittedInstruction { opcode: 0, pos: 0 },
            prev_inst: EmittedInstruction { opcode: 0, pos: 0 },
            symbol_table: SymbolTable::new(),
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
                    self.emit_single(OP_POP);

                    Some(())
                }
                ast::Statement::Block(exp) => {
                    for st in exp.statements.iter() {
                        self.compile(ast::Node::Statement(Box::new(st.clone())))?;
                    }
                    Some(())
                }
                ast::Statement::Assigment(exp) => {
                    self.compile(ast::Node::Expression(Box::new(exp.value)))?;
                    let symbol = self.symbol_table.define(exp.name, exp.variable_type);
                    let index = symbol.index;
                    self.emit(OP_SET_GLOBAL, index);

                    Some(())
                }
                _ => None,
            },
            ast::Node::Expression(exp) => match *exp {
                ast::Expression::String(exp) => {
                    let str_const = object::Object::String(exp.value.clone());
                    let pos = self.add_constant(str_const);
                    self.emit(OP_CONSTANT, pos);

                    Some(())
                }
                ast::Expression::Array(exp) => {
                    for elem in exp.elements.iter() {
                        self.compile(ast::Node::Expression(Box::new(elem.clone())))?;
                    }
                    self.emit(OP_ARRAY, exp.elements.len());

                    Some(())
                }
                ast::Expression::Infix(e) => {
                    if e.operator == Token::LessThan {
                        self.compile(ast::Node::Expression(e.rhs))?;
                        self.compile(ast::Node::Expression(e.lhs))?;

                        self.emit_single(OP_GT);
                    } else {
                        self.compile(ast::Node::Expression(e.lhs))?;
                        self.compile(ast::Node::Expression(e.rhs))?;

                        match e.operator {
                            Token::Plus => self.emit_single(OP_ADD),
                            Token::Slash => self.emit_single(OP_DIV),
                            Token::Asterisk => self.emit_single(OP_MUL),
                            Token::Minus => self.emit_single(OP_SUB),
                            Token::GreaterThan => self.emit_single(OP_GT),
                            Token::Equals => self.emit_single(OP_EQ),
                            Token::NEquals => self.emit_single(OP_NE),
                            _ => return None, // non recognized/supported operator
                        };
                    }
                    Some(())
                }
                ast::Expression::Index(exp) => {
                    self.compile(ast::Node::Expression(exp.lhs))?;
                    self.compile(ast::Node::Expression(exp.index))?;

                    self.emit_single(OP_INDEX);

                    Some(())
                }
                ast::Expression::Identifier(exp) => {
                    let symbol = self.symbol_table.resolve(exp.name)?;
                    // we need to store this in a variable because otherwise the compiler doesn't like it
                    let index = symbol.index;
                    self.emit(OP_GET_GLOBAL, index);

                    Some(())
                }
                ast::Expression::If(e) => {
                    self.compile(ast::Node::Expression(e.cond))?;
                    let not_true_pos = self.emit(OP_JMPNT, 9999);

                    self.compile(ast::Node::Statement(e.after))?;

                    if self.last_instruction_is_pop() {
                        self.remove_last_pop();
                    }

                    let jmp_pos = self.emit(OP_JMP, 9999);
                    let after_pos = self.insts.0.len();
                    self.change_operand(not_true_pos, after_pos);

                    if e.other.is_none() {
                        self.emit_single(OP_NULL);
                    } else {
                        self.compile(ast::Node::Statement(e.other.unwrap()))?;

                        if self.last_instruction_is_pop() {
                            self.remove_last_pop();
                        }
                    }

                    let after_other = self.insts.0.len();
                    self.change_operand(jmp_pos, after_other);

                    Some(())
                }
                ast::Expression::Integer(e) => {
                    let int_obj = object::Object::Integer(e.value);
                    let pos = self.add_constant(int_obj);
                    self.emit(OP_CONSTANT, pos);

                    Some(())
                }
                ast::Expression::Boolean(e) => {
                    if e.value {
                        self.emit_single(OP_TRUE);
                    } else {
                        self.emit_single(OP_FALSE);
                    }

                    Some(())
                }
                ast::Expression::Prefix(e) => {
                    self.compile(ast::Node::Expression(e.rhs))?;

                    match &e.operator {
                        Token::Exclamation => self.emit_single(OP_BANG),
                        Token::Minus => self.emit_single(OP_MINUS),
                        _ => return None, // unregocnized error
                    };

                    Some(())
                }
                _ => None,
            },
        }
    }

    fn set_last_instruction(&mut self, opcode: u8, pos: usize) {
        let prev = EmittedInstruction {
            opcode: self.last_inst.opcode,
            pos: self.last_inst.pos,
        };
        let last = EmittedInstruction { opcode, pos };

        self.prev_inst = prev;
        self.last_inst = last;
    }

    fn add_constant(&mut self, obj: object::Object) -> usize {
        self.consts.push(obj);
        self.consts.len() - 1
    }

    fn change_operand(&mut self, pos: usize, operand: usize) {
        let op = self.insts.0[pos];
        let new_inst = make(op, operand).unwrap();

        self.replace_instruction(pos, new_inst.0);
    }

    fn emit(&mut self, op: u8, operand: usize) -> usize {
        let inst = make(op, operand).unwrap();
        let pos = self.add_inst(&inst.0);

        self.set_last_instruction(op, pos);

        pos
    }

    fn emit_single(&mut self, op: u8) -> usize {
        let inst = make_simple(op);
        let pos = self.add_inst(&inst.0);

        self.set_last_instruction(op, pos);

        pos
    }

    fn last_instruction_is_pop(&self) -> bool {
        self.last_inst.opcode == OP_POP
    }

    fn remove_last_pop(&mut self) {
        self.insts.0 = self.insts.0[..self.last_inst.pos].to_owned();
        self.last_inst = EmittedInstruction {
            opcode: self.prev_inst.opcode,
            pos: self.prev_inst.pos,
        };
    }

    fn replace_instruction(&mut self, pos: usize, insts: Vec<u8>) {
        for i in 0..insts.len() {
            self.insts.0[pos + i] = insts[i];
        }
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
    use crate::{opcode::{self, OP_ARRAY, OP_INDEX, OP_JMP, OP_JMPNT, OP_RETURN_VALUE}, parser, scanner};
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

                    let mut ok = match value_any.downcast_ref::<i32>() {
                        Some(as_i32) => as_i32.to_owned() == val.clone(),
                        _ => false,
                    };

                    // check if the value is an object.
                    if !ok {
                        ok = match value_any.downcast_ref::<object::Object>() {
                            Some(integer_obj) => match integer_obj {
                                object::Object::Integer(v) => v.to_owned() == val.clone(),
                                _ => false,
                            },
                            _ => false,
                        }
                    }

                    assert!(ok);
                }
                object::Object::String(val) => {
                    let value_any = cnst as &dyn Any;

                    match value_any.downcast_ref::<String>() {
                        Some(as_string) => assert!(as_string.to_owned() == val.clone()),
                        _ => assert!(false),
                    }
                }
                object::Object::CompiledFunction(val) => {
                    let value_any = cnst as &dyn Any;

                    match value_any.downcast_ref::<opcode::Inst>() {
                        Some(got_inst) => {
                            test_instructions(&[val.instructions.clone()], got_inst.clone())
                        }
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
        let tests = vec![
            CompilerTestcase {
                input: "1 + 2;".to_owned(),
                expected_consts: vec![1, 2],
                expected_insts: vec![
                    make(OP_CONSTANT, 0).unwrap(),
                    make(OP_CONSTANT, 1).unwrap(),
                    make_simple(OP_ADD),
                    make_simple(OP_POP),
                ],
            },
            CompilerTestcase {
                input: "1 - 2;".to_owned(),
                expected_consts: vec![1, 2],
                expected_insts: vec![
                    make(OP_CONSTANT, 0).unwrap(),
                    make(OP_CONSTANT, 1).unwrap(),
                    make_simple(OP_SUB),
                    make_simple(OP_POP),
                ],
            },
            CompilerTestcase {
                input: "1 * 2;".to_owned(),
                expected_consts: vec![1, 2],
                expected_insts: vec![
                    make(OP_CONSTANT, 0).unwrap(),
                    make(OP_CONSTANT, 1).unwrap(),
                    make_simple(OP_MUL),
                    make_simple(OP_POP),
                ],
            },
            CompilerTestcase {
                input: "1 / 2;".to_owned(),
                expected_consts: vec![1, 2],
                expected_insts: vec![
                    make(OP_CONSTANT, 0).unwrap(),
                    make(OP_CONSTANT, 1).unwrap(),
                    make_simple(OP_DIV),
                    make_simple(OP_POP),
                ],
            },
            CompilerTestcase {
                input: "-1".to_owned(),
                expected_consts: vec![1],
                expected_insts: vec![
                    make(OP_CONSTANT, 0).unwrap(),
                    make_simple(OP_MINUS),
                    make_simple(OP_POP),
                ],
            },
        ];

        run_compiler_test(tests);
    }

    #[test]
    fn test_boolean_expressions() {
        let tests: Vec<CompilerTestcase<i32>> = vec![
            CompilerTestcase {
                input: "true".to_owned(),
                expected_consts: vec![],
                expected_insts: vec![make_simple(OP_TRUE), make_simple(OP_POP)],
            },
            CompilerTestcase {
                input: "false".to_owned(),
                expected_consts: vec![],
                expected_insts: vec![make_simple(OP_FALSE), make_simple(OP_POP)],
            },
            CompilerTestcase {
                input: "1 > 2".to_owned(),
                expected_consts: vec![1, 2],
                expected_insts: vec![
                    make(OP_CONSTANT, 0).unwrap(),
                    make(OP_CONSTANT, 1).unwrap(),
                    make_simple(OP_GT),
                    make_simple(OP_POP),
                ],
            },
            CompilerTestcase {
                input: "1 < 2".to_owned(),
                expected_consts: vec![2, 1],
                expected_insts: vec![
                    make(OP_CONSTANT, 0).unwrap(),
                    make(OP_CONSTANT, 1).unwrap(),
                    make_simple(OP_GT),
                    make_simple(OP_POP),
                ],
            },
            CompilerTestcase {
                input: "1 == 2".to_owned(),
                expected_consts: vec![1, 2],
                expected_insts: vec![
                    make(OP_CONSTANT, 0).unwrap(),
                    make(OP_CONSTANT, 1).unwrap(),
                    make_simple(OP_EQ),
                    make_simple(OP_POP),
                ],
            },
            CompilerTestcase {
                input: "1 != 2".to_owned(),
                expected_consts: vec![1, 2],
                expected_insts: vec![
                    make(OP_CONSTANT, 0).unwrap(),
                    make(OP_CONSTANT, 1).unwrap(),
                    make_simple(OP_NE),
                    make_simple(OP_POP),
                ],
            },
            CompilerTestcase {
                input: "true == false".to_owned(),
                expected_consts: vec![],
                expected_insts: vec![
                    make_simple(OP_TRUE),
                    make_simple(OP_FALSE),
                    make_simple(OP_EQ),
                    make_simple(OP_POP),
                ],
            },
            CompilerTestcase {
                input: "true != false".to_owned(),
                expected_consts: vec![],
                expected_insts: vec![
                    make_simple(OP_TRUE),
                    make_simple(OP_FALSE),
                    make_simple(OP_NE),
                    make_simple(OP_POP),
                ],
            },
            CompilerTestcase {
                input: "!true".to_owned(),
                expected_consts: vec![],
                expected_insts: vec![
                    make_simple(OP_TRUE),
                    make_simple(OP_BANG),
                    make_simple(OP_POP),
                ],
            },
        ];

        run_compiler_test(tests);
    }

    #[test]
    fn test_conditionals() {
        let tests = vec![
            CompilerTestcase {
                input: "if (true) { 10 } else { 20 }; 3333;".to_owned(),
                expected_consts: vec![10, 20, 3333],
                expected_insts: vec![
                    make_simple(OP_TRUE),
                    make(OP_JMPNT, 10).unwrap(),
                    make(OP_CONSTANT, 0).unwrap(),
                    make(OP_JMP, 13).unwrap(),
                    make(OP_CONSTANT, 1).unwrap(),
                    make_simple(OP_POP),
                    make(OP_CONSTANT, 2).unwrap(),
                    make_simple(OP_POP),
                ],
            },
            CompilerTestcase {
                input: "if (true) { 10 }; 3333;".to_owned(),
                expected_consts: vec![10, 3333],
                expected_insts: vec![
                    make_simple(OP_TRUE),
                    make(OP_JMPNT, 10).unwrap(),
                    make(OP_CONSTANT, 0).unwrap(),
                    make(OP_JMP, 11).unwrap(),
                    make_simple(OP_NULL),
                    make_simple(OP_POP),
                    make(OP_CONSTANT, 1).unwrap(),
                    make_simple(OP_POP),
                ],
            },
        ];

        run_compiler_test(tests);
    }

    #[test]
    fn test_global_let_statement() {
        let tests = vec![
            CompilerTestcase {
                input: "int x = 1; int y = 2;".to_owned(),
                expected_consts: vec![object::Object::Integer(1), object::Object::Integer(2)],
                expected_insts: vec![
                    make(OP_CONSTANT, 0).unwrap(),
                    make(OP_SET_GLOBAL, 0).unwrap(),
                    make(OP_CONSTANT, 1).unwrap(),
                    make(OP_SET_GLOBAL, 1).unwrap(),
                ],
            },
            CompilerTestcase {
                input: "int one = 1; one;".to_owned(),
                expected_consts: vec![object::Object::Integer(1)],
                expected_insts: vec![
                    make(OP_CONSTANT, 0).unwrap(),
                    make(OP_SET_GLOBAL, 0).unwrap(),
                    make(OP_GET_GLOBAL, 0).unwrap(),
                    make_simple(OP_POP),
                ],
            },
            CompilerTestcase {
                input: "int one = 1; int two = one; two;".to_owned(),
                expected_consts: vec![object::Object::Integer(1)],
                expected_insts: vec![
                    make(OP_CONSTANT, 0).unwrap(),
                    make(OP_SET_GLOBAL, 0).unwrap(),
                    make(OP_GET_GLOBAL, 0).unwrap(),
                    make(OP_SET_GLOBAL, 1).unwrap(),
                    make(OP_GET_GLOBAL, 1).unwrap(),
                    make_simple(OP_POP),
                ],
            },
        ];

        run_compiler_test(tests);
    }

    #[test]
    fn symbol_table_define_test() {
        let mut global = SymbolTable::new();

        let symbol1 = global.define("a".to_owned(), Token::Integer);
        assert_eq!(symbol1.value_type, Token::Integer);
        assert_eq!(symbol1.name, "a".to_owned());
        assert_eq!(symbol1.scope, Scope::Global);
        assert_eq!(symbol1.index, 0);

        let symbol2 = global.define("b".to_owned(), Token::Integer);
        assert_eq!(symbol2.value_type, Token::Integer);
        assert_eq!(symbol2.name, "b".to_owned());
        assert_eq!(symbol2.scope, Scope::Global);
        assert_eq!(symbol2.index, 1);
    }

    #[test]
    fn symbol_table_resolve_test() {
        let mut global = SymbolTable::new();
        global.define("a".to_owned(), Token::Integer);
        global.define("b".to_owned(), Token::Integer);

        let to_test = ["a", "b"];
        for (i, name) in to_test.iter().enumerate() {
            let res = global.resolve(name.to_string());
            assert!(!res.is_none());

            let res = res.unwrap();
            assert_eq!(res.value_type, Token::Integer);
            assert_eq!(res.name, name.to_string());
            assert_eq!(res.scope, Scope::Global);
            assert_eq!(res.index, i);
        }
    }

    #[test]
    fn test_string_expression() {
        let tests = vec![
            CompilerTestcase {
                input: "\"test\"".to_owned(),
                expected_consts: vec!["test".to_owned()],
                expected_insts: vec![make(OP_CONSTANT, 0).unwrap(), make_simple(OP_POP)],
            },
            CompilerTestcase {
                input: "\"te\" + \"st\"".to_owned(),
                expected_consts: vec!["te".to_owned(), "st".to_owned()],
                expected_insts: vec![
                    make(OP_CONSTANT, 0).unwrap(),
                    make(OP_CONSTANT, 1).unwrap(),
                    make_simple(OP_ADD),
                    make_simple(OP_POP),
                ],
            },
        ];

        run_compiler_test(tests);
    }

    #[test]
    fn test_array_literal() {
        let tests = vec![
            CompilerTestcase {
                input: "[]".to_owned(),
                expected_consts: vec![],
                expected_insts: vec![make(OP_ARRAY, 0).unwrap(), make_simple(OP_POP)],
            },
            CompilerTestcase {
                input: "[1, 2, 3]".to_owned(),
                expected_consts: vec![1, 2, 3],
                expected_insts: vec![
                    make(OP_CONSTANT, 0).unwrap(),
                    make(OP_CONSTANT, 1).unwrap(),
                    make(OP_CONSTANT, 2).unwrap(),
                    make(OP_ARRAY, 3).unwrap(),
                    make_simple(OP_POP),
                ],
            },
            CompilerTestcase {
                input: "[1 + 2, 3 - 4, 5 * 6]".to_owned(),
                expected_consts: vec![1, 2, 3, 4, 5, 6],
                expected_insts: vec![
                    make(OP_CONSTANT, 0).unwrap(),
                    make(OP_CONSTANT, 1).unwrap(),
                    make_simple(OP_ADD),
                    make(OP_CONSTANT, 2).unwrap(),
                    make(OP_CONSTANT, 3).unwrap(),
                    make_simple(OP_SUB),
                    make(OP_CONSTANT, 4).unwrap(),
                    make(OP_CONSTANT, 5).unwrap(),
                    make_simple(OP_MUL),
                    make(OP_ARRAY, 3).unwrap(),
                    make_simple(OP_POP),
                ],
            },
        ];

        run_compiler_test(tests);
    }

    #[test]
    fn test_index_expression() {
        let tests = vec![CompilerTestcase {
            input: "[1, 2, 3][1 + 1]".to_owned(),
            expected_consts: vec![1, 2, 3, 1, 1],
            expected_insts: vec![
                make(OP_CONSTANT, 0).unwrap(),
                make(OP_CONSTANT, 1).unwrap(),
                make(OP_CONSTANT, 2).unwrap(),
                make(OP_ARRAY, 3).unwrap(),
                make(OP_CONSTANT, 3).unwrap(),
                make(OP_CONSTANT, 4).unwrap(),
                make_simple(OP_ADD),
                make_simple(OP_INDEX),
                make_simple(OP_POP),
            ],
        }];

        run_compiler_test(tests);
    }

    #[test]
    fn test_function() {
        let tests = vec![
            CompilerTestcase {
                input: "fn add() -> int { return 5 + 10 }".to_owned(),
                expected_consts: vec![
                    object::Object::Integer(5),
                    object::Object::Integer(10),
                    object::Object::CompiledFunction(object::CompiledFunction::new(
                        concat_instructions(&vec![
                            make(OP_CONSTANT, 1).unwrap(),
                            make(OP_CONSTANT, 2).unwrap(),
                            make_simple(OP_ADD),
                            make_simple(OP_RETURN_VALUE),
                        ]).clone()
                    ))
                ],
                expected_insts: vec![
                    make(OP_CONSTANT, 2).unwrap(),
                    make_simple(OP_POP),
                ]

            }
        ];

        run_compiler_test(tests);
    }
}
