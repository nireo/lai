use std::collections::HashMap;

use crate::{
    ast, object,
    opcode::{
        make, make_simple, Inst, OP_ADD, OP_ARRAY, OP_BANG, OP_CALL, OP_CONSTANT, OP_DIV, OP_EQ,
        OP_FALSE, OP_GET_BUILTIN, OP_GET_GLOBAL, OP_GET_LOCAL, OP_GT, OP_INDEX, OP_JMP, OP_JMPNT,
        OP_MINUS, OP_MUL, OP_NE, OP_NULL, OP_POP, OP_RETURN, OP_RETURN_VALUE, OP_SET_GLOBAL,
        OP_SET_LOCAL, OP_SUB, OP_TRUE,
    },
    scanner::{self, Token},
};

#[derive(PartialEq, Debug, Clone)]
pub enum Scope {
    Global,
    Local,
    Builtin,
}

pub struct CompilationScope {
    pub instructions: Inst,
    pub last_inst: EmittedInstruction,
    pub prev_inst: EmittedInstruction,
}

#[derive(Clone)]
pub struct Symbol {
    pub name: String,
    pub scope: Scope,
    pub index: usize,
    pub value_type: scanner::Token,
}

#[derive(Clone)]
struct SymbolTable {
    pub store: HashMap<String, Symbol>,
    pub definition_count: usize,

    outer: Option<Box<SymbolTable>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            definition_count: 0,
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_inner(outer: Box<SymbolTable>) -> Self {
        Self {
            outer: Some(outer),
            definition_count: 0,
            store: HashMap::new(),
        }
    }

    fn resolve(&mut self, name: String) -> Result<&Symbol, String> {
        if let Some(symbol) = self.store.get(&name) {
            Ok(symbol)
        } else {
            if let Some(outer) = &mut self.outer {
                if let Ok(outer_symbol) = outer.resolve(name.clone()) {
                    return Ok(outer_symbol);
                }
            }

            Err(format!("variable with name '{}' is not defined", name))
        }
    }

    fn define(&mut self, name: String, value_type: Token) -> Symbol {
        let scope = if self.outer.is_some() {
            Scope::Local
        } else {
            Scope::Global
        };

        let symbol = Symbol {
            name: name.clone(),
            index: self.definition_count,
            scope,
            value_type,
        };

        self.store.insert(name, symbol.clone());
        self.definition_count += 1;

        symbol
    }

    fn define_builtin(&mut self, name: String, index: usize) -> Symbol {
        let symbol = Symbol {
            name: name.clone(),
            index,
            scope: Scope::Builtin,
            value_type: Token::Fn,
        };

        self.store.insert(name, symbol.clone());
        symbol
    }
}

#[derive(Clone)]
pub struct EmittedInstruction {
    opcode: u8,
    pos: usize,
}

pub struct Compiler {
    pub insts: Inst,
    pub consts: Vec<object::Object>,

    symbol_table: SymbolTable,

    pub scope_index: usize,
    pub scopes: Vec<CompilationScope>,
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

impl Compiler {
    pub fn new() -> Self {
        let main_scope = CompilationScope {
            instructions: Inst(vec![]),
            last_inst: EmittedInstruction { opcode: 0, pos: 0 },
            prev_inst: EmittedInstruction { opcode: 0, pos: 0 },
        };

        let scopes: Vec<CompilationScope> = vec![main_scope];
        let mut symbol_table = SymbolTable::new();
        symbol_table.define_builtin("len".to_string(), 0);

        Self {
            consts: Vec::new(),
            insts: Inst(Vec::new()),
            scope_index: 0,

            symbol_table,
            scopes,
        }
    }

    pub fn get_consts(&self) -> &[object::Object] {
        &self.consts
    }

    pub fn get_insts(&self) -> &Inst {
        self.current_instructions()
    }

    pub fn current_instructions(&self) -> &Inst {
        &self.scopes[self.scope_index].instructions
    }

    pub fn compile(&mut self, node: ast::Node) -> Result<(), String> {
        match node {
            ast::Node::Root(value) => {
                for st in value.statements.iter() {
                    self.compile(ast::Node::Statement(Box::new(st.clone())))?;
                }
            }
            ast::Node::Statement(st) => match *st {
                ast::Statement::Expression(exp) => {
                    self.compile(ast::Node::Expression(Box::new(exp.value.clone())))?;

                    match &exp.value {
                        ast::Expression::Function(_) => return Ok(()),
                        _ => self.emit_single(OP_POP),
                    };
                }
                ast::Statement::Block(exp) => {
                    for st in exp.statements.iter() {
                        self.compile(ast::Node::Statement(Box::new(st.clone())))?;
                    }
                }
                ast::Statement::Assigment(exp) => {
                    let symbol = self.symbol_table.define(exp.name, exp.variable_type);
                    self.compile(ast::Node::Expression(Box::new(exp.value)))?;
                    let index = symbol.index;
                    if symbol.scope == Scope::Global {
                        self.emit(OP_SET_GLOBAL, index);
                    } else {
                        self.emit(OP_SET_LOCAL, index);
                    }
                }
                ast::Statement::Return(exp) => {
                    self.compile(ast::Node::Expression(Box::new(exp.value)))?;
                    self.emit_single(OP_RETURN_VALUE);
                }
                ast::Statement::Function(exp) => {
                    let name = match &*exp.identifier {
                        ast::Expression::Identifier(val) => val.name.clone(),
                        _ => return Err(String::from("function identifier is not valid")),
                    };

                    let symbol = self.symbol_table.define(name, exp.return_type);
                    self.enter_scope();
                    for arg in &exp.params {
                        match arg {
                            ast::Expression::FunctionParam(val) => {
                                self.symbol_table
                                    .define(val.name.clone(), val.value_type.clone());
                            }
                            _ => return Err(String::from("not a function parameter")),
                        }
                    }

                    self.compile(ast::Node::Statement(exp.body))?;

                    if self.last_instruction_is(OP_POP) {
                        let last_pos = self.scopes[self.scope_index].last_inst.pos;
                        self.replace_instruction(last_pos, make_simple(OP_RETURN_VALUE).0);

                        self.scopes[self.scope_index].last_inst.opcode = OP_RETURN_VALUE;
                    }

                    if !self.last_instruction_is(OP_RETURN_VALUE) {
                        self.emit_single(OP_RETURN);
                    }

                    let num_locals = self.symbol_table.definition_count;
                    let instructions = self.leave_scope();

                    let compiled_function = object::Object::CompiledFunction(
                        object::CompiledFunction::new_with_params(
                            instructions,
                            num_locals,
                            exp.params.len(),
                        ),
                    );

                    let pos = self.add_constant(compiled_function);
                    self.emit(OP_CONSTANT, pos);

                    let index = symbol.index;
                    self.emit(OP_SET_GLOBAL, index);
                }
            },
            ast::Node::Expression(exp) => match *exp {
                ast::Expression::String(exp) => {
                    let str_const = object::Object::String(exp.value);
                    let pos = self.add_constant(str_const);
                    self.emit(OP_CONSTANT, pos);
                }
                ast::Expression::Array(exp) => {
                    for elem in exp.elements.iter() {
                        self.compile(ast::Node::Expression(Box::new(elem.clone())))?;
                    }
                    self.emit(OP_ARRAY, exp.elements.len());
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
                            _ => return Err(String::from("infix operator is not recognised.")), // non recognized/supported operator
                        };
                    }
                }
                ast::Expression::FunctionCall(exp) => {
                    self.compile(ast::Node::Expression(exp.func))?;

                    for arg in exp.args.iter() {
                        self.compile(ast::Node::Expression(Box::new(arg.clone())))?;
                    }

                    self.emit(OP_CALL, exp.args.len());
                }
                ast::Expression::Function(exp) => {
                    self.enter_scope();
                    self.compile(ast::Node::Statement(exp.body))?;

                    if self.last_instruction_is(OP_POP) {
                        let last_pos = self.scopes[self.scope_index].last_inst.pos;
                        self.replace_instruction(last_pos, make_simple(OP_RETURN_VALUE).0);

                        self.scopes[self.scope_index].last_inst.opcode = OP_RETURN_VALUE;
                    }

                    if !self.last_instruction_is(OP_RETURN_VALUE) {
                        self.emit_single(OP_RETURN);
                    }

                    let instructions = self.leave_scope();

                    let compiled_function = object::Object::CompiledFunction(
                        object::CompiledFunction::new(instructions),
                    );

                    let pos = self.add_constant(compiled_function);
                    self.emit(OP_CONSTANT, pos);

                    let name = match &*exp.identifier {
                        ast::Expression::Identifier(val) => val.name.clone(),
                        _ => return Err(String::from("function identifier is not valid")),
                    };

                    let symbol = self.symbol_table.define(name, exp.return_type);
                    let index = symbol.index;
                    self.emit(OP_SET_GLOBAL, index);
                }
                ast::Expression::Index(exp) => {
                    self.compile(ast::Node::Expression(exp.lhs))?;
                    self.compile(ast::Node::Expression(exp.index))?;

                    self.emit_single(OP_INDEX);
                }
                ast::Expression::Identifier(exp) => {
                    let symbol = self.symbol_table.resolve(exp.name)?;
                    // we need to store this in a variable because otherwise the compiler doesn't like it
                    if symbol.scope == Scope::Global {
                        let index = symbol.index;
                        self.emit(OP_GET_GLOBAL, index);
                    } else if symbol.scope == Scope::Local {
                        let index = symbol.index;
                        self.emit(OP_GET_LOCAL, index);
                    } else if symbol.scope == Scope::Builtin {
                        let index = symbol.index;
                        self.emit(OP_GET_BUILTIN, index);
                    }
                }
                ast::Expression::If(e) => {
                    self.compile(ast::Node::Expression(e.cond))?;
                    let not_true_pos = self.emit(OP_JMPNT, 9999);

                    self.compile(ast::Node::Statement(e.after))?;

                    if self.last_instruction_is(OP_POP) {
                        self.remove_last_pop();
                    }

                    let jmp_pos = self.emit(OP_JMP, 9999);
                    let after_pos = self.current_instructions().0.len();
                    self.change_operand(not_true_pos, after_pos);

                    if e.other.is_none() {
                        self.emit_single(OP_NULL);
                    } else {
                        self.compile(ast::Node::Statement(e.other.unwrap()))?;

                        if self.last_instruction_is(OP_POP) {
                            self.remove_last_pop();
                        }
                    }

                    let after_other = self.current_instructions().0.len();
                    self.change_operand(jmp_pos, after_other);
                }
                ast::Expression::Integer(e) => {
                    let int_obj = object::Object::Integer(e.value);
                    let pos = self.add_constant(int_obj);
                    self.emit(OP_CONSTANT, pos);
                }
                ast::Expression::Boolean(e) => {
                    if e.value {
                        self.emit_single(OP_TRUE);
                    } else {
                        self.emit_single(OP_FALSE);
                    }
                }
                ast::Expression::Prefix(e) => {
                    self.compile(ast::Node::Expression(e.rhs))?;

                    match &e.operator {
                        Token::Exclamation => self.emit_single(OP_BANG),
                        Token::Minus => self.emit_single(OP_MINUS),
                        _ => {
                            return Err(String::from(
                                "prefix expressions only support the operators '-' and '!'",
                            ))
                        } // unregocnized error
                    };
                }
                _ => {
                    return Err(String::from(
                        "compiler does not support this statement/expression",
                    ))
                }
            },
        };
        Ok(())
    }

    fn set_last_instruction(&mut self, opcode: u8, pos: usize) {
        let prev = EmittedInstruction {
            opcode: self.scopes[self.scope_index].last_inst.opcode,
            pos: self.scopes[self.scope_index].last_inst.pos,
        };
        let last = EmittedInstruction { opcode, pos };

        self.scopes[self.scope_index].prev_inst = prev;
        self.scopes[self.scope_index].last_inst = last;
    }

    fn add_constant(&mut self, obj: object::Object) -> usize {
        self.consts.push(obj);
        self.consts.len() - 1
    }

    fn change_operand(&mut self, pos: usize, operand: usize) {
        let op = self.current_instructions().0[pos];
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

    fn last_instruction_is(&self, inst: u8) -> bool {
        if self.current_instructions().0.is_empty() {
            false
        } else {
            self.scopes[self.scope_index].last_inst.opcode == inst
        }
    }

    fn remove_last_pop(&mut self) {
        let last = &self.scopes[self.scope_index].last_inst;
        let prev = self.scopes[self.scope_index].prev_inst.clone();

        let old = self.current_instructions();
        let new = &old.0[..last.pos];

        self.scopes[self.scope_index].instructions = Inst(new.to_vec());
        self.scopes[self.scope_index].last_inst = prev;
    }

    fn replace_instruction(&mut self, pos: usize, insts: Vec<u8>) {
        // for i in 0..insts.len() {
        //     self.scopes[self.scope_index].instructions.0[pos + i] = insts[i];
        // }
        //
        self.scopes[self.scope_index].instructions.0[pos..(insts.len() + pos)].clone_from_slice(&insts);
    }

    fn add_inst(&mut self, inst: &[u8]) -> usize {
        let pos_new_inst = self.current_instructions().0.len();
        let mut updated_inst = self.current_instructions().clone();
        updated_inst.0.extend_from_slice(inst);

        self.scopes[self.scope_index].instructions = updated_inst;

        pos_new_inst
    }

    fn enter_scope(&mut self) {
        let scope = CompilationScope {
            instructions: Inst(vec![]),
            last_inst: EmittedInstruction { opcode: 0, pos: 0 },
            prev_inst: EmittedInstruction { opcode: 0, pos: 0 },
        };

        self.scopes.push(scope);
        self.scope_index += 1;
        self.symbol_table = SymbolTable::new_inner(Box::new(self.symbol_table.clone()));
    }

    fn leave_scope(&mut self) -> Inst {
        let last = self.scopes.pop().unwrap();
        self.scope_index -= 1;
        self.symbol_table = *self.symbol_table.outer.as_ref().unwrap().clone();

        last.instructions
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{
        opcode::{OP_ARRAY, OP_INDEX, OP_JMP, OP_JMPNT, OP_RETURN, OP_RETURN_VALUE},
        parser, scanner,
    };
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
        assert!(root_node.is_some());

        ast::Node::Root(Box::new(root_node.unwrap()))
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
        for obj in actual.iter() {
            println!("{}", obj);
        }
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

                    match value_any.downcast_ref::<object::Object>() {
                        Some(obj) => match obj {
                            object::Object::CompiledFunction(vl) => test_instructions(
                                &[val.instructions.clone()],
                                vl.instructions.clone(),
                            ),
                            _ => assert!(false),
                        },
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
            if res.is_err() {
                println!("{}", res.as_ref().err().unwrap())
            }
            assert!(!res.is_err());

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
            assert!(!res.is_err());

            let res = res.unwrap();
            assert_eq!(res.value_type, Token::Integer);
            assert_eq!(res.name, name.to_string());
            assert_eq!(res.scope, Scope::Global);
            assert_eq!(res.index, i);
        }
    }

    #[test]
    fn symbol_resolve_local() {
        struct SymbolTestcase {
            scope: Scope,
            index: usize,
            name: String,
        }

        let mut global = SymbolTable::new();
        global.define("a".to_owned(), Token::Integer);
        global.define("b".to_owned(), Token::Integer);

        let mut local = SymbolTable::new_inner(Box::new(global));
        local.define("c".to_owned(), Token::Integer);
        local.define("d".to_owned(), Token::Integer);

        let expected = vec![
            SymbolTestcase {
                scope: Scope::Global,
                index: 0,
                name: "a".to_string(),
            },
            SymbolTestcase {
                scope: Scope::Global,
                index: 1,
                name: "b".to_string(),
            },
            SymbolTestcase {
                scope: Scope::Local,
                index: 0,
                name: "c".to_string(),
            },
            SymbolTestcase {
                scope: Scope::Local,
                index: 1,
                name: "d".to_string(),
            },
        ];

        for sym in expected.iter() {
            let res = local.resolve(sym.name.clone());
            assert!(res.is_ok());

            let res = res.unwrap();
            assert_eq!(res.scope, sym.scope);
            assert_eq!(res.index, sym.index);
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
        let tests = vec![CompilerTestcase {
            input: "fn func(int x) -> int { 5 + 10 };".to_owned(),
            expected_consts: vec![
                object::Object::Integer(5),
                object::Object::Integer(10),
                object::Object::CompiledFunction(object::CompiledFunction::new(
                    concat_instructions(&vec![
                        make(OP_CONSTANT, 0).unwrap(),
                        make(OP_CONSTANT, 1).unwrap(),
                        make_simple(OP_ADD),
                        make_simple(OP_RETURN_VALUE),
                    ])
                    .clone(),
                )),
            ],
            expected_insts: vec![
                make(OP_CONSTANT, 2).unwrap(),
                make(OP_SET_GLOBAL, 0).unwrap(),
            ],
        }];

        run_compiler_test(tests);
    }

    #[test]
    fn function_without_arguments() {
        let tests = vec![CompilerTestcase {
            input: "fn func() -> void { };".to_owned(),
            expected_consts: vec![object::Object::CompiledFunction(
                object::CompiledFunction::new(
                    concat_instructions(&vec![make_simple(OP_RETURN)]).clone(),
                ),
            )],
            expected_insts: vec![
                make(OP_CONSTANT, 0).unwrap(),
                make(OP_SET_GLOBAL, 0).unwrap(),
            ],
        }];

        run_compiler_test(tests);
    }

    #[test]
    fn function_calls() {
        let tests = vec![
            CompilerTestcase {
                input: "fn func() -> int { 24 }; func();".to_owned(),
                expected_consts: vec![
                    object::Object::Integer(24),
                    object::Object::CompiledFunction(object::CompiledFunction::new(
                        concat_instructions(&vec![
                            make(OP_CONSTANT, 0).unwrap(),
                            make_simple(OP_RETURN_VALUE),
                        ])
                        .clone(),
                    )),
                ],
                expected_insts: vec![
                    make(OP_CONSTANT, 1).unwrap(),
                    make(OP_SET_GLOBAL, 0).unwrap(),
                    make(OP_GET_GLOBAL, 0).unwrap(),
                    make(OP_CALL, 0).unwrap(),
                    make_simple(OP_POP),
                ],
            },
            CompilerTestcase {
                input: "fn oneArg(int a) -> int { return a; }; oneArg(24);".to_owned(),
                expected_consts: vec![
                    object::Object::CompiledFunction(object::CompiledFunction::new(
                        concat_instructions(&vec![
                            make(OP_GET_LOCAL, 0).unwrap(),
                            make_simple(OP_RETURN_VALUE),
                        ])
                        .clone(),
                    )),
                    object::Object::Integer(24),
                ],
                expected_insts: vec![
                    make(OP_CONSTANT, 0).unwrap(),
                    make(OP_SET_GLOBAL, 0).unwrap(),
                    make(OP_GET_GLOBAL, 0).unwrap(),
                    make(OP_CONSTANT, 1).unwrap(),
                    make(OP_CALL, 1).unwrap(),
                    make_simple(OP_POP),
                ],
            },
            CompilerTestcase {
                input: "fn manyArg(int a, int b) -> int { return a + b; }; manyArg(24, 25);"
                    .to_owned(),
                expected_consts: vec![
                    object::Object::CompiledFunction(object::CompiledFunction::new(
                        concat_instructions(&vec![
                            make(OP_GET_LOCAL, 0).unwrap(),
                            make(OP_GET_LOCAL, 1).unwrap(),
                            make_simple(OP_ADD),
                            make_simple(OP_RETURN_VALUE),
                        ])
                        .clone(),
                    )),
                    object::Object::Integer(24),
                    object::Object::Integer(25),
                ],
                expected_insts: vec![
                    make(OP_CONSTANT, 0).unwrap(),
                    make(OP_SET_GLOBAL, 0).unwrap(),
                    make(OP_GET_GLOBAL, 0).unwrap(),
                    make(OP_CONSTANT, 1).unwrap(),
                    make(OP_CONSTANT, 2).unwrap(),
                    make(OP_CALL, 2).unwrap(),
                    make_simple(OP_POP),
                ],
            },
        ];

        run_compiler_test(tests);
    }

    #[test]
    fn function_local_bindings() {
        let tests = vec![CompilerTestcase {
            input: "fn func() -> int { int x = 10; int y = 77; return x + y; }".to_owned(),
            expected_consts: vec![
                object::Object::Integer(10),
                object::Object::Integer(77),
                object::Object::CompiledFunction(object::CompiledFunction::new(
                    concat_instructions(&vec![
                        make(OP_CONSTANT, 0).unwrap(),
                        make(OP_SET_LOCAL, 0).unwrap(),
                        make(OP_CONSTANT, 1).unwrap(),
                        make(OP_SET_LOCAL, 1).unwrap(),
                        make(OP_GET_LOCAL, 0).unwrap(),
                        make(OP_GET_LOCAL, 1).unwrap(),
                        make_simple(OP_ADD),
                        make_simple(OP_RETURN_VALUE),
                    ])
                    .clone(),
                )),
            ],
            expected_insts: vec![
                make(OP_CONSTANT, 2).unwrap(),
                make(OP_SET_GLOBAL, 0).unwrap(),
            ],
        }];

        run_compiler_test(tests);
    }

    #[test]
    fn compiler_scope_test() {
        let mut compiler = Compiler::new();
        assert_eq!(compiler.scope_index, 0);

        compiler.emit_single(OP_MUL);

        compiler.enter_scope();
        assert_eq!(compiler.scope_index, 1);

        compiler.emit_single(OP_SUB);
        assert_eq!(
            compiler.scopes[compiler.scope_index].instructions.0.len(),
            1
        );

        let last = compiler.scopes[compiler.scope_index].last_inst.clone();
        assert_eq!(last.opcode, OP_SUB);

        compiler.leave_scope();
        assert_eq!(compiler.scope_index, 0);
        assert!(compiler.symbol_table.outer.is_none());

        compiler.emit_single(OP_ADD);
        assert_eq!(
            compiler.scopes[compiler.scope_index].instructions.0.len(),
            2
        );

        let last = compiler.scopes[compiler.scope_index].last_inst.clone();
        assert_eq!(last.opcode, OP_ADD);

        let prev = compiler.scopes[compiler.scope_index].prev_inst.clone();
        assert_eq!(prev.opcode, OP_MUL);
    }
}
