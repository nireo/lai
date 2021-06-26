use crate::{
    ast,
    object::{self, ValueObj},
    opcode::{Inst, InstMaker, OP_CONSTANT},
};

pub struct Compiler {
    pub insts: Inst,
    pub consts: Vec<object::Object>,
    pub inst_maker: InstMaker,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            consts: Vec::new(),
            insts: Inst(Vec::new()),
            inst_maker: InstMaker::default(),
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
        let inst = self.inst_maker.make(op, operand).unwrap();
        let pos = self.add_inst(&inst.0);

        pos
    }

    fn add_inst(&mut self, ins: &[u8]) -> usize {
        let pos_new_inst = self.insts.0.len();
        self.insts.0.extend_from_slice(ins);

        pos_new_inst
    }
}
