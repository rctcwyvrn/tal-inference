use crate::syntax::*;
use core::panic;
use std::{collections::HashMap, fmt::Display};
use thiserror::Error;

pub const MAX_REGISTER: usize = 3;
type CodeTy = HashMap<Register, Ty>;

#[derive(Debug, PartialEq, Clone)]
pub enum Ty {
    Int,
    Code(CodeTy),
    UnifVar(usize),
    TyVar(usize),
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ty::Int => write!(f, "Int"),
            Ty::Code(free) => write!(f, "(free = {:?})", free),
            Ty::UnifVar(id) => write!(f, "unifvar_{}", *id),
            Ty::TyVar(id) => write!(f, "var_{}", *id),
        }
    }
}

pub struct Checker {
    // for fresh unifvars
    pub cur_var: usize,

    // local context
    pub register_types: HashMap<Register, Ty>,

    // global context
    pub heap_types: HashMap<Label, Ty>,
    pub constraints: Vec<(Ty, Ty)>,
    pub satisfy: Vec<Vec<(Ty, Ty)>>,
}

#[derive(Debug, Error)]
pub enum TypeError {
    #[error("value {0} does not have type {1}")]
    ConcreteVal(Value, Ty),

    #[error("register {0} is of type {1} but expected to be of type {2}")]
    RegisterConflict(Register, Ty, Ty),

    #[error("failed to constrain jump")]
    FailedJump,

    #[error("failed to unify")]
    FailedUnify,
}

impl Checker {
    fn constrain(&mut self, a: &Ty, b: &Ty) -> bool {
        println!("> Constraining {:?} == {:?}", a, b);
        match (a, b) {
            (Ty::Code(_), Ty::Code(_)) | (_, Ty::UnifVar(_)) | (Ty::UnifVar(_), _) => {
                self.constraints.push((a.clone(), b.clone()));
                return true;
            }
            (a, b) => {
                return a == b;
            }
        }
    }

    fn fresh(&mut self) -> Ty {
        self.cur_var += 1;
        return Ty::UnifVar(self.cur_var);
    }

    fn update_register(&mut self, r: Register, ty: Ty) {
        println!("> Updating r{} to {:?}", r, ty);
        self.register_types.insert(r, ty);
    }

    fn constrain_register(&mut self, r: Register, ty: Ty) -> Result<(), TypeError> {
        let reg_ty = self.register_types[&r].clone();
        if !self.constrain(&reg_ty, &ty) {
            Err(TypeError::RegisterConflict(r, reg_ty, ty))
        } else {
            Ok(())
        }
    }

    fn infer_value(&mut self, val: Value) -> Ty {
        match &val {
            Value::Register(r) => self.register_types[r].clone(),
            Value::Word(wv) => match wv {
                WordValue::Integer(_) => Ty::Int,
                WordValue::Label(l) => self.heap_types[l].clone(),
            },
        }
    }

    fn constrain_value(&mut self, val: Value, ty: Ty) -> Result<(), TypeError> {
        let val_ty = self.infer_value(val.clone());
        if !self.constrain(&val_ty, &ty) {
            Err(TypeError::ConcreteVal(val, ty))
        } else {
            Ok(())
        }
    }

    fn constrain_jump(&mut self, val: Value) -> Result<(), TypeError> {
        let val_ty = self.infer_value(val);
        let code_ty = self.init_code_type();

        // constrain the value that we're jumping to to be a code type
        let mut success = self.constrain(&val_ty, &Ty::Code(code_ty.clone()));

        // constrain the parameters of th code type to be supertypes of what our current registers are
        let mut jump_satisfy = Vec::new();
        for r in 1..=MAX_REGISTER {
            let ty = self.register_types[&r].clone();
            match ty {
                // if we have a concrete type, we can constrain the input type of the function we're jumping to
                Ty::Int => {
                    // we could say that the parameter type is a type variable
                    // but that becomes intractable (??), this appears to work
                    success &= self.constrain(&code_ty[&r], &ty);
                }
                // otherwise we have to check that it can be satisfied
                _ => jump_satisfy.push((self.register_types[&r].clone(), code_ty[&r].clone())),
            }
        }

        self.satisfy.push(jump_satisfy);

        if success {
            Ok(())
        } else {
            Err(TypeError::FailedJump)
        }
    }

    fn check_instruction(&mut self, instr: Instruction) -> Result<(), TypeError> {
        println!("\n[+] Checking {:?}", instr);
        match instr {
            Instruction::Arith(_, r_tar, r_src, val) => {
                self.constrain_register(r_src, Ty::Int)?;
                self.constrain_value(val, Ty::Int)?;
                self.update_register(r_tar, Ty::Int);
                Ok(())
            }
            Instruction::BranchNonZero(r, v) => {
                self.constrain_register(r, Ty::Int)?;
                self.constrain_jump(v)
            }
            Instruction::Load(_, _, _) => todo!(),
            Instruction::Store(_, _, _) => todo!(),
            Instruction::Malloc(_) => todo!(),
            Instruction::Mov(r, v) => {
                let rhs = self.infer_value(v);
                self.update_register(r, rhs);
                Ok(())
            }
        }
    }

    fn check_terminal(&mut self, term: Terminal) -> Result<(), TypeError> {
        println!("\n[+] Checking {:?}", term);
        match term {
            Terminal::Jump(v) => self.constrain_jump(v),
            Terminal::Halt => Ok(()),
        }
    }

    fn check_block(&mut self, block: &Block) -> Result<(), TypeError> {
        let ty = self.heap_types[&block.0].clone();
        if let Ty::Code(gamma) = ty {
            self.register_types = gamma;
            for instr in &block.1 {
                self.check_instruction(instr.clone())?;
            }
            self.check_terminal(block.2.clone())
        } else {
            panic!("Huh??")
        }
    }

    fn init_code_type(&mut self) -> CodeTy {
        let mut ty = HashMap::new();
        for r in 1..=MAX_REGISTER {
            ty.insert(r, self.fresh());
        }
        ty
    }

    pub fn check(&mut self, program: Program) -> Result<(), TypeError> {
        // compute free registers in each block
        // create label types for each block
        for (label, _, _) in &program {
            let ty = Ty::Code(self.init_code_type());
            self.heap_types.insert(label.clone(), ty);
        }

        self.pretty_heap();

        // check each block with the correct context set
        for block in &program {
            // println!("Constraints: {:?}", self.constraints);
            self.check_block(block)?;
        }

        println!("--- Constraints --- ");
        for c in &self.constraints {
            println!("- {:?}", c);
        }

        // unify the variables in the block bodies
        let mapping = self.try_unify()?;
        let mapping = self.chase_all_to_root(mapping);

        println!("--- Mapping --- ");
        for v in 1..=self.cur_var {
            if !mapping.contains_key(&v) {
                println!("  unifVar({}) => unbound", v);
            } else {
                println!("  unifVar({}) => {:?}", v, mapping[&v]);
            }
        }

        // debugging
        self.pretty_heap();
        self.pretty_heap_w_mapping(mapping.clone());

        // unify the jumps to labelled blocks
        self.try_satisfy(&mapping)?;

        Ok(())
    }

    pub fn new() -> Checker {
        Checker {
            cur_var: 0,
            register_types: HashMap::new(),
            heap_types: HashMap::new(),
            constraints: Vec::new(),
            satisfy: Vec::new(),
        }
    }
}
