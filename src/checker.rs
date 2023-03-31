use crate::unify::Unifier;
use crate::{debug::sort_for_print, syntax::*};

use core::panic;
use std::collections::HashMap;
use thiserror::Error;

pub const MAX_REGISTER: usize = 3;
pub type CodeTy = HashMap<Register, Ty>;

#[derive(Debug, PartialEq, Clone)]
pub enum Ty {
    Int,
    Code(CodeTy),
    UnifVar(usize), // really should be in a TyX type but im lazy
    TyVar(usize),

    // Pointer types
    Ptr(HashMap<i64, Ty>, Option<Rho>),
    UniqPtr(HashMap<i64, Ty>, Option<Rho>),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Rho(pub usize);

pub struct Checker {
    // for fresh unifvars
    pub cur_var: usize,
    pub cur_rho: usize,

    // local context
    pub register_types: HashMap<Register, Ty>,

    // global context
    pub label_types: HashMap<Label, (CodeTy, Vec<CodeTy>)>,
    pub constraints: Vec<(Ty, Ty)>,
}

#[derive(Debug, Error)]
pub enum TypeError {
    #[error("value {0} does not have type {1}")]
    ConcreteVal(Value, Ty),

    #[error("register {0} is of type {1} but expected to be of type {2}")]
    RegisterConflict(Register, Ty, Ty),

    #[error("failed to constrain jump")]
    FailedJump,

    #[error("failed to constrain rhos for jump")]
    FailedJumpOnRho,

    #[error("failed to unify")]
    FailedUnify,

    #[error("rho conflict")]
    RhoConflict,

    #[error("rho too small")]
    RhoTooSmall,

    #[error("tried to add two rhos with the same indices together")]
    RhoFailedAdd,

    #[error("pointers conflict")]
    PtrConflict,
}

impl Checker {
    fn constrain(&mut self, a: &Ty, b: &Ty) -> bool {
        println!("> Constraining {} == {}", a, b);
        match (a, b) {
            (Ty::Code(_), Ty::Code(_)) // both code
            | (_, Ty::UnifVar(_)) | (Ty::UnifVar(_), _) // contains a variable
            // some combination of pointer types
            | (Ty::Ptr(_, _), Ty::Ptr(_, _)) | (Ty::UniqPtr(_, _), Ty::Ptr(_, _)) | (Ty::Ptr(_, _), Ty::UniqPtr(_, _)) | (Ty::UniqPtr(_, _), Ty::UniqPtr(_, _))
             => {
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

    fn fresh_rho(&mut self) -> Rho {
        self.cur_rho += 1;
        return Rho(self.cur_rho);
    }

    fn update_register(&mut self, r: Register, ty: Ty) {
        println!("> Updating r{} to {}", r, ty);
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

    fn let_poly(&mut self, label: &String) -> Ty {
        let new_code = self.init_code_type();
        if let Some((_, let_polys)) = self.label_types.get_mut(label) {
            let_polys.push(new_code.clone())
        }
        Ty::Code(new_code)
    }

    fn infer_value(&mut self, val: Value) -> Ty {
        match &val {
            Value::Register(r) => self.register_types[r].clone(),
            Value::Word(wv) => match wv {
                WordValue::Integer(_) => Ty::Int,
                WordValue::Label(l) => self.let_poly(l),
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
        let is_indirect = match &val {
            Value::Register(_) => true, // FIXME: THIS IS NOT THE CORRECT CHECK FOR IS_INDIRECT! need to check that register holds the original unifvar for this code block
            Value::Word(WordValue::Label(_)) => false,
            _ => panic!("too lazy for proper error"),
        };

        let val_ty = self.infer_value(val);
        let code_ty = self.init_code_type();

        // constrain the value that we're jumping to to be a code type
        let mut success = self.constrain(&val_ty, &Ty::Code(code_ty.clone()));

        // Increase the set of functions we can legally jump to by inserting information about what the current registers hold
        for r in 1..=MAX_REGISTER {
            let ty = self.register_types[&r].clone();
            match ty {
                // if we have a concrete type, we can constrain the input type of the function we're jumping to
                // the goal is to build up as much known information as we can
                Ty::Int | Ty::Ptr(_, _) | Ty::UniqPtr(_, _) => {
                    // we could say that the parameter type is a type variable
                    // but that becomes intractable (??), this appears to work
                    success &= self.constrain(&code_ty[&r], &ty);
                }
                // bind code in every case except indirect jumps
                // temporary fix to get around infinite recursive types
                Ty::Code(_) if !is_indirect => {
                    success &= self.constrain(&code_ty[&r], &ty);
                }
                // otherwise we have to check that it can be satisfied
                _ => println!("Skipping constraining {} to register value {}", code_ty[&r], ty)
            }
        }

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
            Instruction::Mov(r, v) => {
                let rhs = self.infer_value(v);
                self.update_register(r, rhs);
                Ok(())
            }

            // heap instructions
            Instruction::Load(r_tar, r_src, idx) => {
                let output_type = self.fresh();
                self.update_register(r_tar, output_type.clone());

                // expect that we at _least_ have a generic pointer with that idx set to the output type
                let some_rho = self.fresh_rho();
                let row_constraint = HashMap::from([(idx, output_type)]);
                let expected_src = Ty::Ptr(row_constraint, Some(some_rho));
                self.constrain_register(r_src, expected_src)
            }
            Instruction::StoreStrong(r_tar, idx, r_src) => {
                let old_rho = self.fresh_rho();
                // generate a unifvar for the old type, but don't constrain it in any way
                let old_type = self.fresh();
                let row_constraint = HashMap::from([(idx, old_type)]);
                // check that we had a UniqPtr with idx set to _any_ type
                // with old_rho holding other fields
                let expected_tar = Ty::UniqPtr(row_constraint, Some(old_rho));
                self.constrain_register(r_tar, expected_tar)?;

                // old_rho remembers all the fields set before
                let new_type = self.register_types[&r_src].clone();
                let new_known = HashMap::from([(idx, new_type)]);
                // we now have a uniqptr with all the old fields and the one new field
                let new_tar = Ty::UniqPtr(new_known, Some(old_rho));
                self.update_register(r_tar, new_tar);
                Ok(())
            }
            Instruction::Store(r_tar, idx, r_src) => {
                // Similar idea to StoreStrong
                let old_rho = self.fresh_rho();
                let old_type = self.fresh();
                let row_constraint = HashMap::from([(idx, old_type.clone())]);
                let expected_tar = Ty::Ptr(row_constraint, Some(old_rho));
                self.constrain_register(r_tar, expected_tar)?;
                // But now we constrain our register to have the old type
                // and we don't update the pointer type at all
                self.constrain_register(r_src, old_type)
            }
            Instruction::Malloc(r_tar, idx) => {
                let ty = (0..idx).map(|idx| (idx, Ty::Int)).collect();
                let ptr_ty = Ty::UniqPtr(ty, None);
                self.update_register(r_tar, ptr_ty);
                Ok(())
            }
            Instruction::Commit(r_tar) => {
                let old_set = self.fresh_rho();
                let old_ty = Ty::UniqPtr(HashMap::new(), Some(old_set));
                let new_ty = Ty::Ptr(HashMap::new(), Some(old_set));
                self.constrain_register(r_tar, old_ty)?;
                self.update_register(r_tar, new_ty);
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
        let gamma = self.label_types[&block.0].0.clone();
        self.register_types = gamma;
        for instr in &block.1 {
            self.check_instruction(instr.clone())?;
        }
        self.check_terminal(block.2.clone())
    }

    fn init_code_type(&mut self) -> CodeTy {
        let mut ty = HashMap::new();
        for r in 1..=MAX_REGISTER {
            ty.insert(r, self.fresh());
        }
        ty
    }

    fn lift_to_typevars(&self, mut unifier: &mut Unifier, ty: Ty) {
        match ty {
            Ty::Int => return,
            Ty::Code(f) => {
                println!("lifting {}", sort_for_print(&f));
                for r in 1..=MAX_REGISTER {
                    self.lift_to_typevars(&mut unifier, f[&r].clone());
                }
            }
            Ty::UnifVar(v) => {
                if !unifier.mapping.contains_key(&v) {
                    println!("lifting function parameter unifvar {} to typevar", v);
                    unifier.mapping.insert(v, Ty::TyVar(v));
                } else {
                    let ty = unifier.mapping[&v].clone();
                    self.lift_to_typevars(&mut unifier, ty);
                }
            }
            Ty::TyVar(_) => panic!("how did we already get typevars"),
            Ty::Ptr(_, _) | Ty::UniqPtr(_, _) => panic!("future lily problem"),
        }
    }

    pub fn check(&mut self, program: Program) -> Result<(), TypeError> {
        // compute free registers in each block
        // create label types for each block
        for (label, _, _) in &program {
            let label_type = self.init_code_type();
            let let_poly_label_types = Vec::new();
            self.label_types.insert(label.clone(), (label_type, let_poly_label_types));
        }

        self.pretty_heap();

        // check each block with the correct context set
        for block in &program {
            // println!("Constraints: {:?}", self.constraints);
            self.check_block(block)?;
        }

        println!("--- Constraints --- ");
        for c in &self.constraints {
            println!("- {} = {}", c.0, c.1);
        }

        let let_poly_constraints = self.label_types.clone().into_values().collect();
        // unify the variables in the block bodies
        let mut unifier =
            Unifier::new(self.constraints.clone(), let_poly_constraints, self.cur_var);

        unifier.unify()?;
        // lift to typevariables
        for (_, (base, _)) in &self.label_types {
            for r in 1..=MAX_REGISTER {
                self.lift_to_typevars(&mut unifier, base[&r].clone());
            }
        } 

        println!("--- Mapping --- ");
        let mapping = unifier.mapping.clone();
        for v in 1..=self.cur_var {
            println!("- unifVar({}) => {:?}", v, mapping.get(&v));
        }
        self.pretty_heap_w_mapping(&mapping);

        // self.pretty_heap_w_mapping(&unifier.mapping);

        // unify the jumps to labelled blocks
        unifier.check_let_poly()?;

        unifier.close_mapping();

        // debugging
        println!("--- Mapping --- ");
        let mapping = unifier.mapping.clone();
        for v in 1..=self.cur_var {
            println!("- unifVar({}) => {:?}", v, mapping[&v]);
        }

        println!("--- Rho mappings --- ");
        for (rho, mapping) in unifier.rho_mappings.clone() {
            println!("- rho({}) => {}", rho, sort_for_print(&mapping));
        }
        self.pretty_heap_w_mapping(&mapping);


        Ok(())
    }

    pub fn new() -> Checker {
        Checker {
            cur_rho: 0,
            cur_var: 0,
            register_types: HashMap::new(),
            label_types: HashMap::new(),
            constraints: Vec::new(),
        }
    }
}
