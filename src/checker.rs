use crate::unify::Unifier;
use crate::{debug::sort_for_print, syntax::*};

use core::panic;
use std::collections::{HashMap, HashSet};
use thiserror::Error;

pub const MAX_REGISTER: usize = 3;
pub type CodeTy = HashMap<Register, Ty>;
pub type CodeTyU = HashMap<Register, TyU>;
#[derive(Debug, PartialEq, Clone)]
pub enum TyU {
    Int,
    Code(CodeTyU),
    Ptr(HashMap<i64, TyU>, Option<Rho>),
    UniqPtr(HashMap<i64, TyU>, Option<Rho>),
    Any,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Ty {
    Int,
    Code(CodeTy),
    UnifVar(usize), 
    Ptr(HashMap<i64, Ty>, Option<Rho>),
    UniqPtr(HashMap<i64, Ty>, Option<Rho>),
}

#[derive(Debug, Clone, Copy)]
pub enum TyRaw {
    Int, Code, Ptr, UniqPtr,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Rho(pub usize);

pub struct Checker {
    // for fresh unifvars
    pub cur_var: usize,
    pub cur_rho: usize,

    // local context
    pub register_types: HashMap<Register, Ty>,
    pub cur_label: String,

    // global context
    pub heap_types: HashMap<Label, Ty>,
    pub constraints: Vec<(Ty, Ty)>,
    pub satisfy: Vec<(CodeTy, CodeTy)>,
    pub not_equal_constraints: Vec<(Ty, TyRaw)>,
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

    #[error("entry label has requirements")]
    InvalidEntrypoint,
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

    fn constrain_not_equal(&mut self, a: &Ty, b: &TyRaw) {
        println!("> Constraining {} != {}", a, b);
        self.not_equal_constraints.push((a.clone(), b.clone()));
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

        // scuffed!
        let is_indirect = {
            let current_label_ty = self.heap_types[&self.cur_label].clone();
            let current_label_starting_gamma = if let Ty::Code(c) = current_label_ty {
                c
            } else {
                panic!()
            };
            let current_label_starting_unifvars: HashSet<usize> = current_label_starting_gamma
                .iter()
                .map(|(_, t)| if let Ty::UnifVar(v) = t { *v } else { panic!() })
                .collect();
            match val_ty {
                Ty::UnifVar(x) if current_label_starting_unifvars.contains(&x) => true,
                _ => false,
            }
        };
        println!("(???) is_indirect? {} ({})", is_indirect, val_ty);
        let code_ty = self.init_code_type();

        // constrain the value that we're jumping to to be a code type
        let mut success = self.constrain(&val_ty, &Ty::Code(code_ty.clone()));

        // note: we could update the register (if val is a register) to be the code type
        // which would reduce unifications a little, probably doesnt matter

        // constrain the parameters of th code type to be supertypes of what our current registers are
        for r in 1..=MAX_REGISTER {
            let ty = self.register_types[&r].clone();
            match ty {
                // if we have a concrete type, we can constrain the input type of the function we're jumping to
                // the goal is to build up as much known information as we can
                Ty::Int | Ty::Ptr(_, _) | Ty::UniqPtr(_, _) if is_indirect => {
                    // we could say that the parameter type is a type variable
                    // but that becomes intractable (??), this appears to work
                    println!("(???) constraining r{} = {}", r, ty);
                    success &= self.constrain(&code_ty[&r], &ty);
                }
                // otherwise we have to check that it can be satisfied
                _ => {
                    println!("(???) skipping r{} = {}", r, ty);
                }
            }
        }
        println!(
            "> Adding jump subtype constraint {} <: {}",
            sort_for_print(&self.register_types),
            sort_for_print(&code_ty)
        );
        self.satisfy
            .push((code_ty.clone(), self.register_types.clone()));

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
                self.constrain_not_equal(&rhs, &TyRaw::UniqPtr);
                self.update_register(r, rhs);
                Ok(())
            }

            // heap instructions
            Instruction::Load(r_tar, r_src, idx) => {
                let output_type = self.fresh();

                // expect that we at _least_ have a generic pointer with that idx set to the output type
                let some_rho = self.fresh_rho();
                let row_constraint = HashMap::from([(idx, output_type.clone())]);
                let expected_src = Ty::Ptr(row_constraint, Some(some_rho));
                self.constrain_register(r_src, expected_src)?;
                self.update_register(r_tar, output_type);
                Ok(())
            }
            // fixme: this should be a subtyping constraint
            // refactor satisfy to store pairs of types like before? 
            // unifier should count them as equal, and then satisfier should check
            Instruction::StoreStrong(r_tar, idx, r_src) => {
                let old_rho = self.fresh_rho();
                // generate a unifvar for the old type, but don't constrain it in any way
                let old_type = self.fresh();
                let row_constraint = HashMap::from([(idx, old_type)]);
                // check that we had a UniqPtr with idx set to _any_ type
                // with old_rho holding other fields
                let expected_tar = Ty::UniqPtr(row_constraint, Some(old_rho));
                self.constrain_register(r_tar, expected_tar)?;

                let new_type = self.register_types[&r_src].clone();
                // check that we aren't storing a uniqptr
                self.constrain_not_equal(&new_type, &TyRaw::UniqPtr);
                let new_known = HashMap::from([(idx, new_type)]);
                // old_rho remembers all the fields set before
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
        let ty = self.heap_types[&block.0].clone();
        if let Ty::Code(gamma) = ty {
            self.register_types = gamma;
            self.cur_label = block.0.clone();
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

    fn check_entrypoint(
        &self,
        program: &Program,
        mapping: &HashMap<usize, TyU>,
    ) -> Result<(), TypeError> {
        let (label, _, _) = &program[0];
        let ty = &self.heap_types[label];
        if let Ty::Code(vars) = ty {
            for r in 1..=MAX_REGISTER {
                if let Ty::UnifVar(v) = vars[&r] {
                    match mapping[&v] {
                        TyU::Any => continue,
                        _ => return Err(TypeError::InvalidEntrypoint),
                    }
                } else {
                    panic!("unreachable")
                }
            }
        } else {
            panic!("unreachable")
        }
        Ok(())
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
            println!("- {} = {}", c.0, c.1);
        }

        // unify the variables in the block bodies
        let mut unifier =
            Unifier::new(self.constraints.clone(), self.satisfy.clone(), self.cur_var, self.not_equal_constraints.clone());

        unifier.unify()?;
        let mut satisfier = unifier.chase_all_to_root();

        println!("--- Mapping --- ");
        let mapping = satisfier.mapping.clone();
        for v in 1..=self.cur_var {
            println!("- unifVar({}) => {}", v, mapping[&v]);
        }

        println!("--- Rho mappings --- ");
        for (rho, mapping) in satisfier.rho_mappings.clone() {
            println!("- rho({}) => {}", rho, sort_for_print(&mapping));
        }

        // debugging
        self.pretty_heap();
        self.pretty_heap_w_mapping(&mapping);

        // unify the jumps to labelled blocks
        satisfier.satisfy()?;
        satisfier.check_neq()?;

        self.check_entrypoint(&program, &satisfier.mapping)?;

        Ok(())
    }

    pub fn new() -> Checker {
        Checker {
            cur_rho: 0,
            cur_var: 0,
            register_types: HashMap::new(),
            cur_label: "".to_owned(),
            heap_types: HashMap::new(),
            constraints: Vec::new(),
            satisfy: Vec::new(),
            not_equal_constraints: Vec::new(),
        }
    }
}
