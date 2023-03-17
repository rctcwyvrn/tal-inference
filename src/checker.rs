use crate::syntax::*;
use core::panic;
use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    hash::Hash,
    iter::Map, char::MAX,
};
use thiserror::Error;

const MAX_REGISTER: usize = 3;
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
    cur_var: usize,

    // local context
    register_types: HashMap<Register, Ty>,

    // global context
    heap_types: HashMap<Label, Ty>,
    constraints: Vec<(Ty, Ty)>,
    satisfy: Vec<(Ty, Ty)>,
}

#[derive(Debug, Error)]
pub enum TypeError {
    #[error("value {0} does not have type {1}")]
    ConcreteVal(Value, Ty),

    #[error("Invalid use of label")]
    InvalidLabel,

    #[error("register {0} is of type {1} but expected to be of type {2}")]
    RegisterConflict(Register, Ty, Ty),

    #[error("failed to constrain jump")]
    FailedJump,

    #[error("failed to unify")]
    FailedUnify
}

impl Checker {
    fn constrain(&mut self, a: &Ty, b: &Ty) -> bool {
        println!("> Constraining {:?} == {:?}", a,b);
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

    // Add constraint that either
    // - a == b
    // - a is a type variable
    fn satisfies(&mut self, a: &Ty, b: &Ty) {
        println!("> Checking satisfies {:?} <: {:?}", a,b);
        self.satisfy.push((a.clone(), b.clone()))
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
        // infer val to either a unifVar or a LabelType directly
        // add \gamma <: val's type to the constraints for later

        // Two cases:
        // - v is a label and we can directly determine its corresponding Code type
        // - v is a register containing a type variable t

        // example case: a set_stuff_and_jump_if_two(r1 = v, r2 = label) function that sets some registers and jumps to label if r1 == 2
        // how does TAL type this?
        // - jump_if_two's type is pre-determined in the heap type, so it would be Code<r1: Int, r2: Code<Gamma'>> in the subtyping form.
        //  the subtyping rule would then check that gamma when you reach the jump in jump_if_two is a subtype of Gamma'
        // - What about for the polymorphic model? It could be Code<r1: Int, r2: Code<Gamma'>>, where gamma has all of its unused registers set as type variables
        //  tal then just has to check that r2[setting gamma' type vars to match gamma] == gamma 

        // what do we want?
        // constrain the ty == a code type
        // check that the current context satisfies new code type (equal up to type variables)
        let val_ty = self.infer_value(val);
        let code_ty = self.init_code_type();
        let success = self.constrain(&val_ty, &Ty::Code(code_ty.clone()));

        for r in 1..=MAX_REGISTER {
            self.satisfies(&code_ty[&r], &self.register_types[&r].clone());
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

    fn try_unify(&mut self) -> Result<HashMap<usize, Ty>, TypeError> {
        let mut mapping = HashMap::new();
        let mut substitute = |v: &mut Vec<(Ty, Ty)>, x, t: Ty| {
            mapping.insert(x, t.clone());
            v.iter_mut().for_each(|(lhs, rhs)| {
                if *lhs == Ty::UnifVar(x) {
                    *lhs = t.clone();
                }
                if *rhs == Ty::UnifVar(x) {
                    *rhs = t.clone();
                }
            });
        };

        let mut eqs = self.constraints.clone();
        while !eqs.is_empty() {
            let next = eqs.pop().unwrap();
            match next {
                (s, t) if s == t => {}
                (t, Ty::UnifVar(x)) | (Ty::UnifVar(x), t) => {
                    substitute(&mut eqs, x, t)
                }
                (Ty::Code(fn_a), Ty::Code(fn_b)) => {
                    for r in 1..=MAX_REGISTER {
                        eqs.push((fn_a[&r].clone(), fn_b[&r].clone()));
                    }
                }
                _ => {
                    println!("> Failed on: {:?}", next);
                    return Err(TypeError::FailedUnify);
                }
            }
        }
        Ok(mapping)
    }

    // fn close(ty: Ty, mapping: &HashMap<usize, Ty>) -> Ty {
    //     if let Ty::UnifVar(v) = ty {
    //         if mapping.contains_key(&v) {
    //             return mapping[&v].clone()
    //         }
    //     }
    //     return ty
    // }

    fn try_satisfy(&mut self, mapping: &HashMap<usize, Ty>) -> Result<(), TypeError> {
        println!("Satisfy");
        for (lhs, rhs) in &self.satisfy {
            let lhs = Checker::chase_to_root(lhs.clone(), mapping);
            let rhs = Checker::chase_to_root(rhs.clone(), mapping);

            println!("{:?} <: {:?}", lhs, rhs);
        }
        Ok(())
    }

    fn init_code_type(&mut self) -> CodeTy {
        let mut ty = HashMap::new();
        for r in 1..=MAX_REGISTER {
            ty.insert(r, self.fresh());
        }
        ty
    }

    fn pretty_heap(&self) {
        println!("--- Heap ---");
        for label in self.heap_types.keys() {
            println!("{} => ", label);
            if let Ty::Code(f) = &self.heap_types[label] {
                for r in 1..=MAX_REGISTER {
                    println!("  {}: {:?}", r, f[&r])
                }
            }
        }
    }

    // returns a non unifVar type, either lifting it to a typevar or finding it within the mapping   
    fn chase_to_root(ty: Ty, mapping: &HashMap<usize, Ty>) -> Ty {
        let mut var = ty;
        loop {
            match var {
                Ty::TyVar(_) => break,
                Ty::Int => break,
                Ty::Code(f) => {
                    let mut roots = HashMap::new();
                    for r in 1..=MAX_REGISTER {
                        roots.insert(r, Checker::chase_to_root(f[&r].clone(), mapping));
                    }
                    var = Ty::Code(roots);
                    break
                }
                Ty::UnifVar(v) => if mapping.contains_key(&v) {
                    var = mapping[&v].clone();
                } else {
                    var = Ty::TyVar(v);
                    break
                }
            }
        }
        var
    }

    fn chase_all_to_root(&self, mut mapping: HashMap<usize, Ty>) -> HashMap<usize, Ty> {
        // close mapping
        for v in 1..=self.cur_var {
            if mapping.contains_key(&v) {
                let ty = Checker::chase_to_root(mapping[&v].clone(), &mapping);
                mapping.insert(v, ty);
            } else {
                mapping.insert(v, Ty::TyVar(v));
            }
        }
        mapping
    }
    
    fn pretty_heap_w_mapping(&self, mapping: HashMap<usize, Ty>) {
        println!("--- Heap w/ mapping ---");
        for label in self.heap_types.keys() {
            println!("{} => ", label);
            if let Ty::Code(f) = &self.heap_types[label] {
                for r in 1..=MAX_REGISTER {
                    let var = Checker::chase_to_root(f[&r].clone(), &mapping);
                    match var {
                        Ty::UnifVar(_) => println!("  {}: unbound", r),
                        t => println!("  {}: {:?}", r, t),
                    }
                }
            } else { panic!("wat") }
        }
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

        // for instruction in program {
        //     self.check_instruction(instruction)?
        // }
        // return Ok(());
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
