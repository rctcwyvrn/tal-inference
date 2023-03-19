use std::collections::HashMap;

use crate::checker::*;

impl Checker {
    pub fn try_unify(&self) -> Result<HashMap<usize, Ty>, TypeError> {
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
                (t, Ty::UnifVar(x)) | (Ty::UnifVar(x), t) => substitute(&mut eqs, x, t),
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

    fn try_satisfy_jump(jump: Vec<(Ty, Ty)>) -> Result<HashMap<usize, Ty>, TypeError> {
        let mut mapping = HashMap::new();
        let mut substitute = |v: &mut Vec<(Ty, Ty)>, x, t: Ty| {
            mapping.insert(x, t.clone());
            v.iter_mut().for_each(|(lhs, rhs)| {
                if *lhs == Ty::TyVar(x) {
                    *lhs = t.clone();
                }
                if *rhs == Ty::TyVar(x) {
                    *rhs = t.clone();
                }
            });
        };

        let mut eqs = jump;
        while !eqs.is_empty() {
            let next = eqs.pop().unwrap();
            match next {
                // types match, done
                (s, t) if s == t => {}
                // function expects a generic parameter, so we can freely substitute
                (t, Ty::TyVar(x)) => substitute(&mut eqs, x, t),
                // function expects a label, we have a label
                (Ty::Code(fn_a), Ty::Code(fn_b)) => {
                    // the block we're jumping to is able to call a function of type fn_b
                    // in order to substitute fn_a in fn_b's place, what do we require?
                    // - if fn_b's r_k is concrete
                    //   then if fn_a's r_k is a type variable or is the same concrete type, we can call it
                    // - if fn_b's r_k is generic
                    //   then we can't assume anything about the contents of r_k before fn_a would be called
                    //     so fn_a's r_k must be a type variable
                    for r in 1..=MAX_REGISTER {
                        // eqs.push((fn_a[&r].clone(), fn_b[&r].clone()));
                        // I THINK
                        eqs.push((fn_b[&r].clone(), fn_a[&r].clone()));
                    }
                }
                _ => {
                    println!("> Failed satisfy_jump on: {:?}", next);
                    return Err(TypeError::FailedJump);
                }
            }
        }
        Ok(mapping)
    }

    pub fn try_satisfy(&mut self, mapping: &HashMap<usize, Ty>) -> Result<(), TypeError> {
        println!("The parameter we're trying to pass it <: what the function is expecting");
        for jump in &self.satisfy {
            let jump: Vec<(Ty, Ty)> = jump
                .iter()
                .map(|(lhs, rhs)| {
                    (
                        Checker::chase_to_root(lhs.clone(), mapping),
                        Checker::chase_to_root(rhs.clone(), mapping),
                    )
                })
                .collect();
            
            // debugging
            println!("---");
            for (lhs, rhs) in &jump {
                println!("{:?} <: {:?}", lhs, rhs);
            }
            
            let mapping = Checker::try_satisfy_jump(jump.clone())?;
            // debugging
            print!("[");
            for (lhs, rhs) in mapping.iter() {
                print!("TyVar({})={:?}, ", lhs, rhs)
            }
            println!("]");
        }
        Ok(())
    }

    // returns a non unifVar type, either lifting it to a typevar or finding it within the mapping
    pub fn chase_to_root(ty: Ty, mapping: &HashMap<usize, Ty>) -> Ty {
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
                    break;
                }
                Ty::UnifVar(v) => {
                    if mapping.contains_key(&v) {
                        var = mapping[&v].clone();
                    } else {
                        var = Ty::TyVar(v);
                        break;
                    }
                }
            }
        }
        var
    }

    pub fn chase_all_to_root(&self, mut mapping: HashMap<usize, Ty>) -> HashMap<usize, Ty> {
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
}
