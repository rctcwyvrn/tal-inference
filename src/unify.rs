use std::{collections::HashMap};

use crate::checker::*;

#[derive(Debug, Clone)]
enum RhoEntry {
    Contains(Ty),
    Absent,
}

type RhoMapping = HashMap<i64, RhoEntry>;

impl Checker {
    pub fn try_unify(&self) -> Result<HashMap<usize, Ty>, TypeError> {
        // might be absent
        let mut rho_mappings: HashMap<usize, RhoMapping> = HashMap::new();
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

        let mut check_rho = |eqs: &mut Vec<(Ty, Ty)>, new_rho: RhoMapping, rho_id | {
            let updated_rho = if rho_mappings.contains_key(&rho_id) {
                let mut mapping = rho_mappings[&rho_id].clone();
                // debug
                println!("++ Checking rho({}) consistency | {:?} = {:?}", rho_id, mapping, new_rho);

                for key in new_rho.keys() {
                    if mapping.contains_key(key) {
                        match (mapping[key].clone(), new_rho[key].clone()) {
                            (RhoEntry::Absent, RhoEntry::Absent) => continue, // both are absent
                            // one says a field is absent, the other says it is not
                            (RhoEntry::Absent, RhoEntry::Contains(_)) | (RhoEntry::Contains(_), RhoEntry::Absent) => return Err(TypeError::RhoConflict),
                            (RhoEntry::Contains(lhs), RhoEntry::Contains(rhs)) => {
                                eqs.push((lhs, rhs));
                            }
                        }
                    } else {
                        //t he new mapping sets a field (either to a ty or absent) that the old mapping does not restrict at all
                        // so its valid, but shove the new information into the mapping
                        mapping.insert(*key, new_rho[key].clone());
                    }
                }

                mapping
            } else {
                new_rho
            };

            println!("++ Inserting rho({}) = {:?}", rho_id, updated_rho);
            rho_mappings.insert(rho_id, updated_rho);
            Ok(())
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

                // expect lhs to be at least as strong as rhs (scuffed subtyping whoops)
                (Ty::UniqPtr(l_set, l_rho), Ty::UniqPtr(r_set, r_rho)) |
                (Ty::UniqPtr(l_set, l_rho), Ty::Ptr(r_set, r_rho)) |
                (Ty::Ptr(l_set, l_rho), Ty::Ptr(r_set, r_rho)) => {
                    // do the unification switcheroo
                    // https://ahnfelt.medium.com/row-polymorphism-crash-course-587f1e7b7c47
                    println!("[+] unifying {:?} {:?} =  {:?} {:?} ", l_set, l_rho, r_set, r_rho);
                    let mut r_rho_mapping = HashMap::new();
                    for l_key in l_set.keys() {
                        if r_set.contains_key(l_key) { 
                            eqs.push((l_set[l_key].clone(), r_set[l_key].clone()));

                            if r_rho.is_some() {
                                println!("-- rhs rho does not have {:?}", l_key);
                                r_rho_mapping.insert(*l_key, RhoEntry::Absent);
                            }
                        } else {
                            if r_rho.is_some() {
                                println!("-- rhs is missing key {:?} but it has rho, inserting", l_key);
                                r_rho_mapping.insert(*l_key, RhoEntry::Contains(l_set[l_key].clone()));
                            } else {
                                return Err(TypeError::NoRho)
                            }
                        }
                    }

                    let mut l_rho_mapping = HashMap::new();
                    for r_key in r_set.keys() {
                        if l_set.contains_key(r_key) {
                            if l_rho.is_some() {
                                println!("-- lhs rho does not have {:?}", r_key);
                                l_rho_mapping.insert(*r_key, RhoEntry::Absent);
                            } 
                        } else {
                            if l_rho.is_some() {
                                println!("-- lhs is missing key {:?} but it has rho, inserting", r_key);
                                l_rho_mapping.insert(*r_key, RhoEntry::Contains(r_set[r_key].clone()));
                            } else {
                                return Err(TypeError::NoRho)
                            }
                        }
                    }

                    if let Some(Rho::Rho(r)) = l_rho {
                        check_rho(&mut eqs,  l_rho_mapping, r)?;
                    }

                    if let Some(Rho::Rho(r)) = r_rho {
                        check_rho(&mut eqs, r_rho_mapping, r)?;
                    }
                }
                _ => {
                    println!("> Failed on: {:?}", next);
                    return Err(TypeError::FailedUnify);
                }
            }
        }
        println!("!! rhos {:?}", rho_mappings);
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
                Ty::Ptr(_, _) => todo!(),
                Ty::UniqPtr(_, _) => todo!(),
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
