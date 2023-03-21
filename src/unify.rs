use core::panic;
use std::collections::{HashMap, VecDeque};

use crate::checker::*;

#[derive(Debug, Clone)]
pub enum RhoEntry {
    Contains(Ty),
    Absent,
}

pub type RhoMapping = HashMap<i64, RhoEntry>;

pub struct Unifier {
    constraints: VecDeque<(Ty, Ty)>,
    satisfy: VecDeque<Vec<(Ty, Ty)>>,
    variables: usize,
    pub mapping: HashMap<usize, Ty>,
    pub rho_mappings: HashMap<usize, RhoMapping>,
}

// todo: newtype wrap usize being the unifvar types, along with seperating out the whole TyX thing

impl Unifier {
    fn substitute(
        mapping: &mut HashMap<usize, Ty>,
        eqs: &mut VecDeque<(Ty, Ty)>,
        variable: Ty,
        substitute: Ty,
    ) {
        let id = match variable {
            Ty::UnifVar(x) => x,
            Ty::TyVar(x) => x,
            _ => panic!("unreachable: substituting something other than a variable"),
        };

        mapping.insert(id, substitute.clone());
        eqs.iter_mut().for_each(|(lhs, rhs)| {
            if *lhs == variable {
                *lhs = substitute.clone();
            }
            if *rhs == variable {
                *rhs = substitute.clone();
            }
        });
    }

    fn unify_ptrs(
        &mut self,
        l_set: HashMap<i64, Ty>,
        l_rho: Option<Rho>,
        r_set: HashMap<i64, Ty>,
        r_rho: Option<Rho>,
    ) -> Result<(), TypeError> {
        // do the unification switcheroo
        // https://ahnfelt.medium.com/row-polymorphism-crash-course-587f1e7b7c47
        println!(
            "[+] unifying {:?} {:?} =  {:?} {:?} ",
            l_set, l_rho, r_set, r_rho
        );
        let mut r_rho_mapping = HashMap::new();
        for l_key in l_set.keys() {
            if r_set.contains_key(l_key) {
                self.constraints
                    .push_back((l_set[l_key].clone(), r_set[l_key].clone()));

                if r_rho.is_some() {
                    println!("-- rhs rho does not have {:?}", l_key);
                    r_rho_mapping.insert(*l_key, RhoEntry::Absent);
                }
            } else {
                if r_rho.is_some() {
                    println!(
                        "-- rhs is missing key {:?} but it has rho, inserting",
                        l_key
                    );
                    r_rho_mapping.insert(*l_key, RhoEntry::Contains(l_set[l_key].clone()));
                } else {
                    return Err(TypeError::NoRho);
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
                    println!(
                        "-- lhs is missing key {:?} but it has rho, inserting",
                        r_key
                    );
                    l_rho_mapping.insert(*r_key, RhoEntry::Contains(r_set[r_key].clone()));
                } else {
                    return Err(TypeError::NoRho);
                }
            }
        }

        if let Some(Rho(r)) = l_rho {
            self.unify_rho(l_rho_mapping, r)?;
        }

        if let Some(Rho(r)) = r_rho {
            self.unify_rho(r_rho_mapping, r)?;
        }
        Ok(())
    }

    fn unify_rho(&mut self, new_rho: RhoMapping, rho_id: usize) -> Result<(), TypeError> {
        if self.rho_mappings.contains_key(&rho_id) {
            let mapping = self.rho_mappings.get_mut(&rho_id).unwrap();
            // debug
            println!(
                "++ Checking rho({}) consistency | {:?} = {:?}",
                rho_id, mapping, new_rho
            );

            for key in new_rho.keys() {
                if mapping.contains_key(key) {
                    match (mapping[key].clone(), new_rho[key].clone()) {
                        // both are absent
                        (RhoEntry::Absent, RhoEntry::Absent) => continue,
                        // one says a field is absent, the other says it is not, which is invalid
                        (RhoEntry::Absent, RhoEntry::Contains(_))
                        | (RhoEntry::Contains(_), RhoEntry::Absent) => {
                            return Err(TypeError::RhoConflict)
                        }
                        // Unify the two rho constraints
                        (RhoEntry::Contains(lhs), RhoEntry::Contains(rhs)) => {
                            self.constraints.push_back((lhs, rhs));
                        }
                    }
                } else {
                    // the new rho contains more fields than the old rho can guarantee
                    return Err(TypeError::RhoConflict);
                }
            }
        } else {
            println!("++ Inserting new rho({}) = {:?}", rho_id, new_rho);
            self.rho_mappings.insert(rho_id, new_rho);
        };

        Ok(())
    }

    pub fn try_unify(&mut self) -> Result<(), TypeError> {
        while !self.constraints.is_empty() {
            let next = self.constraints.pop_front().unwrap();
            match next {
                (s, t) if s == t => {}
                (t, Ty::UnifVar(x)) | (Ty::UnifVar(x), t) => {
                    Unifier::substitute(&mut self.mapping, &mut self.constraints, Ty::UnifVar(x), t)
                }
                (Ty::Code(fn_a), Ty::Code(fn_b)) => {
                    for r in 1..=MAX_REGISTER {
                        self.constraints.push_back((fn_a[&r].clone(), fn_b[&r].clone()));
                    }
                }

                // expect lhs to be at least as strong as rhs (scuffed subtyping whoops)
                (Ty::UniqPtr(l_set, l_rho), Ty::UniqPtr(r_set, r_rho))
                | (Ty::UniqPtr(l_set, l_rho), Ty::Ptr(r_set, r_rho))
                | (Ty::Ptr(l_set, l_rho), Ty::Ptr(r_set, r_rho)) => {
                    self.unify_ptrs(l_set, l_rho, r_set, r_rho)?
                }
                _ => {
                    println!("> Failed on: {:?}", next);
                    return Err(TypeError::FailedUnify);
                }
            }
        }
        // debug
        println!("--- rhos --- ");
        let rhos = self.rho_mappings.keys().collect::<Vec<&usize>>();
        for rho_id in rhos {
            println!("rho({}) = {:?}", rho_id, self.rho_mappings[rho_id])
        }
        Ok(())
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

    pub fn try_satisfy(&mut self) -> Result<(), TypeError> {
        println!("The parameter we're trying to pass it <: what the function is expecting");
        for jump in &self.satisfy {
            let jump: Vec<(Ty, Ty)> = jump
                .iter()
                .map(|(lhs, rhs)| {
                    (
                        self.chase_to_root(lhs.clone()),
                        self.chase_to_root(rhs.clone()),
                    )
                })
                .collect();

            // debugging
            println!("---");
            for (lhs, rhs) in &jump {
                println!("{:?} <: {:?}", lhs, rhs);
            }

            let mapping = Unifier::try_satisfy_jump(jump.clone())?;

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
    pub fn chase_to_root(&self, ty: Ty) -> Ty {
        let mut var = ty;
        loop {
            match var {
                Ty::TyVar(_) => break,
                Ty::Int => break,
                Ty::Code(f) => {
                    let mut roots = HashMap::new();
                    for r in 1..=MAX_REGISTER {
                        roots.insert(r, self.chase_to_root(f[&r].clone()));
                    }
                    var = Ty::Code(roots);
                    break;
                }
                Ty::UnifVar(v) => {
                    if self.mapping.contains_key(&v) {
                        var = self.mapping[&v].clone();
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

    // mutably close this unifiers mapping
    pub fn chase_all_to_root(&mut self) {
        for v in 1..=self.variables {
            if self.mapping.contains_key(&v) {
                let ty = self.chase_to_root(self.mapping[&v].clone());
                self.mapping.insert(v, ty);
            } else {
                self.mapping.insert(v, Ty::TyVar(v));
            }
        }
    }

    pub fn new(
        constraints: Vec<(Ty, Ty)>,
        satisfy: Vec<Vec<(Ty, Ty)>>,
        variables: usize,
    ) -> Unifier {
        Unifier {
            constraints: constraints.into(),
            satisfy: satisfy.into(),
            variables,
            mapping: HashMap::new(),
            rho_mappings: HashMap::new(),
        }
    }
}
