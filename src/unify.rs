use core::panic;
use std::{collections::{HashMap, VecDeque}, char::MAX};

use crate::{checker::*, debug::sort_for_print};

#[derive(Debug, Clone, PartialEq)]
pub enum RhoEntry {
    Contains(Ty),
    Absent,
}

pub type RhoMapping = HashMap<i64, RhoEntry>;

pub struct Unifier {
    constraints: VecDeque<(Ty, Ty)>,
    lp_constraints: Vec<(CodeTy, Vec<CodeTy>)>,
    variables: usize,
    pub mapping: HashMap<usize, Ty>,
    pub rho_mappings: HashMap<usize, RhoMapping>,
}

// todo: newtype wrap usize being the unifvar types, along with seperating out the whole TyX thing

impl Unifier {
    fn constrain(&mut self, a: Ty, b: Ty) {
        println!("> Constraining {} = {}", a, b);
        self.constraints.push_back((a, b));
    }

    fn substitute(
        mapping: &mut HashMap<usize, Ty>,
        eqs: &mut VecDeque<(Ty, Ty)>,
        variable: Ty,
        substitute: Ty,
    ) {
        println!("> Substituting {} = {}", variable, substitute);
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
        println!(
            "\n[+] Unifying pointers ({},{:?}) = ({},{:?})",
            sort_for_print(&l_set),
            l_rho,
            sort_for_print(&r_set),
            r_rho
        );
        let l_set = Unifier::convert(l_set);
        let r_set = Unifier::convert(r_set);
        match (l_rho, r_rho) {
            (None, None) => {
                self.unify_rho_mappings(l_set, r_set)?;
            }
            (None, Some(rhs_rho)) => {
                let expect = self.unify_subtract(l_set, r_set)?;
                self.unify_rho_id_with_mapping(rhs_rho, expect)?
            }
            (Some(lhs_rho), None) => {
                let expect = self.unify_subtract(r_set, l_set)?;
                self.unify_rho_id_with_mapping(lhs_rho, expect)?
            }
            (Some(lhs_rho), Some(r_known)) if self.rho_mappings.contains_key(&r_known.0) => {
                let r_rho = self.rho_mappings[&r_known.0].clone();
                let rhs = Unifier::add(r_set, r_rho)?;
                let expect = self.unify_subtract(rhs, l_set)?;
                self.unify_rho_id_with_mapping(lhs_rho, expect)?
            }
            (Some(l_known), Some(rhs_rho)) if self.rho_mappings.contains_key(&l_known.0) => {
                let l_rho = self.rho_mappings[&l_known.0].clone();
                let lhs = Unifier::add(l_set, l_rho)?;
                let expect = self.unify_subtract(lhs, r_set)?;
                self.unify_rho_id_with_mapping(rhs_rho, expect)?
            }
            _ => {
                // todo: this is where i have to do the switcheroo i think
                // but that never happens for tal i think?
                panic!("I think this is unreachable? When would this happen?")
            }
        }
        println!("---");
        Ok(())
    }

    fn convert(ptr_mapping: HashMap<i64, Ty>) -> RhoMapping {
        ptr_mapping
            .into_iter()
            .map(|(idx, ty)| (idx, RhoEntry::Contains(ty)))
            .collect()
    }

    fn add(set: RhoMapping, rho: RhoMapping) -> Result<RhoMapping, TypeError> {
        println!(
            "(~) Computing X + Y for X = {} | Y = {}",
            sort_for_print(&set),
            sort_for_print(&rho)
        );
        let mut result = set.clone();
        for idx in rho.keys() {
            if !result.contains_key(idx) {
                result.insert(*idx, rho[idx].clone());
            } else {
                // can't add two things that set the same idx
                return Err(TypeError::RhoFailedAdd);
            }
        }
        println!("(~) X + Y = {}", sort_for_print(&result));
        Ok(result)
    }

    fn unify_rho_mappings(&mut self, l: RhoMapping, r: RhoMapping) -> Result<(), TypeError> {
        for key in l.keys() {
            if r.contains_key(key) {
                self.unify_rho_entries(l[key].clone(), r[key].clone())?;
            } else {
                return Err(TypeError::RhoConflict);
            }
        }
        Ok(())
    }

    fn unify_rho_entries(&mut self, l: RhoEntry, r: RhoEntry) -> Result<(), TypeError> {
        match (l, r) {
            // both contain a type
            (RhoEntry::Contains(t1), RhoEntry::Contains(t2)) => {
                self.constrain(t1, t2);
                Ok(())
            }
            // both don't contain a type
            (RhoEntry::Absent, RhoEntry::Absent) => Ok(()),
            // conflict
            // todo: does this ever happen???
            _ => return Err(TypeError::RhoConflict),
        }
    }

    fn unify_subtract(
        &mut self,
        l_set: RhoMapping,
        r_set: RhoMapping,
    ) -> Result<RhoMapping, TypeError> {
        println!(
            "(~) Computing X - Y for X = {} | Y = {}",
            sort_for_print(&l_set),
            sort_for_print(&r_set)
        );
        let mut result = l_set.clone();
        for idx in r_set.keys() {
            if result.contains_key(idx) {
                result.remove(idx);
                self.unify_rho_entries(l_set[idx].clone(), r_set[idx].clone())?;
            } else {
                // different sets of keys
                return Err(TypeError::RhoTooSmall);
            }
        }
        println!("(~) X - Y = {}", sort_for_print(&result));
        Ok(result)
    }

    fn unify_rho_id_with_mapping(&mut self, rho: Rho, set: RhoMapping) -> Result<(), TypeError> {
        let rho_id = rho.0;
        if self.rho_mappings.contains_key(&rho_id) {
            println!(
                "- rho({:?}) requirements match? {} = {}",
                rho_id,
                sort_for_print(&set),
                sort_for_print(&self.rho_mappings[&rho_id])
            );
            // constrain that they exactly match
            let rho = self.rho_mappings[&rho_id].clone();
            self.unify_rho_mappings(rho, set)?;
        } else {
            println!("- new rho({:?}) {}", rho_id, sort_for_print(&set));
            self.rho_mappings.insert(rho_id, set);
        }
        Ok(())
    }

    pub fn unify(&mut self) -> Result<(), TypeError> {
        println!("\n--- starting unify ---");

        while !self.constraints.is_empty() {
            let next = self.constraints.pop_front().unwrap();
            match next {
                (s, t) if s == t => {}
                // asldkfjasldfjlsakdfjklsdf
                // extra cases for the let-poly nonsense
                // typevar initialization
                // UHHHHHH??? IDK IF THIS WORKS 
                (Ty::TyVar(x), t) => {
                    println!("[!!] initializing TyVar({}) = {} in this let-poly", x, t);
                    Unifier::substitute(&mut self.mapping, &mut self.constraints, Ty::UnifVar(x), t)
                }

                (t, Ty::UnifVar(x)) | (Ty::UnifVar(x), t) => {
                    if self.mapping.contains_key(&x) {
                        panic!("LILY YOU FUCKED UP -> this unifvar should have been added to the mapping and chased");
                    }
                    Unifier::substitute(&mut self.mapping, &mut self.constraints, Ty::UnifVar(x), t)
                }
                (Ty::Code(fn_a), Ty::Code(fn_b)) => {
                    for r in 1..=MAX_REGISTER {
                        // self.constrain(fn_a[&r].clone(), fn_b[&r].clone());
                        // WARNING: SCUFFED
                        self.constrain(fn_b[&r].clone(), fn_a[&r].clone());
                    }
                }

                // expect lhs to be at least as strong as rhs (scuffed subtyping whoops)
                (Ty::UniqPtr(l_set, l_rho), Ty::UniqPtr(r_set, r_rho))
                | (Ty::UniqPtr(l_set, l_rho), Ty::Ptr(r_set, r_rho))
                | (Ty::Ptr(l_set, l_rho), Ty::Ptr(r_set, r_rho)) => {
                    self.unify_ptrs(l_set, l_rho, r_set, r_rho)?
                }
                _ => {
                    println!("> Failed on: {} = {}", next.0, next.1);
                    return Err(TypeError::FailedUnify);
                }
            }
        }
        // debug
        println!("--- rhos --- ");
        let rhos = self.rho_mappings.keys().collect::<Vec<&usize>>();
        for rho_id in rhos {
            println!(
                "rho({}) = {}",
                rho_id,
                sort_for_print(&self.rho_mappings[rho_id])
            )
        }
        Ok(())
    }

    pub fn check_let_poly(&mut self) -> Result<(), TypeError> {
        for (base, polys) in &self.lp_constraints {
            for poly in polys {
                println!("[??] Doing let poly {} = {}", sort_for_print(base), sort_for_print(&poly));
                let mut constraints = Vec::new();
                for r in 1..=MAX_REGISTER {
                    // let lhs = self.close_type(base[&r].clone());
                    // let closed = self.close_type(poly[&r].clone());
                    // let rhs = match &closed{
                    //     Ty::TyVar(v) => Ty::UnifVar(*v),
                    //     _ => closed,
                    // };
                    let lhs = self.chase_type(base[&r].clone());
                    let rhs = self.chase_type(poly[&r].clone());
                    constraints.push((lhs, rhs))
                }

                println!("[??] Starting nested unifier {:?}", constraints);
                let mut nested = Unifier { 
                    constraints: constraints.into(),
                    // empty
                    lp_constraints: Vec::new(),
                    variables: self.variables,
                    mapping: self.mapping.clone(),
                    rho_mappings: self.rho_mappings.clone(),
                };
                nested.unify()?;

                for (var, ty) in &nested.mapping {
                    if !self.mapping.contains_key(var) {
                        println!("[??] Updating {} = {}", var, ty);
                        self.mapping.insert(*var, ty.clone());
                    }
                }
                println!("[??] done")
            }
        }
        Ok(())
    }

    // chase the given type under the current mapping
    pub fn chase_type(&self, ty: Ty) -> Ty {
        let mut var = ty.clone();
        match &var {
            Ty::TyVar(_) => (),
            Ty::Int => (),
            Ty::Code(f) => {
                let mut roots = HashMap::new();
                for r in 1..=MAX_REGISTER {
                    roots.insert(r, self.chase_type(f[&r].clone()));
                }
                var = Ty::Code(roots);
            }
            Ty::UnifVar(v) => {
                if self.mapping.contains_key(&v) {
                    var = self.chase_type(self.mapping[&v].clone());
                } 
            }
            Ty::Ptr(set, rho) | Ty::UniqPtr(set, rho) => {
                let mut roots = HashMap::new();
                for idx in set.keys() {
                    roots.insert(*idx, self.chase_type(set[idx].clone()));
                }
                var = match &var {
                    Ty::Ptr(_, _) => Ty::Ptr(roots, rho.clone()),
                    Ty::UniqPtr(_, _) => Ty::UniqPtr(roots, rho.clone()),
                    _ => panic!("unreachable"),
                }
            }
        }
        println!("(+) Chased {} to {}", ty, var);
        var
    }

    // mutably close this unifiers mapping
    pub fn close_mapping(&mut self) {
        // Close all unifvars
        for v in 1..=self.variables {
            if self.mapping.contains_key(&v) {
                let ty = self.chase_type(self.mapping[&v].clone());
                self.mapping.insert(v, ty);
            } else {
                panic!("future lily will figure this out");
            }
        }

        // Close all the rhos
        let mut new_rho_mappings = HashMap::new();
        for rho_id in self.rho_mappings.keys() {
            let mut new_mapping = self.rho_mappings.get(rho_id).unwrap().clone();
            for (idx, entry) in &self.rho_mappings[rho_id] {
                if let RhoEntry::Contains(ty) = entry {
                    let closed = self.chase_type(ty.clone());
                    new_mapping.insert(*idx, RhoEntry::Contains(closed));
                }
            }
            new_rho_mappings.insert(*rho_id, new_mapping);
        }
        self.rho_mappings = new_rho_mappings;
    }

    fn get_free(lp_constraints: &Vec<(CodeTy, Vec<CodeTy>)>) -> Vec<usize> {
        let mut frees = Vec::new();
        for (_, polys) in lp_constraints {
            for poly in polys {
                for r in 1..=MAX_REGISTER {
                    if let Ty::UnifVar(v) = poly[&r] {
                        frees.push(v);
                    }
                }
            } 
        }
        println!("Frees {:?}", frees);
        frees
    }

    pub fn new(
        constraints: Vec<(Ty, Ty)>,
        lp_constraints: Vec<(CodeTy, Vec<CodeTy>)>,
        variables: usize,
    ) -> Unifier {
        Unifier {
            constraints: constraints.into(),
            lp_constraints: lp_constraints.into(),
            variables,
            mapping: HashMap::new(),
            rho_mappings: HashMap::new(),
        }
    }
}
