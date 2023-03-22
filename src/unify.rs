use core::panic;
use std::collections::{HashMap, VecDeque};

use crate::{checker::*, debug::sort_for_print};

#[derive(Debug, Clone, PartialEq)]
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
                if l_set != r_set {
                    return Err(TypeError::PtrConflict);
                }
            }
            (None, Some(rhs_rho)) => {
                let expect = self.unify_subtract(l_set, r_set)?;
                self.unify_rho(rhs_rho, expect)?
            }
            (Some(lhs_rho), None) => {
                let expect = self.unify_subtract(r_set, l_set)?;
                self.unify_rho(lhs_rho, expect)?
            }
            (Some(lhs_rho), Some(r_known)) if self.rho_mappings.contains_key(&r_known.0) => {
                let r_rho = self.rho_mappings[&r_known.0].clone();
                let rhs = Unifier::add(r_set, r_rho)?;
                let expect = self.unify_subtract(rhs, l_set)?;
                self.unify_rho(lhs_rho, expect)?
            }
            (Some(l_known), Some(rhs_rho)) if self.rho_mappings.contains_key(&l_known.0) => {
                let l_rho = self.rho_mappings[&l_known.0].clone();
                let lhs = Unifier::add(l_set, l_rho)?;
                let expect = self.unify_subtract(lhs, r_set)?;
                self.unify_rho(rhs_rho, expect)?
            }
            _ => {
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

    fn unify_rho(&mut self, rho: Rho, set: RhoMapping) -> Result<(), TypeError> {
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
            for key in rho.keys() {
                if set.contains_key(key) {
                    self.unify_rho_entries(set[key].clone(), rho[key].clone())?;
                } else {
                    return Err(TypeError::RhoConflict);
                }
            }
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
                (t, Ty::UnifVar(x)) | (Ty::UnifVar(x), t) => {
                    Unifier::substitute(&mut self.mapping, &mut self.constraints, Ty::UnifVar(x), t)
                }
                (Ty::Code(fn_a), Ty::Code(fn_b)) => {
                    for r in 1..=MAX_REGISTER {
                        self.constrain(fn_a[&r].clone(), fn_b[&r].clone());
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

    fn satisfy_rho(
        &mut self,
        eqs: &mut VecDeque<(Ty, Ty)>,
        additional_rhos: &mut HashMap<usize, HashMap<i64, RhoEntry>>,
        l_set: HashMap<i64, Ty>,
        l_rho: Option<Rho>,
        r_set: HashMap<i64, Ty>,
        r_rho_id: Option<Rho>,
    ) -> Result<(), TypeError> {
        println!(
            "\n(++) Attempting to substitute ({:?}, {:?}) for ({:?}, {:?})",
            l_set, l_rho, r_set, r_rho_id
        );

        // Convert the input l_rho into a concrete mapping
        let l_rho = if let Some(id) = l_rho {
            if self.rho_mappings.contains_key(&id.0) {
                self.rho_mappings[&id.0].clone()
            } else {
                // make no assumptions about an unbound l_rho
                HashMap::new()
            }
        } else {
            // no l_rho
            HashMap::new()
        };

        let lhs = Unifier::add(l_rho, Unifier::convert(l_set))?;
        let r_rho_bound = if let Some(id) = r_rho_id {
            self.rho_mappings.contains_key(&id.0)
        } else {
            true
        };

        if r_rho_bound {
            // case: right rho is bound
            // check that l_set + l_rho = r_set + r_rho
            let r_rho = if let Some(id) = r_rho_id {
                self.rho_mappings[&id.0].clone()
            } else {
                HashMap::new()
            };
            let rhs = Unifier::add(r_rho, Unifier::convert(r_set))?;
            if lhs != rhs {
                return Err(TypeError::FailedJumpOnRho);
            }
        } else {
            // case: right rho is not bound
            // check that l_set + l_rho <: r_set
            let rhs = Unifier::convert(r_set);
            println!(
                "(++) solving for rho {} <: {} + rho({})",
                sort_for_print(&lhs),
                sort_for_print(&rhs),
                r_rho_id.unwrap().0
            );
            for key in rhs.keys() {
                if !lhs.contains_key(key) {
                    return Err(TypeError::FailedJumpOnRho);
                }
                match (lhs[key].clone(), rhs[key].clone()) {
                    (RhoEntry::Contains(l), RhoEntry::Contains(r)) => eqs.push_back((l, r)),
                    (RhoEntry::Absent, RhoEntry::Absent) => continue,
                    (RhoEntry::Contains(_), RhoEntry::Absent)
                    | (RhoEntry::Absent, RhoEntry::Contains(_)) => {
                        return Err(TypeError::FailedJumpOnRho)
                    }
                }
            }
            let mut rho_ty = HashMap::new();
            for key in lhs.keys() {
                if !rhs.contains_key(key) {
                    rho_ty.insert(*key, lhs[key].clone());
                }
            }
            // todo: do we need to store the rho somewhere?
            // is it possible to have multiple rhos referring to each other in the signature???
            additional_rhos.insert(r_rho_id.unwrap().0, rho_ty);
        }
        Ok(())
    }

    fn satisfy_jump(
        &mut self,
        jump: VecDeque<(Ty, Ty)>,
    ) -> Result<(HashMap<usize, Ty>, HashMap<usize, RhoMapping>), TypeError> {
        let mut mapping = HashMap::new();
        let mut additional_rhos = HashMap::new();
        let mut eqs = jump;
        while !eqs.is_empty() {
            let next = eqs.pop_front().unwrap();
            match next {
                // types match, done
                (s, t) if s == t => {}
                // function expects a generic parameter, so we can freely substitute
                (t, Ty::TyVar(x)) => Unifier::substitute(&mut mapping, &mut eqs, Ty::TyVar(x), t),
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
                        eqs.push_back((fn_b[&r].clone(), fn_a[&r].clone()));
                    }
                }
                // insert case for ptrs
                // https://ahnfelt.medium.com/row-polymorphism-crash-course-587f1e7b7c47
                // do the switcheroo to attempt to solve for the rhos
                (Ty::UniqPtr(l_set, l_rho), Ty::UniqPtr(r_set, r_rho))
                | (Ty::UniqPtr(l_set, l_rho), Ty::Ptr(r_set, r_rho))
                | (Ty::Ptr(l_set, l_rho), Ty::Ptr(r_set, r_rho)) => {
                    self.satisfy_rho(&mut eqs, &mut additional_rhos, l_set, l_rho, r_set, r_rho)?;
                }
                _ => {
                    println!("> Failed satisfy_jump on: {} = {}", next.0, next.1);
                    return Err(TypeError::FailedJump);
                }
            }
        }
        Ok((mapping, additional_rhos))
    }

    pub fn satisfy(&mut self) -> Result<(), TypeError> {
        println!("--- Satisfying jumps ---");
        println!("(The parameter we're trying to pass it <: what the function is expecting)");
        for jump in self.satisfy.clone() {
            // debugging
            for (lhs, rhs) in &jump {
                println!("{:?} <: {:?}", lhs, rhs);
            }

            let (mapping, rho_mappings) = self.satisfy_jump(jump.into())?;

            // debugging
            print!("typevars: [");
            for (lhs, rhs) in mapping.iter() {
                print!("TyVar({})={:?}, ", lhs, rhs)
            }
            println!("]");

            print!("rhos: [");
            for rho_id in rho_mappings.keys() {
                print!(
                    "rho({})={}, ",
                    rho_id,
                    sort_for_print(&rho_mappings[rho_id])
                )
            }
            println!("]");

            println!("---");
        }
        Ok(())
    }

    // returns a non unifVar type, either lifting it to a typevar or finding it within the mapping
    pub fn chase_to_root(&self, ty: Ty) -> Ty {
        let mut var = ty;
        match &var {
            Ty::TyVar(_) => (),
            Ty::Int => (),
            Ty::Code(f) => {
                let mut roots = HashMap::new();
                for r in 1..=MAX_REGISTER {
                    roots.insert(r, self.chase_to_root(f[&r].clone()));
                }
                var = Ty::Code(roots);
            }
            Ty::UnifVar(v) => {
                if self.mapping.contains_key(&v) {
                    var = self.chase_to_root(self.mapping[&v].clone());
                } else {
                    var = Ty::TyVar(v.clone());
                }
            }
            Ty::Ptr(set, rho) | Ty::UniqPtr(set, rho) => {
                let mut roots = HashMap::new();
                for idx in set.keys() {
                    roots.insert(*idx, self.chase_to_root(set[idx].clone()));
                }
                var = match &var {
                    Ty::Ptr(_, _) => Ty::Ptr(roots, rho.clone()),
                    Ty::UniqPtr(_, _) => Ty::UniqPtr(roots, rho.clone()),
                    _ => panic!("unreachable"),
                }
            }
        }
        var
    }

    // mutably close this unifiers mapping
    pub fn chase_all_to_root(&mut self) {
        // Close all unifvars
        for v in 1..=self.variables {
            if self.mapping.contains_key(&v) {
                let ty = self.chase_to_root(self.mapping[&v].clone());
                self.mapping.insert(v, ty);
            } else {
                self.mapping.insert(v, Ty::TyVar(v));
            }
        }

        // Close all the rhos
        let mut new_rho_mappings = HashMap::new();
        for rho_id in self.rho_mappings.keys() {
            let mut new_mapping = self.rho_mappings.get(rho_id).unwrap().clone();
            for (idx, entry) in &self.rho_mappings[rho_id] {
                if let RhoEntry::Contains(ty) = entry {
                    let closed = self.chase_to_root(ty.clone());
                    new_mapping.insert(*idx, RhoEntry::Contains(closed));
                }
            }
            new_rho_mappings.insert(*rho_id, new_mapping);
        }
        self.rho_mappings = new_rho_mappings;

        // Close everything in satisfy
        let mut new_satisfy = Vec::new();
        for jump in &self.satisfy {
            let closed_jump: Vec<(Ty, Ty)> = jump
                .iter()
                .map(|(lhs, rhs)| {
                    (
                        self.chase_to_root(lhs.clone()),
                        self.chase_to_root(rhs.clone()),
                    )
                })
                .collect();
            new_satisfy.push(closed_jump);
        }
        self.satisfy = new_satisfy.into();
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
