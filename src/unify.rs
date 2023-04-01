use core::panic;
use std::{
    collections::{HashMap, VecDeque},
    fmt::Display,
};

use crate::{checker::*, debug::sort_for_print};

#[derive(Debug, Clone, PartialEq)]
pub enum RhoEntry<T> {
    Contains(T),
    Absent,
}

pub type RhoMapping<T> = HashMap<i64, RhoEntry<T>>;

pub struct Unifier {
    constraints: VecDeque<(Ty, Ty)>,
    satisfy: Vec<(CodeTy, CodeTy)>,
    variables: usize,
    pub mapping: HashMap<usize, Ty>,
    pub rho_mappings: HashMap<usize, RhoMapping<Ty>>,
}

pub struct Satisfier {
    pub mapping: HashMap<usize, TyU>,
    pub rho_mappings: HashMap<usize, RhoMapping<TyU>>,
    pub satisfy: Vec<(CodeTyU, CodeTyU)>,
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

    fn convert<T: Clone + Display>(ptr_mapping: HashMap<i64, T>) -> RhoMapping<T> {
        ptr_mapping
            .into_iter()
            .map(|(idx, ty)| (idx, RhoEntry::Contains(ty)))
            .collect()
    }

    fn add<T: Clone + Display>(
        set: RhoMapping<T>,
        rho: RhoMapping<T>,
    ) -> Result<RhoMapping<T>, TypeError> {
        println!(
            "  (~) Computing X + Y for X = {} | Y = {}",
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
        println!("  (~) X + Y = {}", sort_for_print(&result));
        Ok(result)
    }

    fn unify_rho_mappings(
        &mut self,
        l: RhoMapping<Ty>,
        r: RhoMapping<Ty>,
    ) -> Result<(), TypeError> {
        for key in l.keys() {
            if r.contains_key(key) {
                self.unify_rho_entries(l[key].clone(), r[key].clone())?;
            } else {
                return Err(TypeError::RhoConflict);
            }
        }
        Ok(())
    }

    fn unify_rho_entries(&mut self, l: RhoEntry<Ty>, r: RhoEntry<Ty>) -> Result<(), TypeError> {
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
        l_set: RhoMapping<Ty>,
        r_set: RhoMapping<Ty>,
    ) -> Result<RhoMapping<Ty>, TypeError> {
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

    fn unify_rho_id_with_mapping(
        &mut self,
        rho: Rho,
        set: RhoMapping<Ty>,
    ) -> Result<(), TypeError> {
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

    // Ty -> TyU
    // UnifVars that were not bound by any restriction are set to TyU::Any
    pub fn chase_to_root(&self, ty: Ty) -> TyU {
        let closed: TyU;
        match &ty {
            Ty::Int => closed = TyU::Int,
            Ty::Code(f) => {
                let mut roots = HashMap::new();
                for r in 1..=MAX_REGISTER {
                    roots.insert(r, self.chase_to_root(f[&r].clone()));
                }
                closed = TyU::Code(roots);
            }
            Ty::UnifVar(v) => {
                if self.mapping.contains_key(&v) {
                    closed = self.chase_to_root(self.mapping[&v].clone());
                } else {
                    closed = TyU::Any;
                }
            }
            Ty::Ptr(set, rho) | Ty::UniqPtr(set, rho) => {
                let mut roots = HashMap::new();
                for idx in set.keys() {
                    roots.insert(*idx, self.chase_to_root(set[idx].clone()));
                }
                closed = match &ty {
                    Ty::Ptr(_, _) => TyU::Ptr(roots, rho.clone()),
                    Ty::UniqPtr(_, _) => TyU::UniqPtr(roots, rho.clone()),
                    _ => panic!("unreachable"),
                }
            }
        }
        closed
    }

    // Close the mapping
    pub fn chase_all_to_root(&mut self) -> Satisfier {
        let mut closed_mapping = HashMap::new();

        // Close all unifvars
        for v in 1..=self.variables {
            if self.mapping.contains_key(&v) {
                let ty = self.chase_to_root(self.mapping[&v].clone());
                closed_mapping.insert(v, ty);
            } else {
                closed_mapping.insert(v, TyU::Any);
            }
        }

        // Close all the rhos
        let mut new_rho_mappings = HashMap::new();
        for rho_id in self.rho_mappings.keys() {
            // let mut new_mapping = self.rho_mappings.get(rho_id).unwrap().clone();
            let mut new_mapping = HashMap::new();
            for (idx, entry) in &self.rho_mappings[rho_id] {
                if let RhoEntry::Contains(ty) = entry {
                    let closed = self.chase_to_root(ty.clone());
                    new_mapping.insert(*idx, RhoEntry::Contains(closed));
                } else {
                    new_mapping.insert(*idx, RhoEntry::Absent);
                }
            }
            new_rho_mappings.insert(*rho_id, new_mapping);
        }
        let closed_rho_mappings = new_rho_mappings;

        // Close everything in satisfy
        let mut new_satisfy = Vec::new();
        for (code, gamma) in &self.satisfy {
            let closed_code = code
                .iter()
                .map(|(k, v)| (*k, self.chase_to_root(v.clone())))
                .collect();
            let closed_gamma = gamma
                .iter()
                .map(|(k, v)| (*k, self.chase_to_root(v.clone())))
                .collect();
            new_satisfy.push((closed_code, closed_gamma));
        }
        let closed_satisfy = new_satisfy;
        Satisfier {
            mapping: closed_mapping,
            rho_mappings: closed_rho_mappings,
            satisfy: closed_satisfy,
        }
    }

    pub fn new(
        constraints: Vec<(Ty, Ty)>,
        satisfy: Vec<(CodeTy, CodeTy)>,
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

impl Satisfier {
    fn satisfy_rho(
        &mut self,
        eqs: &mut VecDeque<(TyU, TyU)>,
        additional_rhos: &mut HashMap<usize, HashMap<i64, RhoEntry<TyU>>>,
        given_set: HashMap<i64, TyU>,
        given_rho_id: Option<Rho>,
        require_set: HashMap<i64, TyU>,
        require_rho_id: Option<Rho>,
    ) -> Result<(), TypeError> {
        println!(
            "  (++) Attempting to substitute ({:?}, {:?}) for ({:?}, {:?})",
            given_set, given_rho_id, require_set, require_rho_id,
        );

        // Convert the input l_rho into a concrete mapping
        let given_rho = if let Some(id) = given_rho_id {
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

        let given = Unifier::add(given_rho, Unifier::convert(given_set))?;
        let requirement_has_bound_rho = if let Some(id) = require_rho_id {
            self.rho_mappings.contains_key(&id.0)
        } else {
            true // rho = {}
        };

        if requirement_has_bound_rho {
            // case: requirement rho is bound
            // check for equality
            //   given_set + given_rho = require_set + require_rho
            let require_rho = if let Some(id) = require_rho_id {
                self.rho_mappings[&id.0].clone()
            } else {
                HashMap::new()
            };
            let require = Unifier::add(require_rho, Unifier::convert(require_set))?;
            if given != require {
                return Err(TypeError::FailedJumpOnRho);
            }
        } else {
            // case: requirement does not have a set rho
            // so it can be filled with any missing fields, so just check that the everything in require_set is in given
            // check that given_set + given_rho <: require_set
            let require = Unifier::convert(require_set);
            println!(
                "  (++) solving for rho {} <: {} + rho({})",
                sort_for_print(&given),
                sort_for_print(&require),
                require_rho_id.unwrap().0
            );
            for key in require.keys() {
                if !given.contains_key(key) {
                    return Err(TypeError::FailedJumpOnRho);
                }
                match (given[key].clone(), require[key].clone()) {
                    (RhoEntry::Contains(g), RhoEntry::Contains(r)) => eqs.push_back((g, r)),
                    (RhoEntry::Absent, RhoEntry::Absent) => continue,
                    (RhoEntry::Contains(_), RhoEntry::Absent)
                    | (RhoEntry::Absent, RhoEntry::Contains(_)) => {
                        return Err(TypeError::FailedJumpOnRho)
                    }
                }
            }
            // fill in the new rho (contains any fields missing from require_set)
            let mut rho_ty = HashMap::new();
            for key in given.keys() {
                if !require.contains_key(key) {
                    rho_ty.insert(*key, given[key].clone());
                }
            }
            // todo: do we need to store the rho somewhere?
            // is it possible to have multiple rhos referring to each other in the signature???
            // just store it for now and we'll see
            additional_rhos.insert(require_rho_id.unwrap().0, rho_ty);
        }
        Ok(())
    }

    fn satisfy_jump(
        &mut self,
        code: CodeTyU,
        gamma: CodeTyU,
    ) -> Result<HashMap<usize, RhoMapping<TyU>>, TypeError> {
        let mut additional_rhos = HashMap::new();
        let mut eqs = VecDeque::new();
        for r in 1..=MAX_REGISTER {
            eqs.push_back((gamma[&r].clone(), code[&r].clone()))
        }
        while !eqs.is_empty() {
            let (given, require) = eqs.pop_front().unwrap();
            println!("[x] {} <: {}", given, require);
            // left = gamma, ie what we have
            // right = code, ie what the function expects
            match (given.clone(), require.clone()) {
                // types match, done
                (s, t) if s == t => {}
                // function allows anything
                (_, TyU::Any) => {}
                // function expects a label, we have a label
                (TyU::Code(fn_a), TyU::Code(fn_b)) => {
                    // the function wants a fn_a, we want to give it a fn_b
                    // the function is able to at least set a context that satisfies fn_a, so check if that satisfies fn_b
                    let code = fn_b.clone();
                    let gamma = fn_a.clone();
                    self.satisfy_jump(code, gamma)?;
                }
                // insert case for ptrs
                // https://ahnfelt.medium.com/row-polymorphism-crash-course-587f1e7b7c47
                // do the switcheroo to attempt to solve for the rhos
                (TyU::UniqPtr(g_set, g_rho), TyU::UniqPtr(r_set, r_rho))
                | (TyU::UniqPtr(g_set, g_rho), TyU::Ptr(r_set, r_rho))
                | (TyU::Ptr(g_set, g_rho), TyU::Ptr(r_set, r_rho)) => {
                    self.satisfy_rho(&mut eqs, &mut additional_rhos, g_set, g_rho, r_set, r_rho)?;
                }
                _ => {
                    println!("> Failed satisfy_jump on: {} <: {}", given, require);
                    return Err(TypeError::FailedJump);
                }
            }
        }
        Ok(additional_rhos)
    }

    pub fn satisfy(&mut self) -> Result<(), TypeError> {
        println!("--- Starting satisfy ---");
        // Reminder:
        // The parameter we're trying to pass it <: what the function is expecting
        for (code, gamma) in self.satisfy.clone() {
            // debugging
            println!(" x-- jump start --x");
            let rho_mappings = self.satisfy_jump(code, gamma)?;

            // debugging
            print!("rhos: [");
            for rho_id in rho_mappings.keys() {
                print!(
                    "rho({})={}, ",
                    rho_id,
                    sort_for_print(&rho_mappings[rho_id])
                )
            }
            println!("]");

            println!(" x-- jump done --x");
        }
        Ok(())
    }
}
