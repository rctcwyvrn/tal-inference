use std::{collections::HashMap, fmt::Display};

use crate::{
    checker::*,
    unify::{RhoEntry, Unifier},
};

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ty::Int => write!(f, "Int"),
            Ty::Code(code) => write!(f, "Code({})", sort_for_print(code)),
            Ty::UnifVar(id) => write!(f, "UnifVar({})", *id),
            Ty::Ptr(set, rho) => write!(f, "Ptr<{}, rho = {:?}>", sort_for_print(set), rho),
            Ty::UniqPtr(set, rho) => write!(f, "Uptr<{}, rho = {:?}>", sort_for_print(set), rho),
        }
    }
}

impl Display for TyU {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TyU::Int => write!(f, "Int"),
            TyU::Code(code) => write!(f, "Code({})", sort_for_print(code)),
            TyU::Ptr(set, rho) => write!(f, "Ptr<{}, rho = {:?}>", sort_for_print(set), rho),
            TyU::UniqPtr(set, rho) => write!(f, "Uptr<{}, rho = {:?}>", sort_for_print(set), rho),
            TyU::Any => write!(f, "Any"),
        }
    }
}

#[derive(Debug)]
pub struct PrintableVec<T>(Vec<T>);

impl<T> From<Vec<T>> for PrintableVec<T> {
    fn from(x: Vec<T>) -> Self {
        PrintableVec(x)
    }
}

impl<T: Display> Display for RhoEntry<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RhoEntry::Contains(ty) => write!(f, "{}", ty),
            RhoEntry::Absent => write!(f, "Absent"),
        }
    }
}

pub fn sort_for_print<K, V>(map: &HashMap<K, V>) -> PrintableVec<(K, V)>
where
    K: Ord + Clone,
    V: Clone,
{
    let mut v: Vec<(K, V)> = map.clone().into_iter().collect();
    v.sort_by(|(lk, _), (rk, _)| lk.cmp(rk));
    v.into()
}

impl<A: Display, B: Display> Display for PrintableVec<(A, B)> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        for item in &self.0 {
            write!(f, "{}:{} ", item.0, item.1)?;
        }
        write!(f, "}}")
    }
}

impl Checker {
    pub fn pretty_heap_w_mapping<T: Display>(&self, mapping: &HashMap<usize, T>) {
        println!("--- Heap w/ mapping ---");
        for label in self.heap_types.keys() {
            println!("{} => ", label);
            if let Ty::Code(f) = &self.heap_types[label] {
                for r in 1..=MAX_REGISTER {
                    match f[&r].clone() {
                        Ty::UnifVar(x) => println!("  {}: {}", r, mapping[&x]),
                        _ => println!("  {}: {}", r, f[&r]),
                    }
                }
            } else {
                panic!("wat")
            }
        }
    }

    pub fn pretty_heap(&self) {
        println!("--- Heap ---");
        for label in self.heap_types.keys() {
            println!("{} => ", label);
            if let Ty::Code(f) = &self.heap_types[label] {
                for r in 1..=MAX_REGISTER {
                    println!("  {}: {}", r, f[&r])
                }
            }
        }
    }
}
