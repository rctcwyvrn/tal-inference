use std::{collections::HashMap, fmt::Display};

use crate::{checker::*, unify::{Unifier, RhoEntry}};


impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ty::Int => write!(f, "Int"),
            Ty::Code(code) => write!(f, "Code({})", sort_for_print(code)),
            Ty::UnifVar(id) => write!(f, "UnifVar({})", *id),
            Ty::TyVar(id) => write!(f, "TyVar({})", *id),
            Ty::Ptr(set, rho) => write!(f, "Ptr<{}, rho = {:?}>", sort_for_print(set), rho),
            Ty::UniqPtr(set, rho) => write!(f, "Uptr<{}, rho = {:?}>", sort_for_print(set), rho),
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

impl Display for RhoEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RhoEntry::Contains(ty) => write!(f, "{}", ty),
            RhoEntry::Absent => write!(f, "Absent"),
        }
    }
}

pub fn sort_for_print<K,V>(map: &HashMap<K, V>) -> PrintableVec<(K,V)> where K: Ord+Clone, V: Clone {
    let mut v: Vec<(K,V)> = map.clone().into_iter().collect();
    v.sort_by(|(lk, _), (rk, _)| lk.cmp(rk));
    v.into()
}

impl<A: Display, B: Display> Display for PrintableVec<(A,B)> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,"{{")?;
        for item in &self.0 {
            write!(f, "{}:{} ", item.0, item.1)?;
        }
        write!(f,"}}")
    }
}

impl Checker {
    pub fn pretty_heap_w_mapping(&self, mapping: &HashMap<usize, Ty>) {
        println!("--- Heap w/ mapping ---");
        for label in self.heap_types.keys() {
            println!("{} => ", label);
            if let Ty::Code(f) = &self.heap_types[label] {
                for r in 1..=MAX_REGISTER {
                    match f[&r].clone() {
                        Ty::UnifVar(x) => println!("  {}: {:?}", r, mapping[&x]),
                        _ => println!("  {}: {:?}", r, f[&r]),
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
                    println!("  {}: {:?}", r, f[&r])
                }
            }
        }
    }
}
