use std::collections::HashMap;

use crate::{checker::*, unify::Unifier};

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
