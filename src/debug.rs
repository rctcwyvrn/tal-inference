use std::collections::HashMap;

use crate::checker::*;

impl Checker {
    pub fn pretty_heap_w_mapping(&self, mapping: HashMap<usize, Ty>) {
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
