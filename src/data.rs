use std::collections::HashMap;

use crate::syntax::*;

struct State {
    registers: HashMap<Register, WordValue>,
    heap: HashMap<usize, HeapValue>,
}
