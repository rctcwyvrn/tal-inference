use std::fmt::Display;

pub type Register = usize;
pub type Label = String;
pub type Block = (String, Vec<Instruction>, Terminal);
pub type Program = Vec<Block>;

// note: can't be copy because of the string in label
#[derive(Debug, Clone)]
pub enum Value {
    Register(Register),
    Word(WordValue),
}

#[derive(Debug, Clone)]
pub enum WordValue {
    Integer(i64),
    Label(Label),
}

#[derive(Debug)]
pub enum HeapValue {
    Product(Vec<WordValue>),
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Arith(Op, Register, Register, Value),
    BranchNonZero(Register, Value),
    Load(Register, Register, i64),
    Store(Register, i64, Register),
    Malloc(Register),
    Mov(Register, Value),
}

#[derive(Debug, Clone)]
pub enum Terminal {
    Jump(Value),
    Halt,
}

#[derive(Debug, Clone, Copy)]
pub enum Op {
    Add,
    Mul,
    Sub,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Register(r) => write!(f, "r_{}", *r),
            Value::Word(w) => write!(f, "{}", w),
        }
    }
}

impl Display for WordValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            WordValue::Integer(int) => write!(f, "{}", int),
            WordValue::Label(label) => write!(f, "label<{}>", label),
        }
    }
}
