use crate::syntax::*;

pub fn make_add_program() -> Program {
    // mov r1 1
    // add r2 r1 5
    // typechecker should assign types
    // - r1: Integer
    // - r2: Integer
    vec![(
        "main".to_owned(),
        vec![
            Instruction::Mov(1, Value::Word(WordValue::Integer(1))),
            Instruction::Arith(Op::Add, 2, 1, Value::Word(WordValue::Integer(5))),
        ],
        Terminal::Halt,
    )]
}

pub fn make_jump_program() -> Program {
    // todo: write an example program using jumps
    vec![
        (
            "entry".to_owned(),
            vec![
                Instruction::Mov(1, Value::Word(WordValue::Integer(1))),
                Instruction::Mov(3, Value::Word(WordValue::Integer(1))),
                Instruction::BranchNonZero(3, Value::Word(WordValue::Label("first".to_owned()))),
            ],
            Terminal::Jump(Value::Word(WordValue::Label("second".to_owned()))),
        ),
        // both first and second should require that their r1 is an integer
        (
            "first".to_owned(),
            vec![Instruction::Arith(
                Op::Add,
                2,
                1,
                Value::Word(WordValue::Integer(5)),
            )],
            Terminal::Halt,
        ),
        (
            "second".to_owned(),
            vec![Instruction::Arith(
                Op::Sub,
                2,
                1,
                Value::Word(WordValue::Integer(5)),
            )],
            Terminal::Halt,
        ),
    ]
}

pub fn make_indirect_jump_program() -> Program {
    // todo: write an example program using conditional jumps to different labels
    // ie
    // bnz r1 L.case2
    // mov r2 L.doA
    // jmp L.go
    // L.case2:
    //   mov r2 L.doB
    // L.go:
    //   mov r3 1
    //   ... other setup
    //   jmp r2
    vec![
        // expected type: all three registers generic
        (
            "entry".to_owned(),
            vec![
                Instruction::Mov(1, Value::Word(WordValue::Integer(1))),
                // check that case2's function type can satisfy r1: Int
                Instruction::BranchNonZero(1, Value::Word(WordValue::Label("case2".to_owned()))),
                // case 1
                Instruction::Mov(2, Value::Word(WordValue::Label("doA".to_owned()))),
            ],
            // check that doA in r2 satisfies the type of r2 in go's code type
            // so there exists some instantiation of doA's type vars so that
            // it equals code<r1: int, r2: var, r3: int>
            Terminal::Jump(Value::Word(WordValue::Label("go".to_owned()))),
        ),
        // expectd type: all three registers generic
        (
            "case2".to_owned(),
            vec![
                // case 2
                Instruction::Mov(2, Value::Word(WordValue::Label("doB".to_owned()))),
            ],
            Terminal::Jump(Value::Word(WordValue::Label("go".to_owned()))),
        ), // check that go's r2 can satisfy doA
        // expected type: registers 1 and 3 generic
        // 2 should have type code<r1: int, r2: var, r3: int>
        // it could also have types
        // - code<r1: var, r2: var, r3: int>
        // - code<r1: int, r2: var, r3: var>
        // - etc.

        // ideally: \forall k : code<r1: int, r2: code<r1: int, r2: k, r3: int>, r3: int>

        // so how do we know which one to pick? -> pick the most generic one?
        // the type that accepts the most values, the most generic one is actually
        // - r2: code<r1: int, r2: var, r3: int>
        //   - r2 allows functions that may require r1 and r3 set
        // - not r2: \forall a,b,c code<r1: a, r2: b, r3: c>
        //   - r2 only allows functions that have no requirement on their functions
        //   - would not accept doA or doB
        (
            "go".to_owned(),
            vec![
                Instruction::Mov(1, Value::Word(WordValue::Integer(1))),
                Instruction::Mov(3, Value::Word(WordValue::Integer(1))),
            ],
            // constrain r2 to a code type
            // check that r2's code type can satisfy gamma
            Terminal::Jump(Value::Register(2)),
        ),
        // expected type: r1 and r3 ints, r2 generic
        (
            "doA".to_owned(),
            vec![Instruction::Arith(Op::Add, 1, 1, Value::Register(3))],
            Terminal::Halt,
        ),
        // expected type: r1 and r3 ints, r2 generic
        (
            "doB".to_owned(),
            vec![Instruction::Arith(Op::Sub, 1, 1, Value::Register(3))],
            Terminal::Halt,
        ),
    ]

    // entry: code<r1: a, r2: b, r3:c>
    //   mov r1 1 : gamma = <r1: int, r2: b, r3:c>
    //   bnz case2[c=int, d=b, e=c]
    //   mov r2 doA : gamma = <r1: int, r2: code<r1: int, r2: k, r3: int>, r3:c>
    //   jump go[f=int, g=int, h=]

    // case2: code<r1: c, r2: d, r3:e>
    //
    // go: code<r1: f, r2: code<r1: g, r2: h, r3:i>, r3:j>
    //
    // doA: code<r1: int, r2: k, r3: int>
    //
    // doB: code<r1: int, r2: l, r3: int>
    //

    // subtyping
    //
    // entry: code<>
    //   mov r1 1 : gamma = <r1: int>
    //   bnz case2 (check subset)
    //   mov r2 doA : gamma = <r1: int, r2: code<r1: int, r3: int>>
    //   jump go

    // case2: code<>
    //
    // go: code<r2: code<r1: int, r3: int>>
    //
    // doA: code<r1: int, r3: int>
    //
    // doB: code<r1: int, r3: int>
    //
}

pub fn ind_jump_wrong_registers() -> Program {
    vec![
        (
            "entry".to_owned(),
            vec![
                Instruction::Mov(1, Value::Word(WordValue::Integer(1))),
                Instruction::BranchNonZero(1, Value::Word(WordValue::Label("case2".to_owned()))),
                Instruction::Mov(2, Value::Word(WordValue::Label("doA".to_owned()))),
            ],
            Terminal::Jump(Value::Word(WordValue::Label("go".to_owned()))),
        ),
        (
            "case2".to_owned(),
            vec![Instruction::Mov(
                2,
                Value::Word(WordValue::Label("doB".to_owned())),
            )],
            Terminal::Jump(Value::Word(WordValue::Label("go".to_owned()))),
        ),
        (
            "go".to_owned(),
            vec![
                Instruction::Mov(1, Value::Word(WordValue::Integer(1))),
                Instruction::Mov(3, Value::Word(WordValue::Integer(1))),
            ],
            Terminal::Jump(Value::Register(2)),
        ),
        (
            "doA".to_owned(),
            vec![Instruction::Arith(Op::Add, 1, 1, Value::Register(2))],
            Terminal::Halt,
        ),
        (
            "doB".to_owned(),
            vec![Instruction::Arith(Op::Sub, 1, 1, Value::Register(3))],
            Terminal::Halt,
        ),
    ]
}

pub fn ind_jump_forgetting() -> Program {
    vec![
        (
            "entry".to_owned(),
            vec![Instruction::Mov(
                1,
                Value::Word(WordValue::Label("poly_halt".to_owned())),
            )],
            Terminal::Jump(Value::Word(WordValue::Label("indirect_jump".to_owned()))),
        ),
        (
            "indirect_jump".to_owned(),
            vec![
                // indirect jump sets r2 to int here
                Instruction::Mov(2, Value::Word(WordValue::Integer(99))),
            ],
            // so by my rules, this jump expects a r1 to contain a code label which expects r2 to be an int
            // so we cant jump to poly_halt from here

            // essentially, we cant forget information on indirect jumps
            // the register set in a block with an indirect jump end up being important
            // eugh
            Terminal::Jump(Value::Register(1)),
        ),
        ("poly_halt".to_owned(), vec![], Terminal::Halt),
    ]
}

pub fn invalid_ind_jump_requirements() -> Program {
    vec![
        (
            "entry".to_owned(),
            vec![Instruction::Mov(
                1,
                Value::Word(WordValue::Label("end".to_owned())),
            )],
            // try to enter indirect_jump with a label in r1 that has too many requirements
            // should fail
            Terminal::Jump(Value::Word(WordValue::Label("indirect_jump".to_owned()))),
        ),
        (
            "indirect_jump".to_owned(),
            vec![],
            // can only jump to functions with no requirements
            Terminal::Jump(Value::Register(1)),
        ),
        (
            "end".to_owned(),
            vec![Instruction::Arith(Op::Sub, 1, 1, Value::Register(3))],
            Terminal::Halt,
        ),
    ]
}

pub fn invalid_conflicting_typevars_1() -> Program {
    vec![
        (
            "entry".to_owned(),
            vec![Instruction::Mov(
                1,
                Value::Word(WordValue::Label("expects_different".to_owned())),
            )],
            // try to pass it a function that expects different types for r2 and r3
            Terminal::Jump(Value::Word(WordValue::Label(
                "mov_then_indirect_jump".to_owned(),
            ))),
        ),
        (
            "mov_then_indirect_jump".to_owned(),
            vec![
                // r2 and r3 have the same type variable
                Instruction::Mov(2, Value::Register(3)),
            ],
            // note: this actually fails because we cant call any function that expects any concrete types
            // the only function we could jump to here would be
            // a function valid over type forall t code<r2: t, r3: t>
            // and theres no like "universal unary operator" that we could use so
            Terminal::Jump(Value::Register(1)),
        ),
        (
            "expects_different".to_owned(),
            // expects an int in r2 and a label in r3
            vec![
                Instruction::Arith(Op::Sub, 1, 2, Value::Register(2)),
                Instruction::BranchNonZero(2, Value::Register(3)),
            ],
            Terminal::Halt,
        ),
        ("poly_halt".to_owned(), vec![], Terminal::Halt),
    ]
}

pub fn basic_heap() -> Program {
    vec![(
        "entry".to_owned(),
        vec![
            // r1 = malloc(5)
            Instruction::Malloc(1, 5),
            // r2 = r1[0]
            // r2: unifVar(4)
            Instruction::Load(2, 1, 0),
            // r3 = entry
            Instruction::Mov(3, Value::Word(WordValue::Label("entry".to_owned()))),
            // r1[1] = r3
            Instruction::StoreStrong(1, 1, 3),
            // r2 = r1[1]
            Instruction::Load(2, 1, 1),
        ],
        Terminal::Halt,
    )]
}

// in order to typecheck that this fails
pub fn ptr_information_loss_1() -> Program {
    vec![(
        "entry".to_owned(),
        vec![
            Instruction::Mov(3, Value::Word(WordValue::Label("entry".to_owned()))),
            // start with a malloc of 1
            Instruction::Malloc(1, 1),
            // r1 is a uniqptr with one field + rho(1)
            // rho(1) asserts that it does not have field 1 but has all other fields
            Instruction::StoreStrong(1, 0, 3),
            // rho(2) asserts that it does not have field 1
            Instruction::StoreStrong(1, 0, 3),
            // load fails because both ptr and rho(2) dont have field 1
            Instruction::Load(2, 1, 1),
        ],
        Terminal::Halt,
    )]
}

// we end up forgetting information
pub fn ptr_information_loss_2() -> Program {
    vec![(
        "entry".to_owned(),
        vec![
            Instruction::Mov(3, Value::Word(WordValue::Label("entry".to_owned()))),
            // start with a malloc of 2
            Instruction::Malloc(1, 2),
            // r1 is a uniqptr with one field + rho(1)
            // rho(1) asserts that it does not have field 1 but has all other fields
            Instruction::StoreStrong(1, 0, 3),
            // rho(2) asserts that it does not have field 1
            Instruction::StoreStrong(1, 0, 3),
            // all we know is that we have a rho(2), which doesnt know we malloc'd enough space earlier
            Instruction::Load(2, 1, 1),
        ],
        Terminal::Halt,
    )]
}

pub fn poly_heap() -> Program {
    vec![
        (
            "entry".to_owned(),
            vec![Instruction::Malloc(1, 5)],
            Terminal::Jump(Value::Word(WordValue::Label("store".to_owned()))),
        ),
        (
            "store".to_owned(),
            vec![
                // can be called with r1 as any uniqptr with index 1 set
                Instruction::Mov(3, Value::Word(WordValue::Label("entry".to_owned()))),
                Instruction::StoreStrong(1, 1, 3),
            ],
            Terminal::Halt,
        ),
    ]
}

pub fn poly_heap_2() -> Program {
    vec![
        (
            "entry".to_owned(),
            vec![
                Instruction::Mov(3, Value::Word(WordValue::Label("entry".to_owned()))),
                Instruction::Malloc(1, 5),
                // shove a bunch of other random stuff in
                Instruction::StoreStrong(1, 0, 3),
                Instruction::StoreStrong(1, 0, 3),
            ],
            Terminal::Jump(Value::Word(WordValue::Label("store".to_owned()))),
        ),
        (
            "store".to_owned(),
            vec![
                // can be called with r1 as any uniqptr with index 1 set
                Instruction::Mov(3, Value::Word(WordValue::Label("entry".to_owned()))),
                Instruction::StoreStrong(1, 1, 3),
            ],
            Terminal::Halt,
        ),
    ]
}

pub fn poly_heap_3() -> Program {
    vec![
        (
            "entry".to_owned(),
            vec![
                Instruction::Mov(3, Value::Word(WordValue::Label("entry".to_owned()))),
                // small malloc
                Instruction::Malloc(1, 1),
                // shove a bunch of other random stuff in
                Instruction::StoreStrong(1, 0, 3),
                Instruction::StoreStrong(1, 0, 3),
            ],
            Terminal::Jump(Value::Word(WordValue::Label("store".to_owned()))),
        ),
        (
            "store".to_owned(),
            vec![
                // cant be called with the small malloc
                Instruction::Mov(3, Value::Word(WordValue::Label("entry".to_owned()))),
                Instruction::StoreStrong(1, 1, 3),
            ],
            Terminal::Halt,
        ),
    ]
}

pub fn full_suite() -> Vec<Program> {
    vec![
        make_add_program(),
        make_jump_program(),
        make_indirect_jump_program(),
        ind_jump_wrong_registers(),
        ind_jump_forgetting(),
        invalid_ind_jump_requirements(),
        invalid_conflicting_typevars_1(),
        basic_heap(),
        ptr_information_loss_1(),
        ptr_information_loss_2(),
        poly_heap(),
        poly_heap_2(),
        poly_heap_3(),
    ]
}
