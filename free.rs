        // fn get_register(val: &Value) -> Option<Register> {
        //     match val {
        //         Value::Register(r) => Some(*r),
        //         Value::Word(_) => None,
        //     }
        // }

        // let mut initialized = HashSet::new();
        // let mut free = HashSet::new();

        // for instr in &block.1 {
        //     let mut check_init = |r: Register| -> Option<Register> {
        //         if !initialized.contains(&r) {
        //             free.insert(r);
        //         }
        //         Some(r)
        //     };

        //     match instr {
        //         Instruction::Arith(_, r_tar, r_src, v_src) => {
        //             check_init(*r_src);
        //             get_register(v_src).and_then(check_init);
        //             initialized.insert(r_tar);
        //         }
        //         Instruction::BranchNonZero(r_pred, v_target) => {
        //             check_init(*r_pred);
        //             get_register(v_target).and_then(check_init);
        //         }
        //         Instruction::Load(_, _, _) => todo!(),
        //         Instruction::Store(_, _, _) => todo!(),
        //         Instruction::Malloc(_) => todo!(),
        //         Instruction::Mov(r_tar, v_src) => {
        //             get_register(v_src).and_then(check_init);
        //             initialized.insert(r_tar);
        //         }
        //     }
        // }

        // create label type
        // let mut ty = HashMap::new();
        // for r in 1..MAX_REGISTER {
        //     if free.contains(&r) {
        //         ty.insert(r, self.fresh());
        //     }
        // }
        // self.label_types.insert(block.0.clone(), ty);