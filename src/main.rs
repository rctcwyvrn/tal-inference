use tests::*;

mod checker;
mod data;
mod debug;
mod syntax;
mod tests;
mod unify;

fn main() {
    let suite = full_suite();
    // let prog = make_int_program();
    // let prog = make_jump_program();
    // let prog = make_indirect_jump_program();
    // let prog = ind_jump_weird();
    // let prog = ind_jump_wrong_registers();
    // let prog = ind_jump_forgetting();
    // let prog = invalid_ind_jump_requirements();
    // let prog = invalid_conflicting_typevars_1();

    // pointers
    // let prog = basic_heap();
    // let prog = ptr_information_loss_1();
    // let prog = ptr_information_loss_2();
    // let prog = invalid_move_unique_ptr();
    // let prog = valid_move_shared_ptr();
    // let prog = invalid_store_strong();
    // let prog = reorder_params_then_indirect_jump();
    let prog = fallthrough();

    // let prog = poly_heap();
    // let prog = poly_heap_2();
    // let prog = poly_heap_3();

    // let prog = invalid_entrypoint();
    // let prog = bug_indirect_jump_heuristic();

    let mut checker = checker::Checker::new();
    println!("Res {:?}", checker.check(prog));
}
