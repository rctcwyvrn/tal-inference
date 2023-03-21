use tests::*;

mod checker;
mod data;
mod debug;
mod syntax;
mod unify;
mod tests;

fn main() {
    let suite = full_suite();
    // let prog = make_int_program();
    // let prog = make_jump_program();
    // let prog = make_indirect_jump_program();
    // let prog = make_invalid_indirect_jump_program();
    // let prog = make_unfortunately_invalid_program();
    // let prog = invalid_ind_jump_requirements();
    // let prog = invalid_conflicting_typevars_1();
    let prog = basic_heap();
    // let prog = poly_heap();

    let mut checker = checker::Checker::new();
    println!("Res {:?}", checker.check(prog));
}
