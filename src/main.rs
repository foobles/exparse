mod parser;



fn main() {
    let input = r#"

         -53

    "#;
    let mut state = parser::ParseState::new(input);

    let r = state.parse(parser::i32());

    match r {
        Ok(i) => println!("Ok: {}", i),
        Err(e) => eprintln!("Error at line {} column {}", e.row(), e.col())
    }
}
