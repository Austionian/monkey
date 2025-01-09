use monkey_interpreter::{object, repl};

fn main() {
    println!("Hello, this is the Monkey programming language!");
    println!("Feel free to type in commands:");
    let mut env = object::Environment::new();

    loop {
        repl::start(&mut env);
    }
}
