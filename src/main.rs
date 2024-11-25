use monkey_interpreter::repl;

fn main() {
    println!("Hello, this is the Monkey programming language!");
    println!("Feel free to type in commands:");

    loop {
        repl::start();
    }
}
