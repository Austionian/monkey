use crate::{evaluate, read};
use object::Environment;

pub fn repl_start(env: &mut Environment) {
    let buffer = read!();
    evaluate!(&buffer, env)
}
