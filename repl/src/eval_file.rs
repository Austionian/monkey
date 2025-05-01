use crate::evaluate;
use object::Environment;

pub fn eval(env: &mut Environment, buffer: &str) {
    evaluate!(buffer, env)
}
