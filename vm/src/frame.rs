use object::ObjectType;

#[derive(Debug, Clone)]
pub(super) struct Frame {
    // always ObjectType::Closure
    pub(super) cl: ObjectType,
    pub(super) ip: isize,
    pub(super) base_pointer: usize,
}

impl Frame {
    pub fn new(cl: ObjectType, base_pointer: usize) -> Self {
        Self {
            cl,
            ip: -1,
            base_pointer,
        }
    }

    pub fn instructions(&self) -> &Vec<u8> {
        if let ObjectType::Closure(func, _) = &self.cl {
            match func.as_ref() {
                ObjectType::CompileFunction(instructions, _, _) => instructions,
                // closures inside closures?
                _ => todo!(),
            }
        } else {
            unreachable!(
                "Only Closures should be inside Frames! Found: {:?}",
                self.cl
            )
        }
    }

    pub const fn default() -> Self {
        Self {
            cl: ObjectType::NullObj,
            ip: -1,
            base_pointer: 0,
        }
    }
}
