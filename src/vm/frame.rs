use crate::object::ObjectType;

#[derive(Debug, Clone)]
pub(super) struct Frame {
    // always ObjectType::CompileFunction
    pub(super) func: ObjectType,
    pub(super) ip: isize,
    pub(super) base_pointer: usize,
}

impl Frame {
    pub fn new(func: ObjectType, base_pointer: usize) -> Self {
        Self {
            func,
            ip: -1,
            base_pointer,
        }
    }

    pub fn instructions(&self) -> &Vec<u8> {
        if let ObjectType::CompileFunction(instructions, _, _) = &self.func {
            instructions
        } else {
            panic!(
                "Only CompileFunctions should be inside Frames! Found: {:?}",
                self.func
            )
        }
    }

    pub const fn default() -> Self {
        Self {
            func: ObjectType::NullObj,
            ip: -1,
            base_pointer: 0,
        }
    }
}
