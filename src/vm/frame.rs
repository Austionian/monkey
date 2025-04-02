use crate::object::ObjectType;

#[derive(Debug, Clone)]
pub(super) struct Frame {
    // always ObjectType::CompileFunction
    pub(super) func: ObjectType,
    pub(super) ip: isize,
}

impl Frame {
    pub fn new(func: ObjectType) -> Self {
        Self { func, ip: -1 }
    }

    pub fn instructions(&self) -> &Vec<u8> {
        if let ObjectType::CompileFunction(instructions) = &self.func {
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
        }
    }
}
