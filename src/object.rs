use std::fmt;

#[derive(Debug)]
pub enum Object {
    Int(i64),
    Bool(bool),
    Null,
    Return(Box<Return>),
}

#[derive(Debug)]
pub struct Return {
    pub value: Object,
}

impl fmt::Display for Return {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value.inspect())
    }
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Int(i) => i.to_string(),
            Object::Bool(b) => b.to_string(),
            Object::Null => String::from("null"),
            Object::Return(r) => r.value.inspect(),
            //     Object::String(s) => s.clone(),
            //     Object::Function(f) => f.inspect(),
            //     Object::Builtin(b) => b.inspect(),
            //     Object::Array(a) => a.inspect(),
            //     Object::Hash(h) => h.inspect(),
            //     Object::CompiledFunction(f) => f.inspect(),
            //     Object::Closure(c) => c.inspect(),
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{}", self.inspect())
    }
}
