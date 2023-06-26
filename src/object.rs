use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone)]
pub enum Object {
    Int(i64),
    Bool(bool),
    Null,
    Return(Box<Return>),
}

#[derive(Debug, Clone)]
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

pub struct Environment {
    pub store: HashMap<String, Object>,
    pub outer: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        match self.store.get(name) {
            Some(obj) => Some(obj.clone()),
            None => match &self.outer {
                Some(o) => o.get(name),
                _ => None,
            },
        }
    }

    pub fn set(&mut self, name: String, obj: Object) {
        self.store.insert(name, obj);
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}
