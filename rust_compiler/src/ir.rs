#[derive(Debug)]
pub struct File(Box<[Declaration]>);

#[derive(Debug)]
pub enum Declaration {
    Function { id: Identifier, return_type: Type },
    ExposedFunction { name: Box<str>, actual: Identifier },
}

#[derive(Debug)]
pub enum Type {
    Int(usize),
}

#[derive(Debug)]
pub struct Identifier(usize);
