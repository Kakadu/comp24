use std::{fmt::Display, rc::Rc};

#[derive(Clone, Debug, Default)]
pub enum ConsList<T> {
    #[default]
    Nil,
    Cons(T, Rc<ConsList<T>>),
}

impl<T> ConsList<T> {
    pub fn new() -> Rc<Self> { Rc::new(ConsList::Nil) }

    pub fn cons(value: T, tail: Rc<ConsList<T>>) -> Rc<Self> { Rc::new(ConsList::Cons(value, tail)) }

    pub fn is_empty(&self) -> bool { matches!(self, ConsList::Nil) }

    pub fn head(&self) -> Option<&T> {
        match self {
            ConsList::Nil => None,
            ConsList::Cons(value, _) => Some(value),
        }
    }

    pub fn tail(&self) -> Option<Rc<ConsList<T>>> {
        match self {
            ConsList::Nil => None,
            ConsList::Cons(_, tail) => Some(Rc::clone(tail)),
        }
    }

    pub fn iter(&self) -> Iter<T> { Iter { current: self } }

    pub fn len(&self) -> usize { self.iter().count() }
}

pub struct Iter<'a, T> {
    current: &'a ConsList<T>,
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        match self.current {
            ConsList::Nil => None,
            ConsList::Cons(value, tail) => {
                self.current = tail;
                Some(value)
            }
        }
    }
}

impl<T: Display> Display for ConsList<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        let mut current = self;
        while let ConsList::Cons(value, tail) = current {
            write!(f, "{}", value)?;
            current = tail;
            if let ConsList::Cons(_, _) = current {
                write!(f, ", ")?;
            }
        }
        write!(f, "]")
    }
}
