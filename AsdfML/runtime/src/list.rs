use std::{fmt::Display, rc::Rc};

type Link<T> = Option<Rc<Node<T>>>;

#[derive(Debug)]
struct Node<T> {
    value: T,
    next: Link<T>,
}
impl<T> Node<T> {
    fn new(value: T, next: Link<T>) -> Rc<Self> { Rc::new(Node { value, next }) }
}

#[derive(Debug)]
pub struct List<T> {
    size: usize,
    head: Link<T>,
}

pub struct Iter<'a, T> {
    next: Option<&'a Node<T>>,
}

impl<T> List<T> {
    pub fn new() -> Self { List { size: 0, head: None } }

    pub fn prepend(&self, value: T) -> Self {
        List {
            size: self.size + 1,
            head: Some(Node::new(value, self.head.as_ref().map(|node| node.clone()))),
        }
    }

    pub fn head(&self) -> Option<&T> { self.head.as_ref().map(|node| &node.value) }

    pub fn tail(&self) -> Self {
        List {
            size: self.size.saturating_sub(1),
            head: self.head.as_ref().and_then(|node| node.next.clone()),
        }
    }

    pub fn len(&self) -> usize { self.size }

    pub fn iter(&self) -> Iter<T> {
        Iter {
            next: self.head.as_deref(),
        }
    }
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        self.next.map(|node| {
            self.next = node.next.as_deref();
            &node.value
        })
    }
}

impl<T> Drop for List<T> {
    fn drop(&mut self) {
        let mut head = self.head.take();
        while let Some(node) = head {
            if let Ok(mut node) = Rc::try_unwrap(node) {
                head = node.next.take();
            } else {
                break;
            }
        }
    }
}

impl<T> Default for List<T> {
    fn default() -> Self { Self::new() }
}

impl<T: Display> Display for List<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        let mut node = self.head.as_deref();
        while let Some(n) = node {
            write!(f, "{}", n.value)?;
            node = n.next.as_deref();
            if node.is_some() {
                write!(f, ", ")?;
            }
        }
        write!(f, "]")
    }
}

mod tests {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn test_list() {
        let list1 = List::new().prepend(0);
        assert_eq!(list1.len(), 1);
        let list2 = list1.prepend(1);
        assert_eq!(list1.len(), 1);
        assert_eq!(list2.len(), 2);
    }
    #[test]
    fn test_head_tail() {
        let list = List::new().prepend(0).prepend(1).prepend(2);
        assert_eq!(list.len(), 3);
        assert_eq!(list.head(), Some(&2));

        let tail_1 = list.tail();
        assert_eq!(tail_1.len(), 2);
        assert_eq!(tail_1.head(), Some(&1));

        let tail_2 = tail_1.tail();
        assert_eq!(tail_2.len(), 1);
        assert_eq!(tail_2.head(), Some(&0));

        let tail_3 = tail_2.tail();
        assert_eq!(tail_3.len(), 0);
        assert_eq!(tail_3.head(), None);

        let tail_4 = tail_3.tail();
        assert_eq!(tail_4.head(), None);
    }

    #[test]
    fn test_iter() {
        let list = List::new().prepend(0).prepend(1).prepend(2);
        let mut iter = list.iter();
        assert_eq!(iter.next(), Some(&2));
        assert_eq!(iter.next(), Some(&1));
        assert_eq!(iter.next(), Some(&0));
        assert_eq!(iter.next(), None);
    }
}
