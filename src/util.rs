use std::collections::HashMap;

pub fn get_by_index<T>(elements: Vec<T>, index: usize, default_value: T) -> T {
    elements.into_iter().nth(index).unwrap_or(default_value)
}

pub trait VecExt<T> {
    fn includes<P>(&self, predicate: P) -> bool
    where
        P: Fn(&T) -> bool;
}

impl<T> VecExt<T> for Vec<T> {
    fn includes<P>(&self, predicate: P) -> bool
    where
        P: Fn(&T) -> bool,
    {
        for element in self {
            if predicate(element) {
                return true;
            }
        }

        false
    }
}

pub trait MapExt<K, V> {
    fn find<P>(&self, predicate: P) -> Option<(K, V)>
    where
        P: FnMut((&K, &V)) -> bool;
}

impl<K: Clone, V: Clone> MapExt<K, V> for HashMap<K, V> {
    fn find<P>(&self, mut predicate: P) -> Option<(K, V)>
    where
        P: FnMut((&K, &V)) -> bool,
    {
        let mut result = None;

        for element in self {
            if predicate(element) {
                result = Some((element.0.clone(), element.1.clone()));

                break;
            }
        }

        result
    }
}
