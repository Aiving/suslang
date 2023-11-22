use std::collections::HashMap;

use std::iter::Peekable;
use std::sync::Arc;
use std::sync::Mutex;
use std::vec::IntoIter;

use crate::lexer::token::Positioned;
use crate::util::MapExt;

use super::value::Value;

pub type Context = Arc<Mutex<Environment>>;

#[derive(Clone, Debug)]
pub struct Environment {
    store: HashMap<String, (Value, Option<Context>)>,
    parent: Option<Context>,
}

impl Environment {
    pub fn new_with_parent(parent: Context) -> Context {
        Arc::new(Mutex::new(Self {
            store: HashMap::new(),
            parent: Some(parent),
        }))
    }

    pub fn new() -> Context {
        Arc::new(Mutex::new(Self {
            store: HashMap::new(),
            parent: None,
        }))
    }

    pub fn change_value<T: Into<String>>(
        &mut self,
        name: &str,
        path: Vec<T>,
        value: Positioned<Value>,
    ) {
        let path = path
            .into_iter()
            .map(|item| item.into())
            .collect::<Vec<String>>()
            .into_iter()
            .peekable();
        let object = self.get_mut(name);

        if let Some((ref mut object, _)) = object {
            Environment::_change_value(object, path, value);
        }
    }

    fn _change_value(
        object: &mut Value,
        mut path: Peekable<IntoIter<String>>,
        value: Positioned<Value>,
    ) {
        let key = path.next().unwrap();
        let key_value = Value::String(key);

        if let Value::Object(ref mut properties) = object {
            let property = properties.find(|property| property.0.value == key_value);

            if let Some(property) = property {
                if path.peek().is_some() {
                    if let Some(property) = properties.get_mut(&property.0) {
                        Environment::_change_value(&mut property.value, path, value);
                    }
                } else {
                    properties.insert(property.0.clone(), value);
                }
            }
        }
    }

    pub fn extend(&mut self, context: Context) {
        self.store.extend(context.lock().unwrap().store.clone())
    }

    pub fn set<T: Into<String>>(&mut self, name: T, (value, context): (Value, Option<Context>)) {
        self.store.insert(name.into(), (value, context));
    }

    pub fn exists(&self, name: &str) -> bool {
        self.store.contains_key(name)
    }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut (Value, Option<Context>)> {
        self.store.get_mut(name)
    }

    pub fn get(&self, name: &str) -> Option<(Value, Option<Context>)> {
        match self.store.get(name) {
            Some(value) => Some(value.clone()),
            None => match &self.parent {
                Some(parent) => parent.lock().unwrap().get(name),
                None => None,
            },
        }
    }
}
