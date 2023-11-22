pub mod environment;
pub mod eval;
pub mod value;

use std::collections::HashMap;
use std::sync::Arc;
use std::sync::RwLock;

use crate::lexer::token::Positioned;
use crate::lexer::token::Span;
use crate::lexer::token::ValueType;
use crate::parser::ast::Ident;
use crate::parser::ast::Program;

use self::environment::Context;
use self::environment::Environment;
use self::value::ErrorCode;
use self::value::Value;

#[derive(Debug, Clone)]
pub struct Runtime {
    types: Arc<RwLock<HashMap<Ident, ValueType>>>,
    context: Context,
}

impl Default for Runtime {
    fn default() -> Self {
        Self {
            types: Arc::new(RwLock::new(HashMap::new())),
            context: Environment::new(),
        }
    }
}

impl Runtime {
    pub fn new(context: Context) -> Self {
        Self {
            types: Arc::new(RwLock::new(HashMap::new())),
            context,
        }
    }

    fn associate_type(&self, name: Ident, ty: ValueType) {
        self.types.write().unwrap().insert(name, ty);
    }

    fn resolve_type(&self, name: Ident) -> Option<ValueType> {
        self.types.read().unwrap().get(&name).cloned()
    }

    fn get_variable(&self, ident: &Positioned<Ident>) -> (Positioned<Value>, Option<Context>) {
        match self.context.lock().unwrap().get(&ident.value.0) {
            Some(value) => (ident.span.wrap(value.0), value.1),
            None => (
                ident.span.wrap(Value::Exception(
                    ErrorCode::Reference,
                    format!("identifier not found: {}", ident.value.0),
                )),
                None,
            ),
        }
    }

    fn set_variable(
        &self,
        Ident(name): Ident,
        (value, _context): (Positioned<Value>, Option<Context>),
    ) -> Positioned<Value> {
        if let Value::Exception(..) = value.value {
            return value;
        }

        let mut context = self.context.lock().unwrap();
        let result = value.clone();

        if let Some((Value::Function(overloads, ..), _)) = context.get_mut(&name) {
            if let Value::Function(..) = value.value {
                overloads.push(value);
            } else {
                return value.span.wrap(Value::Exception(
                    ErrorCode::Type,
                    format!("Function expected, but {} given", value.value.type_of()),
                ));
            }
        } else {
            context.set(name, (value.value, _context));
        }

        result
    }

    fn eval_code_block(&self, mut block: Positioned<Program>) -> Positioned<Value> {
        match block.value.len() {
            0 => Positioned::new(
                Value::None,
                Span {
                    start: 0,
                    end: 0,
                    line: 1,
                    column: 1,
                },
            ),
            1 => self.eval_statement(block.value.remove(0)),
            _ => {
                let statement = block.value.remove(0);
                let value = self.eval_statement(statement);

                if let Value::Exception(..) = value.value {
                    value
                } else if value.value.is_returned() {
                    value
                } else {
                    self.eval_code_block(block)
                }
            }
        }
    }

    pub fn get_context(&self) -> Context {
        self.context.clone()
    }

    fn returned(&self, value: Positioned<Value>) -> Positioned<Value> {
        match value.value {
            Value::ReturnValue(value) => *value,
            _ => value,
        }
    }

    pub fn eval_program(&self, program: Positioned<Program>) -> Positioned<Value> {
        let value = self.eval_code_block(program);

        self.returned(value)
    }
}
