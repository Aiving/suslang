use crate::lexer::token::Positioned;

use crate::parser::ast::IfElseConditionExpression;

use crate::runtime::value::ErrorCode;
use crate::runtime::value::Value;
use crate::runtime::Runtime;

impl Runtime {
    pub fn eval_if_else(&self, condition: Positioned<IfElseConditionExpression>) -> Positioned<Value> {
        let mut value = self.eval_expression(&condition.value.condition).0;

        if let Value::ReturnValue(val) = value.value {
            value = *val;
        }

        if let Value::Exception(..) = value.value {
            return value;
        }

        match value.value {
            Value::Boolean(boolean) => {
                if boolean {
                    self.eval_code_block(condition.value.then)
                } else if let Some(otherwise) = condition.value.otherwise {
                    self.eval_code_block(otherwise)
                } else {
                    condition.value.then.span.wrap(Value::Null)
                }
            }
            _ => value.span.wrap(Value::Exception(
                ErrorCode::Type,
                format!("boolean expected, but {} given", value.value.type_of()),
            )),
        }
    }
}
