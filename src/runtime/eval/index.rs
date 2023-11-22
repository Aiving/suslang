use crate::lexer::token::Positioned;

use crate::parser::ast::Expression;

use crate::parser::ast::IndexExpression;
use crate::runtime::environment::Context;
use crate::runtime::value::ErrorCode;
use crate::runtime::value::Value;
use crate::runtime::Runtime;

use crate::util::get_by_index;

impl Runtime {
    pub fn index_value(
        &self,
        expression: Positioned<IndexExpression>,
    ) -> (Positioned<Value>, Option<Context>) {
        let (target, context) = self.eval_expression(&expression.value.target);

        let (index, _) = match expression.value.index.value {
            Expression::Ident(ref ident) => {
                let variable = self.eval_expression(&expression.value.index);

                if matches!(variable.0.value, Value::Exception(..)) {
                    (
                        expression.value.index.span.wrap(Value::String(ident.0.clone())),
                        None,
                    )
                } else {
                    variable
                }
            }
            _ => self.eval_expression(&expression.value.index),
        };

        if let Value::Exception(..) = target.value {
            return (target, context);
        } else if let Value::Exception(..) = index.value {
            return (index, context);
        }

        match target.value {
            Value::Array(elements) => match index.value {
                Value::Byte(index_number) => (
                    get_by_index(
                        elements,
                        index_number as usize,
                        index.span.wrap(Value::Null),
                    ),
                    context,
                ),
                Value::Short(index_number) => (
                    get_by_index(
                        elements,
                        index_number as usize,
                        index.span.wrap(Value::Null),
                    ),
                    context,
                ),
                Value::Int(index_number) => (
                    get_by_index(
                        elements,
                        index_number as usize,
                        index.span.wrap(Value::Null),
                    ),
                    context,
                ),
                Value::Long(index_number) => (
                    get_by_index(
                        elements,
                        index_number as usize,
                        index.span.wrap(Value::Null),
                    ),
                    context,
                ),
                val => (
                    index.span.wrap(Value::Exception(
                        ErrorCode::Type,
                        format!("can't index array with non-integer value: {:#?}", val),
                    )),
                    context,
                ),
            },
            Value::Object(properties) => match index.value {
                Value::Exception(_, _) => (index, context),
                _ => (
                    properties
                        .into_iter()
                        .find(|(key, _)| key.value == index.value)
                        .map(|(_, value)| value)
                        .unwrap_or(index.span.wrap(Value::Null)),
                    context,
                ),
            },
            value => (
                target.span.wrap(Value::Exception(
                    ErrorCode::Type,
                    format!("unexpected index target: {}", value),
                )),
                context,
            ),
        }
    }
}
