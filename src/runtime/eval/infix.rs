use crate::lexer::token::Positioned;

use crate::parser::ast::Infix;

use crate::parser::ast::InfixExpression;
use crate::runtime::value::ErrorCode;
use crate::runtime::value::Value;
use crate::runtime::Runtime;

impl Runtime {
    pub fn concat_objects(
        &self,
        operation: Positioned<Infix>,
        _left: Positioned<Value>,
        _right: Positioned<Value>,
    ) -> Positioned<Value> {
        if let Value::Exception(..) = _left.value {
            return _left;
        } else if let Value::Exception(..) = _right.value {
            return _right;
        }

        match (_left.value, _right.value) {
            (Value::Byte(left), Value::Byte(right)) => _left
                .span
                .between(_right.span)
                .wrap(Value::Byte(left + right)),
            (Value::Short(left), Value::Short(right)) => _left
                .span
                .between(_right.span)
                .wrap(Value::Short(left + right)),
            (Value::Int(left), Value::Int(right)) => _left
                .span
                .between(_right.span)
                .wrap(Value::Int(left + right)),
            (Value::Long(left), Value::Long(right)) => _left
                .span
                .between(_right.span)
                .wrap(Value::Long(left + right)),
            (Value::Float(left), Value::Float(right)) => _left
                .span
                .between(_right.span)
                .wrap(Value::Float(left + right)),
            (Value::String(left), Value::String(right)) => _left
                .span
                .between(_right.span)
                .wrap(Value::String(left + &right)),

            (left, right) => operation.span.wrap(Value::Exception(
                ErrorCode::Type,
                format!(
                    "{} and {} are incompatible",
                    left.type_of(),
                    right.type_of()
                ),
            )),
        }
    }

    pub fn calculate_infix(&self, infix: Positioned<InfixExpression>) -> Positioned<Value> {
        let _left = self.eval_expression(&infix.value.left).0;
        let _right = self.eval_expression(&infix.value.right).0;

        if matches!(_left.value, Value::Exception(..)) {
            return _left;
        } else if matches!(_right.value, Value::Exception(..)) {
            return _right;
        }

        match infix.value.operation.value {
            Infix::Plus => self.concat_objects(infix.value.operation, _left, _right),
            Infix::Minus => match (_left.value, _right.value) {
                (Value::Byte(left), Value::Byte(right)) => {
                    if right > left {
                        _left.span.between(_right.span).wrap(Value::Exception(
                            ErrorCode::Type,
                            "byte can't be lower than zero".to_string(),
                        ))
                    } else {
                        _left
                            .span
                            .between(_right.span)
                            .wrap(Value::Byte(left - right))
                    }
                }
                (Value::Short(left), Value::Short(right)) => _left
                    .span
                    .between(_right.span)
                    .wrap(Value::Short(left - right)),
                (Value::Int(left), Value::Int(right)) => _left
                    .span
                    .between(_right.span)
                    .wrap(Value::Int(left - right)),
                (Value::Long(left), Value::Long(right)) => _left
                    .span
                    .between(_right.span)
                    .wrap(Value::Long(left - right)),
                (Value::Float(left), Value::Float(right)) => _left
                    .span
                    .between(_right.span)
                    .wrap(Value::Float(left - right)),
                (left, right) => _left.span.between(_right.span).wrap(Value::Exception(
                    ErrorCode::Type,
                    format!(
                        "{} and {} are incompatible",
                        left.type_of(),
                        right.type_of()
                    ),
                )),
            },
            Infix::Divide => match (_left.value, _right.value) {
                (Value::Byte(left), Value::Byte(right)) => _left
                    .span
                    .between(_right.span)
                    .wrap(Value::Byte(left / right)),
                (Value::Short(left), Value::Short(right)) => _left
                    .span
                    .between(_right.span)
                    .wrap(Value::Short(left / right)),
                (Value::Int(left), Value::Int(right)) => _left
                    .span
                    .between(_right.span)
                    .wrap(Value::Int(left / right)),
                (Value::Long(left), Value::Long(right)) => _left
                    .span
                    .between(_right.span)
                    .wrap(Value::Long(left / right)),
                (Value::Float(left), Value::Float(right)) => _left
                    .span
                    .between(_right.span)
                    .wrap(Value::Float(left / right)),
                (left, right) => _left.span.between(_right.span).wrap(Value::Exception(
                    ErrorCode::Type,
                    format!(
                        "{} and {} are incompatible",
                        left.type_of(),
                        right.type_of()
                    ),
                )),
            },
            Infix::Multiply => match (_left.value, _right.value) {
                (Value::Byte(left), Value::Byte(right)) => _left
                    .span
                    .between(_right.span)
                    .wrap(Value::Byte(left * right)),
                (Value::Short(left), Value::Short(right)) => _left
                    .span
                    .between(_right.span)
                    .wrap(Value::Short(left * right)),
                (Value::Int(left), Value::Int(right)) => _left
                    .span
                    .between(_right.span)
                    .wrap(Value::Int(left * right)),
                (Value::Long(left), Value::Long(right)) => _left
                    .span
                    .between(_right.span)
                    .wrap(Value::Long(left * right)),
                (Value::Float(left), Value::Float(right)) => _left
                    .span
                    .between(_right.span)
                    .wrap(Value::Float(left * right)),
                (left, right) => _left.span.between(_right.span).wrap(Value::Exception(
                    ErrorCode::Type,
                    format!(
                        "{} and {} are incompatible",
                        left.type_of(),
                        right.type_of()
                    ),
                )),
            },
            Infix::Equal => _left
                .span
                .between(_right.span)
                .wrap(Value::Boolean(_left.value == _right.value)),
            Infix::NotEqual => _left
                .span
                .between(_right.span)
                .wrap(Value::Boolean(_left.value != _right.value)),
            Infix::GreaterThanEqual => match (_left.value, _right.value) {
                (Value::Byte(left), Value::Byte(right)) => _left
                    .span
                    .between(_right.span)
                    .wrap(Value::Boolean(left >= right)),
                (Value::Short(left), Value::Short(right)) => _left
                    .span
                    .between(_right.span)
                    .wrap(Value::Boolean(left >= right)),
                (Value::Int(left), Value::Int(right)) => _left
                    .span
                    .between(_right.span)
                    .wrap(Value::Boolean(left >= right)),
                (Value::Long(left), Value::Long(right)) => _left
                    .span
                    .between(_right.span)
                    .wrap(Value::Boolean(left >= right)),
                (Value::Float(left), Value::Float(right)) => _left
                    .span
                    .between(_right.span)
                    .wrap(Value::Boolean(left >= right)),
                (left, right) => _left.span.between(_right.span).wrap(Value::Exception(
                    ErrorCode::Type,
                    format!(
                        "{} and {} are incompatible",
                        left.type_of(),
                        right.type_of()
                    ),
                )),
            },
            Infix::GreaterThan => match (_left.value, _right.value) {
                (Value::Byte(left), Value::Byte(right)) => _left
                    .span
                    .between(_right.span)
                    .wrap(Value::Boolean(left > right)),
                (Value::Short(left), Value::Short(right)) => _left
                    .span
                    .between(_right.span)
                    .wrap(Value::Boolean(left > right)),
                (Value::Int(left), Value::Int(right)) => _left
                    .span
                    .between(_right.span)
                    .wrap(Value::Boolean(left > right)),
                (Value::Long(left), Value::Long(right)) => _left
                    .span
                    .between(_right.span)
                    .wrap(Value::Boolean(left > right)),
                (Value::Float(left), Value::Float(right)) => _left
                    .span
                    .between(_right.span)
                    .wrap(Value::Boolean(left > right)),
                (left, right) => _left.span.between(_right.span).wrap(Value::Exception(
                    ErrorCode::Type,
                    format!(
                        "{} and {} are incompatible",
                        left.type_of(),
                        right.type_of()
                    ),
                )),
            },
            Infix::LessThanEqual => match (_left.value, _right.value) {
                (Value::Byte(left), Value::Byte(right)) => _left
                    .span
                    .between(_right.span)
                    .wrap(Value::Boolean(left <= right)),
                (Value::Short(left), Value::Short(right)) => _left
                    .span
                    .between(_right.span)
                    .wrap(Value::Boolean(left <= right)),
                (Value::Int(left), Value::Int(right)) => _left
                    .span
                    .between(_right.span)
                    .wrap(Value::Boolean(left <= right)),
                (Value::Long(left), Value::Long(right)) => _left
                    .span
                    .between(_right.span)
                    .wrap(Value::Boolean(left <= right)),
                (Value::Float(left), Value::Float(right)) => _left
                    .span
                    .between(_right.span)
                    .wrap(Value::Boolean(left <= right)),
                (left, right) => _left.span.between(_right.span).wrap(Value::Exception(
                    ErrorCode::Type,
                    format!(
                        "{} and {} are incompatible",
                        left.type_of(),
                        right.type_of()
                    ),
                )),
            },
            Infix::LessThan => match (_left.value, _right.value) {
                (Value::Byte(left), Value::Byte(right)) => _left
                    .span
                    .between(_right.span)
                    .wrap(Value::Boolean(left < right)),
                (Value::Short(left), Value::Short(right)) => _left
                    .span
                    .between(_right.span)
                    .wrap(Value::Boolean(left < right)),
                (Value::Int(left), Value::Int(right)) => _left
                    .span
                    .between(_right.span)
                    .wrap(Value::Boolean(left < right)),
                (Value::Long(left), Value::Long(right)) => _left
                    .span
                    .between(_right.span)
                    .wrap(Value::Boolean(left < right)),
                (Value::Float(left), Value::Float(right)) => _left
                    .span
                    .between(_right.span)
                    .wrap(Value::Boolean(left < right)),
                (left, right) => _left.span.between(_right.span).wrap(Value::Exception(
                    ErrorCode::Type,
                    format!(
                        "{} and {} are incompatible",
                        left.type_of(),
                        right.type_of()
                    ),
                )),
            },
        }
    }
}
