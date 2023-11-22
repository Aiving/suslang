use crate::lexer::token::Positioned;
use crate::parser::ast::AssignmentExpression;
use crate::runtime::value::ErrorCode;
use crate::runtime::value::Value;
use crate::runtime::Runtime;

impl Runtime {
    pub fn assign_value(&self, expression: Positioned<AssignmentExpression>) -> Positioned<Value> {
        let value = self.eval_expression(&expression.value.value).0;
        let mut variable_iter = expression.value.variable.value.into_iter();

        let variable_name = variable_iter.next().unwrap();
        let position = variable_name.span.between(value.span);

        if variable_iter.len() > 0 {
            self.context.lock().unwrap().change_value(
                &variable_name.value.0,
                variable_iter.map(|item| item.value.0).collect(),
                value,
            );
        } else if !matches!(
            self.get_variable(&variable_name).0.value,
            Value::Exception(..)
        ) {
            return position.wrap(Value::Exception(ErrorCode::Type, "chel value exist".into()));
        } else {
            self.set_variable(variable_name.value.clone(), (value, None));
        }

        position.wrap(Value::None)
    }
}
