use crate::lexer::token::Positioned;

use crate::parser::ast::Expression;
use crate::parser::ast::Ident;

use crate::runtime::environment::Context;
use crate::runtime::value::Value;
use crate::runtime::Runtime;

impl Runtime {
    pub fn eval_expression(
        &self,
        expression: &Positioned<Expression>,
    ) -> (Positioned<Value>, Option<Context>) {
        match expression.value.clone() {
            Expression::Ident(ident) => self.get_variable(&expression.span.wrap(ident)),
            Expression::Literal(literal) => (expression.span.wrap(literal).into(), None),
            Expression::Array(elements) => {
                (self.create_array(expression.span.wrap(elements)), None)
            }
            Expression::Object(properties) => {
                (self.create_object(expression.span.wrap(properties)), None)
            }
            Expression::Block(block) => (self.eval_code_block(expression.span.wrap(block)), None),
            Expression::Index(index) => self.index_value(expression.span.wrap(index)),
            Expression::FunctionCall(call) => (
                self.call_function(self.eval_expression(&call.function), call.arguments),
                None,
            ),
            Expression::IfCondition(condition) => {
                (self.eval_if_else(expression.span.wrap(condition)), None)
            }
            Expression::Function(function) => {
                (self.create_function(expression.span.wrap(function)), None)
            }
            Expression::Infix(infix) => (self.calculate_infix(expression.span.wrap(infix)), None),
            Expression::Null => (expression.span.wrap(Value::Null), None),
            Expression::Constructor(constructor) => (
                self.create_constructor(expression.span.wrap(constructor)),
                None,
            ),
            Expression::This => self.get_variable(&expression.span.wrap(Ident::new("this"))),
            Expression::ClassCreation(creation) => {
                self.create_class(expression.span.wrap(creation))
            }
            Expression::Assignment(assignment) => {
                (self.assign_value(expression.span.wrap(assignment)), None)
            }
            Expression::ClassFunction(function) => (
                self.create_class_function(expression.span.wrap(function)),
                None,
            ),
            Expression::LinearFunction(function) => (
                self.create_linear_function(expression.span.wrap(function)),
                None,
            ),
        }
    }
}
