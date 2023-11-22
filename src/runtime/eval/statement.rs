use crate::lexer::token::Positioned;

use crate::parser::ast::InterfaceStatement;
use crate::parser::ast::Statement;

use crate::runtime::value::Value;
use crate::runtime::Runtime;

impl Runtime {
    pub fn eval_statement(&self, statement: Positioned<Statement>) -> Positioned<Value> {
        match statement.value {
            Statement::Variable(variable) => self.create_variable(statement.span.wrap(variable)),
            Statement::Interface(InterfaceStatement { name, fields }) => {
                self.eval_interface(name, fields)
            }
            Statement::Class(class) => self.eval_class(statement.span.wrap(class)),
            Statement::Return(expression) => statement.span.wrap(Value::ReturnValue(Box::new(
                self.eval_expression(&statement.span.wrap(expression)).0,
            ))),
            Statement::Expression(expression) => {
                self.eval_expression(&statement.span.wrap(expression)).0
            }
        }
    }
}
