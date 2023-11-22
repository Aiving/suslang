use crate::lexer::token::Positioned;

use crate::parser::ast::Ident;
use crate::parser::ast::InterfaceField;

use crate::runtime::value::Value;
use crate::runtime::Runtime;

impl Runtime {
    pub fn eval_interface(
        &self,
        name: Positioned<Ident>,
        fields: Vec<Positioned<InterfaceField>>,
    ) -> Positioned<Value> {
        self.set_variable(
            name.value.clone(),
            (
                if fields.is_empty() {
                    name.span
                } else {
                    name.span.between(fields.last().unwrap().span)
                }
                .wrap(Value::Interface(name, fields)),
                None,
            ),
        )
    }
}
