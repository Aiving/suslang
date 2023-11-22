use crate::lexer::token::Positioned;

use crate::lexer::token::ValueType;
use crate::parser::ast::ArrayExpression;
use crate::parser::ast::ClassCreationExpression;
use crate::parser::ast::ClassFunctionExpression;
use crate::parser::ast::ConstructorExpression;
use crate::parser::ast::FunctionArgument;
use crate::parser::ast::FunctionExpression;
use crate::parser::ast::Ident;
use crate::parser::ast::LinearFunctionExpression;
use crate::parser::ast::ObjectExpression;

use crate::parser::ast::VariableStatement;
use crate::runtime::environment::Context;
use crate::runtime::environment::Environment;
use crate::runtime::value::ErrorCode;
use crate::runtime::value::Value;
use crate::runtime::Runtime;

impl Runtime {
    pub fn create_array(&self, array: Positioned<ArrayExpression>) -> Positioned<Value> {
        let value = Value::Array(
            array
                .value
                .into_iter()
                .map(|expression| self.eval_expression(&expression).0)
                .collect(),
        );

        array.span.wrap(value)
    }

    pub fn create_object(&self, object: Positioned<ObjectExpression>) -> Positioned<Value> {
        let value = Value::Object(
            object
                .value
                .into_iter()
                .map(|p| (p.value.0.into(), self.eval_expression(&p.value.1).0))
                .collect(),
        );

        object.span.wrap(value)
    }

    pub fn create_class(
        &self,
        expression: Positioned<ClassCreationExpression>,
    ) -> (Positioned<Value>, Option<Context>) {
        // TEMPORARY SOLUTION: just creating object...
        let class = self.get_variable(&expression.value.class);

        if matches!(class.0.value, Value::Exception(..)) {
            return class;
        }

        let Value::Class(_, generics, _, constructors, methods, properties) = class.0.value else {
            return (
                expression
                    .value
                    .class
                    .span
                    .between(expression.value.arguments.span)
                    .wrap(Value::error(ErrorCode::Type, "")),
                None,
            );
        };

        if expression.value.generics.len() != generics.len() {
            return (
                expression
                    .span
                    .wrap(Value::error(ErrorCode::Type, "cringe".to_string())),
                None,
            );
        }

        let mut arguments = expression
            .value
            .arguments
            .value
            .into_iter()
            .map(|expression| Some(self.eval_expression(&expression).0))
            .collect::<Vec<_>>();

        let constructor = constructors.into_iter().find(|constructor| {
            if let Value::Constructor(_, _, _, args, _) = &constructor.value {
                args.value
                    .iter()
                    .filter(|argument| argument.value.default_value.is_none())
                    .count()
                    == arguments.len()
            } else {
                false
            }
        });

        if let Some(constructor) = constructor {
            let Value::Constructor(_, _, _, args, body) = constructor.value else {
                return (
                    expression
                        .value
                        .class
                        .span
                        .between(expression.value.arguments.span)
                        .wrap(Value::error(ErrorCode::Type, "()")),
                    None,
                );
            };

            arguments.resize(args.value.len(), None);

            let environment = Environment::new_with_parent(self.get_context());
            let runtime = Runtime::new(environment.clone());
            let zipped = args.value.into_iter().zip(arguments);

            for (index, generic) in expression.value.generics.into_iter().enumerate() {
                runtime.associate_type(generics[index].value.clone(), generic.value);
            }

            let properties = [
                properties
                    .into_iter()
                    .map(|property| {
                        let (_, name, ty, _, default_value) = property.value;

                        (
                            name.span.wrap(Value::String(name.value.0)),
                            default_value.unwrap_or(ty.span.wrap(Value::None)),
                        )
                    })
                    .collect::<Vec<_>>(),
                methods
                    .into_iter()
                    .map(|method| {
                        let Value::Function(.., ref name, _, _, _) = method.value else {
                            unreachable!()
                        };

                        (
                            method.span.wrap(Value::String(name.value.0.clone())),
                            method,
                        )
                    })
                    .collect::<Vec<_>>(),
            ]
            .concat()
            .into_iter()
            .collect();

            runtime.set_variable(
                Ident::new("this"),
                (
                    expression.value.class.span.wrap(Value::Object(properties)),
                    None,
                ),
            );

            for (argument, o) in zipped {
                let FunctionArgument {
                    name,
                    ty,
                    nullable,
                    default_value,
                } = argument.value;

                if o.is_none() && default_value.is_none() {
                    return (
                        name.span.wrap(Value::error(
                            ErrorCode::Type,
                            format!(
                                "no value presented for {}:constructor#{}",
                                expression.value.class.value.0, name.value.0
                            ),
                        )),
                        None,
                    );
                }

                let mut value =
                    o.unwrap_or_else(|| self.eval_expression(&default_value.unwrap()).0);

                if let Value::ReturnValue(val) = value.value {
                    value = *val;
                }

                if value.value == Value::Null && !nullable {
                    return (
                        value.span.wrap(Value::error(
                            ErrorCode::Type,
                            format!("null is not assignable to {}", value.value.type_of()),
                        )),
                        None,
                    );
                }

                println!("{ty:#?}");

                let ty = match ty.value {
                    ValueType::Array(ty) => match *ty {
                        ValueType::Custom(ty) => ValueType::Array(Box::new(
                            runtime
                                .resolve_type(Ident(ty.clone()))
                                .unwrap_or(ValueType::Custom(ty)),
                        )),
                        ty => ValueType::Array(Box::new(ty)),
                    },
                    ValueType::Custom(ty) => runtime
                        .resolve_type(Ident(ty.clone()))
                        .unwrap_or(ValueType::Custom(ty)),
                    ty => ty,
                };

                let is_difference = !value.value.is_type_of(&ty);

                if is_difference {
                    return (
                        value.span.wrap(Value::error(
                            ErrorCode::Type,
                            format!("expected {:?} type, found {}", ty, value.value.type_of()),
                        )),
                        None,
                    );
                }

                runtime.set_variable(name.value, (value, None));
            }

            runtime.eval_code_block(body);

            let this = runtime.get_variable(&expression.value.class.span.wrap(Ident::new("this")));

            (self.returned(this.0), Some(environment))
        } else {
            (
                expression
                    .value
                    .class
                    .span
                    .between(expression.value.arguments.span)
                    .wrap(Value::error(ErrorCode::Type, "()")),
                None,
            )
        }
    }

    pub fn create_constructor(
        &self,
        constructor: Positioned<ConstructorExpression>,
    ) -> Positioned<Value> {
        let value = Value::Constructor(
            vec![],
            constructor.value.access_flags,
            constructor.value.name,
            constructor.value.arguments,
            constructor.value.body,
        );

        constructor.span.wrap(value)
    }

    pub fn create_class_function(
        &self,
        expression: Positioned<ClassFunctionExpression>,
    ) -> Positioned<Value> {
        expression.span.wrap(Value::Function(
            vec![],
            expression.value.access_flags,
            expression.value.name,
            expression.value.return_type,
            expression.value.arguments,
            expression.value.body,
        ))
    }

    pub fn create_function(&self, expression: Positioned<FunctionExpression>) -> Positioned<Value> {
        self.set_variable(
            expression.value.name.value.clone(),
            (
                expression.span.wrap(Value::Function(
                    vec![],
                    expression.value.access_flags,
                    expression.value.name,
                    expression.value.return_type,
                    expression.value.arguments,
                    expression.value.body,
                )),
                None,
            ),
        )
    }

    pub fn create_linear_function(
        &self,
        expression: Positioned<LinearFunctionExpression>,
    ) -> Positioned<Value> {
        expression.span.wrap(Value::LinearFunction(
            expression.value.arguments,
            expression.value.body,
        ))
    }

    pub fn create_variable(&self, statement: Positioned<VariableStatement>) -> Positioned<Value> {
        let (mut value, context) = self.eval_expression(&statement.value.value);

        if let Value::ReturnValue(val) = value.value {
            value = *val;
        }

        if value.value == Value::Null {
            if !statement.value.nullable {
                return value.span.wrap(Value::error(
                    ErrorCode::Type,
                    format!("null is not assignable to {}", value.value.type_of()),
                ));
            }

            if statement.value.ty.is_none() {
                return value.span.wrap(Value::error(
                    ErrorCode::Type,
                    "can't detect type from null".to_string(),
                ));
            }
        }

        if let Some(ty) = statement.value.ty {
            let is_difference = !value.value.is_type_of(&ty.value);

            if is_difference {
                return value.span.wrap(Value::error(
                    ErrorCode::Type,
                    format!(
                        "expected {:?} type, found {}",
                        ty.value,
                        value.value.type_of()
                    ),
                ));
            }
        }

        self.set_variable(statement.value.name.value, (value, context))
    }
}
