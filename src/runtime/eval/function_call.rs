use crate::lexer::token::Positioned;

use crate::parser::ast::Expression;
use crate::parser::ast::FunctionArgument;

use crate::runtime::environment::Context;
use crate::runtime::environment::Environment;
use crate::runtime::value::ErrorCode;
use crate::runtime::value::Value;
use crate::runtime::Runtime;

impl Runtime {
    pub fn call_function(
        &self,
        (function, context): (Positioned<Value>, Option<Context>),
        function_arguments: Vec<Positioned<Expression>>,
    ) -> Positioned<Value> {
        if let Value::Exception(..) = function.value {
            return function;
        }

        match function.value {
            Value::LinearFunction(args, body) => {
                let arguments = function_arguments
                    .into_iter()
                    .map(|expression| self.eval_expression(&expression).0)
                    .collect::<Vec<_>>();

                if arguments.len() != args.value.len() {
                    return function.span.wrap(Value::error(
                        ErrorCode::Type,
                        format!(
                            "wrong number of arguments: {} expected but {} given",
                            args.value.len(),
                            arguments.len()
                        ),
                    ));
                }

                let runtime = Runtime::new(Environment::new_with_parent(self.get_context()));
                let zipped = args.value.into_iter().zip(arguments);

                for (argument, value) in zipped {
                    if matches!(value.value, Value::Exception(..)) {
                        return value;
                    }

                    runtime.set_variable(argument.value, (value, None));
                }

                let value = runtime.eval_code_block(body);

                self.returned(value)
            }
            Value::Function(overloads, _, function_name, return_type, args, body) => {
                let mut arguments = function_arguments
                    .iter()
                    .map(|expression| Some(self.eval_expression(expression).0))
                    .collect::<Vec<_>>();

                let arg_count = args
                    .value
                    .iter()
                    .filter(|argument| argument.value.default_value.is_none())
                    .count();

                if arguments.len() != arg_count {
                    if let Some(function) = overloads.iter().find(|overload| {
                        if let Value::Function(_, _, _, _, args, _) = &overload.value {
                            arguments.len()
                                == args
                                    .value
                                    .iter()
                                    .filter(|argument| argument.value.default_value.is_none())
                                    .count()
                        } else {
                            false
                        }
                    }) {
                        return self.call_function((function.clone(), context), function_arguments);
                    } else {
                        return function.span.wrap(Value::error(
                            ErrorCode::Type,
                            format!(
                                "wrong number of arguments: {} expected but {} given",
                                args.value.len(),
                                arguments.len()
                            ),
                        ));
                    }
                }

                arguments.resize(args.value.len(), None);

                let environment = Environment::new_with_parent(self.get_context());

                if let Some(context) = context {
                    environment.lock().unwrap().extend(context);
                }

                let runtime = Runtime::new(environment.clone());
                let zipped = args.value.into_iter().zip(arguments);

                for (argument, o) in zipped {
                    let FunctionArgument {
                        name,
                        ty,
                        nullable,
                        default_value,
                    } = argument.value;

                    if o.is_none() && default_value.is_none() {
                        return name.span.wrap(Value::error(
                            ErrorCode::Type,
                            format!(
                                "no value presented for {}#{}",
                                function_name.value.0, name.value.0
                            ),
                        ));
                    }

                    let mut value =
                        o.unwrap_or_else(|| self.eval_expression(&default_value.unwrap()).0);

                    if let Value::ReturnValue(val) = value.value {
                        value = *val;
                    }

                    if value.value == Value::Null && !nullable {
                        return value.span.wrap(Value::error(
                            ErrorCode::Type,
                            format!("null is not assignable to {}", value.value.type_of()),
                        ));
                    }

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

                    runtime.set_variable(name.value, (value, None));
                }

                let value = runtime.eval_code_block(body);

                if matches!(value.value, Value::Exception(..)) {
                    return value;
                }

                let value_type = value.value.value_type_of();

                if value_type.is_none()
                    || value_type
                        .as_ref()
                        .is_some_and(|value_type| value_type != &return_type.value)
                {
                    return function.span.wrap(Value::error(
                        ErrorCode::Type,
                        format!(
                            "incorrect return type: {:?} expected, but {:?} given",
                            return_type.value,
                            value_type.unwrap()
                        ),
                    ));
                }

                self.returned(value)
            }
            _ => function.span.wrap(Value::error(
                ErrorCode::Type,
                format!("expected Function, found {}", function.value.type_of()),
            )),
        }
    }
}
