use crate::lexer::token::Positioned;
use crate::lexer::token::ValueType;

use crate::parser::ast::ClassStatement;

use crate::runtime::value::ErrorCode;
use crate::runtime::value::Value;
use crate::runtime::Runtime;

impl Runtime {
    pub fn eval_class(&self, class: Positioned<ClassStatement>) -> Positioned<Value> {
        let mut fields = vec![];
        let constructors = class
            .value
            .constructor
            .iter()
            .map(|constructor| self.eval_expression(constructor).0)
            .collect::<Vec<_>>();
        let methods = class
            .value
            .methods
            .iter()
            .map(|method| self.eval_expression(method).0)
            .collect::<Vec<_>>();

        for field in class.value.properties {
            let default_value = field
                .value
                .default_value
                .as_ref()
                .map(|value| self.eval_expression(value).0);

            if default_value
                .as_ref()
                .is_some_and(|value| matches!(value.value, Value::Exception(..)))
            {
                return default_value.unwrap();
            }

            if default_value.is_none() && field.value.ty.is_none() {
                return field.span.wrap(Value::error(
                    ErrorCode::Type,
                    "can't detect type from nothing",
                ));
            } else if default_value
                .as_ref()
                .is_some_and(|value| value.value.is_type_of(&ValueType::Null))
                && field.value.ty.is_none()
            {
                return field
                    .span
                    .wrap(Value::error(ErrorCode::Type, "can't detect type from null"));
            }

            if default_value
                .as_ref()
                .is_some_and(|value| value.value.is_type_of(&ValueType::Null))
                && !field.value.nullable
            {
                return field.span.wrap(Value::error(
                    ErrorCode::Type,
                    "null value in non-nullable field",
                ));
            }

            let ty = field.value.ty.unwrap_or_else(|| {
                let value = default_value.as_ref().unwrap();

                value.span.wrap(value.value.value_type_of().unwrap())
            });

            fields.push(field.span.wrap((
                field.value.access_flags,
                field.value.name,
                ty,
                field.value.nullable,
                default_value,
            )));
        }

        for implement in &class.value.implements {
            let interface = self.get_variable(implement);

            if let Value::Exception(..) = interface.0.value {
                return interface.0;
            }

            if let Value::Interface(_, interface_fields) = interface.0.value {
                for interface_field in interface_fields {
                    let field = fields
                        .iter()
                        .find(|field| field.value.1.value == interface_field.value.key.value);

                    if let Some(field) = field {
                        let (_, _, ty, nullable, _) = &field.value;

                        if interface_field.value.nullable && !nullable {
                            return field.span.wrap(Value::error(
                                ErrorCode::Implementing,
                                format!(
                                    "class does not correctly implements \"{}\": field type is not nullable",
                                    interface_field.value.key.value.0
                                ),
                            ));
                        } else if interface_field.value.ty.value != ty.value {
                            return ty.span.wrap(Value::error(
                                ErrorCode::Implementing,
                                format!(
                                        "class does not correctly implements \"{}\": field have incorrect type",
                                        interface_field.value.key.value.0
                                    ),
                                ));
                        }
                    } else {
                        return class.value.name.span.wrap(Value::error(
                            ErrorCode::Implementing,
                            format!(
                                "class does not implements \"{}\"",
                                interface_field.value.key.value.0
                            ),
                        ));
                    }
                }
            } else {
                return interface.0.span.wrap(Value::error(
                    ErrorCode::Implementing,
                    "only interfaces can be used as implementations",
                ));
            }
        }

        self.set_variable(
            class.value.name.value.clone(),
            (
                if fields.is_empty() {
                    class.value.name.span
                } else {
                    class.value.name.span.between(fields.last().unwrap().span)
                }
                .wrap(Value::Class(
                    class.value.name,
                    class.value.generics,
                    class.value.implements,
                    constructors,
                    methods,
                    fields,
                )),
                None,
            ),
        )
    }
}
