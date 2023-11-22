use std::collections::HashMap;

use std::fmt::Display;
use std::fmt::Formatter;

use std::hash::Hash;
use std::hash::Hasher;

use owo_colors::colors;
use owo_colors::Color;

use crate::lexer::token::Positioned;
use crate::lexer::token::Token;
use crate::lexer::token::ValueType;

use crate::parser::ast::FunctionArgument;
use crate::parser::ast::Ident;
use crate::parser::ast::InterfaceField;
use crate::parser::ast::Literal;
use crate::parser::ast::Program;

type ClassField = (
    Vec<Positioned<Token>>,
    Positioned<Ident>,
    Positioned<ValueType>,
    bool,
    Option<Positioned<Value>>,
);

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum ErrorCode {
    Type = 0x1250,
    Reference = 0x1350,
    Implementing = 0x1450
}

#[derive(PartialEq, Clone, Debug)]
pub enum Value {
    Array(Vec<Positioned<Value>>),
    Object(HashMap<Positioned<Value>, Positioned<Value>>),
    Byte(u8),
    Short(i16),
    Int(i32),
    Long(i64),
    Float(f32),
    Boolean(bool),
    String(String),
    ReturnValue(Box<Positioned<Value>>),
    Constructor(
        Vec<Positioned<Value>>,
        Vec<Positioned<Token>>,
        Positioned<Ident>,
        Positioned<Vec<Positioned<FunctionArgument>>>,
        Positioned<Program>,
    ),
    Function(
        Vec<Positioned<Value>>,
        Vec<Positioned<Token>>,
        Positioned<Ident>,
        Positioned<ValueType>,
        Positioned<Vec<Positioned<FunctionArgument>>>,
        Positioned<Program>,
    ),
    Interface(Positioned<Ident>, Vec<Positioned<InterfaceField>>),
    Class(
        Positioned<Ident>,
        Vec<Positioned<Ident>>,
        Vec<Positioned<Ident>>,
        Vec<Positioned<Value>>,
        Vec<Positioned<Value>>,
        Vec<Positioned<ClassField>>,
    ),
    LinearFunction(Positioned<Vec<Positioned<Ident>>>, Positioned<Program>),
    Null,
    None,
    Exception(ErrorCode, String),
}

impl Value {
    pub fn error<T: Into<String>>(code: ErrorCode, message: T) -> Self {
        Self::Exception(code, message.into())
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Array(elements) => {
                write!(f, "Array({}) [", elements.len())?;

                for element in elements {
                    element.value.fmt(f)?;

                    write!(f, ",")?;
                }

                write!(f, "]")
            }
            Value::Object(properties) => {
                writeln!(f, "Object({}) {{", properties.len())?;

                for (name, value) in properties {
                    write!(f, "  ")?;

                    name.value.fmt(f)?;

                    write!(f, ": ")?;

                    value.value.fmt(f)?;

                    writeln!(f, ",")?;
                }

                write!(f, "}}")
            }
            Value::Byte(byte) => write!(f, "{byte}"),
            Value::Short(short) => write!(f, "{short}"),
            Value::Int(int) => write!(f, "{int}"),
            Value::Long(long) => write!(f, "{long}"),
            Value::Float(float) => write!(f, "{float}"),
            Value::Boolean(boolean) => write!(f, "{boolean}"),
            Value::String(string) => write!(f, r#""{string}""#),
            Value::ReturnValue(value) => value.value.fmt(f),
            Value::Function(.., name, return_type, arguments, _) => write!(
                f,
                "[Function {}({}) => {:?}]",
                name.value.0,
                arguments.value.len(),
                return_type.value
            ),
            Value::Interface(..) => todo!(),
            Value::Class(..) => todo!(),
            Value::Null => write!(f, "null"),
            Value::None => write!(f, ""),
            Value::Exception(name, message) => write!(f, "[{name:?}]: {message}"),
            Value::Constructor(.., name, arguments, _) => write!(
                f,
                "[Constructor {}({})]",
                name.value.0,
                arguments.value.len(),
            ),
            Value::LinearFunction(args, _) => {
                write!(f, "[Function anonymous({})]", args.value.len())
            }
        }
    }
}

impl Display for Positioned<Value> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.value {
            Value::Exception(name, message) => write!(
                f,
                "==> src/source.sl[{}:{}]\n{name:?}: {message}",
                self.span.line, self.span.column
            ),
            _ => self.value.fmt(f),
        }
    }
}

impl Positioned<Value> {
    pub fn format<T: Into<String>>(&self, file: T, source: T) -> String {
        let file: String = file.into();
        let source: String = source.into();

        match &self.value {
            Value::Exception(name, message) => format!(
                "==> {red}{name:?}{reset} at {green}{file}:{line}:{column}{reset}\n{gray}{pline} |{reset}\n{gray}{line} |{reset} {data}\n{gray}{nline} |{reset}{nspace}{red}|{reset}\n{space}{red}|{reset}\n{space}{red}|{reset}\n{space}{red}|{reset}\n{space}{red}{message}{reset}",
                pline = self.span.line - 1,
                line = self.span.line,
                nline = self.span.line + 1,
                column = self.span.column,
                data = source.lines().collect::<Vec<_>>()[(self.span.line - 1) as usize],
                space = " ".repeat(self.span.column + 4),
                nspace = " ".repeat(self.span.column + 2 - (self.span.line.to_string().len())),
                gray = colors::css::DimGray::ANSI_FG,
                green = colors::css::LightSeaGreen::ANSI_FG,
                red = colors::css::IndianRed::ANSI_FG,
                reset = colors::Default::ANSI_FG
            ),
            _ => format!("{}", self.value),
        }
    }
}

impl Value {
    pub fn is_returned(&self) -> bool {
        matches!(self, Value::ReturnValue(_))
    }

    pub fn is_type_of(&self, ty: &ValueType) -> bool {
        match ty {
            ValueType::String => matches!(self, Value::String(_)),
            ValueType::Byte => matches!(self, Value::Byte(_)),
            ValueType::Short => matches!(self, Value::Short(_)),
            ValueType::Int => matches!(self, Value::Int(_)),
            ValueType::Long => matches!(self, Value::Long(_)),
            ValueType::Boolean => matches!(self, Value::Boolean(_)),
            ValueType::Object => matches!(self, Value::Object(_)),
            ValueType::Array(ty) => {
                if let Value::Array(elements) = self {
                    elements.iter().all(|element| element.value.is_type_of(ty))
                } else {
                    false
                }
            }
            ValueType::Null => matches!(self, Value::Null),
            ValueType::Custom(_) => true,
            ValueType::Function(..) => matches!(self, Value::LinearFunction(..)),
        }
    }

    pub fn value_type_of(&self) -> Option<ValueType> {
        match self {
            Value::Array(elements) => Some(ValueType::Array(Box::new(
                elements[0].value.value_type_of().unwrap(),
            ))),
            Value::Object(_) => Some(ValueType::Object),
            Value::Byte(_) => Some(ValueType::Byte),
            Value::Short(_) => Some(ValueType::Short),
            Value::Int(_) => Some(ValueType::Int),
            Value::Long(_) => Some(ValueType::Long),
            Value::Float(_) => Some(ValueType::Custom("float".into())),
            Value::Boolean(_) => Some(ValueType::Boolean),
            Value::String(_) => Some(ValueType::String),
            Value::ReturnValue(value) => value.value.value_type_of(),
            Value::Function(..) => Some(ValueType::Custom("Function".into())),
            Value::Interface(..) => Some(ValueType::Custom("Interface".into())),
            Value::Class(..) => Some(ValueType::Custom("Class".into())),
            Value::Null => Some(ValueType::Null),
            Value::None => None,
            Value::Exception(..) => Some(ValueType::Custom("Exception".into())),
            Value::Constructor(..) => Some(ValueType::Custom("Constructor".into())),
            Value::LinearFunction(..) => Some(ValueType::Custom("LinearFunction".into())),
        }
    }

    pub fn type_of(&self) -> &str {
        match self {
            Value::Array(_) => "array",
            Value::Object(_) => "object",
            Value::Byte(_) => "byte",
            Value::Short(_) => "short",
            Value::Int(_) => "int",
            Value::Long(_) => "long",
            Value::Float(_) => "float",
            Value::Boolean(_) => "boolean",
            Value::String(_) => "string",
            Value::ReturnValue(_) => "ReturnValue",
            Value::Function(..) => "Function",
            Value::Interface(..) => "Interface",
            Value::Class(..) => "Class",
            Value::Null => "null",
            Value::None => "",
            Value::Exception(..) => "Exception",
            Value::Constructor(..) => "Constructor",
            Value::LinearFunction(..) => "LinearFunction",
        }
    }
}

impl Eq for Value {}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Value::Byte(byte) => byte.hash(state),
            Value::Short(short) => short.hash(state),
            Value::Int(int) => int.hash(state),
            Value::Long(long) => long.hash(state),
            Value::Boolean(boolean) => boolean.hash(state),
            Value::String(string) => string.hash(state),
            _ => "".hash(state),
        }
    }
}

impl From<Positioned<Literal>> for Positioned<Value> {
    fn from(literal: Positioned<Literal>) -> Self {
        match literal.value {
            Literal::String(string) => literal.span.wrap(Value::String(string)),
            Literal::Byte(byte) => literal.span.wrap(Value::Byte(byte)),
            Literal::Short(short) => literal.span.wrap(Value::Short(short)),
            Literal::Int(int) => literal.span.wrap(Value::Int(int)),
            Literal::Long(long) => literal.span.wrap(Value::Long(long)),
            Literal::Float(float) => literal.span.wrap(Value::Float(float)),
            Literal::Bool(boolean) => literal.span.wrap(Value::Boolean(boolean)),
        }
    }
}
