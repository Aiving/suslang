use std::iter::Enumerate;
use std::ops::Range;
use std::ops::RangeFrom;
use std::ops::RangeFull;
use std::ops::RangeTo;

use nom::InputIter;
use nom::InputLength;
use nom::InputTake;
use nom::Needed;
use nom::Slice;

use crate::globals::BytesSpan;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Illegal,
    EOF,
    Comment(String),

    BraceOpen,
    BraceClose,
    BracketOpen,
    BracketClose,
    ParenOpen,
    ParenClose,

    Ident(String),
    StringLiteral(String),
    ByteLiteral(u8),
    ShortLiteral(i16),
    IntLiteral(i32),
    LongLiteral(i64),
    FloatLiteral(f32),
    BoolLiteral(bool),

    // Reserved words
    Const,
    Let,
    Constructor,
    Null,
    Class,
    Interface,
    Implements,
    This,
    Return,
    Function,
    Public,
    Private,
    Static,
    If,
    Else,
    New,

    Comma,
    Dot,
    Ellipsis,
    Plus,
    Star,
    Slash,
    OrOr,
    PlusPlus,
    Minus,
    MinusMinus,
    EqEq,
    Eq,
    Ne,
    Le,
    Ge,
    Lt,
    Gt,
    Not,
    FatArrow,
    DoubleQuote,
    Colon,
    Semi,
    Question,
    Pound,
}

#[derive(Clone, Copy, PartialEq, Debug, Eq, Hash)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub line: u32,
    pub column: usize,
}

impl Span {
    pub fn between(&self, to: Span) -> Span {
        Span {
            start: self.start,
            end: to.end,
            line: self.line,
            column: self.column,
        }
    }

    pub fn wrap<A>(self, value: A) -> Positioned<A> {
        Positioned { value, span: self }
    }
}

impl From<BytesSpan<'_>> for Span {
    fn from(value: BytesSpan) -> Self {
        Span {
            start: value.location_offset(),
            end: value.location_offset() + value.input_len(),
            line: value.location_line(),
            column: value.naive_get_utf8_column(),
        }
    }
}

#[derive(Clone, PartialEq, Debug, Eq, Hash)]
pub struct Positioned<T> {
    pub value: T,
    pub span: Span,
}

impl<T: Clone> Positioned<T> {
    pub fn new(value: T, span: Span) -> Positioned<T> {
        Positioned { value, span }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ValueType {
    String,
    Byte,
    Short,
    Int,
    Long,
    Boolean,
    Object,
    Array(Box<ValueType>),
    Function(Vec<ValueType>, Box<ValueType>),
    Null,
    Custom(String),
}

#[derive(Clone, Copy, PartialEq, Debug)]
#[repr(C)]
pub struct Tokens<'a> {
    pub tok: &'a [Positioned<Token>],
    pub start: usize,
    pub end: usize,
}

impl<'a> Tokens<'a> {
    pub fn new(vec: &'a [Positioned<Token>]) -> Self {
        Tokens {
            tok: vec,
            start: 0,
            end: vec.len(),
        }
    }
}

impl<'a> From<&'a [Positioned<Token>]> for Tokens<'a> {
    fn from(value: &'a [Positioned<Token>]) -> Self {
        Tokens {
            tok: value,
            start: 0,
            end: value.len(),
        }
    }
}

impl<'a> InputLength for Tokens<'a> {
    #[inline]
    fn input_len(&self) -> usize {
        self.tok.len()
    }
}

impl<'a> InputTake for Tokens<'a> {
    #[inline]
    fn take(&self, count: usize) -> Self {
        Tokens {
            tok: &self.tok[0..count],
            start: 0,
            end: count,
        }
    }

    #[inline]
    fn take_split(&self, count: usize) -> (Self, Self) {
        let (prefix, suffix) = self.tok.split_at(count);
        let first = Tokens {
            tok: prefix,
            start: 0,
            end: prefix.len(),
        };
        let second = Tokens {
            tok: suffix,
            start: 0,
            end: suffix.len(),
        };
        (second, first)
    }
}

impl InputLength for Token {
    #[inline]
    fn input_len(&self) -> usize {
        1
    }
}

impl<'a> Slice<Range<usize>> for Tokens<'a> {
    #[inline]
    fn slice(&self, range: Range<usize>) -> Self {
        Tokens {
            tok: self.tok.slice(range.clone()),
            start: self.start + range.start,
            end: self.start + range.end,
        }
    }
}

impl<'a> Slice<RangeTo<usize>> for Tokens<'a> {
    #[inline]
    fn slice(&self, range: RangeTo<usize>) -> Self {
        self.slice(0..range.end)
    }
}

impl<'a> Slice<RangeFrom<usize>> for Tokens<'a> {
    #[inline]
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        self.slice(range.start..self.end - self.start)
    }
}

impl<'a> Slice<RangeFull> for Tokens<'a> {
    #[inline]
    fn slice(&self, _: RangeFull) -> Self {
        Tokens {
            tok: self.tok,
            start: self.start,
            end: self.end,
        }
    }
}

impl<'a> InputIter for Tokens<'a> {
    type Item = &'a Positioned<Token>;
    type Iter = Enumerate<::std::slice::Iter<'a, Positioned<Token>>>;
    type IterElem = ::std::slice::Iter<'a, Positioned<Token>>;

    #[inline]
    fn iter_indices(&self) -> Enumerate<::std::slice::Iter<'a, Positioned<Token>>> {
        self.tok.iter().enumerate()
    }

    #[inline]
    fn iter_elements(&self) -> ::std::slice::Iter<'a, Positioned<Token>> {
        self.tok.iter()
    }

    #[inline]
    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.tok.iter().position(predicate)
    }

    #[inline]
    fn slice_index(&self, count: usize) -> Result<usize, Needed> {
        if self.tok.len() >= count {
            Ok(count)
        } else {
            Err(Needed::Unknown)
        }
    }
}
