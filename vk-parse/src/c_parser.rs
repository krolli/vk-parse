use core::num::NonZeroU32;
use nom::{
    branch::alt,
    bytes::complete::is_not,
    bytes::complete::{escaped, tag, take_till},
    character::complete::{
        alpha1, alphanumeric1, anychar, char, digit0, hex_digit1, multispace0, multispace1,
        newline, oct_digit1, one_of,
    },
    combinator::{map_res, opt, recognize, value},
    error::ParseError,
    multi::{many0_count, separated_list1},
    number::complete::double,
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    Finish, IResult, Parser,
};
use std::fmt::Display;
use std::str::FromStr;
// Using the C grammer from https://www.open-std.org/jtc1/sc22/wg14/www/docs/n1124.pdf

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.
pub fn ws<'a, F: 'a, O, E: ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: FnMut(&'a str) -> IResult<&'a str, O, E>,
{
    delimited(multispace0, inner, multispace0)
}

// C Type Decleration types
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]

pub enum TypeQualifer {
    Const,
    Restrict,
    Volatile,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]

pub enum TypeIdentifier {
    Plain(String),
    Struct(String),
    Union(String),
    Enum(String),
}
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]

pub enum TypeSpecifier {
    Void,
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
    Identifier(TypeIdentifier),
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]

pub enum Type {
    Void,
    // The signedness of char & int, and short, long, & long long dont't appear at all in the refrence, but keeping for technical correctness
    Char {
        signed: Option<bool>,
    },
    Short {
        signed: Option<bool>,
    },
    Int {
        signed: Option<bool>,
    },
    Long {
        signed: Option<bool>,
    },
    LongLong {
        signed: Option<bool>,
    },
    Float,
    Double,
    ExactInteger {
        bits: u32,
        signed: bool,
    },
    Identifier(TypeIdentifier),
    Pointer {
        is_const: bool,
        pointee_ty: Box<Type>,
    },
    Array(Option<NonZeroU32>, Box<Type>),
    Function {
        return_ty: Box<Type>,
        name: String,
        arg_tys: Vec<Type>,
    },
    Qualified(TypeQualifer, Box<Type>),
}

impl From<TypeSpecifier> for Type {
    fn from(s: TypeSpecifier) -> Self {
        match s {
            TypeSpecifier::Void => Type::Void,
            TypeSpecifier::Char => Type::Char { signed: None },
            TypeSpecifier::Short => Type::Short { signed: None },
            TypeSpecifier::Int => Type::Int { signed: None },
            TypeSpecifier::Long => Type::Long { signed: None },
            TypeSpecifier::Float => Type::Float,
            TypeSpecifier::Double => Type::Double,
            TypeSpecifier::Identifier(ident) => Type::Identifier(ident),
        }
    }
}

fn type_specifier(input: &str) -> IResult<&str, TypeSpecifier> {
    alt((
        value(TypeSpecifier::Void, tag("void")),
        value(TypeSpecifier::Char, tag("char")),
        value(TypeSpecifier::Short, tag("short")),
        value(TypeSpecifier::Int, tag("int")),
        value(TypeSpecifier::Long, tag("long")),
        value(TypeSpecifier::Float, tag("float")),
        value(TypeSpecifier::Double, tag("float")),
        tag("signed").map(|_| todo!()),
        tag("unsigned").map(|_| todo!()),
        tag("_Bool").map(|_| todo!()),
        tag("_Complex").map(|_| todo!()),
        preceded(pair(tag("struct"), multispace0), identifier)
            .map(|i| TypeSpecifier::Identifier(TypeIdentifier::Struct(i.to_string()))),
        preceded(pair(tag("union"), multispace0), identifier)
            .map(|i| TypeSpecifier::Identifier(TypeIdentifier::Union(i.to_string()))),
        preceded(pair(tag("enum"), multispace0), identifier)
            .map(|i| TypeSpecifier::Identifier(TypeIdentifier::Enum(i.to_string()))),
        identifier.map(|i| TypeSpecifier::Identifier(TypeIdentifier::Plain(i.to_string()))),
    ))(input)
}

fn type_qualifier(input: &str) -> IResult<&str, TypeQualifer> {
    alt((
        value(TypeQualifer::Const, tag("const")),
        value(TypeQualifer::Restrict, tag("restrict")),
        value(TypeQualifer::Volatile, tag("volatile")),
    ))(input)
}

enum TypeSpecifierOrQual {
    Specifier(TypeSpecifier),
    Qualifier(TypeQualifer),
}

fn specifier_qualifier_list(input: &str) -> IResult<&str, Vec<TypeSpecifierOrQual>> {
    separated_list1(
        multispace1,
        alt((
            type_specifier.map(TypeSpecifierOrQual::Specifier),
            type_qualifier.map(TypeSpecifierOrQual::Qualifier),
        )),
    )(input)
}

fn direct_abstract_declarator(input: &str) -> IResult<&str, Type> {
    todo!()
}

fn abstract_declarator(input: &str) -> IResult<&str, Type> {
    todo!()
}

fn type_name(input: &str) -> IResult<&str, Type> {
    // todo!()
    alt((type_specifier.map(Type::from),))(input)
}

pub fn parse_ctype(input: &str) -> Result<Type, nom::error::Error<&str>> {
    match type_name(input).finish() {
        Ok((s, ty)) if s.chars().all(|c| c.is_whitespace()) => Ok(ty),
        Ok((s, _)) => panic!("Parsing did not complete {:?}", s),
        Err(e) => Err(e),
    }
}

// C Expression types

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
pub enum FixOrder {
    Prefix,
    Postfix,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]

pub enum ComparisionOp {
    LT,
    LTE,
    Eq,
    NEq,
    GTE,
    GT,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]

pub enum UnaryOp {
    Address,
    Indirection,
    Positive,
    Negative,
    BitwiseNegation,
    LogicalNegation,
    Increment(FixOrder),
    Decrement(FixOrder),
    Cast(Type),
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]

pub enum BinaryOp {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Remainder,
    LeftShift,
    RightShift,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LogicalAnd,
    LogicalOr,
}

#[derive(Debug, Clone, Copy, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]

pub enum Constant {
    Char(u8),
    Integer(u64),
    Float(f64),
}

// SAFETY: Floating point constants must be finite (NaN is a macro)
impl Eq for Constant {}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
pub enum Expression {
    Identifier(String),
    Constant(Constant),
    Literal(String),
    SizeOf(Type),
    Unary(UnaryOp, Box<Expression>),
    Binary(BinaryOp, Box<Expression>, Box<Expression>),
    Comparision(ComparisionOp, Box<Expression>, Box<Expression>),
    Assignment(Option<BinaryOp>, Box<Expression>, Box<Expression>),
    TernaryIfElse(Box<Expression>, Box<Expression>, Box<Expression>),
    FunctionCall(Box<Expression>, Vec<Expression>),
    Comma(Box<Expression>, Box<Expression>),
    Member(Box<Expression>, String),
    PointMember(Box<Expression>, String),
    ArrayElement(Box<Expression>, Box<Expression>),
}

fn wrap_unary((op, e): (UnaryOp, Expression)) -> Expression {
    Expression::Unary(op, Box::new(e))
}

fn wrap_binary((el, op, er): (Expression, BinaryOp, Expression)) -> Expression {
    Expression::Binary(op, Box::new(el), Box::new(er))
}

fn wrap_comparision((el, op, er): (Expression, ComparisionOp, Expression)) -> Expression {
    Expression::Comparision(op, Box::new(el), Box::new(er))
}

pub fn identifier(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0_count(alt((alphanumeric1, tag("_")))),
    ))(input)
}

pub fn literal(input: &str) -> IResult<&str, &str> {
    delimited(
        char('"'),
        escaped(is_not("\""), '\\', one_of(r#""n\"#)),
        char('"'),
    )(input)
}

fn decimal(input: &str) -> IResult<&str, u64> {
    map_res(recognize(pair(one_of("123456789"), digit0)), u64::from_str)(input)
}

fn octal(input: &str) -> IResult<&str, u64> {
    map_res(preceded(char('0'), oct_digit1), |out: &str| {
        u64::from_str_radix(out, 8)
    })(input)
}

fn hexadecimal(input: &str) -> IResult<&str, u64> {
    map_res(
        preceded(alt((tag("0x"), tag("0X"))), hex_digit1),
        |out: &str| u64::from_str_radix(out, 16),
    )(input)
}

fn integer_constant(input: &str) -> IResult<&str, u64> {
    let integer_suffix = alt((
        recognize(pair(one_of("uU"), opt(one_of("lL")))),
        recognize(pair(one_of("uU"), alt((tag("ll"), tag("LL"))))),
        recognize(pair(one_of("lL"), opt(one_of("uU")))),
        recognize(pair(alt((tag("ll"), tag("LL"))), opt(one_of("uU")))),
    ));
    terminated(alt((decimal, octal, hexadecimal, value(0, char('0')))), opt(integer_suffix))(input)
}

pub fn constant(input: &str) -> IResult<&str, Constant> {
    alt((
        integer_constant.map(Constant::Integer),
        terminated(double, one_of("flFL")).map(Constant::Float),
        delimited(char('\''), anychar, char('\'')).map(|c| Constant::Char(c as _)),
    ))(input)
}

pub(crate) fn argument_exp_list(input: &str) -> IResult<&str, Vec<Expression>> {
    separated_list1(terminated(char(','), multispace0), assignment_exp)(input)
}

fn primary_exp(input: &str) -> IResult<&str, Expression> {
    alt((
        identifier.map(|s| Expression::Identifier(s.to_owned())),
        literal.map(|s| Expression::Literal(s.to_owned())),
        constant.map(|c| Expression::Constant(c)),
        delimited(char('('), ws(expr), char(')')),
    ))(input)
}

fn postfix_exp(input: &str) -> IResult<&str, Expression> {
    alt((
        primary_exp,
        separated_pair(
            postfix_exp,
            multispace0,
            delimited(char('['), ws(expr), char(']')),
        )
        .map(|(e1, e2)| Expression::ArrayElement(Box::new(e1), Box::new(e2))),
        separated_pair(
            postfix_exp,
            multispace0,
            delimited(char('('), ws(argument_exp_list), char(')')),
        )
        .map(|(f, ve)| Expression::FunctionCall(Box::new(f), ve)),
        terminated(
            postfix_exp,
            tuple((multispace0, char('('), multispace0, char(')'))),
        )
        .map(|f| Expression::FunctionCall(Box::new(f), Vec::new())),
        separated_pair(postfix_exp, ws(char('.')), identifier)
            .map(|(e, m)| Expression::Member(Box::new(e), m.to_owned())),
        separated_pair(postfix_exp, ws(tag("->")), identifier)
            .map(|(e, m)| Expression::PointMember(Box::new(e), m.to_owned())),
        separated_pair(
            postfix_exp,
            multispace0,
            alt((
                value(UnaryOp::Increment(FixOrder::Postfix), tag("++")),
                value(UnaryOp::Decrement(FixOrder::Postfix), tag("--")),
            )),
        )
        .map(|(e, op)| wrap_unary((op, e))),
    ))(input)
}

fn unary_exp(input: &str) -> IResult<&str, Expression> {
    alt((
        separated_pair(
            alt((
                value(UnaryOp::Increment(FixOrder::Prefix), tag("++")),
                value(UnaryOp::Decrement(FixOrder::Prefix), tag("--")),
            )),
            multispace0,
            unary_exp,
        )
        .map(wrap_unary),
        separated_pair(
            alt((
                value(UnaryOp::Address, char('&')),
                value(UnaryOp::Indirection, char('*')),
                value(UnaryOp::Positive, char('+')),
                value(UnaryOp::Negative, char('-')),
                value(UnaryOp::BitwiseNegation, char('~')),
                value(UnaryOp::LogicalNegation, char('!')),
            )),
            multispace0,
            cast_exp,
        )
        .map(wrap_unary),
        tag("sizeof").map(|_| todo!()),
        postfix_exp,
    ))(input)
}

fn cast_exp(input: &str) -> IResult<&str, Expression> {
    alt((
        separated_pair(
            delimited(char('('), ws(type_name), char(')')).map(UnaryOp::Cast),
            multispace0,
            cast_exp,
        )
        .map(wrap_unary),
        unary_exp,
    ))(input)
}

fn mult_exp(input: &str) -> IResult<&str, Expression> {
    alt((
        tuple((
            cast_exp,
            ws(alt((
                value(BinaryOp::Multiplication, char('*')),
                value(BinaryOp::Division, char('/')),
                value(BinaryOp::Remainder, char('%')),
            ))),
            mult_exp,
        ))
        .map(wrap_binary),
        cast_exp,
    ))(input)
}

fn additive_exp(input: &str) -> IResult<&str, Expression> {
    alt((
        tuple((
            mult_exp,
            ws(alt((
                value(BinaryOp::Addition, char('+')),
                value(BinaryOp::Subtraction, char('-')),
            ))),
            additive_exp,
        ))
        .map(wrap_binary),
        mult_exp,
    ))(input)
}

fn shift_expression(input: &str) -> IResult<&str, Expression> {
    alt((
        tuple((
            additive_exp,
            ws(alt((
                value(BinaryOp::LeftShift, tag("<<")),
                value(BinaryOp::RightShift, tag(">>")),
            ))),
            shift_expression,
        ))
        .map(wrap_binary),
        additive_exp,
    ))(input)
}

fn relational_exp(input: &str) -> IResult<&str, Expression> {
    alt((
        tuple((
            shift_expression,
            ws(alt((
                value(ComparisionOp::LTE, tag("<=")),
                value(ComparisionOp::GTE, tag(">=")),
                value(ComparisionOp::LT, char('<')),
                value(ComparisionOp::GT, char('>')),
            ))),
            relational_exp,
        ))
        .map(wrap_comparision),
        shift_expression,
    ))(input)
}

fn equality_exp(input: &str) -> IResult<&str, Expression> {
    alt((
        tuple((
            relational_exp,
            ws(alt((
                value(ComparisionOp::Eq, tag("==")),
                value(ComparisionOp::NEq, tag("!=")),
            ))),
            equality_exp,
        ))
        .map(wrap_comparision),
        relational_exp,
    ))(input)
}

fn and_exp(input: &str) -> IResult<&str, Expression> {
    alt((
        separated_pair(equality_exp, ws(char('&')), and_exp)
            .map(|(l, r)| Expression::Binary(BinaryOp::BitwiseAnd, Box::new(l), Box::new(r))),
        equality_exp,
    ))(input)
}

fn exclusive_or_exp(input: &str) -> IResult<&str, Expression> {
    alt((
        separated_pair(and_exp, ws(char('^')), exclusive_or_exp)
            .map(|(l, r)| Expression::Binary(BinaryOp::BitwiseXor, Box::new(l), Box::new(r))),
        and_exp,
    ))(input)
}

fn inclusive_or_exp(input: &str) -> IResult<&str, Expression> {
    alt((
        separated_pair(exclusive_or_exp, ws(char('|')), inclusive_or_exp)
            .map(|(l, r)| Expression::Binary(BinaryOp::BitwiseOr, Box::new(l), Box::new(r))),
        exclusive_or_exp,
    ))(input)
}

fn logical_and_exp(input: &str) -> IResult<&str, Expression> {
    alt((
        separated_pair(inclusive_or_exp, ws(tag("&&")), logical_and_exp)
            .map(|(l, r)| Expression::Binary(BinaryOp::LogicalAnd, Box::new(l), Box::new(r))),
        inclusive_or_exp,
    ))(input)
}

fn logical_or_exp(input: &str) -> IResult<&str, Expression> {
    alt((
        separated_pair(logical_and_exp, ws(tag("||")), logical_or_exp)
            .map(|(l, r)| Expression::Binary(BinaryOp::LogicalOr, Box::new(l), Box::new(r))),
        logical_and_exp,
    ))(input)
}

fn conditional_exp(input: &str) -> IResult<&str, Expression> {
    alt((
        tuple((
            logical_or_exp,
            ws(char('?')),
            expr,
            ws(char(':')),
            conditional_exp,
        ))
        .map(|(ecmp, _, etrue, _, efalse)| {
            Expression::TernaryIfElse(Box::new(ecmp), Box::new(etrue), Box::new(efalse))
        }),
        logical_or_exp,
    ))(input)
}

fn assignment_operator(input: &str) -> IResult<&str, Option<BinaryOp>> {
    use BinaryOp::*;

    alt((
        value(None, char('=')),
        value(Some(Addition), tag("+=")),
        value(Some(Subtraction), tag("-=")),
        value(Some(Multiplication), tag("*=")),
        value(Some(Division), tag("/=")),
        value(Some(Remainder), tag("%=")),
        value(Some(LeftShift), tag("<<=")),
        value(Some(RightShift), tag(">>=")),
        value(Some(BitwiseAnd), tag("&=")),
        value(Some(BitwiseOr), tag("|=")),
        value(Some(BitwiseXor), tag("^=")),
        value(Some(LogicalAnd), tag("&&=")),
        value(Some(LogicalOr), tag("||=")),
    ))(input)
}

fn assignment_exp(input: &str) -> IResult<&str, Expression> {
    alt((
        tuple((unary_exp, ws(assignment_operator), assignment_exp))
            .map(|(l, op, r)| Expression::Assignment(op, Box::new(l), Box::new(r))),
        conditional_exp,
    ))(input)
}

pub fn expr(input: &str) -> IResult<&str, Expression> {
    alt((
        separated_pair(assignment_exp, ws(char(',')), expr)
            .map(|(l, r)| Expression::Comma(Box::new(l), Box::new(r))),
        assignment_exp,
    ))(input)
}

pub fn parse_cexpr(input: &str, strict: bool) -> Result<Expression, nom::error::Error<&str>> {
    match expr(input).finish() {
        Ok((s, expr)) if !strict || s.chars().all(|c| c.is_whitespace()) => Ok(expr),
        Ok((s, _)) => panic!("Parsing did not complete {:?}", s),
        Err(e) => Err(e),
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOp::Address => write!(f, "&"),
            UnaryOp::Indirection => write!(f, "*"),
            UnaryOp::Positive => write!(f, "+"),
            UnaryOp::Negative => write!(f, "-"),
            UnaryOp::BitwiseNegation => write!(f, "~"),
            UnaryOp::LogicalNegation => write!(f, "!"),
            UnaryOp::Increment(_) => write!(f, "++"),
            UnaryOp::Decrement(_) => write!(f, "--"),
            UnaryOp::Cast(ty) => write!(f, "({})", ty),
        }
    }
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOp::Addition => write!(f, "+"),
            BinaryOp::Subtraction => write!(f, "-"),
            BinaryOp::Multiplication => write!(f, "*"),
            BinaryOp::Division => write!(f, "/"),
            BinaryOp::Remainder => write!(f, "%"),
            BinaryOp::LeftShift => write!(f, "<<"),
            BinaryOp::RightShift => write!(f, ">>"),
            BinaryOp::BitwiseAnd => write!(f, "&"),
            BinaryOp::BitwiseOr => write!(f, "|"),
            BinaryOp::BitwiseXor => write!(f, "^"),
            BinaryOp::LogicalAnd => write!(f, "&&"),
            BinaryOp::LogicalOr => write!(f, "||"),
        }
    }
}

impl Display for ComparisionOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ComparisionOp::LT => write!(f, "<"),
            ComparisionOp::LTE => write!(f, "<="),
            ComparisionOp::Eq => write!(f, "=="),
            ComparisionOp::NEq => write!(f, "!="),
            ComparisionOp::GTE => write!(f, ">="),
            ComparisionOp::GT => write!(f, ">"),
        }
    }
}

impl Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Constant::Char(c) => write!(f, "'{}'", (*c) as char),
            Constant::Integer(i) => write!(f, "{}", i),
            Constant::Float(n) => write!(f, "{}", n),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Identifier(id) => write!(f, "{}", id),
            Expression::Constant(c) => write!(f, "{}", c),
            Expression::Literal(lit) => write!(f, "{:?}", lit),
            Expression::SizeOf(_) => todo!(),
            Expression::Unary(UnaryOp::Increment(FixOrder::Postfix), e) => write!(f, "{}++", e),
            Expression::Unary(UnaryOp::Decrement(FixOrder::Postfix), e) => write!(f, "{}--", e),
            Expression::Unary(op, e) => write!(f, "{}{}", op, e),
            Expression::Binary(op, l, r) => write!(f, "{} {} {}", l, op, r),
            Expression::Comparision(op, l, r) => write!(f, "{} {} {}", l, op, r),
            Expression::Assignment(_, _, _) => todo!(),
            Expression::TernaryIfElse(cond, et, ef) => write!(f, "{} ? {} : {}", cond, et, ef),
            Expression::FunctionCall(func, args) => {
                write!(f, "{}(", func)?;
                for arg in args.iter() {
                    write!(f, "{}, ", arg)?;
                }
                write!(f, ")")
            }
            Expression::Comma(e0, e1) => write!(f, "{}, {}", e0, e1),
            Expression::Member(s, member) => write!(f, "{}.{}", s, member),
            Expression::PointMember(s, member) => write!(f, "{}->{}", s, member),
            Expression::ArrayElement(e, i) => write!(f, "{}[{}]", e, i),
        }
    }
}

pub fn line_comment(input: &str) -> IResult<&str, &str> {
    delimited(tag("//"), take_till(|c: char| c == '\n'), newline)(input)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_expr() {
        assert_eq!(
            parse_cexpr("2* size", true),
            Ok(Expression::Binary(
                BinaryOp::Multiplication,
                Box::new(Expression::Constant(Constant::Integer(2))),
                Box::new(Expression::Identifier("size".to_string()))
            ))
        )
    }
}
