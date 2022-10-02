use core::num::NonZeroU32;
use std::fmt::Display;
// Using the C grammer from https://www.open-std.org/jtc1/sc22/wg14/www/docs/n1124.pdf

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

enum TypeSpecifierOrQual {
    Specifier(TypeSpecifier),
    Qualifier(TypeQualifer),
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
    FunctionCall(Box<Expression>, Box<[Expression]>),
    Comma(Box<[Expression]>),
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

fn comma_expr_or_single(mut v: Vec<Expression>) -> Expression {
    if v.len() == 1 {
        v.pop().unwrap()
    } else {
        Expression::Comma(v.into_boxed_slice())
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
            Expression::Comma(v) => {
                debug_assert!(v.len() > 1);
                write!(f, "{}", &v[0])?;
                for e in &v[1..] {
                    write!(f, ", {}", e)?;
                }
                Ok(())
            }
            Expression::Member(s, member) => write!(f, "{}.{}", s, member),
            Expression::PointMember(s, member) => write!(f, "{}->{}", s, member),
            Expression::ArrayElement(e, i) => write!(f, "{}[{}]", e, i),
        }
    }
}

// C vk.xml text parsing helpers
#[derive(Debug)]
pub enum TypeDefineText1Res {
    Simple(Expression),
    Function(Vec<String>, String),
}

#[derive(Debug, Default, Clone, Copy)]
pub struct ParsedPreTypeTag {
    pub is_const: bool,
    pub is_struct: bool,
}

use crate::PointerKind;

impl ParsedPreTypeTag {
    pub fn fix_ptr_kind(&self, ptr_kind: Option<PointerKind>) -> Option<PointerKind> {
        match (self.is_const, ptr_kind) {
            (is_const, Some(PointerKind::Single { .. })) => Some(PointerKind::Single { is_const }),
            (is_const, Some(PointerKind::Double { inner_is_const, .. })) => {
                Some(PointerKind::Double {
                    is_const,
                    inner_is_const,
                })
            }
            (true, None) => unreachable!(),
            (false, None) => None,
        }
    }
}

peg::parser! {
    pub grammar c_with_vk_ext() for str {
        use crate::PointerKind;

        pub rule line_comment() = "//" [^ '\n']* ("\n" / ![_])

        rule _() = ([' ' | '\t' | '\n' | '\r']+ / line_comment())*

        rule __() = ([' ' | '\t' | '\n' | '\r']+ / line_comment())+

        pub rule identifier() -> String
          = i:$(quiet!{[ '_' | 'a'..='z' | 'A'..='Z'] ['_' | 'a'..='z' | 'A'..='Z' | '0'..='9']*}) { i.to_owned() }
          / expected!("identifier")

        pub rule type_name() -> Type
          = i:identifier() { Type::Identifier(TypeIdentifier::Plain(i)) }


        // C-expr rules

        rule zero() -> u64 = "0" { 0 }

        rule decimal() -> u64
          = n:$(quiet!{['1'..='9']['0'..='9']*}) {? n.parse().or(Err("u64")) }
          / expected!("decimal constant")

        rule octal() -> u64
          = "0" n:$(['0'..='7']+) {? u64::from_str_radix(n, 8).or(Err("u64")) }

        rule hexadecimal() -> u64
          = "0[xX]" n:$(['0'..='9'|'a'..='f'|'A'..='F']+) {? u64::from_str_radix(n, 16).or(Err("u64")) }

        rule integer_constant() -> u64
          = n:(octal() / decimal() / hexadecimal() / zero()) ['u' | 'U' | 'l' | 'L' ]* { n }

        rule float_constant() -> f64
          = n:$(['0'..='9']+ ("." ['0'..='9']*)?) ['f' | 'F' | 'l' | 'L' ]? {? n.parse().or(Err("f64")) }

        rule char_constant() -> u8
          = "L"? "'" c:$("\\.|[^\\']"+) "'" {? c.parse::<char>().map(|c| c as u8).or(Err("u64")) }

        pub rule constant() -> Constant
          = (n:integer_constant() {Constant::Integer(n)}) / (n:float_constant() {Constant::Float(n)}) / (c:char_constant() {Constant::Char(c)})

        rule literal() -> String
          = "L"? "\"" s:$("\\.|[^\\\"]"*) "\"" { s.to_owned() }

        rule primary_expr() -> Expression
          = i:identifier() { Expression::Identifier(i) }
          / l:literal() { Expression::Literal(l) }
          / c:constant() { Expression::Constant(c) }
          / "(" _ e:expr() _ ")" { e }

        rule arithmetic() -> Expression = precedence!{
          x:@ _ "=" _ y:(@) { Expression::Assignment(None, Box::new(x), Box::new(y)) }
          x:@ _ "+=" _ y:(@) { Expression::Assignment(Some(BinaryOp::Addition), Box::new(x), Box::new(y)) }
          x:@ _ "-=" _ y:(@) { Expression::Assignment(Some(BinaryOp::Subtraction), Box::new(x), Box::new(y)) }
          x:@ _ "*=" _ y:(@) { Expression::Assignment(Some(BinaryOp::Multiplication), Box::new(x), Box::new(y)) }
          x:@ _ "/=" _ y:(@) { Expression::Assignment(Some(BinaryOp::Division), Box::new(x), Box::new(y)) }
          x:@ _ "%=" _ y:(@) { Expression::Assignment(Some(BinaryOp::Remainder), Box::new(x), Box::new(y)) }
          x:@ _ "<<=" _ y:(@) { Expression::Assignment(Some(BinaryOp::LeftShift), Box::new(x), Box::new(y)) }
          x:@ _ ">>=" _ y:(@) { Expression::Assignment(Some(BinaryOp::RightShift), Box::new(x), Box::new(y)) }
          x:@ _ "&=" _ y:(@) { Expression::Assignment(Some(BinaryOp::BitwiseAnd), Box::new(x), Box::new(y)) }
          x:@ _ "|=" _ y:(@) { Expression::Assignment(Some(BinaryOp::BitwiseOr), Box::new(x), Box::new(y)) }
          x:@ _ "^=" _ y:(@) { Expression::Assignment(Some(BinaryOp::BitwiseXor), Box::new(x), Box::new(y)) }
          x:@ _ "&&=" _ y:(@) { Expression::Assignment(Some(BinaryOp::LogicalAnd), Box::new(x), Box::new(y)) }
          x:@ _ "||=" _ y:(@) { Expression::Assignment(Some(BinaryOp::LogicalOr), Box::new(x), Box::new(y)) }
          --
          x:@ _ "?" _ y:expr() _ ":" _ z:(@) { Expression::TernaryIfElse(Box::new(x), Box::new(y), Box::new(z)) }
          --
          x:(@) _ "||" _ y:@ { wrap_binary((x, BinaryOp::LogicalOr, y)) }
          --
          x:(@) _ "&&" _ y:@ { wrap_binary((x, BinaryOp::LogicalAnd, y)) }
          --
          x:(@) _ "|" _ y:@ { wrap_binary((x, BinaryOp::BitwiseOr, y)) }
          --
          x:(@) _ "^" _ y:@ { wrap_binary((x, BinaryOp::BitwiseXor, y)) }
          --
          x:(@) _ "&" _ y:@ { wrap_binary((x, BinaryOp::BitwiseAnd, y)) }
          --
          x:(@) _ "==" _ y:@ { wrap_comparision((x, ComparisionOp::Eq, y)) }
          x:(@) _ "!=" _ y:@ { wrap_comparision((x, ComparisionOp::NEq, y)) }
          --
          x:(@) _ "<=" _ y:@ { wrap_comparision((x, ComparisionOp::LTE, y)) }
          x:(@) _ ">=" _ y:@ { wrap_comparision((x, ComparisionOp::GTE, y)) }
          x:(@) _ "<" _ y:@ { wrap_comparision((x, ComparisionOp::LT, y)) }
          x:(@) _ ">" _ y:@ { wrap_comparision((x, ComparisionOp::LT, y)) }
          --
          x:(@) _ "<<" _ y:@ { wrap_binary((x, BinaryOp::LeftShift, y)) }
          x:(@) _ ">>" _ y:@ { wrap_binary((x, BinaryOp::RightShift, y)) }
          --
          x:(@) _ "+" _ y:@ { wrap_binary((x, BinaryOp::Addition, y)) }
          x:(@) _ "-" _ y:@ { wrap_binary((x, BinaryOp::Subtraction, y)) }
          --
          x:(@) _ "*" _ y:@ { wrap_binary((x, BinaryOp::Multiplication, y)) }
          x:(@) _ "/" _ y:@ { wrap_binary((x, BinaryOp::Division, y)) }
          x:(@) _ "%" _ y:@ { wrap_binary((x, BinaryOp::Remainder, y)) }
          --
          "(" ty:type_name() ")" _ x:(@) { wrap_unary((UnaryOp::Cast(ty), x)) }
          --
          "&" _ x:(@) { wrap_unary((UnaryOp::Address, x)) }
          "*" _ x:(@) { wrap_unary((UnaryOp::Indirection, x)) }
          "+" _ x:(@) { wrap_unary((UnaryOp::Positive, x)) }
          "-" _ x:(@) { wrap_unary((UnaryOp::Negative, x)) }
          "~" _ x:(@) { wrap_unary((UnaryOp::BitwiseNegation, x)) }
          "!" _ x:(@) { wrap_unary((UnaryOp::LogicalNegation, x)) }
          "++" _ x:(@) { wrap_unary((UnaryOp::Increment(FixOrder::Prefix), x)) }
          "--" _ x:(@) { wrap_unary((UnaryOp::Decrement(FixOrder::Prefix), x)) }
          --
          x:(@) "." y:identifier() { Expression::Member(Box::new(x), y) }
          x:(@) "->" y:identifier() { Expression::PointMember(Box::new(x), y) }
          x:(@) "[" _ y:expr() _ "]" { Expression::ArrayElement(Box::new(x), Box::new(y)) }
          x:(@) "(" _ y:(arithmetic() ** (_ "," _)) _ ("," _)? ")" { Expression::FunctionCall(Box::new(x), y.into_boxed_slice()) }
          x:(@) "++" { wrap_unary((UnaryOp::Increment(FixOrder::Postfix), x)) }
          x:(@) "--" { wrap_unary((UnaryOp::Decrement(FixOrder::Postfix), x)) }
          --
          p:primary_expr() { p }
        }

        pub rule expr() -> Expression
          = v:(arithmetic() ++ (_ "," _)) { comma_expr_or_single(v) }


        // C vk.xml exts
        rule define_macro_value() -> String
          = l:$([^ '\\' | '\n']*) ** ("\\" [' ' | '\t' | '\r']* "\n") { l.concat() }

        /// handle const_ptr / struct info, can be 'const', 'struct', or 'const struct'
        pub rule pre_type_tag_text() -> ParsedPreTypeTag
            = _ c:"const"? _ s:"struct"? _ { ParsedPreTypeTag { is_const: c.is_some(), is_struct: s.is_some() } }

        // handle pointer info, can be '*' or '**' or '* const*'
        pub rule post_type_tag_text() -> Option<PointerKind>
            = _ p:("*" _ b:(c:"const"? _ "*" { c })? { b })? {
                match p {
                    Some(None) => Some(PointerKind::Single { is_const: false }),
                    Some(Some(None)) => Some(PointerKind::Double { is_const: false, inner_is_const: false }),
                    Some(Some(Some(()))) => Some(PointerKind::Double { is_const: false, inner_is_const: true }),
                    None => None,
                }
            }

        /// parses the text between <type category="define"> ... <name>
        pub rule type_define_text_0()
          = _ "#define" __

        /// parses the text between </name> ... (<type> or </type>)
        pub rule type_define_text_1() -> TypeDefineText1Res
          = __ e:expr() _ { TypeDefineText1Res::Simple(e) }
          / "(" _ args:(identifier() ** (_ "," _)) _ ("," _)? ")" [' ' | '\t' | '\r']+ v:define_macro_value() { TypeDefineText1Res::Function(args, v) }

        /// parses the text between </type>....</type>
        pub rule type_define_text_2() -> Box<[Expression]>
          = "(" _ v:(arithmetic() ** (_ "," _)) _ ("," _)? ")" _ { v.into_boxed_slice() }


        /// parses the text between <type category="funcptr"> ... <name>
        pub rule type_funcptr_text_0() -> (String, Option<PointerKind>)
          = _ "typedef" __ id:identifier() _ ptr:"*"? _ "(" _ "VKAPI_PTR" _ "*" _ { (id, ptr.map(|()| PointerKind::Single { is_const: false })) }
        //   = _ "typedef" __ rty:type_name() _ "(" _ "VKAPI_PTR" _ "*" _ { rty }

        /// parses the text between </name> ... (<type> or </type>)
        pub rule type_funcptr_text_args_start() -> Option<ParsedPreTypeTag>
          = _ ")" _ "(" _ v:("void" _ ")" _ ";" _ { None } / p:pre_type_tag_text() {Some(p)} ) { v }

        pub rule type_funcptr_text_inter_args() -> (Option<PointerKind>, String, Option<ParsedPreTypeTag>)
          = _ post:post_type_tag_text() _ name:identifier() _ pre:( "," p:pre_type_tag_text() {Some(p)} / ")" _ ";" _ { None } ) { (post, name, pre) }
  }
}

pub use c_with_vk_ext::expr as parse_cexpr;

#[cfg(test)]
mod test_peg {
    use super::{
        c_with_vk_ext::{type_define_text_1, type_define_text_2},
        *,
    };

    #[test]
    fn test_expr() {
        assert_eq!(
            parse_cexpr("2* size").unwrap(),
            Expression::Binary(
                BinaryOp::Multiplication,
                Box::new(Expression::Constant(Constant::Integer(2))),
                Box::new(Expression::Identifier("size".to_string()))
            )
        )
    }

    #[test]
    fn test_rasterization_sample_expr() {
        assert_eq!(
            parse_cexpr("(rasterizationSamples + 31) / 32").unwrap(),
            Expression::Binary(
                BinaryOp::Division,
                Box::new(Expression::Binary(
                    BinaryOp::Addition,
                    Box::new(Expression::Identifier("rasterizationSamples".to_string())),
                    Box::new(Expression::Constant(Constant::Integer(31))),
                )),
                Box::new(Expression::Constant(Constant::Integer(32))),
            )
        )
    }

    #[test]
    fn test_vk_macro_fn_define() {
        let res = type_define_text_1("(major, minor, patch) \\\n    ((((uint32_t)(major)) << 22) | (((uint32_t)(minor)) << 12) | ((uint32_t)(patch)))").unwrap();
        match res {
            TypeDefineText1Res::Simple(_) => panic!(),
            TypeDefineText1Res::Function(args, value) => {
                assert_eq!(args.as_slice(), &["major", "minor", "patch"]);
                assert_eq!(value.trim_start(), "((((uint32_t)(major)) << 22) | (((uint32_t)(minor)) << 12) | ((uint32_t)(patch)))");
            }
        }
    }

    #[test]
    fn test_vk_macro_fn_define_cast() {
        let res =
            type_define_text_2("(1, 0, 0) // Patch version should always be set to 0").unwrap();
        assert_eq!(
            &*res,
            &[
                Expression::Constant(Constant::Integer(1)),
                Expression::Constant(Constant::Integer(0)),
                Expression::Constant(Constant::Integer(0))
            ]
        )
    }
}
