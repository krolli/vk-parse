use core::num::{NonZeroU32, NonZeroU8};
use std::fmt;
use std::{borrow::Cow, fmt::Display, ops::Deref};
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
    Specifier(TypeSpecifier),
    Pointer {
        pointee_ty: Box<Type>,
    },
    Array(Box<Type>, Option<NonZeroU32>),
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

use logos::Lexer;

use crate::c_lexer::{Constant, Token};

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

impl Display for TypeIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeIdentifier::Plain(ident) => write!(f, "{}", ident),
            TypeIdentifier::Struct(ident) => write!(f, "struct {}", ident),
            TypeIdentifier::Union(ident) => write!(f, "union {}", ident),
            TypeIdentifier::Enum(ident) => write!(f, "enum {}", ident),
        }
    }
}

impl Display for TypeSpecifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeSpecifier::Void => write!(f, "void"),
            TypeSpecifier::Char => write!(f, "char"),
            TypeSpecifier::Short => write!(f, "short"),
            TypeSpecifier::Int => write!(f, "int"),
            TypeSpecifier::Long => write!(f, "long"),
            TypeSpecifier::Float => write!(f, "float"),
            TypeSpecifier::Double => write!(f, "double"),
            TypeSpecifier::Identifier(ident) => write!(f, "{}", ident),
        }
    }
}

impl Display for Type {
    fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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

#[derive(Debug, Clone, PartialEq)]
pub enum VkXMLToken<'a> {
    C(Token<'a>),
    TextTag {
        name: Cow<'a, str>,
        text: Cow<'a, str>,
    },
}

impl<'a> fmt::Display for VkXMLToken<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VkXMLToken::C(token) => write!(f, "{}", token),
            VkXMLToken::TextTag { name, text } => write!(f, "<{0}>{1}</{0}>", name, text),
        }
    }
}

#[derive(Debug, Clone)]
pub struct VkXMLTokens<'s, 'a>(pub Cow<'s, [VkXMLToken<'a>]>);

impl<'s, 'a> Deref for VkXMLTokens<'s, 'a> {
    type Target = [VkXMLToken<'a>];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a> FromIterator<VkXMLToken<'a>> for VkXMLTokens<'static, 'a> {
    fn from_iter<T: IntoIterator<Item = VkXMLToken<'a>>>(iter: T) -> Self {
        VkXMLTokens(Cow::Owned(iter.into_iter().collect()))
    }
}

impl<'s, 'a> peg::Parse for VkXMLTokens<'s, 'a> {
    type PositionRepr = usize;
    fn start(&self) -> usize {
        0
    }

    fn is_eof(&self, pos: usize) -> bool {
        pos >= self.len()
    }

    fn position_repr(&self, pos: usize) -> usize {
        pos
    }
}

impl<'input: 's, 's, 'a> peg::ParseElem<'input> for VkXMLTokens<'s, 'a> {
    type Element = &'s VkXMLToken<'a>;

    fn parse_elem(&'input self, pos: usize) -> peg::RuleResult<Self::Element> {
        match self[pos..].first() {
            Some(c) => peg::RuleResult::Matched(pos + 1, c),
            None => peg::RuleResult::Failed,
        }
    }
}

impl<'s, 'a> peg::ParseLiteral for VkXMLTokens<'s, 'a> {
    fn parse_string_literal(&self, pos: usize, literal: &str) -> peg::RuleResult<()> {
        if let Some(VkXMLToken::C(tok)) = self.get(pos) {
            let literal_token =
                Token::from_literal(literal).unwrap_or_else(|| panic!("I'm dum {:?}", literal));
            if &literal_token == tok {
                peg::RuleResult::Matched(pos + 1, ())
            } else {
                peg::RuleResult::Failed
            }
        } else {
            peg::RuleResult::Failed
        }
    }
}

impl<'input, 's, 'a: 'input> peg::ParseSlice<'input> for VkXMLTokens<'s, 'a> {
    type Slice = &'input [VkXMLToken<'a>];
    fn parse_slice(&'input self, p1: usize, p2: usize) -> Self::Slice {
        &self[p1..p2]
    }
}

// C vk.xml text parsing helpers
use crate::ArrayLength;

#[derive(Debug)]
enum BitFieldSizeOrArrayshape {
    BitfieldSize(NonZeroU8),
    ArrayShape(Vec<ArrayLength>),
}

#[derive(Debug, Default, Clone, Copy)]
struct ParsedPreTypeTag {
    pub is_const: bool,
    pub is_struct: bool,
}

peg::parser! {
    pub grammar c_with_vk_ext<'s, 'a>() for VkXMLTokens<'s, 'a> {
        use crate::PointerKind;
        use crate::TypeDefine;
        use crate::TypeDefineValue;
        use crate::NameWithType;
        use crate::TypeFunctionPointer;

        pub rule line_comment() = [VkXMLToken::C(Token::Comment)] "\n"?

        rule _() = [VkXMLToken::C(Token::Whitespace | Token::NewLine | Token::Comment)]*

        rule __() = [VkXMLToken::C(Token::Whitespace | Token::NewLine | Token::Comment)]*

        pub rule identifier() -> String
          = i:[VkXMLToken::C(Token::Identifier(i))] { i.to_string() }

        rule type_identifier() -> TypeIdentifier
          = "struct" i:identifier() { TypeIdentifier::Struct(i) }
          / "union" i:identifier() { TypeIdentifier::Union(i) }
          / "enum" i:identifier() { TypeIdentifier::Enum(i) }
          / i:identifier() { TypeIdentifier::Plain(i) }

        rule type_specifier() -> TypeSpecifier
          = i:type_identifier() { TypeSpecifier::Identifier(i) }
          / "void" { TypeSpecifier::Void }
          / "char" { TypeSpecifier::Char }
          / "short" { TypeSpecifier::Short }
          / "int" { TypeSpecifier::Int }
          / "long" { TypeSpecifier::Long }
          / "float" { TypeSpecifier::Float }
          / "double" { TypeSpecifier::Double }

        pub rule type_name() -> Type
          = ty:type_specifier() { Type::Specifier(ty) }


        // C-expr rules

        pub rule constant() -> Constant
          = c:[VkXMLToken::C(Token::Constant(c))] { c.clone() }

        rule literal() -> String
          = l:[VkXMLToken::C(Token::Literal(l))] { l.to_string() }

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
          = l:$([^ VkXMLToken::C(Token::BackSlash | Token::NewLine)]*) ** ("\\" [VkXMLToken::C(Token::Whitespace)]* "\n") { l.into_iter().flat_map(|l| l.iter().map(|v| v.to_string())).collect() }

        /// handle const_ptr / struct info, can be 'const', 'struct', or 'const struct'
        rule pre_type_tag_text() -> ParsedPreTypeTag
            = _ c:"const"? _ s:"struct"? _ { ParsedPreTypeTag { is_const: c.is_some(), is_struct: s.is_some() } }

        // handle pointer info, can be '*' or '**' or '* const*'
        rule post_type_tag_text() -> Option<PointerKind>
            = _ p:("*" _ b:(c:"const"? _ "*" { c })? { b })? {
                match p {
                    Some(None) => Some(PointerKind::Single),
                    Some(Some(None)) => Some(PointerKind::Double { inner_is_const: false }),
                    Some(Some(Some(()))) => Some(PointerKind::Double { inner_is_const: true }),
                    None => None,
                }
            }

        rule typed_tag<T: Display>(name_text: rule<T>) -> NameWithType
            = pre:pre_type_tag_text() type_name:([VkXMLToken::TextTag { name, text } if name == "type"] { text }) pointer_kind:post_type_tag_text() name:name_text() {
                NameWithType {
                    type_name: type_name.to_string(),
                    pointer_kind,
                    is_const: pre.is_const,
                    is_struct: pre.is_struct,
                    name: name.to_string(),
                    ..NameWithType::default()
                }
            }

        rule name_tag() -> Cow<'a, str> = [VkXMLToken::TextTag { name, text } if name == "name"] { text.to_owned() }

        /// parses the content of <member> ... </member> or <proto> ... </proto> or <param> ... </param>
        pub rule name_with_type() -> NameWithType
            = typed:typed_tag(<name_tag()>) bitOrArr:(
                ":" bitfield_size:([VkXMLToken::C(Token::Constant(Constant::Integer(v)))] { NonZeroU8::new((*v).try_into().unwrap()).unwrap() }) { BitFieldSizeOrArrayshape::BitfieldSize(bitfield_size) }
                / array_shape:("[" n:(
                    [VkXMLToken::C(Token::Constant(Constant::Integer(v)))] { ArrayLength::Static(NonZeroU32::new((*v).try_into().unwrap()).unwrap()) }
                    / [VkXMLToken::TextTag { name, text } if name == "enum"] { ArrayLength::Constant(name.to_string()) }
                ) "]" { n })+ { BitFieldSizeOrArrayshape::ArrayShape(array_shape) }
            )? comment:([VkXMLToken::TextTag { name, text } if name == "comment"] { text })? {
                let (bitfield_size, array_shape) = match bitOrArr {
                    Some(BitFieldSizeOrArrayshape::BitfieldSize(size)) => (Some(size), None),
                    Some(BitFieldSizeOrArrayshape::ArrayShape(shape)) => (None, Some(shape)),
                    None => (None, None),
                };
                NameWithType {
                    bitfield_size,
                    array_shape,
                    comment: comment.map(|s| s.to_string()),
                    ..typed
                }
            }

        /// <type category="define"> ... </type>
        pub rule type_define(name_attr: Option<&str>, requires_attr: Option<&str>) -> TypeDefine
            = _ is_disabled:("#define" {false} / "//#define"? {true}) __ name:([VkXMLToken::TextTag { name, text } if name == "name"] { text }) value:(
                // / "(" _ params:(identifier() ** (_ "," _)) _ ("," _)? ")" [VkXMLToken::C(Token::Whitespace)]* e:define_macro_value() {
                "(" params:(identifier() ** ",") ")" e:$([VkXMLToken::C(_)]+) {
                    TypeDefineValue::FunctionDefine {
                        params: params.into_boxed_slice(),
                        expression: e.iter().map(|v| v.to_string()).collect(),
                    }
                }
                / __ e:expr() _ { TypeDefineValue::Expression(e) }
                / _ macro_name:([VkXMLToken::TextTag { name, text } if name == "type"] { text }) "(" _ args:(arithmetic() ** (_ "," _)) _ ("," _)? ")" _ { TypeDefineValue::MacroFunctionCall {
                    name: macro_name.to_string(),
                    args: args.into_boxed_slice(),
                } }
            ) { TypeDefine {
                name: name.to_string(),
                comment: None,
                requires: requires_attr.map(|s| s.to_string()),
                is_disabled,
                value,
            } }
            / l:$([VkXMLToken::C(_)]+) {
                name_attr.unwrap_or_else(|| panic!("{:?}", l));
                TypeDefine {
                name: name_attr.map(|s| s.to_string()).expect("If no name is found inside the tag <type category=\"define\"> then it must be an attribute"),
                comment: None, requires: requires_attr.map(|s| s.to_string()), is_disabled: false, value: TypeDefineValue::Code(l.iter().map(|v| v.to_string()).collect())
            } }


        /// <type category="funcptr"> ... </type>
        pub rule type_funcptr(requires_attr: Option<&str>) -> TypeFunctionPointer
          = _ "typedef" __ ty_name:type_specifier() _ ptr:"*"? _ "(" _ [VkXMLToken::C(Token::Identifier(id)) if id == "VKAPI_PTR"] _ "*" _ name:([VkXMLToken::TextTag { name, text } if name == "name"] { text }) ")" "(" _ params:(
            "void" ")" ";" { Vec::new() }
            / params:(typed_tag(<identifier()>) ** (_ "," _)) ")" ";" { params }
          ) { TypeFunctionPointer { proto: NameWithType { name: name.to_string(), type_name: ty_name.to_string(), pointer_kind: ptr.map(|()| PointerKind::Single), ..NameWithType::default() }, params, requires: requires_attr.map(|s| s.to_string()) } }
  }
}

// pub use c_with_vk_ext::expr as parse_cexpr;
// pub fn parse_cexpr(text: &str) -> Result<Expression, impl std::error::Error> {
pub fn parse_cexpr(text: &str) -> Option<Expression> {
    let tokens = Lexer::new(text)
        .into_iter()
        .map(|token: Token| VkXMLToken::<'static>::C(token.into_owned()))
        .collect();
    c_with_vk_ext::expr(&tokens).ok()
}
