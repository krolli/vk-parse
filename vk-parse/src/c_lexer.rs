use core::fmt;
use logos::{Lexer, Logos};
use std::{borrow::Cow, str::FromStr};

#[derive(Debug, Clone, Copy, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
pub enum Constant {
    Char(u8),
    Integer(u64),
    Float(f64),
}

// SAFETY: Floating point constants must be finite (NaN is a macro)
impl Eq for Constant {}

impl Constant {
    fn from_c_char<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Result<Self, <char as FromStr>::Err> {
        let s = lex.slice();
        let s = s.strip_prefix('L').unwrap_or(s);
        let end = s.len() - 1;
        Ok(Constant::Char(char::from_str(&s[1..end])? as _))
    }
}

const IS: &[char] = &['u', 'U', 'l', 'L'];
const FS: &[char] = &['f', 'F', 'l', 'L'];

fn fix_literal(lit: &str) -> Cow<str> {
    let lit = lit.strip_prefix('L').unwrap_or(lit);
    let lit = lit.strip_prefix('"').unwrap().strip_suffix('"').unwrap();
    // TODO remove un-escaped newlines
    Cow::Borrowed(lit)
}

#[derive(Logos, Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[logos(subpattern decimal = r"[1-9][0-9]*")]
#[logos(subpattern hex = r"[0-9a-fA-F]+")]
#[logos(subpattern octal = r"[0-7]+")]
#[logos(subpattern exp = r"[Ee][+-]?[0-9]+")]
#[logos(subpattern float_suffix = r"[fFlL]")]
#[logos(subpattern int_suffix = r"[uUlL]*")]
pub enum Token<'a> {
    // #[regex(r"//[^\n]*", logos::skip)]
    // #[regex(r"[ \t\n\f]+", logos::skip)]
    #[error]
    Error,
    #[token("auto")]
    Auto,
    #[token("break")]
    Break,
    #[token("case")]
    Case,
    #[token("char")]
    Char,
    #[token("const")]
    Const,
    #[token("continue")]
    Continue,
    #[token("default")]
    Default,
    #[token("do")]
    Do,
    #[token("double")]
    Double,
    #[token("else")]
    Else,
    #[token("enum")]
    Enum,
    #[token("extern")]
    Extern,
    #[token("float")]
    Float,
    #[token("for")]
    For,
    #[token("goto")]
    Goto,
    #[token("if")]
    If,
    #[token("inline")]
    Inline,
    #[token("int")]
    Int,
    #[token("long")]
    Long,
    #[token("register")]
    Register,
    #[token("restrict")]
    Restrict,
    #[token("return")]
    Return,
    #[token("short")]
    Short,
    #[token("signed")]
    Signed,
    #[token("sizeof")]
    SizeOf,
    #[token("static")]
    Static,
    #[token("struct")]
    Struct,
    #[token("switch")]
    Switch,
    #[token("typedef")]
    TypeDef,
    #[token("union")]
    Union,
    #[token("unsigned")]
    UnSigned,
    #[token("void")]
    Void,
    #[token("volatile")]
    Volatile,
    #[token("while")]
    While,
    #[token("_Bool")]
    Bool,
    #[token("_Complex")]
    Complex,
    #[token("_Imaginary")]
    Imaginary,
    #[token("[")]
    LBrack,
    #[token("]")]
    RBrack,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token(".")]
    Dot,
    #[token("->")]
    Point,
    #[token("++")]
    Increment,
    #[token("--")]
    Decrement,
    #[token("&")]
    Ampersand,
    #[token("*")]
    MulStar,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("~")]
    Tilde,
    #[token("!")]
    Exclamation,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("<<")]
    LShift,
    #[token(">>")]
    RShift,
    #[token("<")]
    LessThan,
    #[token(">")]
    GreaterThan,
    #[token("<=")]
    LessThanEqual,
    #[token(">=")]
    GreaterThanEqual,
    #[token("==")]
    Equal,
    #[token("!=")]
    NotEqual,
    #[token("^")]
    Caret,
    #[token("|")]
    VerticalBar,
    #[token("&&")]
    And,
    #[token("||")]
    Or,
    #[token("?")]
    Question,
    #[token(":")]
    Colon,
    #[token(";")]
    SemiColon,
    #[token("...")]
    Ellipsis,
    #[token("=")]
    Assign,
    #[token("*=")]
    MulAssign,
    #[token("/=")]
    DivAssign,
    #[token("%=")]
    RemAssign,
    #[token("+=")]
    AddAssign,
    #[token("-=")]
    SubAssign,
    #[token("<<=")]
    LShiftAssign,
    #[token(">>=")]
    RShiftAssign,
    #[token("&=")]
    AndAssign,
    #[token("|=")]
    OrAssign,
    #[token("^=")]
    XorAssign,
    #[token(",")]
    Comma,

    //
    #[regex(r"\n", logos::skip)]
    NewLine,
    #[regex(r"[ \t\f]+", logos::skip)]
    Whitespace,

    // pre-processing tokens
    // FIXME
    #[doc(hidden)]
    #[token("//#define\\s*")]
    _MalformedDefine,
    #[regex(r"//[^\n]*", logos::skip)]
    Comment,
    #[token("#")]
    Pound,
    #[token("##")]
    DoublePound,
    #[token("\\", logos::skip)]
    BackSlash,
    #[token("#define")]
    Define,

    #[regex(r"[_a-zA-Z][_a-zA-Z0-9]*", |lex| Cow::Borrowed(lex.slice()))]
    Identifier(Cow<'a, str>),
    #[regex("0(?&int_suffix)?", |_lex| Constant::Integer(0))]
    #[regex("(?&decimal)(?&int_suffix)?", |lex| lex.slice().trim_end_matches(IS).parse().map(Constant::Integer))]
    #[regex("0(?&octal)(?&int_suffix)?", |lex| u64::from_str_radix(lex.slice()[1..].trim_end_matches(IS), 8).map(Constant::Integer))]
    #[regex("0[xX](?&hex)(?&int_suffix)?", |lex| u64::from_str_radix(lex.slice()[2..].trim_end_matches(IS), 16).map(Constant::Integer))]
    #[regex(r"L?'(\\.|[^\\'])+'", |lex| Constant::from_c_char(lex))]
    #[regex(r#"[+-]?((\d\.\d?(?&exp)?[fFdD]?)|(\.\d(?&exp)?[fFdD]?)|(\d(?&exp)[fFdD]?)|(\d(?&exp)?[fFdD]))"#, |lex| lex.slice().trim_end_matches(FS).parse().map(Constant::Float))]
    Constant(Constant),
    #[regex(r#"L?"([^"\\]|\\t|\\u|\\n|\\")*""#, |lex| fix_literal(lex.slice()))]
    Literal(Cow<'a, str>),
}

impl<'a> Token<'a> {
    pub fn from_literal(s: &str) -> Option<Self> {
        match s {
            "auto" => Some(Token::Auto),
            "break" => Some(Token::Break),
            "case" => Some(Token::Case),
            "char" => Some(Token::Char),
            "const" => Some(Token::Const),
            "continue" => Some(Token::Continue),
            "default" => Some(Token::Default),
            "do" => Some(Token::Do),
            "double" => Some(Token::Double),
            "else" => Some(Token::Else),
            "enum" => Some(Token::Enum),
            "extern" => Some(Token::Extern),
            "float" => Some(Token::Float),
            "for" => Some(Token::For),
            "goto" => Some(Token::Goto),
            "if" => Some(Token::If),
            "inline" => Some(Token::Inline),
            "int" => Some(Token::Int),
            "long" => Some(Token::Long),
            "register" => Some(Token::Register),
            "restrict" => Some(Token::Restrict),
            "return" => Some(Token::Return),
            "short" => Some(Token::Short),
            "signed" => Some(Token::Signed),
            "sizeof" => Some(Token::SizeOf),
            "static" => Some(Token::Static),
            "struct" => Some(Token::Struct),
            "switch" => Some(Token::Switch),
            "typedef" => Some(Token::TypeDef),
            "union" => Some(Token::Union),
            "unsigned" => Some(Token::UnSigned),
            "void" => Some(Token::Void),
            "volatile" => Some(Token::Volatile),
            "while" => Some(Token::While),
            "_Bool" => Some(Token::Bool),
            "_Complex" => Some(Token::Complex),
            "_Imaginary" => Some(Token::Imaginary),
            "[" => Some(Token::LBrack),
            "]" => Some(Token::RBrack),
            "(" => Some(Token::LParen),
            ")" => Some(Token::RParen),
            "{" => Some(Token::LBrace),
            "}" => Some(Token::RBrace),
            "." => Some(Token::Dot),
            "->" => Some(Token::Point),
            "++" => Some(Token::Increment),
            "--" => Some(Token::Decrement),
            "&" => Some(Token::Ampersand),
            "*" => Some(Token::MulStar),
            "+" => Some(Token::Plus),
            "-" => Some(Token::Minus),
            "~" => Some(Token::Tilde),
            "!" => Some(Token::Exclamation),
            "/" => Some(Token::Slash),
            "%" => Some(Token::Percent),
            "<<" => Some(Token::LShift),
            ">>" => Some(Token::RShift),
            "<" => Some(Token::LessThan),
            ">" => Some(Token::GreaterThan),
            "<=" => Some(Token::LessThanEqual),
            ">=" => Some(Token::GreaterThanEqual),
            "==" => Some(Token::Equal),
            "!=" => Some(Token::NotEqual),
            "^" => Some(Token::Caret),
            "|" => Some(Token::VerticalBar),
            "&&" => Some(Token::And),
            "||" => Some(Token::Or),
            "?" => Some(Token::Question),
            ":" => Some(Token::Colon),
            ";" => Some(Token::SemiColon),
            "..." => Some(Token::Ellipsis),
            "=" => Some(Token::Assign),
            "*=" => Some(Token::MulAssign),
            "/=" => Some(Token::DivAssign),
            "%=" => Some(Token::RemAssign),
            "+=" => Some(Token::AddAssign),
            "-=" => Some(Token::SubAssign),
            "<<=" => Some(Token::LShiftAssign),
            ">>=" => Some(Token::RShiftAssign),
            "&=" => Some(Token::AndAssign),
            "|=" => Some(Token::OrAssign),
            "^=" => Some(Token::XorAssign),
            "," => Some(Token::Comma),
            "#" => Some(Token::Pound),
            "##" => Some(Token::DoublePound),
            "\\" => Some(Token::BackSlash),
            "#define" => Some(Token::Define),
            "//#define" => Some(Token::_MalformedDefine),
            "\n" => Some(Token::NewLine),
            " " => Some(Token::Whitespace),
            _ => None,
        }
    }

    pub fn into_owned(self) -> Token<'static> {
        match self {
            Token::Identifier(id) => Token::Identifier(Cow::Owned(id.into_owned())),
            Token::Literal(lit) => Token::Literal(Cow::Owned(lit.into_owned())),
            Token::Error => Token::Error, // todo!(),
            Token::Constant(c) => Token::Constant(c),

            Token::Auto => Token::Auto,
            Token::Break => Token::Break,
            Token::Case => Token::Case,
            Token::Char => Token::Char,
            Token::Const => Token::Const,
            Token::Continue => Token::Continue,
            Token::Default => Token::Default,
            Token::Do => Token::Do,
            Token::Double => Token::Double,
            Token::Else => Token::Else,
            Token::Enum => Token::Enum,
            Token::Extern => Token::Extern,
            Token::Float => Token::Float,
            Token::For => Token::For,
            Token::Goto => Token::Goto,
            Token::If => Token::If,
            Token::Inline => Token::Inline,
            Token::Int => Token::Int,
            Token::Long => Token::Long,
            Token::Register => Token::Register,
            Token::Restrict => Token::Restrict,
            Token::Return => Token::Return,
            Token::Short => Token::Short,
            Token::Signed => Token::Signed,
            Token::SizeOf => Token::SizeOf,
            Token::Static => Token::Static,
            Token::Struct => Token::Struct,
            Token::Switch => Token::Switch,
            Token::TypeDef => Token::TypeDef,
            Token::Union => Token::Union,
            Token::UnSigned => Token::UnSigned,
            Token::Void => Token::Void,
            Token::Volatile => Token::Volatile,
            Token::While => Token::While,
            Token::Bool => Token::Bool,
            Token::Complex => Token::Complex,
            Token::Imaginary => Token::Imaginary,
            Token::LBrack => Token::LBrack,
            Token::RBrack => Token::RBrack,
            Token::LParen => Token::LParen,
            Token::RParen => Token::RParen,
            Token::LBrace => Token::LBrace,
            Token::RBrace => Token::RBrace,
            Token::Dot => Token::Dot,
            Token::Point => Token::Point,
            Token::Increment => Token::Increment,
            Token::Decrement => Token::Decrement,
            Token::Ampersand => Token::Ampersand,
            Token::MulStar => Token::MulStar,
            Token::Plus => Token::Plus,
            Token::Minus => Token::Minus,
            Token::Tilde => Token::Tilde,
            Token::Exclamation => Token::Exclamation,
            Token::Slash => Token::Slash,
            Token::Percent => Token::Percent,
            Token::LShift => Token::LShift,
            Token::RShift => Token::RShift,
            Token::LessThan => Token::LessThan,
            Token::GreaterThan => Token::GreaterThan,
            Token::LessThanEqual => Token::LessThanEqual,
            Token::GreaterThanEqual => Token::GreaterThanEqual,
            Token::Equal => Token::Equal,
            Token::NotEqual => Token::NotEqual,
            Token::Caret => Token::Caret,
            Token::VerticalBar => Token::VerticalBar,
            Token::And => Token::And,
            Token::Or => Token::Or,
            Token::Question => Token::Question,
            Token::Colon => Token::Colon,
            Token::SemiColon => Token::SemiColon,
            Token::Ellipsis => Token::Ellipsis,
            Token::Assign => Token::Assign,
            Token::MulAssign => Token::MulAssign,
            Token::DivAssign => Token::DivAssign,
            Token::RemAssign => Token::RemAssign,
            Token::AddAssign => Token::AddAssign,
            Token::SubAssign => Token::SubAssign,
            Token::LShiftAssign => Token::LShiftAssign,
            Token::RShiftAssign => Token::RShiftAssign,
            Token::AndAssign => Token::AndAssign,
            Token::OrAssign => Token::OrAssign,
            Token::XorAssign => Token::XorAssign,
            Token::Comma => Token::Comma,
            Token::Pound => Token::Pound,
            Token::DoublePound => Token::DoublePound,
            Token::BackSlash => Token::BackSlash,
            Token::_MalformedDefine => Token::_MalformedDefine,
            Token::NewLine => Token::NewLine,
            Token::Whitespace => Token::Whitespace,
            Token::Comment => Token::Comment,
            Token::Define => Token::Define,
        }
    }
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Auto => write!(f, "auto"),
            Token::Break => write!(f, "break"),
            Token::Case => write!(f, "case"),
            Token::Char => write!(f, "char"),
            Token::Const => write!(f, "const"),
            Token::Continue => write!(f, "continue"),
            Token::Default => write!(f, "default"),
            Token::Do => write!(f, "do"),
            Token::Double => write!(f, "double"),
            Token::Else => write!(f, "else"),
            Token::Enum => write!(f, "enum"),
            Token::Extern => write!(f, "extern"),
            Token::Float => write!(f, "float"),
            Token::For => write!(f, "for"),
            Token::Goto => write!(f, "goto"),
            Token::If => write!(f, "if"),
            Token::Inline => write!(f, "inline"),
            Token::Int => write!(f, "int"),
            Token::Long => write!(f, "long"),
            Token::Register => write!(f, "register"),
            Token::Restrict => write!(f, "restrict"),
            Token::Return => write!(f, "return"),
            Token::Short => write!(f, "short"),
            Token::Signed => write!(f, "signed"),
            Token::SizeOf => write!(f, "sizeof"),
            Token::Static => write!(f, "static"),
            Token::Struct => write!(f, "struct"),
            Token::Switch => write!(f, "switch"),
            Token::TypeDef => write!(f, "typedef"),
            Token::Union => write!(f, "union"),
            Token::UnSigned => write!(f, "unsigned"),
            Token::Void => write!(f, "void"),
            Token::Volatile => write!(f, "volatile"),
            Token::While => write!(f, "while"),
            Token::Bool => write!(f, "_Bool"),
            Token::Complex => write!(f, "_Complex"),
            Token::Imaginary => write!(f, "_Imaginary"),
            Token::LBrack => write!(f, "["),
            Token::RBrack => write!(f, "]"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::Dot => write!(f, "."),
            Token::Point => write!(f, "->"),
            Token::Increment => write!(f, "++"),
            Token::Decrement => write!(f, "--"),
            Token::Ampersand => write!(f, "&"),
            Token::MulStar => write!(f, "*"),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Tilde => write!(f, "~"),
            Token::Exclamation => write!(f, "!"),
            Token::Slash => write!(f, "/"),
            Token::Percent => write!(f, "%"),
            Token::LShift => write!(f, "<<"),
            Token::RShift => write!(f, ">>"),
            Token::LessThan => write!(f, "<"),
            Token::GreaterThan => write!(f, ">"),
            Token::LessThanEqual => write!(f, "<="),
            Token::GreaterThanEqual => write!(f, ">="),
            Token::Equal => write!(f, "=="),
            Token::NotEqual => write!(f, "!="),
            Token::Caret => write!(f, "^"),
            Token::VerticalBar => write!(f, "|"),
            Token::And => write!(f, "&&"),
            Token::Or => write!(f, "||"),
            Token::Question => write!(f, "?"),
            Token::Colon => write!(f, ":"),
            Token::SemiColon => write!(f, ";"),
            Token::Ellipsis => write!(f, "..."),
            Token::Assign => write!(f, "="),
            Token::MulAssign => write!(f, "*="),
            Token::DivAssign => write!(f, "/="),
            Token::RemAssign => write!(f, "%="),
            Token::AddAssign => write!(f, "+="),
            Token::SubAssign => write!(f, "-="),
            Token::LShiftAssign => write!(f, "<<="),
            Token::RShiftAssign => write!(f, ">>="),
            Token::AndAssign => write!(f, "&="),
            Token::OrAssign => write!(f, "|="),
            Token::XorAssign => write!(f, "^="),
            Token::Comma => write!(f, ","),
            Token::Pound => write!(f, "#"),
            Token::DoublePound => write!(f, "##"),
            Token::BackSlash => write!(f, "\\"),
            Token::_MalformedDefine => write!(f, "//#define"),
            Token::NewLine => write!(f, "\n"),
            Token::Whitespace => write!(f, " "),
            Token::Comment => write!(f, "//\n"),
            Token::Identifier(id) => write!(f, "{}", id),
            Token::Constant(c) => write!(f, "{}", c),
            Token::Literal(lit) => {
                let lit: &str = &*lit;
                // FIXME
                write!(f, "{:?}", lit)
            }
            Token::Error => unreachable!(),
            Token::Define => write!(f, "#define"),
        }?;
        write!(f, " ")
    }
}
