use std::fs::File;
use std::io::Read;
use syn;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum OCamlError {
    FileNotFound(String),
    Parse(String),
    UnableToReadFile(String),
    // Unknown(String),
}

impl std::fmt::Display for OCamlError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OCamlError::FileNotFound(file_path) => {
                write!(f, "File not found: '{}'", file_path)
            }
            OCamlError::Parse(file_path) => {
                write!(f, "Unable to parse file: {}", file_path)
            }
            OCamlError::UnableToReadFile(file_path) => {
                write!(f, "Unable to read file: '{}'", file_path)
            } // OCamlError::Unknown(error) => {
              //     write!(f, "Unknown error: {}", error)
              // }
        }
    }
}

#[derive(Debug, PartialEq, PartialOrd, Ord, Eq)]
pub enum OCamlType {
    Pointer{ ty: Box<OCamlType>, is_const: bool },
    Array{ ty: Box<OCamlType>, size: Option<usize> },
    Function{ args: Vec<OCamlType>, ret: Box<OCamlType> },
    Path(Vec<String>),
    Tuple(Vec<OCamlType>),
    Never,
    Unit,
    Paren(Box<OCamlType>),
    Verbatim(String),
}

#[derive(Debug, PartialEq, PartialOrd, Ord, Eq)]
pub enum OCaml {
    Let {
        name: String,
        ty: Option<OCamlType>,
        value: Option<OCamlExpr>,
    },
    Statements(Vec<OCaml>),
}

impl OCaml {
    pub fn from_rust_file(file_path: &str) -> Result<Self, OCamlError> {
        let mut file =
            File::open(file_path).map_err(|_| OCamlError::FileNotFound(file_path.to_string()))?;

        let mut src = String::new();
        file.read_to_string(&mut src)
            .map_err(|_| OCamlError::UnableToReadFile(file_path.to_string()))?;

        Self::from_source(&src)
    }

    pub fn from_source(src: &str) -> Result<Self, OCamlError> {
        let syntax =
            syn::parse_file(src).map_err(|e| OCamlError::Parse(format!("'{}': {}", src, e)))?;

        // Rust AST
        // dbg!(&syntax);

        let syntax_items: Vec<Self> = syntax.items.iter().map(|item| item.into()).collect();

        print!("{:#?}", syntax_items);

        Ok(Self::Statements(syntax_items))
    }
}

#[derive(Debug, PartialEq, PartialOrd, Ord, Eq)]
pub enum OCamlExpr {
    Literal(OCamlLiteral),
    Path(Vec<String>),
    Unary(Box<OCamlUnary>),
    Binary(Box<OCamlBinary>),
    //Struct
}

impl OCamlExpr {
    pub fn eval_to_int(&self) -> Option<usize> {
        match self {
            OCamlExpr::Literal(OCamlLiteral::Integer { digits, .. }) => digits.parse().ok(),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, PartialOrd, Ord, Eq)]
pub enum OCamlLiteral {
    Integer {
        digits: String,
        width: Option<usize>,
        is_signed: Option<bool>,
        is_native: bool,
    },
    Float {
        digits: String,
        width: Option<usize>,
    },
    Bool(bool),
    Char(char),
    String(String),
    Byte(u8),
    ByteStr(Vec<u8>),
}

#[derive(Debug, PartialEq, PartialOrd, Ord, Eq)]
pub enum OCamlUnary {
    Minus(OCamlExpr),
    Deref(OCamlExpr),
    Not(OCamlExpr),
}

#[derive(Debug, PartialEq, PartialOrd, Ord, Eq)]
pub enum OCamlBinary {
    Plus { left: OCamlExpr, right: OCamlExpr },
    Minus { left: OCamlExpr, right: OCamlExpr },
    Multiply { left: OCamlExpr, right: OCamlExpr },
    Divide { left: OCamlExpr, right: OCamlExpr },
    Modulo { left: OCamlExpr, right: OCamlExpr },
    And { left: OCamlExpr, right: OCamlExpr },
    Or { left: OCamlExpr, right: OCamlExpr },
    BitXor { left: OCamlExpr, right: OCamlExpr },
    BitAnd { left: OCamlExpr, right: OCamlExpr },
    BitOr { left: OCamlExpr, right: OCamlExpr },
    Shl { left: OCamlExpr, right: OCamlExpr },
    Shr { left: OCamlExpr, right: OCamlExpr },
    Eq { left: OCamlExpr, right: OCamlExpr },
    Lt { left: OCamlExpr, right: OCamlExpr },
    Le { left: OCamlExpr, right: OCamlExpr },
    Ne { left: OCamlExpr, right: OCamlExpr },
    Gt { left: OCamlExpr, right: OCamlExpr },
    Ge { left: OCamlExpr, right: OCamlExpr },

    // Phantom operations that will be optimized out after the AST
    // is transformed into the OCaml AST
    AddAssign { left: OCamlExpr, right: OCamlExpr },
    SubAssign { left: OCamlExpr, right: OCamlExpr },
    MulAssign { left: OCamlExpr, right: OCamlExpr },
    DivAssign { left: OCamlExpr, right: OCamlExpr },
    RemAssign { left: OCamlExpr, right: OCamlExpr },
    BitXorAssign { left: OCamlExpr, right: OCamlExpr },
    BitAndAssign { left: OCamlExpr, right: OCamlExpr },
    BitOrAssign { left: OCamlExpr, right: OCamlExpr },
    ShlAssign { left: OCamlExpr, right: OCamlExpr },
    ShrAssign { left: OCamlExpr, right: OCamlExpr },
}

struct SynPath<'a>(&'a syn::Path);

impl From<SynPath<'_>> for Vec<String> {
    fn from(value: SynPath) -> Self {
        value
            .0
            .segments
            .iter()
            .map(|seg| seg.ident.to_string())
            .collect()
    }
}

struct SynReturnType<'a>(&'a syn::ReturnType);

impl From<SynReturnType<'_>> for OCamlType {
    fn from(value: SynReturnType) -> Self {
        match value.0 {
            syn::ReturnType::Type(_, ty) => SynType(&*ty).into(),
            syn::ReturnType::Default => OCamlType::Unit,
            _ => todo!("{:#?} is not implemented", value.0),
        }
    }
}

struct SynType<'a>(&'a syn::Type);

impl From<SynType<'_>> for OCamlType {
    fn from(value: SynType) -> Self {
        match value.0 {
            syn::Type::Path(path_type) => OCamlType::Path(SynPath(&path_type.path).into()),
            syn::Type::Ptr(ty) => OCamlType::Pointer {
                ty: Box::new(SynType(&ty.elem).into()),
                is_const: ty.const_token.is_some(),
            },
            syn::Type::Array(ty) => {
                let len_expr: OCamlExpr = (&ty.len).into();
                OCamlType::Array {
                    ty: Box::new(SynType(&ty.elem).into()),
                    size: len_expr.eval_to_int(),
                }
            },
            syn::Type::BareFn(ty) => {
                let args: Vec<OCamlType> = ty
                    .inputs
                    .iter()
                    .map(|arg| SynType(&arg.ty).into())
                    .collect();
                let ret = Box::new(SynReturnType(&ty.output).into());
                OCamlType::Function { args, ret }
            },
            syn::Type::Verbatim(ty) => OCamlType::Verbatim(ty.to_string()),
            syn::Type::Slice(ty) => OCamlType::Array {
                ty: Box::new(SynType(&ty.elem).into()),
                size: None,
            },
            syn::Type::Reference(ty) => OCamlType::Pointer {
                ty: Box::new(SynType(&ty.elem).into()),
                is_const: ty.mutability.is_none(),
            },
            syn::Type::Tuple(ty) => OCamlType::Tuple(
                ty.elems
                    .iter()
                    .map(|elem| SynType(elem).into())
                    .collect(),
            ),
            syn::Type::Never(_) => OCamlType::Never,
            syn::Type::Paren(ty) => OCamlType::Paren(Box::new(SynType(&*ty.elem).into())),
            _ => todo!("{:#?} is not implemented", value.0),
        }
    }
}

impl From<&syn::Item> for OCaml {
    fn from(item: &syn::Item) -> Self {
        match item {
            syn::Item::Const(syn::ItemConst {
                ident: name,
                expr: value,
                ty: ty,
                ..
            }) => OCaml::Let {
                name: name.to_string(),
                value: Some(value.as_ref().into()),
                ty: Some(SynType(&ty).into()),
            },
            _ => todo!("{:#?} is not implemented", item),
        }
    }
}

impl From<&syn::Expr> for OCamlExpr {
    fn from(value: &syn::Expr) -> Self {
        match value {
            syn::Expr::Lit(syn::ExprLit { lit, .. }) => OCamlExpr::Literal(lit.into()),
            syn::Expr::Path(syn::ExprPath { path, .. }) => OCamlExpr::Path(SynPath(path).into()),
            syn::Expr::Unary(unary) => OCamlExpr::Unary(Box::new(unary.into())),
            syn::Expr::Binary(expr) => OCamlExpr::Binary(Box::new(expr.into())),
            _ => todo!("{:#?} is not implemented", value),
        }
    }
}

impl From<&syn::Lit> for OCamlLiteral {
    fn from(value: &syn::Lit) -> Self {
        match value {
            syn::Lit::Int(int) => int.into(),
            syn::Lit::Float(float) => float.into(),
            syn::Lit::Bool(b) => OCamlLiteral::Bool(b.value),
            syn::Lit::Char(c) => OCamlLiteral::Char(c.value()),
            syn::Lit::Str(s) => OCamlLiteral::String(s.value()),
            syn::Lit::Byte(b) => OCamlLiteral::Byte(b.value()),
            syn::Lit::ByteStr(b) => OCamlLiteral::ByteStr(b.value()),
            _ => todo!("{:#?} is not implemented", value),
        }
    }
}

pub(crate) fn size_from_rust_basic_number_type(ty: &str) -> (u8, Option<usize>) {
    let type_width = &ty[1..];
    if ty.starts_with('f'){
        return (b'f', type_width.parse().ok());
    }

    let prefix = if ty.starts_with('i') { b'i' } else { b'u' };
    if type_width == "size" {
        return (prefix, None);
    }

    (prefix, type_width.parse().ok())
}

impl From<&syn::LitInt> for OCamlLiteral {
    fn from(value: &syn::LitInt) -> Self {
        let suffix = value.suffix();
        let digits = value.token().to_string();

        if suffix.is_empty() {
            return OCamlLiteral::Integer {
                digits,
                width: None,
                is_signed: None,
                is_native: false,
            };
        }

        let digits = digits.trim_end_matches(suffix).to_string();

        let (prefix, width) = size_from_rust_basic_number_type(suffix);
        let is_signed = if prefix == b'i' { true } else { false };

        if let None = width {
            return OCamlLiteral::Integer {
                digits,
                width: None,
                is_signed: Some(is_signed),
                is_native: true,
            };
        }

       return if prefix == b'f' {
            OCamlLiteral::Float {
                digits,
                width,
            }
        } else {
            OCamlLiteral::Integer {
                digits,
                width,
                is_signed: Some(is_signed),
                is_native: false,
            }
        }
    }
}

impl From<&syn::LitFloat> for OCamlLiteral {
    fn from(value: &syn::LitFloat) -> Self {
        let suffix = value.suffix();
        let digits = value.token().to_string();

        if suffix.is_empty() {
            return OCamlLiteral::Float {
                digits,
                width: None,
            };
        }

        let digits = digits.trim_end_matches(suffix).to_string();
        let (prefix, width) = size_from_rust_basic_number_type(suffix);

        if prefix == b'f' && width.is_some() {
            return OCamlLiteral::Float {
                digits,
                width,
            };
        }

        unreachable!("Unknown suffix: {}", suffix)
    }
}

impl From<&syn::ExprUnary> for OCamlUnary {
    fn from(value: &syn::ExprUnary) -> Self {
        match value.op {
            syn::UnOp::Deref(_) => OCamlUnary::Deref(value.expr.as_ref().into()),
            syn::UnOp::Not(_) => OCamlUnary::Not(value.expr.as_ref().into()),
            syn::UnOp::Neg(_) => OCamlUnary::Minus(value.expr.as_ref().into()),
            _ => todo!(),
        }
    }
}

macro_rules! impl_from_expr_binary {
    ($value:expr, $ocaml:ident) => {
        OCamlBinary::$ocaml {
            left: $value.left.as_ref().into(),
            right: $value.right.as_ref().into(),
        }
    };
}

impl From<&syn::ExprBinary> for OCamlBinary {
    // this doesn't support float vs int operators (or does it?!!!!!!!!!!!!!!!!!)
    fn from(value: &syn::ExprBinary) -> Self {
        match value.op {
            syn::BinOp::Add(_) => impl_from_expr_binary!(value, Plus),
            syn::BinOp::Sub(_) => impl_from_expr_binary!(value, Minus),
            syn::BinOp::Mul(_) => impl_from_expr_binary!(value, Multiply),
            syn::BinOp::Div(_) => impl_from_expr_binary!(value, Divide),
            syn::BinOp::Rem(_) => impl_from_expr_binary!(value, Modulo),
            syn::BinOp::And(_) => impl_from_expr_binary!(value, And),
            syn::BinOp::Or(_) => impl_from_expr_binary!(value, Or),
            syn::BinOp::BitAnd(_) => impl_from_expr_binary!(value, BitAnd),
            syn::BinOp::BitXor(_) => impl_from_expr_binary!(value, BitXor),
            syn::BinOp::BitOr(_) => impl_from_expr_binary!(value, BitOr),
            syn::BinOp::Shl(_) => impl_from_expr_binary!(value, Shl),
            syn::BinOp::Shr(_) => impl_from_expr_binary!(value, Shr),
            syn::BinOp::Eq(_) => impl_from_expr_binary!(value, Eq),
            syn::BinOp::Lt(_) => impl_from_expr_binary!(value, Lt),
            syn::BinOp::Le(_) => impl_from_expr_binary!(value, Le),
            syn::BinOp::Ne(_) => impl_from_expr_binary!(value, Ne),
            syn::BinOp::Gt(_) => impl_from_expr_binary!(value, Gt),
            syn::BinOp::Ge(_) => impl_from_expr_binary!(value, Ge),
            syn::BinOp::AddAssign(_) => impl_from_expr_binary!(value, AddAssign),
            syn::BinOp::SubAssign(_) => impl_from_expr_binary!(value, SubAssign),
            syn::BinOp::MulAssign(_) => impl_from_expr_binary!(value, MulAssign),
            syn::BinOp::DivAssign(_) => impl_from_expr_binary!(value, DivAssign),
            syn::BinOp::RemAssign(_) => impl_from_expr_binary!(value, RemAssign),
            syn::BinOp::BitXorAssign(_) => impl_from_expr_binary!(value, BitXorAssign),
            syn::BinOp::BitAndAssign(_) => impl_from_expr_binary!(value, BitAndAssign),
            syn::BinOp::BitOrAssign(_) => impl_from_expr_binary!(value, BitOrAssign),
            syn::BinOp::ShlAssign(_) => impl_from_expr_binary!(value, ShlAssign),
            syn::BinOp::ShrAssign(_) => impl_from_expr_binary!(value, ShrAssign),
            _ => unimplemented!("{:#?} is not implemented", value),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::vec;

    use super::*;

    fn parse_number_literal_helper(
        name: &str,
        ty: &str,
        digits: &str,
        width: Option<usize>,
        is_signed: Option<bool>,
        is_native: bool,
        is_float: bool,
    ) -> OCaml {
        let lit = if is_float {
            OCamlLiteral::Float {
                digits: digits.to_string(),
                width,
            }
        } else {
            OCamlLiteral::Integer {
                digits: digits.to_string(),
                width,
                is_signed,
                is_native,
            }
        };

        OCaml::Let {
            name: name.to_string(),
            ty: Some(OCamlType::Path(vec![ty.to_string()])),
            value: Some(OCamlExpr::Literal(lit)),
        }
    }

    #[test]
    fn test_parse_number_literal() {
        let source = r#"
            const A: c_int = 1u8;
            const A: c_int = 1u16;
            const A: c_int = 1u32;
            const A: c_int = 1u64;
            const A: c_int = 1usize;
            const A: c_int = 1i8;
            const A: c_int = 1i16;
            const A: c_int = 1i32;
            const A: c_int = 1i64;
            const A: c_int = 1isize;
            const A: c_float = 1f32;
            const A: c_float = 1f64;

            const B: c_float = 1.0f32;
            const B: c_float = 1.0f64;

            const C: c_int = 1;
            const C: c_float = 1.0;

            const A: c_int = 1u128;
            const A: c_int = 1i128;
        "#;

        let syntax = syn::parse_file(source).unwrap();
        let syntax_items: Vec<OCaml> = syntax.items.iter().map(|item| item.into()).collect();

        assert_eq!(syntax_items.len(), 18);
        assert_eq!(
            syntax_items[0],
            parse_number_literal_helper("A", "c_int", "1", Some(8), Some(false), false, false)
        );
        assert_eq!(
            syntax_items[1],
            parse_number_literal_helper("A", "c_int", "1", Some(16), Some(false), false, false)
        );
        assert_eq!(
            syntax_items[2],
            parse_number_literal_helper("A", "c_int", "1", Some(32), Some(false), false, false)
        );
        assert_eq!(
            syntax_items[3],
            parse_number_literal_helper("A", "c_int", "1", Some(64), Some(false), false, false)
        );
        assert_eq!(
            syntax_items[4],
            parse_number_literal_helper("A", "c_int", "1", None, Some(false), true, false)
        );
        assert_eq!(
            syntax_items[5],
            parse_number_literal_helper("A", "c_int", "1", Some(8), Some(true), false, false)
        );
        assert_eq!(
            syntax_items[6],
            parse_number_literal_helper("A", "c_int", "1", Some(16), Some(true), false, false)
        );
        assert_eq!(
            syntax_items[7],
            parse_number_literal_helper("A", "c_int", "1", Some(32), Some(true), false, false)
        );
        assert_eq!(
            syntax_items[8],
            parse_number_literal_helper("A", "c_int", "1", Some(64), Some(true), false, false)
        );
        assert_eq!(
            syntax_items[9],
            parse_number_literal_helper("A", "c_int", "1", None, Some(true), true, false)
        );
        assert_eq!(
            syntax_items[10],
            parse_number_literal_helper("A", "c_float", "1", Some(32), None, false, true)
        );
        assert_eq!(
            syntax_items[11],
            parse_number_literal_helper("A", "c_float", "1", Some(64), None, false, true)
        );
        assert_eq!(
            syntax_items[12],
            parse_number_literal_helper("B", "c_float", "1.0", Some(32), None, false, true)
        );
        assert_eq!(
            syntax_items[13],
            parse_number_literal_helper("B", "c_float", "1.0", Some(64), None, false, true)
        );
        assert_eq!(
            syntax_items[14],
            parse_number_literal_helper("C", "c_int", "1", None, None, false, false)
        );
        assert_eq!(
            syntax_items[15],
            parse_number_literal_helper("C", "c_float", "1.0", None, None, false, true)
        );
        assert_eq!(
            syntax_items[16],
            parse_number_literal_helper("A", "c_int", "1", Some(128), Some(false), false, false)
        );
        assert_eq!(
            syntax_items[17],
            parse_number_literal_helper("A", "c_int", "1", Some(128), Some(true), false, false)
        );
    }

    #[test]
    fn test_parse_literal() {
        let source = r##"
            const BYTES_STRING: c_str = b"Hello, World!";
            const STRING: c_str = "Hello, World!";
            const CHAR: c_char = 'a';
            const BOOL: c_bool = true;
            const BOOL: c_bool = false;
            const BYTE: c_uchar = b'a';
            const RAW_STRING: c_str = r#"Hello, World!"#;
        "##;

        let syntax = syn::parse_file(source).unwrap();
        let syntax_items: Vec<OCaml> = syntax.items.iter().map(|item| item.into()).collect();

        assert_eq!(syntax_items.len(), 7);

        assert_eq!(
            syntax_items[0],
            OCaml::Let {
                name: "BYTES_STRING".to_string(),
                ty: Some(OCamlType::Path(vec!["c_str".to_string()])),
                value: Some(OCamlExpr::Literal(OCamlLiteral::ByteStr(
                    "Hello, World!".as_bytes().to_vec()
                ))),
            }
        );

        assert_eq!(
            syntax_items[1],
            OCaml::Let {
                name: "STRING".to_string(),
                ty: Some(OCamlType::Path(vec!["c_str".to_string()])),
                value: Some(OCamlExpr::Literal(OCamlLiteral::String(
                    "Hello, World!".to_string()
                ))),
            }
        );

        assert_eq!(
            syntax_items[2],
            OCaml::Let {
                name: "CHAR".to_string(),
                ty: Some(OCamlType::Path(vec!["c_char".to_string()])),
                value: Some(OCamlExpr::Literal(OCamlLiteral::Char('a'))),
            }
        );

        assert_eq!(
            syntax_items[3],
            OCaml::Let {
                name: "BOOL".to_string(),
                ty: Some(OCamlType::Path(vec!["c_bool".to_string()])),
                value: Some(OCamlExpr::Literal(OCamlLiteral::Bool(true))),
            }
        );

        assert_eq!(
            syntax_items[4],
            OCaml::Let {
                name: "BOOL".to_string(),
                ty: Some(OCamlType::Path(vec!["c_bool".to_string()])),
                value: Some(OCamlExpr::Literal(OCamlLiteral::Bool(false))),
            }
        );

        assert_eq!(
            syntax_items[5],
            OCaml::Let {
                name: "BYTE".to_string(),
                ty: Some(OCamlType::Path(vec!["c_uchar".to_string()])),
                value: Some(OCamlExpr::Literal(OCamlLiteral::Byte(b'a'))),
            }
        );

        assert_eq!(
            syntax_items[6],
            OCaml::Let {
                name: "RAW_STRING".to_string(),
                ty: Some(OCamlType::Path(vec!["c_str".to_string()])),
                value: Some(OCamlExpr::Literal(OCamlLiteral::String(
                    "Hello, World!".to_string()
                ))),
            }
        );
    }
}
