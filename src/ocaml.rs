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
pub enum OCaml {
    Let {
        name: String,
        ty: Option<String>,
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
        let syntax = syn::parse_file(src)
            .map_err(|e| OCamlError::Parse(format!("'{}': {}", src, e)))?;

        let syntax_items: Vec<Self> = syntax.items.iter().map(|item| item.into()).collect();

        Ok(Self::Statements(syntax_items))
    }
}

#[derive(Debug, PartialEq, PartialOrd, Ord, Eq)]
pub enum OCamlExpr {
    Literal(OCamlLiteral),
    Path(Vec<String>),
    Unary(Box<OCamlUnary>), //Binary
                            //Struct
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
    Verbatim(String),
}

#[derive(Debug, PartialEq, PartialOrd, Ord, Eq)]
pub enum OCamlUnary {
    Minus(OCamlExpr),
    Deref(OCamlExpr),
    Not(OCamlExpr),
}

//#[derive(Debug)]
//enum OCamlBinaryExpr {
//    And { left: OCamlExpr, right: OCamlExpr },
//    Or { left: OCamlExpr, right: OCamlExpr },
//}

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

struct SynType<'a>(&'a syn::Type);

impl SynType<'_> {
    fn take_last(self) -> Option<String> {
        let mut paths: Vec<String> = self.into();
        paths.pop()
    }
}

impl From<SynType<'_>> for Vec<String> {
    fn from(value: SynType) -> Self {
        match value.0 {
            syn::Type::Path(path_type) => SynPath(&path_type.path).into(),
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
                ty: SynType(&ty).take_last(),
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
            syn::Lit::Verbatim(v) => OCamlLiteral::Verbatim(v.to_string()),
            _ => todo!("{:#?} is not implemented", value),
        }
    }
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
        let type_width = &suffix[1..];

        if type_width == "size" {
            return OCamlLiteral::Integer {
                digits,
                width: None,
                is_signed: Some(suffix.starts_with("i")),
                is_native: true,
            };
        }

        if let Ok(type_width) = type_width.parse::<usize>() {
            if suffix.starts_with("f") {
                return OCamlLiteral::Float {
                    digits,
                    width: Some(type_width),
                };
            } else {
                return OCamlLiteral::Integer {
                    digits,
                    width: Some(type_width),
                    is_signed: Some(suffix.starts_with("i")),
                    is_native: false,
                };
            }
        }

        unreachable!("Unknown suffix: {}", suffix)
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
        let type_width = &suffix[1..];

        if suffix.starts_with("f") {
            if let Ok(type_width) = type_width.parse::<usize>() {
                return OCamlLiteral::Float {
                    digits,
                    width: Some(type_width),
                };
            }
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
#[cfg(test)]
mod tests {
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
            ty: Some(ty.to_string()),
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
                ty: Some("c_str".to_string()),
                value: Some(OCamlExpr::Literal(OCamlLiteral::ByteStr(
                    "Hello, World!".as_bytes().to_vec()
                ))),
            }
        );

        assert_eq!(
            syntax_items[1],
            OCaml::Let {
                name: "STRING".to_string(),
                ty: Some("c_str".to_string()),
                value: Some(OCamlExpr::Literal(OCamlLiteral::String(
                    "Hello, World!".to_string()
                ))),
            }
        );

        assert_eq!(
            syntax_items[2],
            OCaml::Let {
                name: "CHAR".to_string(),
                ty: Some("c_char".to_string()),
                value: Some(OCamlExpr::Literal(OCamlLiteral::Char('a'))),
            }
        );

        assert_eq!(
            syntax_items[3],
            OCaml::Let {
                name: "BOOL".to_string(),
                ty: Some("c_bool".to_string()),
                value: Some(OCamlExpr::Literal(OCamlLiteral::Bool(true))),
            }
        );

        assert_eq!(
            syntax_items[4],
            OCaml::Let {
                name: "BOOL".to_string(),
                ty: Some("c_bool".to_string()),
                value: Some(OCamlExpr::Literal(OCamlLiteral::Bool(false))),
            }
        );

        assert_eq!(
            syntax_items[5],
            OCaml::Let {
                name: "BYTE".to_string(),
                ty: Some("c_uchar".to_string()),
                value: Some(OCamlExpr::Literal(OCamlLiteral::Byte(b'a'))),
            }
        );

        assert_eq!(
            syntax_items[6],
            OCaml::Let {
                name: "RAW_STRING".to_string(),
                ty: Some("c_str".to_string()),
                value: Some(OCamlExpr::Literal(OCamlLiteral::String(
                    "Hello, World!".to_string()
                ))),
            }
        );

    }
}

