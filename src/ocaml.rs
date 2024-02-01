use std::fs::File;
use std::io::Read;
use syn;

const CHAR_BIT: usize = 8;

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

#[derive(Debug)]
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

        let syntax = syn::parse_file(&src).map_err(|e| {
            OCamlError::Parse(format!("'{}': {}", file_path, e))
        })?;

        //println!("{:#?}", syntax);

        let syntax_items: Vec<Self> = syntax.items.iter().map(|item| item.into()).collect();

        // Debug impl is available if Syn is built with "extra-traits" feature.
        println!("{:#?}", syntax_items);

        Ok(Self::Statements(syntax_items))
    }
}

#[derive(Debug)]
pub enum OCamlExpr {
    Literal(OCamlLiteral),
    Path(Vec<String>),
    Unary(Box<OCamlUnaryExpr>), //Binary
                                //Struct
}

#[derive(Debug)]
pub enum OCamlLiteral {
    Integer{ digits: String, width: Option<usize>, is_signed: Option<bool>, is_native: bool },
    Float{ digits: String, width: Option<usize> },
}

#[derive(Debug)]
pub enum OCamlUnaryExpr {
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
            _ => todo!("{:#?} is not implemented", value),
        }
    }
}

impl From<&syn::Lit> for OCamlLiteral {
    fn from(value: &syn::Lit) -> Self {
        match value {
            syn::Lit::Int(int) => int.into(),
            syn::Lit::Float(float) => float.into(),
            _ => todo!("{:#?} is not implemented", value),
        }
    }
}

fn size_of<T>() -> usize {
    std::mem::size_of::<T>() * CHAR_BIT
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
                is_native: false
            }
        }

        let digits = digits.trim_end_matches(suffix).to_string();

        match suffix {
            "u8" | "i8" => OCamlLiteral::Integer {
                digits,
                width: Some(size_of::<u8>()),
                is_signed: Some(suffix.starts_with("i")),
                is_native: false
            },
            "u16" | "i16" => OCamlLiteral::Integer {
                digits,
                width: Some(size_of::<u16>()),
                is_signed: Some(suffix.starts_with("i")),
                is_native: false
            },
            "u32" | "i32" => OCamlLiteral::Integer {
                digits,
                width: Some(size_of::<u32>()),
                is_signed: Some(suffix.starts_with("i")),
                is_native: false
            },
            "u64" | "i64" => OCamlLiteral::Integer {
                digits,
                width: Some(size_of::<u64>()),
                is_signed: Some(suffix.starts_with("i")),
                is_native: false
            },
            "u128" | "i128" => OCamlLiteral::Integer {
                digits,
                width: Some(size_of::<u128>()),
                is_signed: Some(suffix.starts_with("i")),
                is_native: false
            },
            "usize" | "isize" => OCamlLiteral::Integer {
                digits,
                width: Some(size_of::<usize>()),
                is_signed: Some(suffix.starts_with("i")),
                is_native: true
            },
            "f32" => OCamlLiteral::Float {
                digits,
                width: Some(size_of::<f32>()),
            },
            "f64" => OCamlLiteral::Float {
                digits,
                width: Some(size_of::<f64>()),
            },
            _ => unreachable!("Unknown suffix: {}", suffix)
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
            }
        }

        let digits = digits.trim_end_matches(suffix).to_string();

        match suffix {
            "f32" => OCamlLiteral::Float {
                digits,
                width: Some(size_of::<f32>()),
            },
            "f64" => OCamlLiteral::Float {
                digits,
                width: Some(size_of::<f64>()),
            },
            _ => unreachable!("Unknown suffix: {}", suffix)
        }
    }
}
