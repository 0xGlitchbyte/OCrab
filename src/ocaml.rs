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
    Number(String),
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
            syn::Lit::Int(int) => OCamlLiteral::Number(int.to_string()),
            _ => todo!("{:#?} is not implemented", value),
        }
    }
}
