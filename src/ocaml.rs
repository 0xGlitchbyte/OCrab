use std::fs::File;
use std::io::Read;
use syn;

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
    pub fn from_rust_file(file_path: &str) -> Self {
        let mut file = File::open(file_path).expect("Unable to open file");

        let mut src = String::new();
        file.read_to_string(&mut src).expect("Unable to read file");

        let syntax = syn::parse_file(&src).expect("Unable to parse file");

        //println!("{:#?}", syntax);

        let syntax_items = syntax
            .items
            .iter()
            .map(|item| item.into())
            .collect::<Vec<Self>>();

        // Debug impl is available if Syn is built with "extra-traits" feature.
        println!("{:#?}", syntax_items);

        Self::Statements(syntax_items)
    }
}

#[derive(Debug)]
pub enum OCamlExpr {
    Literal(OCamlLiteral),
    Path(Vec<String>),
    Unary(Box<OCamlUnaryOperator>), //Binary
                                    //Struct
}

#[derive(Debug)]
pub enum OCamlLiteral {
    Number(String),
}

#[derive(Debug)]
pub enum OCamlUnaryOperator {
    Minus(OCamlExpr),
    Deref(OCamlExpr),
    Not(OCamlExpr),
}

//#[derive(Debug)]
//enum OCamlBinaryExpr {
//    And { left: OCamlExpr, right: OCamlExpr },
//    Or { left: OCamlExpr, right: OCamlExpr },
//}

fn extract_var_from_rust_ast(path: &syn::Path) -> Vec<String> {
    path.segments
        .iter()
        .map(|seg| seg.ident.to_string())
        .collect()
}

pub fn extract_type_from_rust_ast(ty: &syn::Type) -> Option<String> {
    // Type -> Path -> Segements -> PathSegment -> Ident(String)
    // Type::TypePath::Path::Punctuated{PathSegment{Ident}}
    match ty {
        syn::Type::Path(type_path) => {
            let path = &type_path.path;
            let seg = path.segments.last()?;
            let ident = &seg.ident;
            Some(ident.to_string())
        }
        _ => todo!("{:#?} is not implemented", ty),
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
                ty: extract_type_from_rust_ast(&ty),
            },
            _ => todo!("{:#?} is not implemented", item),
        }
    }
}

impl From<&syn::Expr> for OCamlExpr {
    fn from(value: &syn::Expr) -> Self {
        match value {
            syn::Expr::Lit(syn::ExprLit { lit, .. }) => OCamlExpr::Literal(lit.into()),
            syn::Expr::Path(syn::ExprPath { path, .. }) => {
                OCamlExpr::Path(extract_var_from_rust_ast(path))
            }
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
