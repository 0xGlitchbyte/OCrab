use std::fs::File;
use std::io::Read;
use std::{fmt::Display, fs};
use syn::{Expr, ExprLit, ExprPath, Item, ItemConst, Lit, Path, Type};

//use syn::File;

#[derive(Debug)]
enum OCaml {
    Let {
        name: String,
        ty: Option<String>,
        value: Option<OCamlExpr>,
    },
    Statements(Vec<OCaml>),
}

impl Display for OCaml {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OCaml::Let { name, ty, value } => match (ty, value) {
                (Some(ty), None) => write!(f, "let {} : {}", name.to_lowercase(), ty),
                (None, Some(value)) => write!(f, "let {} = {}", name.to_lowercase(), value),
                (Some(ty), Some(value)) => {
                    write!(f, "let {} : {} = {}", name.to_lowercase(), ty, value)
                }
                (None, None) => Ok(()),
            },
            OCaml::Statements(s) => {
                for item in s.iter() {
                    writeln!(f, "{}", item)?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Debug)]
enum OCamlExpr {
    Literal(OCamlLiteral),
    Path(Vec<String>), //Unary
                 //Binary
                 //Struct
}

impl Display for OCamlExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OCamlExpr::Literal(lit) => write!(f, "{}", lit),
            OCamlExpr::Path(p) => write!(f, "{}", p.iter().map(|s| s.to_string()).collect::<Vec<String>>().join(".")),
        }
    }
}

#[derive(Debug)]
enum OCamlLiteral {
    Number(String),
}

impl Display for OCamlLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OCamlLiteral::Number(int) => write!(f, "{}", int),
        }
    }
}

fn main() {
    let filename = "empty.rs";

    let mut file = File::open(filename).expect("Unable to open file");

    let mut src = String::new();
    file.read_to_string(&mut src).expect("Unable to read file");

    let syntax = syn::parse_file(&src).expect("Unable to parse file");

    //println!("{:#?}", syntax);

    let syntax_items = syntax
        .items
        .into_iter()
        .flat_map(rust_item_to_ocaml_item)
        .collect::<Vec<OCaml>>();

    // Debug impl is available if Syn is built with "extra-traits" feature.
    println!("{:#?}", syntax_items);

    let statements = OCaml::Statements(syntax_items);

    write_ocaml_to_ml_file(statements);
}

fn rust_item_to_ocaml_item(item: syn::Item) -> Option<OCaml> {
    match item {
        Item::Const(ItemConst {
            ident: name,
            expr: value,
            ty: ty,
            ..
        }) => Some(OCaml::Let {
            name: format!("{}", name),
            value: rust_expr_to_ocaml_expr(&value),
            ty: extract_type_from_rust_ast(&ty),
        }),
        _ => todo!("{:#?} is not implemented", item),
    }
}

fn rust_expr_to_ocaml_expr(expr: &Expr) -> Option<OCamlExpr> {
    match expr {
        Expr::Lit(ExprLit { lit, .. }) => {
            Some(OCamlExpr::Literal(rust_literal_to_ocaml_literal(lit)?))
        }
        Expr::Path(ExprPath { path, .. }) => Some(OCamlExpr::Path(extract_var_from_rust_ast(path))),
        _ => todo!("{:#?} is not implemented", expr),
    }
}

fn rust_literal_to_ocaml_literal(lit: &Lit) -> Option<OCamlLiteral> {
    match lit {
        Lit::Int(int) => Some(OCamlLiteral::Number(format!("{}", int))),
        _ => todo!("{:#?} is not implemented", lit),
    }
}

fn extract_var_from_rust_ast(path: &Path) -> Vec<String> {
    let mut path: Vec<String> = path
        .segments
        .iter()
        .map(|seg| seg.ident.to_string())
        .collect();

    if let Some(last) = path.last_mut() {
        *last = last.to_lowercase();
    }

    return path
}

fn extract_type_from_rust_ast(ty: &Type) -> Option<String> {
    // Type -> Path -> Segements -> PathSegment -> Ident(String)
    // Type::TypePath::Path::Punctuated{PathSegment{Ident}}
    match ty {
        Type::Path(type_path) => {
            let path = &type_path.path;
            let seg = path.segments.last()?;
            let ident = &seg.ident;
            Some(ident.to_string())
        }
        _ => todo!("{:#?} is not implemented", ty),
    }
}

fn write_ocaml_to_ml_file(ocaml_code: OCaml) {
    let data = ocaml_code.to_string();
    let file_name = "libc.ml";
    fs::write(file_name, data).expect("Unable to write file");
}
