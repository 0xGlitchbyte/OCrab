use std::fmt::Display;
use std::fs::File;
use std::io::Read;
use syn::{Expr, ExprLit, Item, ItemConst, Lit};

//use syn::File;

#[derive(Debug)]
enum OCaml {
    Let {
        name: String,
        //type_: String,
        value: String,
    },
}

impl Display for OCaml {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OCaml::Let { name, value } => write!(f, "let {} = {}", name, value),
        }
    }
}

#[derive(Debug)]
enum OCamlExpr {
    Literal(OCamlLiteral),
}

impl Display for OCamlExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OCamlExpr::Literal(lit) => write!(f, "{}", lit),
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

    let mut file = File::open(&filename).expect("Unable to open file");

    let mut src = String::new();
    file.read_to_string(&mut src).expect("Unable to read file");

    let syntax = syn::parse_file(&src).expect("Unable to parse file");

    println!("{:#?}", syntax);

    let syntax_items: String = syntax
        .items
        .into_iter()
        .flat_map(rust_item_to_ocaml_item)
        .map(|item| item.to_string())
        .collect::<Vec<String>>()
        .join("\n");

    // Debug impl is available if Syn is built with "extra-traits" feature.
    println!("{:#?}", syntax_items);
}

fn rust_item_to_ocaml_item(item: syn::Item) -> Option<OCaml> {
    match item {
        Item::Const(ItemConst {
            ident: name,
            expr: value,
            ..
        }) => Some(OCaml::Let {
            name: format!("{}", name),
            value: format!("{}", rust_expr_to_ocaml_expr(&value).unwrap()),
        }),
        _ => None,
    }
}

fn rust_expr_to_ocaml_expr(expr: &Expr) -> Option<OCamlExpr> {
    match expr {
        Expr::Lit(ExprLit { attrs, lit }) => {
            Some(OCamlExpr::Literal(rust_literal_to_ocaml_literal(&lit)?))
        }
        _ => None,
    }
}

fn rust_literal_to_ocaml_literal(lit: &Lit) -> Option<OCamlLiteral> {
    match lit {
        Lit::Int(int) => Some(OCamlLiteral::Number(format!("{}", int))),
        _ => None,
    }
}
