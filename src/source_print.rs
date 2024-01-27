use std::{fmt::Display, fs};

use crate::ocaml::*;

pub struct SourcePrinter(OCaml);

impl SourcePrinter {
    pub fn from_ocaml_ast(ocaml_ast: OCaml) -> Self {
        Self(ocaml_ast)
    }

    pub fn print_sources(self, file_name: &str) {
        let data = self.0.to_string();
        fs::write(file_name, data).expect("Unable to write file");
    }
}

impl Display for OCaml {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OCaml::Let { name, ty, value } => match (ty, value) {
                (Some(ty), None) => write!(f, "let {} : {}", name, ty),
                (None, Some(value)) => write!(f, "let {} = {}", name, value),
                (Some(ty), Some(value)) => {
                    write!(f, "let {} : {} = {}", name, ty, value)
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

impl Display for OCamlExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OCamlExpr::Literal(lit) => write!(f, "{}", lit),
            OCamlExpr::Path(p) => write!(
                f,
                "{}",
                p.iter()
                    .map(|s| s.to_string())
                    .collect::<Vec<String>>()
                    .join(".")
            ),
            OCamlExpr::Unary(unary) => write!(f, "{}", unary),
        }
    }
}

impl Display for OCamlLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OCamlLiteral::Number(int) => write!(f, "{}", int),
        }
    }
}

impl Display for OCamlUnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OCamlUnaryOperator::Minus(neg) => write!(f, "-{}", neg),
            OCamlUnaryOperator::Not(not) => write!(f, "{}", not),
            OCamlUnaryOperator::Deref(star) => write!(f, "{}", star),
        }
    }
}
