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
                (Some(ty), Some(value)) => write!(f, "let {} : {} = {}", name, ty, value),
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
            OCamlExpr::Path(p) => write!(f, "{}", p.join(".")),
            OCamlExpr::Unary(unary) => write!(f, "{}", unary),
        }
    }
}

impl Display for OCamlLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OCamlLiteral::Integer {
                digits,
                width,
                is_signed: _is_signed,
                is_native,
            } => {
                // TODO: use is_signed
                if *is_native == true {
                    write!(f, "{}n", digits)
                } else if let Some(width) = width {
                    match width {
                        8 => write!(f, "{}", digits),
                        16 => write!(f, "{}", digits),
                        32 => write!(f, "{}l", digits),
                        64 => write!(f, "{}L", digits),
                        _ => panic!("Unknown width: {}", width),
                    }
                } else {
                    write!(f, "{}", digits)
                }
            }
            OCamlLiteral::Float { digits, width } => {
                if let Some(width) = width {
                    let digits = if !digits.contains(".") {
                        format!("{}.0", digits)
                    } else {
                        digits.to_owned()
                    };

                    match width {
                        32 => write!(f, "{}", digits),
                        64 => write!(f, "{}", digits),
                        _ => panic!("Unknown width: {}", width),
                    }
                } else {
                    write!(f, "{}", digits)
                }
            }
            OCamlLiteral::String(s) => write!(f, "\"{}\"", s),
            OCamlLiteral::Char(c) => write!(f, "'{}'", c),
            OCamlLiteral::Bool(b) => write!(f, "{}", b),
            OCamlLiteral::Byte(b) => write!(f, "'\\x{:02x}'", b),
            OCamlLiteral::ByteStr(bs) => write!(f, "Bytes.of_string \"{}\"", bs.iter().map(|b| format!("\\x{:02x}", b)).collect::<String>()),
            OCamlLiteral::Verbatim(s) => write!(f, "{}", s),
        }
    }
}

impl Display for OCamlUnary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Minus(neg) => write!(f, "-({})", neg),
            Self::Not(not) => write!(f, "!({})", not),
            Self::Deref(star) => write!(f, "{}", star),
        }
    }
}
