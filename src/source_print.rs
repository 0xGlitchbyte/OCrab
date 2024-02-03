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

fn to_ocaml_type(ty: &str) -> String {
    if !(ty.starts_with('f') || ty.starts_with('i') || ty.starts_with('u')) {
        return ty.to_string();
    }

    let (prefix, width) = size_from_rust_basic_number_type(ty);
    if prefix == b'f' {
        return format!("float")
    }

    if let Some(width) = width {
        match width {
            8 => format!("char"),
            16 => format!("int"),
            32 => format!("int32"),
            64 => format!("int64"),
            _ => panic!("Unknown width: {}", width),
        }
    } else {
        format!("nativeint")
    }
}

impl Display for OCamlType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OCamlType::Pointer { ty, is_const: _is_const } => write!(f, "ref {}", ty),
            OCamlType::Array { ty, size } => write!(f, "{} array", ty),
            OCamlType::Function { args, ret } => {
                for ty in args.iter() {
                    write!(f, "{} -> ", ty)?;
                }
                write!(f, "{}", ret)
            },
            OCamlType::Path(p) => {
                for part in p.iter().take(p.len() - 1) {
                    write!(f, "{}.", part)?;
                }

                if let Some(last) = p.last() {
                    write!(f, "{}", to_ocaml_type(last))?;
                }
                Ok(())
            },
            OCamlType::Tuple(t) => {
                for (index, ty) in t.iter().enumerate() {
                    write!(f, "{}", ty)?;
                    if index < t.len() - 1 {
                        write!(f, " * ")?;
                    }
                }
                Ok(())
            },
            OCamlType::Verbatim(ty) => write!(f, "{}", ty),
        }
    }
}

impl Display for OCamlExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OCamlExpr::Literal(lit) => write!(f, "{}", lit),
            OCamlExpr::Path(p) => write!(f, "{}", p.join(".")),
            OCamlExpr::Unary(unary) => write!(f, "{}", unary),
            OCamlExpr::Binary(binary) => write!(f, "{}", binary),
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
                        8 => write!(f, "'\\x{:02x}'", digits.parse::<u8>().unwrap()),
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

impl std::fmt::Display for OCamlBinary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // ========================= Binary Operations Based on Types =========================
            // TODO: Detect the type of the left and right and use the correct binary operator
            OCamlBinary::Plus { left, right } => write!(f, "({left} + {right})"),
            OCamlBinary::Minus { left, right } => write!(f, "({left} - {right})"),
            OCamlBinary::Multiply { left, right } => write!(f, "({left} * {right})"),
            OCamlBinary::Divide { left, right } => write!(f, "({left} / {right})"),
            OCamlBinary::Eq { left, right } => write!(f, "({left} = {right})"),
            OCamlBinary::Ne { left, right } => write!(f, "({left} <> {right})"),

            // =====================================================================================

            OCamlBinary::Modulo { left, right } => write!(f, "({left} mod {right})"),
            OCamlBinary::And { left, right } => write!(f, "({left} && {right})"),
            OCamlBinary::Or { left, right } => write!(f, "({left} || {right})"),
            OCamlBinary::BitXor { left, right } => write!(f, "({left} lxor {right})"),
            OCamlBinary::BitAnd { left, right } => write!(f, "({left} land {right})"),
            OCamlBinary::BitOr { left, right } => write!(f, "({left} lor {right})"),
            OCamlBinary::Shl { left, right } => write!(f, "({left} lsl {right})"),
            OCamlBinary::Shr { left, right } => write!(f, "({left} lsr {right})"),
            OCamlBinary::Lt { left, right } => write!(f, "({left} < {right})"),
            OCamlBinary::Le { left, right } => write!(f, "({left} <= {right})"),
            OCamlBinary::Gt { left, right } => write!(f, "({left} > {right})"),
            OCamlBinary::Ge { left, right } => write!(f, "({left} >= {right})"),
            OCamlBinary::AddAssign { .. } => unimplemented!("These operations are not supported and should be transformed into simple binary operation."),
            OCamlBinary::SubAssign { .. } => unimplemented!("These operations are not supported and should be transformed into simple binary operation."),
            OCamlBinary::MulAssign { .. } => unimplemented!("These operations are not supported and should be transformed into simple binary operation."),
            OCamlBinary::DivAssign { .. } => unimplemented!("These operations are not supported and should be transformed into simple binary operation."),
            OCamlBinary::RemAssign { .. } => unimplemented!("These operations are not supported and should be transformed into simple binary operation."),
            OCamlBinary::BitXorAssign { .. } => unimplemented!("These operations are not supported and should be transformed into simple binary operation."),
            OCamlBinary::BitAndAssign { .. } => unimplemented!("These operations are not supported and should be transformed into simple binary operation."),
            OCamlBinary::BitOrAssign { .. } => unimplemented!("These operations are not supported and should be transformed into simple binary operation."),
            OCamlBinary::ShlAssign { .. } => unimplemented!("These operations are not supported and should be transformed into simple binary operation."),
            OCamlBinary::ShrAssign { .. } => unimplemented!("These operations are not supported and should be transformed into simple binary operation."),
            
        }
    }
}
