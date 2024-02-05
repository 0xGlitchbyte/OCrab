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
            Self::Let { name, ty, value } => match (ty, value) {
                (Some(ty), None) => write!(f, "let {} : {}", name, ty),
                (None, Some(value)) => write!(f, "let {} = {}", name, value),
                (Some(ty), Some(value)) => write!(f, "let {} : {} = {}", name, ty, value),
                (None, None) => Ok(()),
            },
            Self::Statements(s) => {
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

impl OCamlType {
    fn maybe_unit(&self) -> bool {
        match self {
            Self::Tuple(t) => t.is_empty(),
            Self::Unit | Self::Never => true,
            _ => false,
        }
    }
}

impl Display for OCamlType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Pointer { ty, is_const: _is_const } => write!(f, "ref {}", ty),
            Self::Array { ty, size: _size } => write!(f, "{} array", ty),
            Self::Function { args, ret } => {
                for ty in args.iter() {
                    write!(f, "{} -> ", ty)?;
                }
                if args.is_empty() {
                    write!(f, "unit -> ")?;
                }
                if ret.maybe_unit() {
                    write!(f, "unit")
                } else {
                    write!(f, "{}", ret)
                }
            },
            Self::Path(p) => {
                for part in p.iter().take(p.len() - 1) {
                    write!(f, "{}.", part)?;
                }

                if let Some(last) = p.last() {
                    write!(f, "{}", to_ocaml_type(last))?;
                }
                Ok(())
            },
            Self::Tuple(t) => {
                for (index, ty) in t.iter().enumerate() {
                    write!(f, "{}", ty)?;
                    if index < t.len() - 1 {
                        write!(f, " * ")?;
                    }
                }
                if t.is_empty() {
                    write!(f, "()")?;
                }
                Ok(())
            },
            Self::Never | Self::Unit => write!(f, "unit"),
            Self::Paren(ty) => write!(f, "({})", ty),
            Self::Verbatim(ty) => write!(f, "{}", ty),
        }
    }
}

impl Display for OCamlExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(lit) => write!(f, "{}", lit),
            Self::Path(p) => write!(f, "{}", p.join(".")),
            Self::Unary(unary) => write!(f, "{}", unary),
            Self::Binary(binary) => write!(f, "{}", binary),
        }
    }
}

impl Display for OCamlLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer {
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
            Self::Float { digits, width } => {
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
            Self::String(s) => write!(f, "\"{}\"", s),
            Self::Char(c) => write!(f, "'{}'", c),
            Self::Bool(b) => write!(f, "{}", b),
            Self::Byte(b) => write!(f, "'\\x{:02x}'", b),
            Self::ByteStr(bs) => write!(f, "Bytes.of_string \"{}\"", bs.iter().map(|b| format!("\\x{:02x}", b)).collect::<String>()),
        }
    }
}

impl Display for OCamlUnary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Minus(neg) => write!(f, "-({})", neg),
            Self::Not(not) => write!(f, "not ({})", not),
            Self::Deref(star) => write!(f, "!({})", star),
        }
    }
}

impl std::fmt::Display for OCamlBinary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // ========================= Binary Operations Based on Types =========================
            // TODO: Detect the type of the left and right and use the correct binary operator
            Self::Plus { left, right } => write!(f, "({left} + {right})"),
            Self::Minus { left, right } => write!(f, "({left} - {right})"),
            Self::Multiply { left, right } => write!(f, "({left} * {right})"),
            Self::Divide { left, right } => write!(f, "({left} / {right})"),
            Self::Eq { left, right } => write!(f, "({left} = {right})"),
            Self::Ne { left, right } => write!(f, "({left} <> {right})"),

            // =====================================================================================

            Self::Modulo { left, right } => write!(f, "({left} mod {right})"),
            Self::And { left, right } => write!(f, "({left} && {right})"),
            Self::Or { left, right } => write!(f, "({left} || {right})"),
            Self::BitXor { left, right } => write!(f, "({left} lxor {right})"),
            Self::BitAnd { left, right } => write!(f, "({left} land {right})"),
            Self::BitOr { left, right } => write!(f, "({left} lor {right})"),
            Self::Shl { left, right } => write!(f, "({left} lsl {right})"),
            Self::Shr { left, right } => write!(f, "({left} lsr {right})"),
            Self::Lt { left, right } => write!(f, "({left} < {right})"),
            Self::Le { left, right } => write!(f, "({left} <= {right})"),
            Self::Gt { left, right } => write!(f, "({left} > {right})"),
            Self::Ge { left, right } => write!(f, "({left} >= {right})"),
            Self::AddAssign { .. } => unimplemented!("These operations are not supported and should be transformed into simple binary operation."),
            Self::SubAssign { .. } => unimplemented!("These operations are not supported and should be transformed into simple binary operation."),
            Self::MulAssign { .. } => unimplemented!("These operations are not supported and should be transformed into simple binary operation."),
            Self::DivAssign { .. } => unimplemented!("These operations are not supported and should be transformed into simple binary operation."),
            Self::RemAssign { .. } => unimplemented!("These operations are not supported and should be transformed into simple binary operation."),
            Self::BitXorAssign { .. } => unimplemented!("These operations are not supported and should be transformed into simple binary operation."),
            Self::BitAndAssign { .. } => unimplemented!("These operations are not supported and should be transformed into simple binary operation."),
            Self::BitOrAssign { .. } => unimplemented!("These operations are not supported and should be transformed into simple binary operation."),
            Self::ShlAssign { .. } => unimplemented!("These operations are not supported and should be transformed into simple binary operation."),
            Self::ShrAssign { .. } => unimplemented!("These operations are not supported and should be transformed into simple binary operation."),
            
        }
    }
}
