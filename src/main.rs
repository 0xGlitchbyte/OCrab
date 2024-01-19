#[warn(unused_imports)]
use std::env;
use std::fmt::format;
use std::fs::File;
use std::io::Read;
use std::process;
use syn::{Item, ItemConst, Pat::Ident};

//use syn::File;

#[derive(Debug)]
enum OCaml {
    Let {
        name: String,
        //type_: String,
        //value: String,
    },
}

fn main() {
    let filename = "src/empty.rs";

    let mut file = File::open(&filename).expect("Unable to open file");

    let mut src = String::new();
    file.read_to_string(&mut src).expect("Unable to read file");

    let syntax = syn::parse_file(&src).expect("Unable to parse file");

    let syntax_items: String = syntax
        .items
        .into_iter()
        .flat_map(rust_item_to_ocaml_item)
        .map(ocaml_item_to_ocaml_code)
        .collect::<Vec<String>>()
        .join("\n");

    // Debug impl is available if Syn is built with "extra-traits" feature.
    println!("{:#?}", syntax_items);
}

fn rust_item_to_ocaml_item(item: syn::Item) -> Option<OCaml> {
    match item {
        Item::Const(ItemConst { ident: name, .. }) => Some(OCaml::Let {
            name: format!("{}", name),
        }),
        _ => None,
    }
}

fn ocaml_item_to_ocaml_code(ocaml: OCaml) -> String {
    match ocaml {
        OCaml::Let { name } => format!("let {} = 0", name),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        unimplemented!();
    }
}
