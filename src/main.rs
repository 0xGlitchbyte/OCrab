use std::fs::File;
use std::io::Read;

mod ocaml;
use ocaml::*;
//use syn::File;
fn main() {
    let rust_file = "empty.rs";
    let ocaml_file = "libc.ml";
    let ocaml_ast = OCaml::from_rust_file(rust_file);
    ocaml_ast.print_sources(ocaml_file);
}
