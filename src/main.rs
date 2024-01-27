mod ocaml;
mod source_print;

use ocaml::*;
use source_print::*;

fn main() {
    let rust_file = "empty.rs";
    let ocaml_file = "libc.ml";
    let ocaml_ast = OCaml::from_rust_file(rust_file);
    let printer = SourcePrinter::from_ocaml_ast(ocaml_ast);
    printer.print_sources(ocaml_file);
}
