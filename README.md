# Rust to OCaml Translator

Takes in Rust code, outputs OCaml code.

Specifically, the goal is to take in the [Rust Raw Libc FFI bindings]() and turn them into OCaml Raw Libc FFI bindings. This will allow OCaml to have a unified layer for accessing system calls.

## TODO:
- [X] modularising the code 
- [X] Unary operations
- [ ] Binary types
- [ ] Transform variables/types to lowercase?
- [ ] Structs 
- [ ] impl blocks
- [ ] accept file as command line arg

## Contributing
Have a look through existing [Issues](https://github.com/0xGlitchbyte/rust2ocaml/issues) and [Pull Requests](https://github.com/0xGlitchbyte/rust2ocaml/pulls) that you could help with. If you'd like to request a feature or report a bug, please create a GitHub Issue using one of the templates provided.
