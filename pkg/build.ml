#!/usr/bin/env ocaml
#directory "pkg";;
#use "topkg.ml";;

let () =
  Pkg.describe "fmt" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "src/fmt";
    Pkg.lib ~exts:Exts.module_library "src/fmt_tty";
    Pkg.lib ~exts:Exts.library "src/fmt_top";
    Pkg.lib "src/fmt_top_init.ml";
    Pkg.doc "README.md";
    Pkg.doc "CHANGES.md"; ]
