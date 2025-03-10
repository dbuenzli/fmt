open B0_kit.V000
open Result.Syntax

(* OCaml library names *)

let compiler_libs_toplevel = B0_ocaml.libname "compiler-libs.toplevel"
let cmdliner = B0_ocaml.libname "cmdliner"
let unix = B0_ocaml.libname "unix"

let fmt = B0_ocaml.libname "fmt"
let fmt_cli = B0_ocaml.libname "fmt.cli"
let fmt_tty = B0_ocaml.libname "fmt.tty"
let fmt_top = B0_ocaml.libname "fmt.top"

(* Libraries *)

let fmt_lib =
  let srcs = Fpath.[`File (v "src/fmt.mli"); `File (v "src/fmt.ml")] in
  let requires = [] in
  B0_ocaml.lib fmt ~doc:"The fmt library" ~srcs ~requires

let fmt_cli =
  let srcs = Fpath.[`File (v "src/fmt_cli.mli"); `File (v "src/fmt_cli.ml")]in
  let requires = [cmdliner; fmt] in
  B0_ocaml.lib fmt_cli ~doc:"The fmt.cli library" ~srcs ~requires

let fmt_tty =
  let srcs = Fpath.[`File (v "src/fmt_tty.mli"); `File (v "src/fmt_tty.ml")]in
  let requires = [unix; fmt] in
  B0_ocaml.lib fmt_tty ~doc:"The fmt.tty library" ~srcs ~requires

let fmt_top =
  let srcs = Fpath.[`File (v "src/fmt_top.ml")] in
  let requires = [compiler_libs_toplevel] in
  B0_ocaml.lib fmt_top ~doc:"The fmt.top library" ~srcs ~requires

(* Tests *)

let test =
  let srcs = Fpath.[`File (v "test/test.ml")] in
  let meta = B0_meta.(empty |> tag test) in
  let requires = [ fmt ] in
  B0_ocaml.exe "test" ~doc:"Test suite" ~srcs ~meta ~requires

let styled_test_bug =
  let srcs = Fpath.[`File (v "test/styled_perf_bug.ml")] in
  let meta = B0_meta.(empty |> tag test) in
  let requires = [unix; fmt] in
  B0_ocaml.exe "styled_perf_bug" ~srcs ~meta ~requires

(* Packs *)

let default =
  let meta =
    B0_meta.empty
    |> ~~ B0_meta.authors ["The fmt programmers"]
    |> ~~ B0_meta.maintainers
       ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> ~~ B0_meta.homepage "https://erratique.ch/software/fmt"
    |> ~~ B0_meta.online_doc "https://erratique.ch/software/fmt/doc/"
    |> ~~ B0_meta.licenses ["ISC"]
    |> ~~ B0_meta.repo "git+https://erratique.ch/repos/fmt.git"
    |> ~~ B0_meta.issues "https://github.com/dbuenzli/fmt/issues"
    |> ~~ B0_meta.description_tags
      ["string"; "format"; "pretty-print"; "org:erratique"]
    |> B0_meta.tag B0_opam.tag
    |> ~~ B0_opam.depopts ["base-unix", ""; "cmdliner", ""]
    |> ~~ B0_opam.conflicts
      [ "cmdliner", {|< "1.3.0"|}]
    |> ~~ B0_opam.depends
      [ "ocaml", {|>= "4.08.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
      ]
    |> ~~ B0_opam.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"
          "--with-base-unix" "%{base-unix:installed}%"
          "--with-cmdliner" "%{cmdliner:installed}%"]]|}
  in
  B0_pack.make "default" ~doc:"fmt package" ~meta ~locked:true @@
  B0_unit.list ()
