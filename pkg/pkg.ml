#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg
let ( let* ) = ( >>= )

let unix = Conf.with_pkg "base-unix"
let cmdliner = Conf.with_pkg "cmdliner"

let is_ocaml_5_1 c =
  let major, minor, _, _ = Conf.OCaml.version (Conf.OCaml.v c `Host_os) in
  (major, minor) >= (5, 1)

let copy src dst =
  let* () = OS.File.delete ~must_exist:false dst in
  let* contents = OS.File.read src in
  OS.File.write dst contents

let select_isatty_impl c =
  let is_ocaml_5_1 = is_ocaml_5_1 c in
  let meta_src = if is_ocaml_5_1 then "pkg/META.nounix" else "pkg/META.unix" in
  let isatty_src = if is_ocaml_5_1 then "isatty_oc.ml" else "isatty_unix.ml" in
  let* () = copy ("src/tty/" ^ isatty_src) "src/tty/isatty.ml" in
  let* () = copy meta_src "pkg/META" in
  if is_ocaml_5_1 then
    OS.File.delete ~must_exist:false "src/tty/_tags"
  else
    OS.File.write "src/tty/_tags" "<isatty.ml> or <fmt_tty*> : package(unix)\n"


let () =
  let build = Pkg.build ~pre:select_isatty_impl () in
  Pkg.describe ~build "fmt" @@ fun c ->
  let unix = Conf.value c unix in
  let cmdliner = Conf.value c cmdliner in
  let build_tty = is_ocaml_5_1 c || unix in
  Ok [ Pkg.mllib "src/fmt.mllib";
       Pkg.mllib ~cond:build_tty ~api:["Fmt_tty"] ~dst_dir:"tty" "src/tty/fmt_tty.mllib";
       Pkg.mllib ~cond:cmdliner ~dst_dir:"cli" "src/cli/fmt_cli.mllib";
       Pkg.mllib ~api:[] ~dst_dir:"top" "src/top/fmt_top.mllib";
       Pkg.lib ~dst:"top/fmt_tty_top_init.ml" "src/top/fmt_tty_top_init.ml";
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld" ]
