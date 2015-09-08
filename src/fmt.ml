(*---------------------------------------------------------------------------
   Copyright 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Errors *)

let err_str_formatter = "Format.str_formatter can't be set."

(* Formatting *)

let pf = Format.fprintf
let kpf = Format.kfprintf
let strf = Format.asprintf
let kstrf f fmt =
  let buf = Buffer.create 17 in
  let f fmt = Format.pp_print_flush fmt () ; f (Buffer.contents buf) in
  Format.kfprintf f (Format.formatter_of_buffer buf) fmt

(* Standard output formatting *)

let stdout = Format.std_formatter
let stderr = Format.err_formatter
let pr = Format.printf
let epr = Format.eprintf

(* Formatters *)

type 'a t = Format.formatter -> 'a -> unit

let nop fmt ppf = ()
let cut = Format.pp_print_cut
let sp = Format.pp_print_space
let const pp_v v ppf () = pf ppf "%a" pp_v v
let unit fmt ppf () = pf ppf fmt
let fmt fmt ppf = pf ppf fmt
let always fmt ppf v = pf ppf fmt

(* Base type formatters *)

let bool = Format.pp_print_bool
let int = Format.pp_print_int
let nativeint ppf v = pf ppf "%nd" v
let int32 ppf v = pf ppf "%ld" v
let int64 ppf v = pf ppf "%Ld" v
let uint ppf v = pf ppf "%u" v
let uint32 ppf v = pf ppf "%lu" v
let uint64 ppf v = pf ppf "%Lu" v
let unativeint ppf v = pf ppf "%nu" v

let char = Format.pp_print_char
let string = Format.pp_print_string
let buffer ppf b = string ppf (Buffer.contents b)

(* Floats *)

let float ppf v = pf ppf "%g" v

let round x = floor (x +. 0.5)
let round_dfrac d x =
  if x -. (round x) = 0. then x else                   (* x is an integer. *)
  let m = 10. ** (float_of_int d) in                (* m moves 10^-d to 1. *)
  (floor ((x *. m) +. 0.5)) /. m

let round_dsig d x =
  if x = 0. then 0. else
  let m = 10. ** (floor (log10 (abs_float x))) in       (* to normalize x. *)
  (round_dfrac d (x /. m)) *. m

let float_dfrac d ppf f = pf ppf "%g" (round_dfrac d f)
let float_dsig d ppf f = pf ppf "%g" (round_dsig d f)

(* Polymorphic type formatters *)

let pair ?sep:(pp_sep = cut) pp_fst pp_snd ppf (fst, snd) =
  pp_fst ppf fst; pp_sep ppf (); pp_snd ppf snd

let option ?none:(pp_none = nop) pp_v ppf = function
| None -> pp_none ppf ()
| Some v -> pp_v ppf v

let iter ?sep:(pp_sep = cut) iter pp_elt ppf v =
  let is_first = ref true in
  let pp_elt v =
    if !is_first then (is_first := false) else pp_sep ppf ();
    pp_elt ppf v
  in
  iter pp_elt v

let iter_bindings ?sep:(pp_sep = cut) iter pp_binding ppf v =
  let is_first = ref true in
  let pp_binding k v =
    if !is_first then (is_first := false) else pp_sep ppf ();
    pp_binding ppf (k, v)
  in
  iter pp_binding v

let list ?sep pp_elt = iter ?sep List.iter pp_elt
let array ?sep pp_elt = iter ?sep Array.iter pp_elt
let hashtbl ?sep pp_binding = iter_bindings ?sep Hashtbl.iter pp_binding
let queue ?sep pp_elt = iter Queue.iter pp_elt
let stack ?sep pp_elt = iter Stack.iter pp_elt

let using f pp ppf v = pp ppf (f v)

module Dump = struct

  let pair pp_fst pp_snd ppf (fst, snd) =
    pf ppf "@[<1>(@[%a@],@ @[%a@])@]" pp_fst fst pp_snd snd

  let option pp_v ppf = function
  | None -> pf ppf "None"
  | Some v -> pf ppf "@[<1>Some@ @[%a@]@]" pp_v v

  let list pp_elt ppf vs =
    let rec loop = function
    | [] -> ()
    | v :: vs ->
        if vs = [] then (pf ppf "@[%a@]" pp_elt v) else
        (pf ppf "@[%a@];@ " pp_elt v; loop vs)
    in
    pf ppf "@[<1>["; loop vs; pf ppf "]@]"

  let array pp_elt ppf a =
    pf ppf "@[<2>[|";
    for i = 0 to Array.length a - 1 do
      if i = 0 then pf ppf "@[%a@]" pp_elt a.(i) else
      pf ppf ";@ @[%a@]" pp_elt a.(i)
    done;
    pf ppf "|]@]"

  let iter iter pp_name pp_elt ppf v =
    let is_first = ref true in
    let pp_elt v =
      if !is_first then (is_first := false) else pf ppf "@ ";
      pf ppf "@[%a@]" pp_elt v
    in
    pf ppf "@[<1>(%a@ " pp_name v;
    iter pp_elt v;
    pf ppf ")@]"

  let iter_bindings iter pp_name pp_k pp_v ppf bs =
    let is_first = ref true in
    let pp_binding k v =
      if !is_first then () else pf ppf "@ ";
      pf ppf "@[<1>(@[%a@],@ @[%a@])@]" pp_k k pp_v v
    in
    pf ppf "@[<1>(%a@ " pp_name bs;
    iter pp_binding bs;
    pf ppf ")@]"

  let hashtbl pp_k pp_v =
    iter_bindings Hashtbl.iter (always "hashtbl") pp_k pp_v

  let stack pp_elt = iter Stack.iter (always "stack") pp_elt
  let queue pp_elt = iter Queue.iter (always "queue") pp_elt
end

(* Boxes *)

let box ?(indent = 0) pp ppf =
  Format.pp_open_hovbox ppf indent; pf ppf "%a@]" pp

let hbox pp ppf =
  Format.pp_open_hbox ppf (); pf ppf "%a@]" pp

let vbox ?(indent = 0) pp ppf =
  Format.pp_open_vbox ppf indent; pf ppf "%a@]" pp

let hvbox ?(indent = 0) pp ppf =
  Format.pp_open_hvbox ppf indent; pf ppf "%a@]" pp

(* Brackets *)

let parens pp_v ppf v = pf ppf "@[<1>(%a)@]" pp_v v
let brackets pp_v ppf v = pf ppf "@[<1>[%a]@]" pp_v v
let braces pp_v ppf v = pf ppf "@[<1>{%a}@]" pp_v v

(* Text and lines *)

let white_str ~spaces ppf s =
  let left = ref 0 and right = ref 0 and len = String.length s in
  let flush () =
    Format.pp_print_string ppf (String.sub s !left (!right - !left));
    incr right; left := !right;
  in
  while (!right <> len) do
    if s.[!right] = '\n' then (flush (); Format.pp_force_newline ppf ()) else
    if spaces && s.[!right] = ' ' then (flush (); Format.pp_print_space ppf ())
    else incr right;
  done;
  if !left <> len then flush ()

let text = white_str ~spaces:true
let lines = white_str ~spaces:false
let text_range ppf ((l0, c0), (l1, c1)) = pf ppf "%d.%d-%d.%d" l0 c0 l1 c1

let doomed ppf reason =
  pf ppf "Something@ unreasonable@ is@ going@ on (%a).@ You@ are@ doomed."
    text reason

(* Appending *)

let append pp_v0 pp_v1 ppf (v0, v1) = pp_v0 ppf v0 ; pp_v1 ppf v1
let prefix pp_p pp_v ppf v = pp_p ppf (); pp_v ppf v
let suffix pp_s pp_v ppf v = pp_v ppf v; pp_s ppf ()

(* Byte sizes *)

let _pp_byte_size k i ppf s =
  let pp_frac = float_dfrac 1 in
  let div_round_up m n = (m + n - 1) / n in
  let float = float_of_int in
  if s < k then pf ppf "%dB" s else
  let m = k * k in
  if s < m then begin
    let kstr = if i = "" then "k" (* SI *) else "K" (* IEC *) in
    let sk = s / k in
    if sk < 10
    then pf ppf "%a%s%sB" pp_frac (float s /. float k) kstr i
    else pf ppf "%d%s%sB" (div_round_up s k) kstr i
  end else
  let g = k * m in
  if s < g then begin
    let sm = s / m in
    if sm < 10
    then pf ppf "%aM%sB" pp_frac (float s /. float m) i
    else pf ppf "%dM%sB" (div_round_up s m) i
  end else
  let t = k * g in
  if s < t then begin
    let sg = s / g in
    if sg < 10
    then pf ppf "%aG%sB" pp_frac (float s /. float g) i
    else pf ppf "%dG%sB" (div_round_up s g) i
  end else
  let p = k * t in
  if s < p then begin
    let st = s / t in
    if st < 10
    then pf ppf "%aT%sB" pp_frac (float s /. float t) i
    else pf ppf "%dT%sB" (div_round_up s t) i
  end else begin
    let sp = s / p in
    if sp < 10
    then pf ppf "%aP%sB" pp_frac (float s /. float p) i
    else pf ppf "%dP%sB" (div_round_up s p) i
  end

let byte_size ppf s = _pp_byte_size 1000 "" ppf s
let bi_byte_size ppf s = _pp_byte_size 1024 "i" ppf s

(* Conditional UTF-8 and styled formatting.

   This is very ugly, formally what we would like is to be able to
   store arbitrary typed metadata in formatters for clients to consult
   (tried to provide an API for doing that but dismissed it for
   uglyness and lack of an efficient implementation). In the following
   we are using the tags functions (but not the tags mechanism itself)
   as a way to store two metadata keys, one for formatter UTF-8
   awareness and the other for the formatter style renderer. *)

let utf_8_tag = "fmt.utf8"

let utf_8_of_raw = function
| "\x00" -> false
| "\x01" -> true
| _ -> true

let utf_8_to_raw = function
| false -> "\x00"
| true -> "\x01"

type style_renderer = [ `Ansi_tty | `None ]

let style_renderer_tag = "fmt.style_renderer"

let style_renderer_of_raw = function
| "\x00" -> `None
| "\x01" -> `Ansi_tty
| _ -> `None

let style_renderer_to_raw = function
| `None -> "\x00"
| `Ansi_tty -> "\x01"

let meta_store ppf = Format.pp_get_formatter_tag_functions ppf ()
let meta_raw store tag = store.Format.mark_open_tag tag
let set_meta ppf store ~utf_8 ~style_renderer =
  let meta = function
  | "fmt.utf8" -> utf_8
  | "fmt.style_renderer" -> style_renderer
  | _ -> "Fmt: do not use the tags mecanism, it is a broken idea"
  in
  let store = { store with Format.mark_open_tag = meta } in
  Format.pp_set_formatter_tag_functions ppf store

let utf_8 ppf = utf_8_of_raw (meta_raw (meta_store ppf) utf_8_tag)
let set_utf_8 ppf utf_8 =
  if ppf == Format.std_formatter then invalid_arg err_str_formatter else
  let store = meta_store ppf in
  let style_renderer = meta_raw store style_renderer_tag in
  let utf_8 = utf_8_to_raw utf_8 in
  set_meta ppf store ~utf_8 ~style_renderer

let style_renderer ppf =
  style_renderer_of_raw (meta_raw (meta_store ppf) style_renderer_tag)

let set_style_renderer ppf renderer =
  if ppf == Format.std_formatter then invalid_arg err_str_formatter else
  let store = meta_store ppf in
  let utf_8 = meta_raw store utf_8_tag in
  let style_renderer = style_renderer_to_raw renderer in
  set_meta ppf store ~utf_8 ~style_renderer

(* Conditional UTF-8 formatting *)

let if_utf_8 pp_u pp = fun ppf v -> (if utf_8 ppf then pp_u else pp) ppf v

(* Styled formatting *)

type style =
  [ `Bold | `Underline | `Black | `Red | `Green | `Yellow | `Blue | `Magenta
  | `Cyan | `White | `None ]

let ansi_style_code = function
| `Bold -> "\027[01m"
| `Underline -> "\027[04m"
| `Black -> "\027[30m"
| `Red -> "\027[31m"
| `Green -> "\027[32m"
| `Yellow -> "\027[33m"
| `Blue -> "\027[34m"
| `Magenta -> "\027[35m"
| `Cyan -> "\027[36m"
| `White -> "\027[37m"
| `None -> "\027[m"

let ansi_style_reset = "\027[m"

let styled style pp_v ppf = match style_renderer ppf with
| `None -> pp_v ppf
| `Ansi_tty ->
    let reset ppf = pf ppf "@<0>%s" ansi_style_reset in
    kpf reset ppf "@<0>%s%a" (ansi_style_code style) pp_v

let styled_unit style fmt = styled style (unit fmt)

(* Converting with string converters. *)

let of_to_string f ppf v = string ppf (f v)
let to_to_string pp_v v = strf "%a" pp_v v

(*---------------------------------------------------------------------------
   Copyright 2014 Daniel C. Bünzli.
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)
