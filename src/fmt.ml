(*---------------------------------------------------------------------------
   Copyright 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Formatting *)

let pf = Format.fprintf
let kpf = Format.kfprintf
let pr = Format.printf
let epr = Format.eprintf
let strf = Format.asprintf

(* Formatters *)

type 'a t = Format.formatter -> 'a -> unit

let nop fmt ppf = ()
let cut = Format.pp_print_cut
let sp = Format.pp_print_space
let const pp_v v ppf () = pf ppf "%a" pp_v v

(* OCaml base type formatters *)

let bool = Format.pp_print_bool
let int = Format.pp_print_int
let int32 ppf v = pf ppf "%ld" v
let int64 ppf v = pf ppf "%Ld" v
let uint32 ppf v = pf ppf "%lu" v
let uint64 ppf v = pf ppf "%Lu" v
let uint ppf v = pf ppf "%u" v

let string = Format.pp_print_string
let const_string s ppf () = pf ppf "%s" s

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

(* OCaml container formatters *)

let none ppf () = pf ppf "None"
let some pp_v ppf v = pf ppf "@[<1>Some@ %a@]" pp_v v
let option ?(pp_none = fun ppf () -> ()) pp_v ppf = function
| None -> pp_none ppf ()
| Some v -> pp_v ppf v

let rec list ?(pp_sep = cut) pp_v ppf = function
| [] -> ()
| v :: vs ->
    pp_v ppf v; if vs <> [] then (pp_sep ppf (); list ~pp_sep pp_v ppf vs)

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

(* Conditional UTF-8 formatting *)

let utf_8_enabled, set_utf_8_enabled =
  let enabled = ref false in
  (fun () -> !enabled), (fun b -> enabled := b)

let if_utf_8 pp_u pp ppf v = (if utf_8_enabled () then pp_u else pp) ppf v

(* Styled formatting *)

type style_tags = [ `Ansi | `None ]
type style =
  [ `Bold | `Underline | `Black | `Red | `Green | `Yellow | `Blue | `Magenta
  | `Cyan | `White | `None ]

let (style_tags : unit -> style_tags), (set_style_tags : style_tags -> unit) =
  let style_tags = ref `None in
  (fun () -> !style_tags), (fun s -> style_tags := s)

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

let styled style pp_v ppf = match style_tags () with
| `None -> pp_v ppf
| `Ansi ->
    Format.kfprintf
      (fun ppf -> pf ppf "@<0>%s" ansi_style_reset) ppf "@<0>%s%a"
      (ansi_style_code style) pp_v

let styled_string style = styled style string

(* Formatters *)

let stdout = Format.std_formatter
let stderr = Format.err_formatter

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
