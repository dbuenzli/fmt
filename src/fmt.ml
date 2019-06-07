(*---------------------------------------------------------------------------
   Copyright (c) 2014 The fmt programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Errors *)

let err_str_formatter = "Format.str_formatter can't be set."

(* Formatting *)

let pf = Format.fprintf
let pr = Format.printf
let epr = Format.eprintf
let strf = Format.asprintf
let kpf = Format.kfprintf
let kstrf f fmt =
  let buf = Buffer.create 64 in
  let f fmt =
    Format.pp_print_flush fmt ();
    let s = Buffer.contents buf in
    Buffer.reset buf; f s
  in
  Format.kfprintf f (Format.formatter_of_buffer buf) fmt

(* Standard output formatting *)

let stdout = Format.std_formatter
let stderr = Format.err_formatter

(* Exception formatting *)

let invalid_arg' = invalid_arg
let failwith fmt = kstrf failwith fmt
let invalid_arg fmt = kstrf invalid_arg fmt

(* Formatters *)

type 'a t = Format.formatter -> 'a -> unit

let nop fmt ppf = ()
let const pp_v v ppf _ = pf ppf "%a" pp_v v
let unit fmt ppf () = pf ppf fmt
let fmt fmt ppf = pf ppf fmt
let always fmt ppf v = pf ppf fmt

(* Separators *)

let cut ppf _ = Format.pp_print_cut ppf ()
let sp ppf _ = Format.pp_print_space ppf ()
let comma ppf _ = pf ppf ",@ "
let semi ppf _ = pf ppf ";@ "

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

let exn ppf e = string ppf (Printexc.to_string e)
let exn_backtrace ppf (e, bt) =
  let pp_backtrace_str ppf s =
    let stop = String.length s - 1 (* there's a newline at the end *) in
    let rec loop left right =
      if right = stop then string ppf (String.sub s left (right - left)) else
      if s.[right] <> '\n' then loop left (right + 1) else
      begin
        string ppf (String.sub s left (right - left));
        cut ppf ();
        loop (right + 1) (right + 1)
      end
    in
    if s = "" then (string ppf "No backtrace available.") else
    loop 0 0
  in
  pf ppf "@[<v>Exception: %a@,%a@]"
    exn e pp_backtrace_str (Printexc.raw_backtrace_to_string bt)

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

let result ~ok ~error ppf = function
| Ok v -> ok ppf v
| Error e -> error ppf e

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
let seq ?sep pp_elt = iter ?sep Seq.iter pp_elt
let hashtbl ?sep pp_binding = iter_bindings ?sep Hashtbl.iter pp_binding
let queue ?sep pp_elt = iter Queue.iter pp_elt
let stack ?sep pp_elt = iter Stack.iter pp_elt

let using f pp ppf v = pp ppf (f v)

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
let oxford_brackets pp_v ppf v = pf ppf "@[<2>[|%a|]@]" pp_v v
let braces pp_v ppf v = pf ppf "@[<1>{%a}@]" pp_v v
let quote ?(mark = "\"") pp_v ppf v =
  pf ppf "@[<1>@<1>%s%a@<1>%s@]" mark pp_v v mark

module Dump = struct

  let sig_names = Sys.[
    sigabrt, "SIGABRT";
    sigalrm, "SIGALRM";
    sigfpe, "SIGFPE";
    sighup, "SIGHUP";
    sigill, "SIGILL";
    sigint, "SIGINT";
    sigkill, "SIGKILL";
    sigpipe, "SIGPIPE";
    sigquit, "SIGQUIT";
    sigsegv, "SIGSEGV";
    sigterm, "SIGTERM";
    sigusr1, "SIGUSR1";
    sigusr2, "SIGUSR2";
    sigchld, "SIGCHLD";
    sigcont, "SIGCONT";
    sigstop, "SIGSTOP";
    sigtstp, "SIGTSTP";
    sigttin, "SIGTTIN";
    sigttou, "SIGTTOU";
    sigvtalrm, "SIGVTALRM";
    sigprof, "SIGPROF";
    sigbus, "SIGBUS";
    sigpoll, "SIGPOLL";
    sigsys, "SIGSYS";
    sigtrap, "SIGTRAP";
    sigurg, "SIGURG";
    sigxcpu, "SIGXCPU";
    sigxfsz, "SIGXFSZ"; ]

  let signal ppf s = match List.assq_opt s sig_names with
  | Some name -> string ppf name
  | None -> pf ppf "SIG(%d)" s

  let uchar ppf u = pf ppf "U+%04X" (Uchar.to_int u)

  let pair pp_fst pp_snd ppf (fst, snd) =
    pf ppf "@[<1>(@[%a@],@ @[%a@])@]" pp_fst fst pp_snd snd

  let option pp_v ppf = function
  | None -> pf ppf "None"
  | Some v -> pf ppf "@[<2>Some@ @[%a@]@]" pp_v v

  let result ~ok ~error ppf = function
  | Ok v -> pf ppf "@[<2>Ok@ @[%a@]@]" ok v
  | Error e -> pf ppf "@[<2>Error@ @[%a@]@]" error e

  let list pp_elt = list ~sep:semi (box pp_elt) |> brackets
  let array pp_elt = array ~sep:semi (box pp_elt) |> oxford_brackets
  let seq pp_elt = seq ~sep:semi (box pp_elt) |> brackets

  let named pp1 pp2 ppf v = pf ppf "%a@ %a" pp1 v pp2 v

  let iter iter_f pp_name pp_elt =
    let pp_v = iter ~sep:sp iter_f (box pp_elt) in
    named pp_name pp_v |> parens

  let iter_bindings iter_f pp_name pp_k pp_v =
    let pp_v = iter_bindings ~sep:sp iter_f (pair pp_k pp_v) in
    named pp_name pp_v |> parens

  let hashtbl pp_k pp_v =
    iter_bindings Hashtbl.iter (always "hashtbl") pp_k pp_v

  let stack pp_elt = iter Stack.iter (always "stack") pp_elt
  let queue pp_elt = iter Queue.iter (always "queue") pp_elt
end

(* Text and lines *)

let is_nl c = c = '\n'
let is_nl_or_sp c = is_nl c || c = ' '
let is_white = function ' ' | '\t' .. '\r'  -> true | _ -> false
let not_white c = not (is_white c)
let not_white_or_nl c = is_nl c || not_white c

let rec stop_at sat ~start ~max s =
  if start > max then start else
  if sat s.[start] then start else
  stop_at sat ~start:(start + 1) ~max s

let sub s start stop ~max =
  if start = stop then "" else
  if start = 0 && stop > max then s else
  String.sub s start (stop - start)

let words ppf s =
  let max = String.length s - 1 in
  let rec loop start s = match stop_at is_white ~start ~max s with
  | stop when stop > max -> Format.pp_print_string ppf (sub s start stop ~max)
  | stop ->
      Format.pp_print_string ppf (sub s start stop ~max);
      match stop_at not_white ~start:stop ~max s with
      | stop when stop > max -> ()
      | stop -> Format.pp_print_space ppf (); loop stop s
  in
  let start = stop_at not_white ~start:0 ~max s in
  if start > max then () else loop start s

let paragraphs ppf s =
  let max = String.length s - 1 in
  let rec loop start s = match stop_at is_white ~start ~max s with
  | stop when stop > max -> Format.pp_print_string ppf (sub s start stop ~max)
  | stop ->
      Format.pp_print_string ppf (sub s start stop ~max);
      match stop_at not_white_or_nl ~start:stop ~max s with
      | stop when stop > max -> ()
      | stop ->
          if s.[stop] <> '\n'
          then (Format.pp_print_space ppf (); loop stop s) else
          match stop_at not_white_or_nl ~start:(stop + 1) ~max s with
          | stop when stop > max -> ()
          | stop ->
              if s.[stop] <> '\n'
              then (Format.pp_print_space ppf (); loop stop s) else
              match stop_at not_white ~start:(stop + 1) ~max s with
              | stop when stop > max -> ()
              | stop ->
                  Format.pp_force_newline ppf ();
                  Format.pp_force_newline ppf ();
                  loop stop s
  in
  let start = stop_at not_white ~start:0 ~max s in
  if start > max then () else loop start s

let text ppf s =
  let max = String.length s - 1 in
  let rec loop start s = match stop_at is_nl_or_sp ~start ~max s with
  | stop when stop > max -> Format.pp_print_string ppf (sub s start stop ~max)
  | stop ->
      Format.pp_print_string ppf (sub s start stop ~max);
      begin match s.[stop] with
      | ' ' -> Format.pp_print_space ppf ()
      | '\n' -> Format.pp_force_newline ppf ()
      | _ -> assert false
      end;
      loop (stop + 1) s
  in
  loop 0 s

let lines ppf s =
  let max = String.length s - 1 in
  let rec loop start s = match stop_at is_nl ~start ~max s with
  | stop when stop > max -> Format.pp_print_string ppf (sub s start stop ~max)
  | stop ->
      Format.pp_print_string ppf (sub s start stop ~max);
      Format.pp_force_newline ppf ();
      loop (stop + 1) s
  in
  loop 0 s

let text_loc ppf ((l0, c0), (l1, c1)) =
  if (l0 : int) == (l1 : int) && (c0 : int) == (c1 : int)
  then pf ppf "%d.%d" l0 c0
  else pf ppf "%d.%d-%d.%d" l0 c0 l1 c1

(* Appending *)

let append pp_v0 pp_v1 ppf v = pp_v0 ppf v; pp_v1 ppf v
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

(* Binary formatting *)

type 'a vec = int * (int -> 'a)

let iter_vec f (n, get) = for i = 0 to n - 1 do f i (get i) done
let vec ?sep = iter_bindings ?sep iter_vec

let on_string = using String.(fun s -> length s, get s)
let on_bytes = using Bytes.(fun b -> length b, get b)

let sub_vecs w (n, get) =
  (n - 1) / w + 1,
  fun j ->
    let off = w * j in
    min w (n - off), fun i -> get (i + off)

let prefix0x = [
  0xf       , fmt "%01x";
  0xff      , fmt "%02x";
  0xfff     , fmt "%03x";
  0xffff    , fmt "%04x";
  0xfffff   , fmt "%05x";
  0xffffff  , fmt "%06x";
  0xfffffff , fmt "%07x";
  0xffffffff, fmt "%08x"; ]

let padded0x ~max = match List.find_opt (fun (x, _) -> max <= x) prefix0x with
| Some (_, pp) -> pp
| None -> fmt "%x"

let ascii ?(w = 0) ?(subst = const char '.') () ppf (n, _ as v) =
  let pp_char ppf (_, c) =
    if '\x20' <= c && c < '\x7f' then char ppf c else subst ppf ()
  in
  vec pp_char ppf v;
  if n < w then Format.pp_print_break ppf (w - n) 0

let octets ?(w = 0) ?(sep = sp) () ppf (n, _ as v) =
  let pp_sep ppf i = if i > 0 && i mod 2 = 0 then sep ppf () in
  let pp_char ppf (i, c) = pf ppf "%a%02x" pp_sep i (Char.code c) in
  vec ~sep:nop pp_char ppf v;
  for i = n to w - 1 do pf ppf "%a@ @ " pp_sep i done

let addresses ?addr ?(w = 16) pp_vec ppf (n, _ as v) =
  let addr = match addr with
  | Some pp -> pp
  | _ ->
      let pp = padded0x ~max:(((n - 1) / w) * w) in
      fun ppf -> pf ppf "%a: " pp
  in
  let pp_sub ppf (i, sub) = pf ppf "%a@[%a@]" addr (i * w) pp_vec sub in
  vbox (vec pp_sub) ppf (sub_vecs w v)

let hex ?(w = 16) () =
  let octets = octets ~w () and ascii = ascii ~w () in
  addresses ~w (fun ppf v -> pf ppf "@[%a@]@ @ @[%a@]" octets v ascii v)

(* Conditional UTF-8 and styled formatting. *)

type any = ..
type 'a attr = int * ('a -> any) * (any -> 'a)

let id = ref 0
let attr (type a) () =
  incr id;
  let module M = struct type any += K of a end in
  !id, (fun x -> M.K x), (function M.K x -> x | _ -> assert false)

module Int = struct type t = int let compare a b = compare (a: int) b end
module Imap = Map.Make (Int)

let attrs = ref []
let store ppf =
  let open Ephemeron.K1 in
  let rec go ppf top = function
  | [] ->
      let e = create () and v = ref Imap.empty in
      attrs := e :: List.rev top; set_key e ppf; set_data e v; v
  | e::es ->
      match get_key e with
      | None -> go ppf top es
      | Some k when not (k == ppf) -> go ppf (e::top) es
      | Some k ->
          let v = match get_data e with Some v -> v | _ -> assert false in
          if not (top == []) then attrs := e :: List.rev_append top es;
          ignore (Sys.opaque_identity k); v
  in
  go ppf [] !attrs

let get (k, _, prj) ppf =
  match Imap.find_opt k !(store ppf) with Some x -> Some (prj x) | _ -> None

let set (k, inj, _) v ppf =
  if ppf == Format.str_formatter then invalid_arg' err_str_formatter else
  let s = store ppf in
  s := Imap.add k (inj v) !s

let def x = function Some y -> y | _ -> x

let utf_8_attr = attr ()
let utf_8 ppf = get utf_8_attr ppf |> def true
let set_utf_8 ppf x = set utf_8_attr x ppf

type style_renderer = [ `Ansi_tty | `None ]
let style_renderer_attr = attr ()
let style_renderer ppf = get style_renderer_attr ppf |> def `None
let set_style_renderer ppf x = set style_renderer_attr x ppf

let with_buffer ?like buf =
  let ppf = Format.formatter_of_buffer buf in
  (match like with Some like -> store ppf := !(store like) | _ -> ());
  ppf

let strf_like ppf fmt =
  let buf = Buffer.create 64 in
  let bppf = with_buffer ~like:ppf buf in
  let flush ppf =
    Format.pp_print_flush ppf ();
    let s = Buffer.contents buf in
    Buffer.reset buf; s
  in
  Format.kfprintf flush bppf fmt

(* Conditional UTF-8 formatting *)

let if_utf_8 pp_u pp = fun ppf v -> (if utf_8 ppf then pp_u else pp) ppf v

(* Styled formatting *)

type colour =
  [ `Black | `Red | `Green | `Yellow | `Blue | `Magenta | `Cyan | `White ]

type style =
  [ `Bold | `Italic | `Underline | `Reverse
  | colour | `Hi of colour | `Bg of [ colour | `Hi of colour ]
  | `None ]

let ansi_style_code = function
| `Bold -> "1"
| `Italic -> "3"
| `Underline -> "4"
| `Reverse -> "7"
| `Black -> "30"
| `Red -> "31"
| `Green -> "32"
| `Yellow -> "33"
| `Blue -> "34"
| `Magenta -> "35"
| `Cyan -> "36"
| `White -> "37"
| `Bg `Black -> "40"
| `Bg `Red -> "41"
| `Bg `Green -> "42"
| `Bg `Yellow -> "43"
| `Bg `Blue -> "44"
| `Bg `Magenta -> "45"
| `Bg `Cyan -> "46"
| `Bg `White -> "47"
| `Hi `Black -> "90"
| `Hi `Red -> "91"
| `Hi `Green -> "92"
| `Hi `Yellow -> "93"
| `Hi `Blue -> "94"
| `Hi `Magenta -> "95"
| `Hi `Cyan -> "96"
| `Hi `White -> "97"
| `Bg (`Hi `Black) -> "100"
| `Bg (`Hi `Red) -> "101"
| `Bg (`Hi `Green) -> "102"
| `Bg (`Hi `Yellow) -> "103"
| `Bg (`Hi `Blue) -> "104"
| `Bg (`Hi `Magenta) -> "105"
| `Bg (`Hi `Cyan) -> "106"
| `Bg (`Hi `White) -> "107"
| `None -> "0"

let pp_sgr ppf style =
  Format.pp_print_as ppf 0 "\027[";
  Format.pp_print_as ppf 0 style;
  Format.pp_print_as ppf 0 "m"

let curr_style = attr ()

let styled style pp_v ppf v = match style_renderer ppf with
| `None -> pp_v ppf v
| `Ansi_tty ->
    let curr = match get curr_style ppf with
    | None -> let s = ref "0" in set curr_style s ppf; s
    | Some s -> s in
    let prev = !curr and here = ansi_style_code style in
    curr := (match style with `None -> here | _ -> prev ^ ";" ^ here);
    try
      pf ppf "%a%a%a" pp_sgr here pp_v v pp_sgr prev; curr := prev
    with e -> curr := prev; raise e

let styled_unit style fmt = styled style (unit fmt)

(* Converting with string converters. *)

let of_to_string f ppf v = string ppf (f v)
let to_to_string pp_v v = strf "%a" pp_v v

(*---------------------------------------------------------------------------
   Copyright (c) 2014 The fmt programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
