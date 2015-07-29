(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** {!Format} pretty-printer combinators. *)

(** {1 Formatters} *)

type 'a t = Format.formatter -> 'a -> unit
(** The type for formatters of values of type ['a]. *)

val pp : Format.formatter ->
  ('a, Format.formatter, unit) Pervasives.format -> 'a
(** [pp] is {!Format.fprintf}. *)

val kpp : (Format.formatter -> 'a) -> Format.formatter ->
  ('b, Format.formatter, unit, 'a) format4 -> 'b
(** [kpp] is {!Format.kfprintf}. *)

val rpp : ('a, Format.formatter, unit) Pervasives.format ->
  Format.formatter -> 'a
(** [rpp] is [pp fmt ppf] *)

val nop : 'a t
(** [nop] formats nothing. *)

val cut : unit t
(** [cut] is {!Format.pp_print_cut}. *)

val sp : unit t
(** [sp] is {!Format.pp_print_space}. *)

val const : 'a t -> 'a -> unit t
(** [const pp_v v] always formats [v] using [pp_v]. *)

val doomed : string t
(** [doomed] should be used for printing a message when reasonable
    assumptions are being violated. The string should be a short
    description of what is going on. *)

(** {1:basetypes OCaml base type formatters} *)

val bool : bool t
(** [bool] is {!Format.pp_print_bool}. *)

val int : int t
(** [int] is {!Format.pp_print_int}. *)

val int32 : int32 t
(** [int32 ppf] is [pp ppf "%ld"]. *)

val int64 : int64 t
(** [int64 ppf] is [pp ppf "%Ld"]. *)

val uint32 : int32 t
(** [int32 ppf] is [pp ppf "%lu"]. *)

val uint64 : int64 t
(** [uint64 ppf] is [pp ppf "%Lu"]. *)

val uint : int t
(** [uint ppf] is [pp ppf "%u"]. *)

val float : float t
(** [float ppf] is [pp ppf "%g".] *)

val float_dfrac : int -> float t
(** [float_dfrac d] rounds the float to the [d]th {e decimal}
    fractional digit and formats the result with ["%g"]. Ties are
    rounded towards positive infinity. The result is only defined
    for [0 <= d <= 16]. *)

val float_dsig : int -> float t
(** [pp_float_dsig d] rounds the normalized {e decimal} significand
    of the float to the [d]th decimal fractional digit and formats
    the result with ["%g"]. Ties are rounded towards positive
    infinity. The result is NaN on infinities and only defined for
    [0 <= d <= 16].

    {b Warning.} The current implementation overflows on large [d]
    and floats. *)

val string : string t
(** [string] is {!Format.pp_print_string}. *)

val const_string : string -> unit t
(** [const_string s] is [const string s]. *)

(** {1:conts OCaml container formatters} *)

val none : unit t
(** [none ppf] is [pp ppf "None"]. *)

val some : 'a t -> 'a t
(** [some pp_v ppf] is [pp ppf "@[<1>Some@ %a@]" pp_v]. *)

val option : ?pp_none:unit t -> 'a t -> 'a option t
(** [option pp_none pp_v] formats value of type ['a option] using
    [pp_v] and [pp_none] defaults to {!nop}. *)

val list : ?pp_sep:unit t -> 'a t -> 'a list t
(** [pp_list pp_sep pp_v] formats lists of type ['a]. Each value is
     printed with [pp_v], and values are separated by [pp_sep]
     (defaults to {!cut}). {!nop} on empty lists. *)

(** {1:bracks Brackets} *)

val parens : 'a t -> 'a t
(** [parens pp_v ppf] is [pp "@[<1>(%a)@]" pp_v]. *)

val brackets : 'a t -> 'a t
(** [brackets pp_v ppf] is [pp "@[<1>[%a]@]" pp_v]. *)

val braces : 'a t -> 'a t
(** [brackets pp_v ppf] is [pp "@[<1>{%a}@]" pp_v]. *)

(** {1:text Text and lines} *)

val text : string t
  (** [pp_text] formats text by replacing spaces and newlines in the string
      with calls to {!Format.pp_print_space} and {!Format.pp_force_newline}. *)

val lines : string t
(** [pp_lines] formats lines by replacing newlines in the string
      with calls to {!Format.pp_force_newline}. *)

val text_range : ((int * int) * (int * int)) t
(** [text_range] formats a line-column text range according to
    {{:http://www.gnu.org/prep/standards/standards.html#Errors}
    GNU conventions}. *)

(** {1 Byte sizes} *)

val byte_size : int t
(** [pp_byte_size] formats a byte size according to its magnitude
    using {{:http://www.bipm.org/en/publications/si-brochure/chapter3.html}
    SI prefixes} up to peta bytes (10{^15}). *)

val bi_byte_size : int t
(** [pp_bi_byte_size] formats a byte size according to its magnitude
    using {{:https://en.wikipedia.org/wiki/Binary_prefix}binary prefixes}
    up to pebi bytes (2{^15}). *)

(** {1:utf8_cond Conditional UTF-8 formatting}

    {b Note.} Since {!Format} is not UTF-8 aware using UTF-8 output
    may derail the pretty printing process. Use the pretty-printers
    from {!Uuseg_string} if you are serious about UTF-8 formatting. *)

val if_utf_8 : 'a t -> 'a t -> 'a t
(** [if_utf_8 pp_u pp] is a t that will use [pp_u] if UTF-8
    output is {{!utf_8_enabled}enabled} and [pp] otherwise. *)

(** {2:utf8_cond Conditional UTF-8 formatting control} *)

val utf_8_enabled : unit -> bool
(** [utf_8_enabled ()] is [true] if UTF-8 pretty-printing is enabled. *)

val set_utf_8_enabled : bool -> unit
(** [set_utf_8_enabled b] sets UTF-8 pretty-printing to [b]. *)

(** {1:styled Styled formatting} *)

type style =
  [ `Bold
  | `Underline
  | `Black
  | `Red
  | `Green
  | `Yellow
  | `Blue
  | `Magenta
  | `Cyan
  | `White
  | `None ]
(** The type for styles. *)

val styled : style -> 'a t -> 'a t
(** [styled style pp] formats according to [pp] but styled with [style]. *)

val styled_string : style -> string t
(** [styled_string style] is [pp_styled style string]. *)

(** {2 Styled formatting control} *)

type style_tags = [ `Ansi | `None ]
(** The type for style tags.
      {ul
      {- [`Ansi], tags the text with
       {{:http://www.ecma-international.org/publications/standards/Ecma-048.htm}
           ANSI escape sequences}.}
      {- [`None], text remains untagged.}} *)

val style_tags : unit -> style_tags
(** [style_tags ()] is the current tag style used by {!Fmt.pp_styled}.
      Initial value is [`None]. *)

val set_style_tags : style_tags -> unit
(** [set_style_tags s] sets the current tag style used by
      {!Fmt.pp_styled}. *)

(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli.
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
