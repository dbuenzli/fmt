(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** {!Format} pretty-printer combinators.

    Consult {{!nameconv}naming conventions} for your pretty-printers.

    {b References}
    {ul
    {- The {!Format} module documentation.}
    {- The required reading {!Format} module
       {{:https://ocaml.org/learn/tutorials/format.html}tutorial}.}}

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}} *)

include Fmt_sig.S with type formatter := Format.formatter

val strf : ('a, Format.formatter, unit, string) format4 -> 'a
(** [strf] is {!Format.asprintf}.

    {b Note.} When using [strf] {!utf_8} and {!style_renderer} are
    always respectively set to [true] and [`None]. See also
    {!strf_like}. *)

val kstrf : (string -> 'a) ->
  ('b, Format.formatter, unit, 'a) format4 -> 'b
(** [kstrf] is like {!strf} but continuation based. *)

val strf_like : Format.formatter ->
  ('a, Format.formatter, unit, string) format4 -> 'a
(** [strf_like ppf] is like {!strf} except its {!utf_8} and {!style_renderer}
    settings are those of [ppf]. *)

val with_buffer : ?like:Format.formatter -> Buffer.t -> Format.formatter
(** [with_buffer ~like b] is a formatter whose {!utf_8} and {!style_renderer}
    settings are copied from those of {!like} (if provided). *)

(** {1:fmt Formatting to standard outputs} *)

val stdout : Format.formatter
(** [stdout] is the standard output formatter. *)

val stderr : Format.formatter
(** [stderr] is the standard error formatter. *)

val pr : ('a, Format.formatter, unit) format -> 'a
(** [pr] is [pf stdout]. *)

val epr : ('a, Format.formatter, unit) format -> 'a
(** [epr] is [pf stderr]. *)

(** {1:fmt_exns Formatting exceptions} *)

val failwith : ('a, Format.formatter, unit, 'b) format4 -> 'a
(** [failwith] is [kstrf failwith], raises {!Pervasives.Failure} with
    a pretty-printed string argument. *)

val invalid_arg : ('a, Format.formatter, unit, 'b) format4 -> 'a
(** [invalid_arg] is [kstrf invalid_arg], raises
    {!Pervasives.Invalid_argument} with a pretty-printed string argument. *)

(** {1:utf8_cond Conditional UTF-8 formatting}

    {b Note.} Since {!Format} is not UTF-8 aware using UTF-8 output
    may derail the pretty printing process. Use the pretty-printers
    from {!Uuseg_string} if you are serious about UTF-8 formatting. *)

val if_utf_8 : 'a t -> 'a t -> 'a t
(** [if_utf_8 pp_u pp ppf v] is:
    {ul
    {- [pp_u ppf v] if [utf_8 ppf] is [true].}
    {- [pp ppf v] otherwise.}} *)

val utf_8 : Format.formatter -> bool
(** [utf_8 ppf] is [true] if UTF-8 output is enabled on [ppf]. If
    {!set_utf_8} hasn't been called on [ppf] this is [true]. *)

val set_utf_8 : Format.formatter -> bool -> unit
(** [set_utf_8 ppf b] enables or disables conditional UTF-8 formatting
    on [ppf].

    {b Warning.} Using this function replaces any {!Format.tag} functions
    that may be in place.

    @raise Invalid_argument if [ppf] is {!Format.str_formatter}: it is
    is always UTF-8 enabled. *)

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
(** [styled s pp] formats like [pp] but styled with [s]. *)

val styled_unit : style -> (unit, Format.formatter, unit) Pervasives.format ->
  unit t
(** [styled_unit s fmt] is [style s (unit fmt)]. *)

(** {2 Style rendering control} *)

type style_renderer = [ `Ansi_tty | `None ]
(** The type for style renderers.
    {ul
    {- [`Ansi_tty], renders styles using
       {{:http://www.ecma-international.org/publications/standards/Ecma-048.htm}
       ANSI escape sequences}.}
    {- [`None], styled rendering has no effect.}} *)

val style_renderer : Format.formatter  -> style_renderer
(** [style_renderer ppf] is the style renderer used by [ppf].  If
    {!set_style_renderer} has never been called on [ppf] this is
    [`None]. *)

val set_style_renderer : Format.formatter -> style_renderer -> unit
(** [set_style_renderer ppf r] sets the style renderer of [ppf] to [r].

    {b Warning.} Using this function replaces any {!Format.tag} functions
    that may be in place.

    @raise Invalid_argument if [ppf] is {!Format.str_formatter}: its
    renderer is always [`None]. *)

(** {1:stringconverters Converting with string value converters} *)

val of_to_string : ('a -> string) -> 'a t
(** [of_to_string f ppf v] is [string ppf (f v)]. *)

val to_to_string : 'a t -> 'a -> string
(** [to_to_string pp_v v] is [strf "%a" pp_v v]. *)

(** {2 Functorial API} *)

module type S = sig
  type formatter
  val kfprintf : 
    (formatter -> 'a) ->
    formatter -> ('b, formatter, unit, 'a) format4 -> 'b
end
module Make (F : S) : Fmt_sig.S with type formatter = F.formatter

(** {1:nameconv Naming conventions}

    Given a type [ty] use:

    {ul
    {- [pp_ty] for a pretty printer that provides full control to the
       client and does not wrap the formatted value in an enclosing
       box. See {{!polytypes}these examples}.}
    {- [dump_ty] for a pretty printer that provides little control
       over the pretty-printing process, wraps the rendering in an
       enclosing box and tries as much as possible to respect the
       OCaml syntax. These pretty-printers should make it easy to
       inspect and understand values of the given type, they are
       mainly used for quick printf debugging and/or toplevel interaction.
       See {{!Dump.polytypes} these examples}.}}

    If you are in a situation where making a difference between [dump_ty]
    and [pp_ty] doesn't make sense then use [pp_ty].

    For a type [ty] that is the main type of the module (the "[M.t]"
    convention) drop the suffix, that is simply use [M.pp] and
    [M.dump]. *)

(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli

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
