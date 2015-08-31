(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** [Fmt] TTY setup.

    [Fmt_tty] provides simple automatic setup on channel formatters for:
    {ul
    {- {!Fmt.set_style_renderer}. [`Ansi_tty] is used if the channel
       {{!Unix.isatty}is a tty} and the environment variable
       [TERM] is defined and its value is not ["dumb"]. [`None] is
       used otherwise.}
    {- {!Fmt.set_utf_8}. [true] is used if one of the following
       environment variables has ["UTF-8"] as a case insensitive
       substring: [LANG], [LC_ALL], [LC_CTYPE].}} *)

(** {1 TTY setup}. *)

val setup : ?style_renderer:Fmt.style_renderer -> ?utf_8:bool ->
  out_channel -> Format.formatter
(** [setup ?style_renderer ?utf_8 outc] is a formatter for [outc] with
    {!Fmt.set_style_renderer} and {!Fmt.set_utf_8} correctly setup. If
    [style_renderer] or [utf_8] are specified they override the automatic
    setup.

    If [outc] is {!stdout}, {!Fmt.stdout} is returned. If [outc] is
    {!stderr}, {!Fmt.stderr} is returned. *)

val setup_std_outputs : ?style_renderer:Fmt.style_renderer -> ?utf_8:bool ->
  unit -> unit
(** [setup_std_outputs ?style_renderer ?utf_8 ()] applies {!setup}
    on {!stdout} and {!stderr}. *)

(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli.
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
