(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** {!Cmdliner} support for [Fmt].

    {e Release %%VERSION%% - %%MAINTAINER%% } *)

(** {1 Color option} *)

val color : ?env:Cmdliner.Arg.env -> ?docs:string -> unit ->
  Fmt.style_renderer option Cmdliner.Term.t
(** [color ?env ?docs ()] is a {!Cmdliner} option [--color] that can
    be directly used with the optional arguments of
    {{!Fmt_tty.tty_setup}TTY setup} or to control
    {{!Fmt.set_style_renderer}style rendering}.  The option is
    documented under [docs] (defaults to the default in
    {!Cmdliner.Arg.info}).

    The option is a tri-state enumerated value that when used with
    {{!Fmt_tty.tty_setup}TTY setup} takes over the automatic setup:
    {ul
    {- [--color=never], the value is [Some `None], forces no styling.}
    {- [--color=always], the value is [Some `Ansi], forces ANSI styling.}
    {- [--color=auto] or absent, the value is [None], automatic setup
       takes place.}}

    If [env] is provided, the option default value ([None]) can be
    overridden by the corresponding environment variable. *)

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
