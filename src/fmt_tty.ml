(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

let is_infix ~affix s =
  (* Damned, already missing astring, from which this is c&p *)
  let len_a = String.length affix in
  let len_s = String.length s in
  if len_a > len_s then false else
  let max_idx_a = len_a - 1 in
  let max_idx_s = len_s - len_a in
  let rec loop i k =
    if i > max_idx_s then false else
    if k > max_idx_a then true else
    if k > 0 then
      if String.get affix k = String.get s (i + k) then loop i (k + 1) else
      loop (i + 1) 0
    else if String.get affix 0 = String.get s i then loop i 1 else
    loop (i + 1) 0
  in
  loop 0 0

let setup ?style_renderer ?utf_8 oc =
  let ppf =
    if oc == Pervasives.stdout then Fmt.stdout else
    if oc == Pervasives.stderr then Fmt.stderr else
    Format.formatter_of_out_channel oc
  in
  let style_renderer = match style_renderer with
  | Some r -> r
  | None ->
      let dumb = try Sys.getenv "TERM" = "dumb" with
      | Not_found -> true
      in
      let isatty = try Unix.(isatty (descr_of_out_channel oc)) with
      | Unix.Unix_error _ -> false
      in
      if not dumb && isatty then `Ansi_tty else `None
  in
  let utf_8 = match utf_8 with
  | Some b -> b
  | None ->
      let has_utf_8 var =
        try is_infix "UTF-8" (String.uppercase (Sys.getenv var))
        with Not_found -> false
      in
      has_utf_8 "LANG" || has_utf_8 "LC_ALL" || has_utf_8 "LC_CTYPE"
  in
  Fmt.set_style_renderer ppf style_renderer;
  Fmt.set_utf_8 ppf utf_8;
  ppf

let setup_std_outputs ?style_renderer ?utf_8 () =
  ignore (setup ?style_renderer ?utf_8 stdout);
  ignore (setup ?style_renderer ?utf_8 stderr);
  ()

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
