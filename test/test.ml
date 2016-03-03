(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% v%%VERSION%%
  ---------------------------------------------------------------------------*)

let test_utf_8 () =
  let ppf = Format.formatter_of_buffer (Buffer.create 23) in
  assert (Fmt.utf_8 ppf = true);
  Fmt.set_utf_8 ppf false;
  assert (Fmt.utf_8 ppf = false);
  Fmt.set_utf_8 ppf true;
  assert (Fmt.utf_8 ppf = true);
  ()

let test_style_renderer () =
  let ppf = Format.formatter_of_buffer (Buffer.create 23) in
  assert (Fmt.style_renderer ppf = `None);
  Fmt.set_style_renderer ppf `Ansi_tty;
  assert (Fmt.style_renderer ppf = `Ansi_tty);
  Fmt.set_style_renderer ppf `None;
  assert (Fmt.style_renderer ppf = `None);
  ()

let tests () =
  test_utf_8 ();
  test_style_renderer ();
  Printf.printf "Done.\n";
  ()

let () = tests ()

(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli

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