(*
 * Copyright (c) 2014 David Sheets <sheets@alum.mit.edu>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open Ctypes
open View

module Kstatfs = struct
  type t
  let t : t structure typ = structure "Kstatfs"
  let ( -:* ) s x = field t s x
  let blocks  = "blocks"  -:* uint64_t
  let bfree   = "bfree"   -:* uint64_t
  let bavail  = "bavail"  -:* uint64_t
  let files   = "files"   -:* uint64_t
  let ffree   = "ffree"   -:* uint64_t
  let bsize   = "bsize"   -:* uint32_t
  let namelen = "namelen" -:* uint32_t
  let frsize  = "frsize"  -:* uint32_t
  let padding = "padding" -:* uint32_t
  let spare   = "spare"   -:* array 6 uint32_t
  let () = seal t
end

module File_lock = struct
  type t
  let t : t structure typ = structure "File_lock"
  let ( -:* ) s x = field t s x
  let start  = "start" -:* uint64_t
  let end_   = "end_"  -:* uint64_t
  let type_  = "type_" -:* uint32_t
  let pid    = "pid"   -:* uint32_t (* tgid *)
  let () = seal t
end

module SetattrValid = struct
  let onbit  = Int32.shift_left 1l

  let none = 0l

  let attrs =
    [|
      "mode";
      "uid";
      "gid";
      "size";
      "atime";
      "mtime";
      "handle";
      "atime_now";
      "mtime_now";
      "lock_owner";
    |]
  let index_of v =
    let rec is_i i = if attrs.(i) = v then i else is_i (i+1) in
    is_i 0

  let mode       = onbit (index_of "mode")
  let uid        = onbit (index_of "uid")
  let gid        = onbit (index_of "gid")
  let size       = onbit (index_of "size")
  let atime      = onbit (index_of "atime")
  let mtime      = onbit (index_of "mtime")
  let handle     = onbit (index_of "handle")
  let atime_now  = onbit (index_of "atime_now")
  let mtime_now  = onbit (index_of "mtime_now")
  let lock_owner = onbit (index_of "lock_owner")

  let is_set flags bit = (Int32.logand flags bit) <> none
  let attrs flags =
    let len = Array.length attrs in
    let rec collect i lst =
      if i > 31 then lst
      else collect (i+1) begin if is_set flags (onbit i)
        then if i >= len
          then ("bit"^(string_of_int i))::lst
          else attrs.(i)::lst
        else lst
      end
    in collect 0 []
end
