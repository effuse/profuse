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

module type HANDLE = sig
  type t

  val close : t -> unit
end

type id = int64
type ('d, 'f) fh =
| Dir of 'd
| File of 'f * Unix_sys_stat.File_kind.t
type 'k h = {
  space : 'k h space;
  id : id;
  kind : 'k;
}
and 'h space

module Unix_dir  : sig
  include HANDLE with type t = Unix.dir_handle * int

  val set_dir_offset : (t, _) fh h -> int -> unit
end
module Unix_file : HANDLE with type t = Unix.file_descr

module Make(D : HANDLE)(F : HANDLE) : sig
  type handle = (D.t, F.t) fh h
  type t = handle space

  val create         : ?label:string -> unit -> t
  val free           : handle -> unit
  val alloc          : t -> (D.t, F.t) fh -> handle
  val get            : t -> id -> handle
  val with_dir_fd    : t -> id -> (handle -> D.t -> 'a) -> 'a
  val with_file_fd   :
    t -> id -> (handle -> F.t -> Unix_sys_stat.File_kind.t -> 'a) -> 'a
end
