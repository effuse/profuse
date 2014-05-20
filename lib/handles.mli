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

module Unix_dir  : HANDLE with type t = Unix.dir_handle
module Unix_file : HANDLE with type t = Unix.file_descr

module Make(D : HANDLE)(F : HANDLE) : sig
  type id = int64
  type fh =
  | Dir of D.t * int
  | File of F.t * Unix_sys_stat.File_kind.t
  type handle = { space : t; id : id; path : string; kind : fh; }
  and t

  val create         : ?label:string -> unit -> t
  val free           : handle -> unit
  val alloc          : t -> string -> fh -> handle
  val get            : t -> id -> handle
  val with_dir_fd    : t -> id -> (handle -> D.t -> int -> 'a) -> 'a
  val with_file_fd   :
    t -> id -> (handle -> F.t -> Unix_sys_stat.File_kind.t -> 'a) -> 'a
  val set_dir_offset : handle -> int -> unit
end
