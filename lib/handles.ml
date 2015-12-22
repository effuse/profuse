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

(* TODO: this should either stop using Unix or move elsewhere *)

module Stat = Sys_stat

module type HANDLE = sig
  type t

  val close : t -> unit
end

type id = int64
type 'k h = {
  space : 'k h space;
  id    : id;
  kind  : 'k;
}
and 'h space = {
  label        : string;
  table        : (id, 'h) Hashtbl.t;
  mutable free : id list;
  mutable max  : id;
}
type ('d,'f) fh =
| Dir of 'd
| File of 'f * Unix.file_kind

let update { space; id } h' = Hashtbl.replace space.table id h'

module Unix_dir = struct
  type t = Unix.dir_handle * int

  let set_dir_offset = function
    | { kind=Dir (dir, _) } as h -> fun offset ->
      update h { h with kind=Dir (dir, offset) }
    | { kind=File (_,_) } -> raise Unix.(Unix_error (ENOTDIR,"",""))

  let close (h,_) = Unix.closedir h
end

module Unix_file : HANDLE with type t = Unix.file_descr = struct
  type t = Unix.file_descr

  let close = Unix.close
end

module Make(D : HANDLE)(F : HANDLE) = struct
  type handle = (D.t, F.t) fh h
  type t = handle space

  let handles_count = ref 0

  let create ?(label="handles_"^(string_of_int !handles_count)) () =
    incr handles_count;
    {
      label;
      table = Hashtbl.create 256;
      free  = [];
      max   = 0L;
    }

  let free_fh space id =
    let { table; free } = space in
    Hashtbl.remove table id;
    space.free <- id::free

  let free h =
    let { space; id; kind } = h in
    free_fh space id;
    match kind with
    | Dir  dir    -> D.close dir
    | File (fd,_) -> F.close fd

  let alloc space kind =
    let id = match space.free with
      | id::r -> space.free <- r; id
      | [] -> let id = space.max in space.max <- Int64.add id 1L; id
    in
    let h = { space; id; kind; } in
    Hashtbl.replace space.table id h;
    h

  let get space id =
    let { table } = space in
    try Hashtbl.find table id
    with Not_found -> raise (Unix.(Unix_error (EBADF,"","")))

  let with_dir_fd space id f = match get space id with
    | { kind=Dir dir } as h -> f h dir
    | { kind=File (_,_) } -> raise Unix.(Unix_error (ENOTDIR,"",""))
  let with_file_fd space id f = match get space id with
    | { kind=File (fd, kind) } as h -> f h fd kind
    | { kind=Dir _ } -> raise Unix.(Unix_error (EISDIR,"",""))
end
