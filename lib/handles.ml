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

module Stat = Unix_sys_stat

module type HANDLE = sig
  type t

  val close : t -> unit
end

module Unix_dir : HANDLE with type t = Unix.dir_handle = struct
  type t = Unix.dir_handle

  let close = Unix.closedir
end

module Unix_file : HANDLE with type t = Unix.file_descr = struct
  type t = Unix.file_descr

  let close = Unix.close
end

module Make(D : HANDLE)(F : HANDLE) = struct
  type id = int64
  type fh =
  | Dir of D.t * int
  | File of F.t * Stat.File_kind.t
  type handle = {
    space : t;
    id    : id;
    path  : string;
    kind  : fh;
  }
  and t = {
    label        : string;
    table        : (id, handle) Hashtbl.t;
    mutable free : id list;
    mutable max  : id;
  }

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
    | Dir  (dir,_) -> D.close dir
    | File (fd,_)  -> F.close fd

  let alloc space path kind =
    let id = match space.free with
      | id::r -> space.free <- r; id
      | [] -> let id = space.max in space.max <- Int64.add id 1L; id
    in
    let h = { space; id; path; kind; } in
    Hashtbl.replace space.table id h;
    h

  let get space id =
    let { table } = space in
    try Hashtbl.find table id
    with Not_found -> raise (Unix.(Unix_error (EBADF,"","")))

  let with_dir_fd space id f = match get space id with
    | { kind=Dir (dir, off) } as h -> f h dir off
    | { kind=File (_,_) } -> raise Unix.(Unix_error (ENOTDIR,"",""))
  let with_file_fd space id f = match get space id with
    | { kind=File (fd, kind) } as h -> f h fd kind
    | { kind=Dir (_,_) } -> raise Unix.(Unix_error (EISDIR,"",""))

  let set_dir_offset = function
    | { space; id; kind=Dir (dir, _) } as h -> fun offset ->
      Hashtbl.replace space.table id { h with kind=Dir (dir, offset) }
    | { kind=File (_,_) } -> raise Unix.(Unix_error (ENOTDIR,"",""))
end
