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

type id = int64
type node = {
  space    : t;
  parent   : id;
  gen      : int64;
  id       : id;
  name     : string;
  path     : string;
  children : (string, id) Hashtbl.t;
  lookups  : int;
}
and t = {
  label        : string;
  root         : string;
  table        : (id,node) Hashtbl.t;
  mutable free : (int64 * id) list;
  mutable max  : id;
}

let nodes_count = ref 0

let create ?(label="nodes_"^(string_of_int !nodes_count)) root =
  incr nodes_count;
  {
    label;
    root;
    table = Hashtbl.create 256;
    free  = [];
    max   = 2L; (* 0 is the FS, 1 is the root *)
  }

let get space id =
  let { table } = space in
  try Hashtbl.find table id
  with Not_found ->
    if id=1L then
      let node = {
        space;
        parent = 1L;
        gen = 0L;
        id = 1L;
        name = "";
        path = space.root;
        children = Hashtbl.create 32;
        lookups = 0;
      } in
      Hashtbl.replace table id node;
      node
    else raise Not_found

let string_of_id s id =
  if id = Int64.zero (* TODO: should be in get for FUSE_INIT? *)
  then "id=0"
  else let {gen; id; path} = get s id in
       Printf.sprintf "%Ld.%Ld.%s" gen id path

let alloc_id s =
  match s.free with
  | genid::r -> s.free <- r; genid
  | [] -> let id = s.max in s.max <- Int64.add id 1L; (0L, id)

let to_string s =
  Printf.sprintf "%s table size: %d" s.label (Hashtbl.length s.table)

let lookup parent name =
  let { space } = parent in
  let { table } = space in
  try
    let id = Hashtbl.find parent.children name in
    try
      let node = Hashtbl.find table id in
      Hashtbl.replace table id { node with lookups = node.lookups + 1 };
      node
    with Not_found ->
      raise (Failure
               (Printf.sprintf "parent %s has %s but %s does not"
                  (string_of_id space parent.id)
                  name space.label
               ))
  with Not_found ->
    let path = Filename.concat parent.path name in
    let (gen,id) = alloc_id space in
    let node = {
      space;
      parent=parent.id;
      gen; id; name; path;
      children=Hashtbl.create 8;
      lookups=1;
    } in
    Hashtbl.replace parent.children name id;
    Hashtbl.replace table id node;
    node

let forget node =
  let { space } = node in
  let { table } = space in
  let parent = Hashtbl.find table node.parent in
  Hashtbl.remove parent.children node.name;
  Hashtbl.remove table node.id;
  space.free <- (Int64.add node.gen 1L, node.id)::space.free

let update node =
  let { space } = node in
  let { table } = space in
  Hashtbl.replace table node.id node
