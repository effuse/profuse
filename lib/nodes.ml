(*
 * Copyright (c) 2014-2016 David Sheets <sheets@alum.mit.edu>
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
type 'a node = {
  space    : 'a space;
  parent   : id option;
  gen      : int64;
  id       : id;
  name     : string;
  data     : 'a;
  children : (string, id) Hashtbl.t;
  lookups  : int;
}
and 'a space = {
  label        : string;
  root         : 'a;
  table        : (id,'a node) Hashtbl.t;
  mutable free : (int64 * id) list;
  mutable max  : id;
}

module type NODE = sig
  type t

  val to_string : t -> string
  val child : t node -> string -> t
  val rename : t node -> t node -> string -> unit
end

module Path : NODE with type t = string list = struct
  type t = string list

  let to_string = function
    | [] | [""] -> Filename.dir_sep
    | path -> String.concat Filename.dir_sep (List.rev path)

  let child node name = name::node.data

  let rec set_trunk k trunk branch = function
    | _ when k = 0 -> List.rev_append branch trunk
    | h::t -> set_trunk (k - 1) trunk (h::branch) t
    | [] -> assert false

  let rec rename_subtree trunk = function
    | [] -> ()
    | (k, node)::rest ->
      let { table } = node.space in
      Hashtbl.replace table node.id {
        node with data = set_trunk k trunk [] node.data
      };
      let k = k + 1 in
      rename_subtree trunk (Hashtbl.fold (fun _ id list ->
        (k, Hashtbl.find table id)::list
      ) node.children rest)

  let rename parent node name =
    let node = { node with name; } in
    (* TODO: explore trade-off between computing paths dynamically
       which will slightly slow every operation and this
       implementation which requires operations on every descendent
       when a directory is moved *)
    rename_subtree (child parent name) [0, node];
    Hashtbl.replace parent.children name node.id
end

module Make(N : NODE) = struct
  type t = N.t space

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
          parent = Some 1L;
          gen = 0L;
          id = 1L;
          name = "";
          data = space.root;
          children = Hashtbl.create 32;
          lookups = 0;
        } in
        Hashtbl.replace table id node;
        node
      else raise Not_found

  let string_of_id s id =
    if id = Int64.zero (* TODO: should be in get for FUSE_INIT? *)
    then "id=0"
    else let {gen; id; data} = get s id in
         Printf.sprintf "%Ld.%Ld.%s" gen id (N.to_string data)

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
      let data = N.child parent name in
      let (gen,id) = alloc_id space in
      let node = {
        space;
        parent=Some parent.id;
        gen; id; name; data;
        children=Hashtbl.create 8;
        lookups=1;
      } in
      Hashtbl.replace parent.children name id;
      Hashtbl.replace table id node;
      node

  let rename srcpn src destpn dest =
    let { space } = srcpn in
    let { table } = space in
    let id =
      try Hashtbl.find srcpn.children src
      with Not_found ->
        failwith
          (Printf.sprintf "rename: source child %s / %s missing"
             (string_of_id space srcpn.id) src)
    in
    let srcn =
      try Hashtbl.find table id
      with Not_found ->
        failwith
          (Printf.sprintf "rename: source node %s / %s (%Ld) missing"
             (string_of_id space srcpn.id) src id)
    in
    Hashtbl.remove srcpn.children src;
    N.rename destpn srcn dest

  let unlink node =
    let { space } = node in
    let { table } = space in
    match node.parent with
    | None -> ()
    | Some parent ->
      let parent = Hashtbl.find table parent in
      Hashtbl.remove parent.children node.name;
      Hashtbl.replace table node.id { node with parent = None }

  let forget node n =
    let { space } = node in
    let { table } = space in
    let lookups = node.lookups - n in
    if lookups > 0
    then Hashtbl.replace table node.id { node with lookups }
    else begin
      Hashtbl.remove table node.id;
      space.free <- (Int64.add node.gen 1L, node.id)::space.free;
      match node.parent with
      | None -> ()
      | Some parent ->
        let parent = Hashtbl.find table parent in
        Hashtbl.remove parent.children node.name
    end
end
