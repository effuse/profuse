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
type fh = int64
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
  handles      : (fh,id) Hashtbl.t;
  mutable freeh: fh list;
  mutable maxh : fh;
}

module type NODE = sig
  type t
  type h
  type v

  val of_value  : v -> t
  val new_child : t node -> string -> t

  val to_string : t -> string

  val get_handle : t node -> fh -> h
  val get_handles : t node -> (fh * h) list
  val set_handle : t node -> fh -> h -> t
  val free_handle : t node -> fh -> t

  val rename : t node -> t node -> string -> unit
end

(* TODO: this should either stop using Unix or move elsewhere *)

module UnixHandle = struct
  type t =
    | Dir of Unix.dir_handle * int
    | File of Unix.file_descr * Sys_stat.File_kind.t

  let close = function
    | Dir  (dir,_) -> Unix.closedir dir
    | File (fd,_)  -> Unix.close fd

end

module Path : NODE with type v = string list and type h = UnixHandle.t = struct
  type h = UnixHandle.t
  type v = string list
  type t = {
    path    : string list;
    handles : (fh * h) list;
  }

  let of_value path = { path; handles = [] }

  let new_child parent name = of_value (name::parent.data.path)

  let to_string { path } = match path with
    | [] | [""] -> Filename.dir_sep
    | path -> String.concat Filename.dir_sep (List.rev path)

  let free_handle node fh =
    let { data } = node in
    let { handles } = data in
    begin try UnixHandle.close (List.assoc fh handles)
      with Not_found -> ()
    end;
    { data with handles = List.remove_assoc fh handles }

  let set_handle node fh h =
    let { data } = node in
    let { handles } = data in
    let handles = List.remove_assoc fh handles in
    { data with handles = (fh,h)::handles }

  let get_handle node fh = List.assoc fh node.data.handles

  let get_handles node = node.data.handles

  let rec set_trunk k trunk branch = function
    | _ when k = 0 -> List.rev_append branch trunk
    | h::t -> set_trunk (k - 1) trunk (h::branch) t
    | [] -> assert false

  let rec rename_subtree trunk = function
    | [] -> ()
    | (k, node)::rest ->
      let { table } = node.space in
      Hashtbl.replace table node.id {
        node with data = {
        node.data with path = set_trunk k trunk [] node.data.path;
      }
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
    rename_subtree (name::parent.data.path) [0, node];
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
      handles = Hashtbl.create 256;
      freeh = [];
      maxh   = 0L;
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

  module Handle = struct
    let set space fh h =
      let { table; handles } = space in
      try
        let id = Hashtbl.find handles fh in
        try
          let node = Hashtbl.find table id in
          let node = { node with data = N.set_handle node fh h } in
          Hashtbl.replace table id node
        with Not_found ->
          failwith (Printf.sprintf "set: node %Ld for fh %Ld missing from %s"
                   id fh (to_string space))
      with Not_found ->
        failwith (Printf.sprintf "set: fh %Ld missing from %s"
                    fh (to_string space))

    let open_ node kind =
      let { space } = node in
      let fh = match space.freeh with
        | fh::r -> space.freeh <- r; fh
        | [] -> let fh = space.maxh in space.maxh <- Int64.add fh 1L; fh
      in
      let node = { node with data = N.set_handle node fh kind } in
      Hashtbl.replace space.table node.id node;
      Hashtbl.replace space.handles fh node.id;
      fh

    let free space fh =
      let { table; handles; freeh } = space in
      try
        let id = Hashtbl.find handles fh in
        Hashtbl.remove handles fh;
        space.freeh <- fh::freeh;
        try
          let node = Hashtbl.find table id in
          let node = { node with data = N.free_handle node fh } in
          Hashtbl.replace table node.id node
        with Not_found ->
          failwith (Printf.sprintf "free: node %Ld for fh %Ld missing from %s"
                      id fh (to_string space))
      with Not_found ->
        failwith (Printf.sprintf "free: fh %Ld missing from %s"
                    fh (to_string space))

    let get space fh =
      let { table; handles } = space in
      try
        let id = Hashtbl.find handles fh in
        let node = get space id in
        N.get_handle node fh
      with Not_found -> raise (Unix.(Unix_error (EBADF,"","")))
  end

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
      let data = N.new_child parent name in
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

  let handles = N.get_handles

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

  let unlink parent name =
    let { space } = parent in
    let { table } = space in
    try
      let id = Hashtbl.find parent.children name in
      Hashtbl.remove parent.children name;
      try
        let node = Hashtbl.find table id in
        Hashtbl.replace table node.id { node with parent = None };
      with Not_found -> ()
    with Not_found -> ()

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
