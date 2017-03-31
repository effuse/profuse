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
  pins     : int;
  deps     : int;
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
  val value : t -> v
  val with_value : t -> v -> t
  val new_child : t node -> string -> t

  val to_string : t -> string

  val get_handle : t node -> fh -> h
  val get_handles : t node -> (fh * h) list
  val set_handle : t node -> fh -> h -> t
  val free_handle : t node -> fh -> t

  val rename : t node -> t node -> string -> unit
end

module type HANDLE = sig
  type t

  val close : t -> unit
end

module type METADATA = sig
  type t

  val to_path : t -> string list
  val create_child : t -> string -> t
  val rename : t -> t -> string -> t
end

module Path(Metadata : METADATA)(Handle : HANDLE)
  : NODE with type v = Metadata.t and type h = Handle.t = struct
  type h = Handle.t
  type v = Metadata.t
  type t = {
    meta    : Metadata.t;
    path    : string list;
    handles : (fh * h) list;
  }

  let of_value meta = { meta; path = Metadata.to_path meta; handles = []; }

  let value { meta } = meta
  let with_value t meta = { t with meta }

  let new_child parent name =
    of_value (Metadata.create_child parent.data.meta name)

  let to_string { path } = match path with
    | [] | [""] -> Filename.dir_sep
    | path -> String.concat Filename.dir_sep (List.rev path)

  let free_handle node fh =
    let { data } = node in
    let { handles } = data in
    begin try Handle.close (List.assoc fh handles)
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

  let rec rename_subtree = function
    | [] -> ()
    | (node, parent)::rest ->
      let { table } = node.space in
      let data = node.data in
      let meta = Metadata.rename data.meta parent node.name in
      let data = { data with path = Metadata.to_path meta; meta } in
      Hashtbl.replace table node.id { node with data };
      rename_subtree (Hashtbl.fold (fun _ id list ->
        (Hashtbl.find table id, meta)::list
      ) node.children rest)

  let rename parent node name =
    let node = { node with name; parent = Some parent.id } in
    (* TODO: explore trade-off between computing paths dynamically
       which will slightly slow every operation and this
       implementation which requires operations on every descendent
       when a directory is moved *)
    rename_subtree [node, parent.data.meta];
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
          pins = 0;
          deps = 0;
        } in
        Hashtbl.replace table id node;
        node
      else raise Not_found

  let refresh { space; id } = get space id

  let root space = get space 1_L

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

    let open_ space id kind =
      let { table; handles } = space in
      let fh = match space.freeh with
        | fh::r -> space.freeh <- r; fh
        | [] -> let fh = space.maxh in space.maxh <- Int64.add fh 1L; fh
      in
      let node = Hashtbl.find table id in
      let node = { node with data = N.set_handle node fh kind } in
      Hashtbl.replace table id node;
      Hashtbl.replace handles fh id;
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
      let { handles } = space in
      try
        let id = Hashtbl.find handles fh in
        let node = get space id in
        N.get_handle node fh
      with Not_found -> raise (Unix.(Unix_error (EBADF,"","")))
  end

  let new_child parent name =
    let { space } = parent in
    let data = N.new_child parent name in
    let (gen,id) = alloc_id space in
    let node = {
      space;
      parent=Some parent.id;
      gen; id; name; data;
      children=Hashtbl.create 8;
      lookups=1;
      pins=0;
      deps=0;
    } in
    Hashtbl.replace parent.children name id;
    node

  let preload parent name meta_fn =
    let { space } = parent in
    let { table } = space in
    try
      let id = Hashtbl.find parent.children name in
      try
        let node = Hashtbl.find table id in
        node
      with Not_found ->
        raise (Failure
                 (Printf.sprintf "parent %s has %s but %s does not"
                    (string_of_id space parent.id)
                    name space.label
                 ))
    with Not_found ->
      let node = { (new_child parent name) with lookups = 0 } in
      let meta = N.value node.data in
      let node = { node with data = N.with_value node.data (meta_fn meta) } in
      Hashtbl.replace table node.id node;
      node

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
      let node = new_child parent name in
      Hashtbl.replace table node.id node;
      node

  let handles = N.get_handles

  let release space table id node =
    Hashtbl.remove table id;
    space.free <- (Int64.add node.gen 1L, id)::space.free;
    match node.parent with
    | None -> ()
    | Some parent ->
      let parent = Hashtbl.find table parent in
      Hashtbl.remove parent.children node.name

  let store node =
    let { space } = node in
    let { table } = space in
    Hashtbl.replace table node.id node

  let rec dep_to_root k node =
    store node;
    match node.parent with
    | None -> () (* unlinked *)
    | Some id when id = node.id -> () (* root *)
    | Some id -> dep k (get node.space id)
  and dep k node =
    if k <> 0
    then dep_to_root k { node with deps = node.deps + k }

  let pin node =
    let node = { node with pins = node.pins + 1 } in
    dep_to_root 1 node;
    node

  let rec undep_from_root k node =
    begin
      if node.pins = 0 && node.deps = 0 && node.lookups = 0
      then
        let { space; id } = node in
        release space space.table id node
      else store node
    end;
    match node.parent with
    | None -> () (* unlinked *)
    | Some id when id = node.id -> () (* root *)
    | Some id ->
      let node = get node.space id in
      undep k node
  and undep k node =
    if k <> 0
    then if node.deps >= k
      then undep_from_root k { node with deps = node.deps - k }
      else
        failwith (Printf.sprintf "undep: node %Ld was not depended on %d"
                    node.id k)

  let unpin node =
    if node.pins < 1
    then failwith (Printf.sprintf "unpin: node %Ld was not pinned" node.id)
    else begin
      let unpinned_node = { node with pins = node.pins - 1 } in
      undep_from_root 1 unpinned_node;
      if unpinned_node.pins = 0
      && unpinned_node.deps = 0
      && unpinned_node.lookups = 0
      then None
      else Some unpinned_node
    end

  let unlink parent name =
    let { space } = parent in
    let { table } = space in
    try
      let id = Hashtbl.find parent.children name in
      Hashtbl.remove parent.children name;
      try
        let node = Hashtbl.find table id in
        Hashtbl.replace table node.id { node with parent = None };
        let deps = node.deps + node.pins in
        undep deps parent
      with Not_found -> ()
    with Not_found -> ()

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
    let srcdeps = srcn.deps + srcn.pins in
    undep srcdeps srcpn;
    unlink destpn dest;
    N.rename destpn srcn dest;
    dep srcdeps destpn

  let forget space id n =
    let { table } = space in
    let node = Hashtbl.find table id in
    let lookups = node.lookups - n in
    if lookups > 0
    then Hashtbl.replace table id { node with lookups }
    else if lookups < 0
    then failwith (Printf.sprintf "forget: node %Ld has %d lookups" id lookups)
    else if node.pins <> 0 || node.deps <> 0
    then Hashtbl.replace table id {
      node with lookups;
                gen = Int64.add node.gen 1L;
    }
    else release space table id node
end
