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

module Stat = Unix_sys_stat.File_kind
module In = In.Linux_7_8
module Out = Out.Linux_7_8

let to_mode_t = PosixTypes.(Ctypes.(Unsigned.(coerce uint32_t mode_t)))
let to_dev_t  = PosixTypes.(Ctypes.(Unsigned.(coerce uint64_t dev_t)))

let file_kind_to_code k = match Stat.(to_code ~host k) with
  | Some x -> Int32.of_int x
  | None ->
    raise (Failure ("This host doesn't know about sys/stat.h:"
                    ^(Stat.to_string k)))

type state = { root : string }

module Linux_7_8 : Profuse.RW_FULL with type t = state = struct
  type t = state

  type fh =
  | Dir of string * Unix.dir_handle * int
  | File of string * Unix.file_descr

  let fh_table = Hashtbl.create 256
  let fh_free = ref []
  let fh_max = ref 0L
  let alloc_fh () =
    match !fh_free with
    | h::r -> fh_free := r; h
    | [] -> let fh = !fh_max in fh_max := Int64.add fh 1L; fh
  let free_fh fh =
    begin match Hashtbl.find fh_table fh with
    | Dir (_,dir,_) -> Unix.closedir dir
    | File (_,fd) -> Unix.close fd
    end;
    Hashtbl.remove fh_table fh;
    fh_free := fh::!fh_free

  type nodeid = int64
  type node = {
    parent   : nodeid;
    gen      : int64;
    id       : nodeid;
    name     : string;
    path     : string;
    children : (string,nodeid) Hashtbl.t;
    lookups  : int;
  }

  let node_table = Hashtbl.create 256
  let node_free = ref []
  let node_max = ref 0L

  let nodeid req = In.(Ctypes.getf req.Fuse.hdr Hdr.nodeid)

  let get_node st nodeid =
    let id = Unsigned.UInt64.to_int64 nodeid in
    if id=1L then {
      parent = 1L;
      gen = 0L;
      id = 1L;
      name = "";
      path = st.root;
      children = Hashtbl.create 32;
      lookups = 0;
    } else Hashtbl.find node_table id

  let string_of_nodeid nodeid st =
    let id = Unsigned.UInt64.to_int64 nodeid in
    if id = Int64.zero (* TODO: should be in get_node for FUSE_INIT? *)
    then "id=0"
    else (get_node st nodeid).path

  let alloc_nodeid () =
    match !node_free with
    | genid::r -> node_free := r; genid
    | [] -> let nodeid = !node_max in
            node_max := Int64.add nodeid 1L;
            (0L, nodeid)

  let lookup_node parent name =
    try
      let nodeid = Hashtbl.find parent.children name in
      try
        let node = Hashtbl.find node_table nodeid in
      Hashtbl.replace node_table nodeid { node with lookups = node.lookups + 1 };
        node
      with Not_found -> (* TODO: log consistency error *) raise Not_found
    with Not_found ->
      let path = Filename.concat parent.path name in
      let (gen,id) = alloc_nodeid () in
      let node = {
        parent=parent.id;
        gen; id; name; path;
        children=Hashtbl.create 8;
        lookups=1;
      } in
      Hashtbl.replace parent.children name id;
      Hashtbl.replace node_table id node;
      node

  let forget_node node =
    let parent = Hashtbl.find node_table node.parent in
    Hashtbl.remove parent.children node.name;
    Hashtbl.remove node_table node.id;
    node_free := (Int64.add node.gen 1L, node.id)::!node_free

  let store_attr_of_stats s = Unix.LargeFile.(Int64.(
    let size = s.st_size in
    Struct_linux_7_8.Attr.store
      ~ino:(of_int s.st_ino)
      ~size
      ~blocks:Int64.(add 1L (shift_right_logical size 9)) (* TODO: ? *)
      ~atime:(of_float s.st_atime) ~atimensec:0l
      ~mtime:(of_float s.st_mtime) ~mtimensec:0l
      ~ctime:(of_float s.st_ctime) ~ctimensec:0l
      ~mode:Int32.(logor
                     (file_kind_to_code s.st_kind)
                     (of_int s.st_perm))
      ~nlink:Int32.(of_int s.st_nlink)
      ~uid:Int32.(of_int s.st_uid)
      ~gid:Int32.(of_int s.st_gid)
      ~rdev:Int32.(of_int s.st_rdev) (* TODO: dev? *)
  ))

  let getattr req st = Out.(
    try
      let { path } = get_node st (nodeid req) in
      let stats = Unix.LargeFile.lstat path in
      write_reply req
        (Attr.create ~attr_valid:0L ~attr_valid_nsec:0l
           ~store_attr:(store_attr_of_stats stats));
      st
    with Not_found ->
      (* TODO: log? *)
      write_error req Unix.ENOENT;
      st
    | Unix.Unix_error (Unix.ENOENT,_,_) ->
      (* TODO: clean up tables *)
      write_error req Unix.ENOENT;
      st
  )

  let opendir op req st = Out.(
    try
      let { path } = get_node st (nodeid req) in
      let dir = Unix.opendir path in
      let fh = alloc_fh () in
      Hashtbl.replace fh_table fh (Dir (path, dir, 0));
      write_reply req (Open.create ~fh ~open_flags:0l); (* TODO: open_flags?? *)
      st
    with Not_found ->
      (* TODO: log? *)
      Printf.eprintf "opendir not found\n%!";
      write_error req Unix.ENOENT;
      st
    | Unix.Unix_error (Unix.ENOENT,_,_) ->
      (* TODO: clean up tables *)
      write_error req Unix.ENOENT;
      st
  )

  let forget n req st = Out.(
    try
      let node = get_node st (nodeid req) in
      let lookups = node.lookups - n in
      (if lookups <= 0
       then forget_node node
       else Hashtbl.replace node_table node.id { node with lookups });
      st
    with Not_found -> (* TODO: log? *)
      write_error req Unix.ENOENT; st
  )

  let respond_with_entry node req = Out.(
    try
      let nodeid = node.id in
      let generation = node.gen in
      let stats = Unix.LargeFile.lstat node.path in
      write_reply req
        (Out.Entry.create ~nodeid ~generation
           ~entry_valid:0L ~entry_valid_nsec:0l ~attr_valid:0L ~attr_valid_nsec:0l
           ~store_attr:(store_attr_of_stats stats))
    with Unix.Unix_error (Unix.ENOENT,_,_) ->
      (* TODO: clean up tables *)
      write_error req Unix.ENOENT
  )

  let lookup name req st =
    let pnode = get_node st (nodeid req) in
    try
      let node = lookup_node pnode name in
      respond_with_entry node req;
      st
    with Not_found ->
      (* TODO: log? *)
      Printf.eprintf "lookup not found\n%!";
      Out.write_error req Unix.ENOENT;
      st

  let readdir r req st = Out.(
    let req_off = Int64.to_int (Ctypes.getf r In.Read.offset) in
    let rec seek dir off =
      if off < req_off then (ignore (Unix.readdir dir); seek dir (off + 1))
      else if off > req_off then (Unix.rewinddir dir; seek dir 0)
      else off
    in
    let fh = Ctypes.getf r In.Read.fh in
    let handle = try Hashtbl.find fh_table fh
      with Not_found ->
        (* TODO: log invalid fh error *)
        raise Not_found
    in
    match handle with
    | Dir (path, dir, off) ->
      let off = seek dir off in
      assert (off = req_off);
      write_reply req
        (Out.Dirent.of_list begin
          try
            let name = Unix.readdir dir in
            let stats = Unix.LargeFile.lstat (Filename.concat path name) in
            let off = off + 1 in
            Hashtbl.replace fh_table fh (Dir (path, dir, off));
            Unix.LargeFile.([off, Int64.of_int stats.st_ino, name,
                             Unix_dirent.File_kind.of_file_kind stats.st_kind])
          with End_of_file -> []
        end 0);
      st
    | File (_,_) -> write_error req Unix.EBADF; st (* TODO: correct? *)
  )

  let readlink req st =
    let node = get_node st (nodeid req) in
    let target = Unix.readlink node.path in (* TODO: error? *)
    Out.(write_reply req (Readlink.create ~target));
    st

  let open_ op req st = Out.(
    try
      let { path } = get_node st (nodeid req) in
      let mode = Ctypes.getf op In.Open.mode in (* TODO: is only file_perm? *)
      let flags = Ctypes.getf op In.Open.flags in
      let flags = Unix_fcntl.Oflags.(
        List.rev_map to_open_flag_exn (of_code ~host flags)
      ) in
      let file = Unix.openfile path flags (Int32.to_int mode) in
      let fh = alloc_fh () in
      Hashtbl.replace fh_table fh (File (path, file));
      Out.(write_reply req (Open.create ~fh ~open_flags:0l)); (* TODO: flags *)
      st
    with Not_found ->
      (* TODO: log? *)
      write_error req Unix.ENOENT; st
    | Unix.Unix_error (Unix.ENOENT,_,_) ->
      (* TODO: clean up tables *)
      write_error req Unix.ENOENT; st
  )

  let read r req st =
    let fh = Ctypes.getf r In.Read.fh in
    let offset = Ctypes.getf r In.Read.offset in
    let size = Ctypes.getf r In.Read.size in
    try match Hashtbl.find fh_table fh with
    | File (path, fd) -> Out.(
      write_reply req
        (Read.create ~size ~data_fn:(fun buf ->
          let ptr = Ctypes.(to_voidp (bigarray_start array1 buf)) in
          let off = Unix.LargeFile.lseek fd offset Unix.SEEK_SET in
          assert (off=offset); (* TODO: necessary? *)
          Unix_unistd.read fd ptr size
         )));
      st
    | Dir (_,_,_) -> Out.write_error req Unix.EISDIR; st
    with Not_found ->
      (* TODO: log invalid fh error *)
      raise Not_found

  (* TODO: anything? *)
  let flush f req st = Out.(write_reply req (Hdr.packet ~count:0)); st

  (* TODO: flags? *)
  (* TODO: distinguish release/releasedir? *)
  let release r req st =
    try
      free_fh (Ctypes.getf r In.Release.fh);
      Out.(write_reply req (Hdr.packet ~count:0));
      st
    with Not_found ->
      Out.write_error req Unix.EBADF; st

  (* TODO: errors? *)
  let symlink name target req st = Out.(
    let ({ path } as pnode) = get_node st (nodeid req) in
    let path = Filename.concat path name in
    Unix.symlink target path;
    lookup name req st (* TODO: still increment lookups? *)
  )

  (* TODO: errors? *)
  let rename r src dest req st = Out.(
    let { path } = get_node st (nodeid req) in
    let newdir = get_node st (Ctypes.getf r In.Rename.newdir) in
    Unix.rename (Filename.concat path src) (Filename.concat newdir.path dest);
    try
      let node = lookup_node newdir dest in (* TODO: still increment lookups? *)
      respond_with_entry node req;
      st
    with Not_found ->
      (* TODO: log? *)
      Printf.eprintf "lookup(rename) not found\n%!";
      write_error req Unix.ENOENT;
      st
  )

  (* TODO: errors? *)
  let unlink name req st = Out.(
    let { path } = get_node st (nodeid req) in
    let path = Filename.concat path name in
    Unix.unlink path;
    write_reply req (Hdr.packet ~count:0);
    st
  )

  (* TODO: errors? *)
  let rmdir name req st = Out.(
    let { path } = get_node st (nodeid req) in
    let path = Filename.concat path name in
    Unix.rmdir path;
    write_reply req (Hdr.packet ~count:0);
    st
  )

  (* TODO: do *)
  let statfs req st = Out.write_error req Unix.ENOSYS; st

  (* TODO: do *)
  let fsync f req st = Out.write_error req Unix.ENOSYS; st

  (* TODO: errors *)
  (* TODO: write flags? *)
  let write w req st =
    let fh = Ctypes.getf w In.Write.fh in
    let offset = Ctypes.getf w In.Write.offset in
    let size = Ctypes.getf w In.Write.size in
    try match Hashtbl.find fh_table fh with
    | File (path, fd) -> Out.(
      let data = Ctypes.(to_voidp (CArray.start (getf w In.Write.data))) in
      let off = Unix.LargeFile.lseek fd offset Unix.SEEK_SET in
      assert (off=offset); (* TODO: necessary? *)
      let size = Unix_unistd.write fd data size in
      write_reply req (Write.create ~size);
      st
    )
    | Dir (_,_,_) -> Out.write_error req Unix.EISDIR; st
    with Not_found ->
      (* TODO: log invalid fh error *)
      raise Not_found

  (* TODO: errors *)
  let link l name req st =
    let { path } = get_node st (nodeid req) in
    let oldnode = get_node st (Ctypes.getf l In.Link.oldnodeid) in
    let path = Filename.concat path name in
    Unix.link oldnode.path path;
    lookup name req st (* TODO: still increment lookups? *)

  (* TODO: do *)
  let getxattr g req st = Out.write_error req Unix.ENOSYS; st

  (* TODO: do *)
  let setxattr s req st = Out.write_error req Unix.ENOSYS; st

  (* TODO: do *)
  let listxattr g req st = Out.write_error req Unix.ENOSYS; st

  (* TODO: do *)
  let removexattr name req st = Out.write_error req Unix.ENOSYS; st

  (* TODO: forward pid; fuse server != requestor *)
  let access a req st =
    let { path } = get_node st (nodeid req) in
    let code = Ctypes.getf a In.Access.mask in
    let perms = Unix_unistd.Access.(of_code ~host code) in
    try
      Unix.access path perms;
      Out.(write_reply req (Hdr.packet ~count:0));
      st
    with Unix.Unix_error(err,_,_) ->
      Out.write_error req err; (* TODO: correct? *)
      st

  (* TODO: do mknod + open *)
  let create c name req st = Out.write_error req Unix.ENOSYS; st
    (*let { path } = get_node (nodeid req) in
      let flags = Ctypes.getf c In.Create.flags in
      let mode = Ctypes.getf c In.Create.mode in
    *)

  let mknod m name req st =
    let ({ path } as pnode) = get_node st (nodeid req) in
    let mode = Ctypes.getf m In.Mknod.mode in
    let rdev = Ctypes.getf m In.Mknod.rdev in (* TODO: use this? *)
    let path = Filename.concat path name in
    try
      (* TODO: translate mode and dev from client host rep to local host rep *)
      (* TODO: dev_t is usually 64-bit but rdev is 32-bit. translate how? *)
      Unix_sys_stat.mknod path
        (to_mode_t mode)
        (to_dev_t (Unsigned.UInt64.of_int64
                     (Int64.of_int32
                        (Unsigned.UInt32.to_int32 rdev))));
      let node = lookup_node pnode name in
      respond_with_entry node req;
      st
    with Not_found ->
      (* TODO: log? *)
      Printf.eprintf "lookup(mknod) not found\n%!";
      Out.write_error req Unix.ENOENT;
      st

  let mkdir m name req st =
    let ({ path } as pnode) = get_node st (nodeid req) in
    let mode = Ctypes.getf m In.Mkdir.mode in
    let path = Filename.concat path name in
    try
      Unix.mkdir path (Unsigned.UInt32.to_int mode);
      let node = lookup_node pnode name in
      respond_with_entry node req;
      st
    with Not_found ->
      (* TODO: log? *)
      Printf.eprintf "lookup(mkdir) not found\n%!";
      Out.write_error req Unix.ENOENT;
      st

  (* TODO: do *)
  let fsyncdir f req st = Out.write_error req Unix.ENOSYS; st

  (* TODO: do *)
  let getlk lk req st = Out.write_error req Unix.ENOSYS; st

  (* TODO: do *)
  let setlk lk req st = Out.write_error req Unix.ENOSYS; st

  (* TODO: do *)
  let setlkw lk req st = Out.write_error req Unix.ENOSYS; st

  (* TODO: do *)
  let interrupt i req st = Out.write_error req Unix.ENOSYS; st

  (* TODO: do *)
  let bmap b req st = Out.write_error req Unix.ENOSYS; st

  (* TODO: more? *)
  let destroy req st = Out.(write_reply req (Hdr.packet ~count:0)); st

  let setattr s req st =
    let { path } = get_node st (nodeid req) in
    let valid = Ctypes.getf s In.Setattr.valid in
    (if In.Setattr.Valid.(is_set valid mode)
     then ());
    (if In.Setattr.Valid.(is_set valid uid)
     then ());
    (if In.Setattr.Valid.(is_set valid gid)
     then ());
    (if In.Setattr.Valid.(is_set valid size)
     then ());
    (if In.Setattr.Valid.(is_set valid atime)
     then ());
    (if In.Setattr.Valid.(is_set valid mtime)
     then ());
    (if In.Setattr.Valid.(is_set valid handle)
     then ());
    getattr req st
end
