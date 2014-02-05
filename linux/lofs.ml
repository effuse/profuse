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
module In = In.Linux_7_8
module Out = Out.Linux_7_8

let to_mode_t = PosixTypes.(Ctypes.(Unsigned.(coerce uint32_t mode_t)))
let to_dev_t  = PosixTypes.(Ctypes.(Unsigned.(coerce uint64_t dev_t)))

type state = { root : string }

module Linux_7_8 : Profuse.RW_FULL with type t = state = struct
  type t = state

  type fh =
  | Dir of string * Unix.dir_handle * int
  | File of string * Unix.file_descr * Stat.File_kind.t Lazy.t

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
    | File (_,fd,_) -> Unix.close fd
    end;
    Hashtbl.remove fh_table fh;
    fh_free := fh::!fh_free

  let get_fd fh =
    try Hashtbl.find fh_table fh
    with Not_found -> raise Unix.(Unix_error (EBADF,"",""))

  let get_dir_fd fh f = match get_fd fh with
    | Dir (path, dir, off) -> f path dir off
    | File (_,_,_) -> raise Unix.(Unix_error (ENOTDIR,"",""))
  let get_file_fd fh f = match get_fd fh with
    | File (path, fd, kind) -> f path fd kind
    | Dir (_,_,_) -> raise Unix.(Unix_error (EISDIR,"",""))

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

  let uint64_of_int64 = Unsigned.UInt64.of_int64
  let uint32_of_uint64 x = Unsigned.(UInt32.of_int (UInt64.to_int x))

  let store_attr_of_path path = Stat.(Stat.(
    let s = lstat path in
    Struct_linux_7_8.Attr.store
      ~ino:(ino_int s)
      ~size:(uint64_of_int64 (size_int s))
      ~blocks:(uint64_of_int64 (blocks_int s))
      ~atime:(uint64_of_int64 (atime_int s))
      ~atimensec:(atimensec_int s)
      ~mtime:(uint64_of_int64 (mtime_int s))
      ~mtimensec:(mtimensec_int s)
      ~ctime:(uint64_of_int64 (ctime_int s))
      ~ctimensec:(ctimensec_int s)
      ~mode:(mode_int s)
      ~nlink:(uint32_of_uint64 (nlink_int s))
      ~uid:(uid_int s)
      ~gid:(gid_int s)
      ~rdev:(uint32_of_uint64 (rdev_int s))
  ))

  let getattr req st = Out.(
    try
      let { path } = get_node st (nodeid req) in
      write_reply req
        (Attr.create ~attr_valid:0L ~attr_valid_nsec:0l
           ~store_attr:(store_attr_of_path path));
      st
    with Not_found ->
      (* TODO: log? *)
      write_error req Unix.ENOENT;
      st
  )

  let opendir op req st = Out.(
    let fh = alloc_fh () in
    try
      let { path } = get_node st (nodeid req) in
      let dir = Unix.opendir path in
      Hashtbl.replace fh_table fh (Dir (path, dir, 0));
      write_reply req (Open.create ~fh ~open_flags:0l); (* TODO: open_flags?? *)
      st
    with Not_found ->
      free_fh fh;
      (* TODO: log? *)
      Printf.eprintf "opendir not found\n%!";
      write_error req Unix.ENOENT;
      st
    | Unix.Unix_error (e,c,s) ->
      free_fh fh;
      raise (Unix.Unix_error (e,c,s))
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

  let store_entry node = Out.(
    let nodeid = node.id in
    let generation = node.gen in
    Entry.store ~nodeid ~generation
      ~entry_valid:0L ~entry_valid_nsec:0l
      ~attr_valid:0L ~attr_valid_nsec:0l
      ~store_attr:(store_attr_of_path node.path)
  )

  let respond_with_entry node req = Out.(
    let nodeid = node.id in
    let generation = node.gen in
    write_reply req
      (Entry.create ~nodeid ~generation
         ~entry_valid:0L ~entry_valid_nsec:0l
         ~attr_valid:0L ~attr_valid_nsec:0l
         ~store_attr:(store_attr_of_path node.path))
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
    get_dir_fd fh (fun path dir off ->
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
        end 0)
    );
    st
  )

  (* Can raise Unix.Unix_error *)
  let readlink req st =
    let node = get_node st (nodeid req) in
    (* errors caught by our caller *)
    let target = Unix.readlink node.path in
    Out.(write_reply req (Readlink.create ~target));
    st

  let open_ op req st = Out.(
    let fh = alloc_fh () in
    try
      let { path } = get_node st (nodeid req) in
      let mode = Ctypes.getf op In.Open.mode in (* TODO: is only file_perm? *)
      let flags = Ctypes.getf op In.Open.flags in
      let flags = Unix_fcntl.Oflags.(
        List.rev_map to_open_flag_exn (of_code ~host flags)
      ) in
      Printf.eprintf "before lofs open %s\n%!" path;
      let file = Unix.openfile path flags (Int32.to_int mode) in
      Printf.eprintf "after lofs open\n%!";
      let kind () = let { Unix.st_kind } = Unix.fstat file in st_kind in
      Hashtbl.replace fh_table fh (File (path, file, Lazy.from_fun kind));
      Out.(write_reply req (Open.create ~fh ~open_flags:0l)); (* TODO: flags *)
      st
    with Not_found ->
      (* TODO: log? *)
      free_fh fh;
      write_error req Unix.ENOENT; st
    | Unix.Unix_error (e,c,s) ->
      free_fh fh;
      raise (Unix.Unix_error (e,c,s))
  )

  let read r req st =
    let fh = Ctypes.getf r In.Read.fh in
    let offset = Ctypes.getf r In.Read.offset in
    let size = Ctypes.getf r In.Read.size in
    get_file_fd fh (fun path fd _zk -> Out.(
      write_reply req
        (Read.create ~size ~data_fn:(fun buf ->
          let ptr = Ctypes.(to_voidp (bigarray_start array1 buf)) in
          let off = Unix.LargeFile.lseek fd offset Unix.SEEK_SET in
          assert (off=offset); (* TODO: necessary? *)
          Unix_unistd.read fd ptr size
         )))
    );
    st

  (* TODO: anything? *)
  let flush f req st = Out.(write_reply req (Hdr.packet ~count:0)); st

  (* TODO: flags? *)
  let release r req st =
    try
      free_fh (Ctypes.getf r In.Release.fh);
      Out.(write_reply req (Hdr.packet ~count:0));
      st
    with Not_found ->
      Out.write_error req Unix.EBADF; st

  (* TODO: distinguish? *)
  let releasedir = release

  (* Can raise Unix.Unix_error *)
  let symlink name target req st = Out.(
    let ({ path } as pnode) = get_node st (nodeid req) in
    let path = Filename.concat path name in
    (* errors caught by our caller *)
    Unix.symlink target path;
    lookup name req st (* TODO: still increment lookups? *)
  )

  (* Can raise Unix.Unix_error *)
  let rename r src dest req st = Out.(
    let { path } = get_node st (nodeid req) in
    let newdir = get_node st (Ctypes.getf r In.Rename.newdir) in
    (* errors caught by our caller *)
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

  (* Can raise Unix.Unix_error *)
  let unlink name req st = Out.(
    let { path } = get_node st (nodeid req) in
    let path = Filename.concat path name in
    (* errors caught by our caller *)
    Unix.unlink path;
    write_reply req (Hdr.packet ~count:0);
    st
  )

  (* Can raise Unix.Unix_error *)
  let rmdir name req st = Out.(
    let { path } = get_node st (nodeid req) in
    let path = Filename.concat path name in
    (* errors caught by our caller *)
    Unix.rmdir path;
    write_reply req (Hdr.packet ~count:0);
    st
  )

  (* TODO: do *)
  let statfs req st = Out.write_error req Unix.ENOSYS; st

  (* TODO: do *)
  let fsync f req st = Out.write_error req Unix.ENOSYS; st

  (* Can raise Unix.Unix_error *)
  (* TODO: write flags? *)
  let write w req st =
    let fh = Ctypes.getf w In.Write.fh in
    let offset = Ctypes.getf w In.Write.offset in
    let size = Ctypes.getf w In.Write.size in
    get_file_fd fh (fun path fd _zk -> Out.(
      let data = Ctypes.(to_voidp (CArray.start (getf w In.Write.data))) in
      (* errors caught by our caller *)
      let off = Unix.LargeFile.lseek fd offset Unix.SEEK_SET in
      assert (off=offset); (* TODO: necessary? *)
      let size = Unix_unistd.write fd data size in
      write_reply req (Write.create ~size)
    ));
    st

  (* Can raise Unix.Unix_error *)
  let link l name req st =
    let { path } = get_node st (nodeid req) in
    let oldnode = get_node st (Ctypes.getf l In.Link.oldnodeid) in
    let path = Filename.concat path name in
    (* errors caught by our caller *)
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
      Out.write_error req err;
      st

  let create c name req st = Out.(
    let fh = alloc_fh () in
    try
      let ({ path } as pnode) = get_node st (nodeid req) in
      let mode = Ctypes.getf c In.Create.mode in (* TODO: is only file_perm? *)
      let flags = Ctypes.getf c In.Create.flags in
      let flags = Unix_fcntl.Oflags.(
        List.rev_map to_open_flag_exn (of_code ~host flags)
      ) in
      let path = Filename.concat path name in
      let file = Unix.(
        openfile path (O_WRONLY::O_CREAT::O_TRUNC::flags) (Int32.to_int mode)
      ) in
      let kind () = let { Unix.st_kind } = Unix.fstat file in st_kind in
      Hashtbl.replace fh_table fh (File (path, file, Lazy.from_fun kind));
      write_reply req
        (Create.create
           ~store_entry:(store_entry (lookup_node pnode name))
           ~store_open:(Open.store ~fh ~open_flags:0l));(* TODO: flags *)
      st
    with Not_found ->
      (* TODO: log? *)
      free_fh fh;
      write_error req Unix.ENOENT; st
    | Unix.Unix_error (e,c,s) ->
      free_fh fh;
      raise (Unix.Unix_error (e,c,s))
  )

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

  let destroy _req _st = ()

  let setattr s req st = In.Setattr.(
    let valid = Ctypes.getf s valid in
    begin
      if Valid.(is_set valid handle)
      then get_file_fd (Ctypes.getf s fh) (fun _path fd zk ->
        (if Valid.(is_set valid mode)
         then let mode = Ctypes.getf s mode in
              let (kind,perm) = Stat.Mode.(
                of_code_exn ~host (Int32.to_int mode)
              ) in
              assert (kind = Lazy.force zk); (* TODO: ???!!! *)
              Unix.fchmod fd perm);
        (let set_uid = Valid.(is_set valid uid) in
         let set_gid = Valid.(is_set valid gid) in
         if set_uid || set_gid
         then Unix.fchown fd
           (if set_uid then Ctypes.getf s uid else -1)
           (if set_gid then Ctypes.getf s gid else -1)
        );
        (if Valid.(is_set valid size)
         then Unix.LargeFile.ftruncate fd (Ctypes.getf s size));
        (if Valid.(is_set valid atime) (* TODO: do *)
         then prerr_endline "setting atime");
        (if Valid.(is_set valid mtime) (* TODO: do *)
         then prerr_endline "setting mtime");
      )
      else
        let { path } = get_node st (nodeid req) in
        (if Valid.(is_set valid mode)
         then let mode = Ctypes.getf s mode in
              let (kind,perm) = Stat.Mode.(
                of_code_exn ~host (Int32.to_int mode)
              ) in
              let { Unix.st_kind } = Unix.stat path in
              assert (kind = st_kind); (* TODO: ???!!! *)
              Unix.chmod path perm);
        (let set_uid = Valid.(is_set valid uid) in
         let set_gid = Valid.(is_set valid gid) in
         if set_uid || set_gid
         then Unix.chown path
           (if set_uid then Ctypes.getf s uid else -1)
           (if set_gid then Ctypes.getf s gid else -1)
        );
        (if Valid.(is_set valid size)
         then Unix.LargeFile.truncate path (Ctypes.getf s size)
        );
        (if Valid.(is_set valid atime) (* TODO: do *)
         then prerr_endline "setting atime");
        (if Valid.(is_set valid mtime) (* TODO: do *)
         then prerr_endline "setting mtime");
    end;
    getattr req st
  )
end
