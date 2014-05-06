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

let to_mode_t = PosixTypes.(Ctypes.(Unsigned.(coerce uint32_t mode_t)))
let to_dev_t  = PosixTypes.(Ctypes.(Unsigned.(coerce uint64_t dev_t)))

type state = {
  nodes : Nodes.t;
  handles : Handles.t;
  agents : Agent_handler.t;
}
type t = state

(* TODO: check Nodes.get raising Not_found *)
(* TODO: check uid/gid rights *)

(* can be overridden with include *)
let trace_channel = stderr

let string_of_nodeid nodeid st = Nodes.string_of_id st.nodes nodeid

let string_of_state req st =
  Printf.sprintf "Nodes: %s" (Nodes.to_string st.nodes)

let uint64_of_int64 = Unsigned.UInt64.of_int64
let uint32_of_uint64 x = Unsigned.(UInt32.of_int (UInt64.to_int x))

let make root = {
  nodes = Nodes.create root;
  handles = Handles.create ();
  agents = Agent_handler.create ();
}

module Linux_7_8(Out : Out.LINUX_7_8) : Profuse.FULL with type t = state =
struct
  type t = state

  module Support = Profuse.Linux_7_8(Out)

  let mount ~argv ~mnt st =
    ignore (Unix.umask 0o000);
    Support.mount ~argv ~mnt st

  let enosys = Support.enosys
  let nodeid = Support.nodeid

  let store_attr_of_path path = Stat.(Stat.(
    let s = lstat path in
    Struct_linux_7_8.Attr.store
      ~ino:(Unsigned.UInt64.to_int64 (ino_int s))
      ~size:(size_int s)
      ~blocks:(blocks_int s)
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
      let { Nodes.path } = Nodes.get st.nodes (nodeid req) in
      write_reply req
        (Attr.create ~attr_valid:0L ~attr_valid_nsec:0l
           ~store_attr:(store_attr_of_path path));
      st
    with Not_found ->
      (* TODO: log? *)
      Printf.eprintf "getattr not found\n%!";
      write_error req Unix.ENOENT;
      st
  )

  let opendir op req st = Out.(
    try
      let { Nodes.path } = Nodes.get st.nodes (nodeid req) in
      let dir = Unix.opendir path in
      let h = Handles.(alloc st.handles path (Dir (dir, 0))) in
      write_reply req (Open.create ~fh:h.Handles.id ~open_flags:0l);
      (* TODO: open_flags?? *)
      st
    with Not_found ->
      (* TODO: log? *)
      Printf.eprintf "opendir not found\n%!";
      write_error req Unix.ENOENT;
      st
  )

  let forget n req st = Out.(
    try
      let node = Nodes.get st.nodes (nodeid req) in
      Nodes.forget node n;
      st
    with Not_found ->
      (* TODO: FORGET is a non-returning command. log? *)
      Printf.eprintf "forget not found\n%!";
      st
  )

  let store_entry = Support.store_entry store_attr_of_path
  let respond_with_entry = Support.respond_with_entry store_attr_of_path

  let lookup name req st =
    respond_with_entry (Nodes.get st.nodes (nodeid req)) name req;
    st

  let readdir r req st = Out.(
    let req_off = Int64.to_int (Ctypes.getf r In.Read.offset) in
    let rec seek dir off =
      if off < req_off then (ignore (Unix.readdir dir); seek dir (off + 1))
      else if off > req_off then (Unix.rewinddir dir; seek dir 0)
      else off
    in
    let fh = Ctypes.getf r In.Read.fh in
    Handles.with_dir_fd st.handles fh (fun h dir off ->
      let off = seek dir off in
      assert (off = req_off);
      let host = Fuse.(req.chan.host) in
      write_reply req
        (Out.Dirent.of_list ~host begin
          try
            let name = Unix.readdir dir in
            let path = Filename.concat h.Handles.path name in
            let stats = Unix.LargeFile.lstat path in
            let off = off + 1 in
            Handles.set_dir_offset h off;
            Unix.LargeFile.([off, Int64.of_int stats.st_ino, name,
                             Unix_dirent.File_kind.of_file_kind stats.st_kind])
          with End_of_file -> []
        end 0)
    );
    st
  )

  (* Can raise Unix.Unix_error *)
  let readlink req st =
    let node = Nodes.get st.nodes (nodeid req) in
    (* errors caught by our caller *)
    let target = Unix.readlink node.Nodes.path in
    Out.(write_reply req (Readlink.create ~target));
    st

  let open_ op req st = Out.(
    try
      let { Nodes.path } = Nodes.get st.nodes (nodeid req) in
      let mode = Ctypes.getf op In.Open.mode in (* TODO: is only file_perm? *)
      let flags = Ctypes.getf op In.Open.flags in
      let phost = Fuse.(req.chan.host.unix_fcntl.Unix_fcntl.oflags) in
      let flags = Unix_fcntl.Oflags.(
        List.rev_map to_open_flag_exn (of_code ~host:phost flags)
      ) in
      let file = Unix.openfile path flags (Int32.to_int mode) in
      let kind = Unix.((fstat file).st_kind) in
      let h = Handles.(alloc st.handles path (File (file, kind))) in
      Out.(write_reply req (Open.create ~fh:h.Handles.id ~open_flags:0l));
      (* TODO: flags *)
      st
    with Not_found ->
      (* TODO: log? *)
      write_error req Unix.ENOENT; st
  )

  let read r req st =
    let fh = Ctypes.getf r In.Read.fh in
    let offset = Ctypes.getf r In.Read.offset in
    let size = Ctypes.getf r In.Read.size in
    Handles.with_file_fd st.handles fh (fun { Handles.path } fd _k -> Out.(
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
  let flush f req st = Out.write_ack req; st

  (* TODO: flags? *)
  let release r req st =
    try
      Handles.(free (get st.handles (Ctypes.getf r In.Release.fh)));
      Out.write_ack req;
      st
    with Not_found ->
      Out.write_error req Unix.EBADF; st

  (* TODO: distinguish? *)
  let releasedir = release

  (* Can raise Unix.Unix_error *)
  let symlink name target req st = Out.(
    let ({ Nodes.path } as pnode) = Nodes.get st.nodes (nodeid req) in
    let path = Filename.concat path name in
    (* errors caught by our caller *)
    Unix.symlink target path;
    lookup name req st (* TODO: still increment lookups? *)
  )

  (* Can raise Unix.Unix_error *)
  let rename r src dest req st = Out.(
    let { Nodes.path } = Nodes.get st.nodes (nodeid req) in
    let newdir = Nodes.get st.nodes (Ctypes.getf r In.Rename.newdir) in
    (* errors caught by our caller *)
    Unix.rename (Filename.concat path src)
      (Filename.concat newdir.Nodes.path dest);
    (* TODO: still increment lookups? *)
    respond_with_entry newdir dest req;
    st
  )

  (* Can raise Unix.Unix_error *)
  let unlink name req st = Out.(
    let { Nodes.path } = Nodes.get st.nodes (nodeid req) in
    let path = Filename.concat path name in
    (* errors caught by our caller *)
    Unix.unlink path;
    write_ack req;
    st
  )

  (* Can raise Unix.Unix_error *)
  let rmdir name req st = Out.(
    let { Nodes.path } = Nodes.get st.nodes (nodeid req) in
    let path = Filename.concat path name in
    (* errors caught by our caller *)
    Unix.rmdir path;
    write_ack req;
    st
  )

  (* TODO: do *)
  let statfs = enosys

  (* TODO: do *)
  let fsync _f = enosys

  (* Can raise Unix.Unix_error *)
  (* TODO: write flags? *)
  let write w req st =
    let fh = Ctypes.getf w In.Write.fh in
    let offset = Ctypes.getf w In.Write.offset in
    let size = Ctypes.getf w In.Write.size in
    Handles.with_file_fd st.handles fh (fun { Handles.path } fd _k -> Out.(
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
    let { Nodes.path } = Nodes.get st.nodes (nodeid req) in
    let oldnode = Nodes.get st.nodes (Ctypes.getf l In.Link.oldnodeid) in
    let path = Filename.concat path name in
    (* errors caught by our caller *)
    Unix.link oldnode.Nodes.path path;
    lookup name req st (* TODO: still increment lookups? *)

  (* TODO: do *)
  let getxattr _g = enosys

  (* TODO: do *)
  let setxattr _s = enosys

  (* TODO: do *)
  let listxattr _g = enosys

  (* TODO: do *)
  let removexattr _name = enosys

  let access a req st =
    let { agents } = st in
    let { Nodes.path } = Nodes.get st.nodes (nodeid req) in
    let uid = Ctypes.getf req.Fuse.hdr In.Hdr.uid in
    let gid = Ctypes.getf req.Fuse.hdr In.Hdr.gid in
    let code = Ctypes.getf a In.Access.mask in
    let phost = Fuse.(req.chan.host.unix_unistd.Unix_unistd.access) in
    let perms = Unix_unistd.Access.(of_code ~host:phost code) in
    try
      agents.Agent_handler.access ~uid ~gid path perms;
      Out.write_ack req;
      st
    with Unix.Unix_error(err,_,_) ->
      Out.write_error req err;
      st

  let create c name req st = Out.(
    try
      let ({ Nodes.path } as pnode) = Nodes.get st.nodes (nodeid req) in
      let mode = Ctypes.getf c In.Create.mode in (* TODO: is only file_perm? *)
      let flags = Ctypes.getf c In.Create.flags in
      let phost = Fuse.(req.chan.host.unix_fcntl.Unix_fcntl.oflags) in
      let flags = Unix_fcntl.Oflags.(
        List.rev_map to_open_flag_exn (of_code ~host:phost flags)
      ) in
      let path = Filename.concat path name in
      let file = Unix.(
        openfile path (O_WRONLY::O_CREAT::O_TRUNC::flags) (Int32.to_int mode)
      ) in
      let kind = Unix.((Unix.fstat file).st_kind) in
      let h = Handles.(alloc st.handles path (File (file, kind))) in
      write_reply req
        (Create.create
           ~store_entry:(store_entry pnode name)
           ~store_open:(Open.store ~fh:h.Handles.id ~open_flags:0l));
      (* TODO: flags *)
      st
    with Not_found ->
      (* TODO: log? *)
      write_error req Unix.ENOENT; st
  )

  let mknod m name req st =
    let ({ Nodes.path } as pnode) = Nodes.get st.nodes (nodeid req) in
    let path = Filename.concat path name in
    let mode = Ctypes.getf m In.Mknod.mode in
    let rdev = Ctypes.getf m In.Mknod.rdev in (* TODO: use this? *)
    (* TODO: translate mode and dev from client host rep to local host rep *)
    (* TODO: dev_t is usually 64-bit but rdev is 32-bit. translate how? *)
    (* TODO: regular -> open with O_CREAT | O_EXCL | O_WRONLY for compat? *)
    (* TODO: fifo -> mkfifo for compat? *)
    Unix_sys_stat.mknod path
      (to_mode_t mode)
      (to_dev_t (Unsigned.UInt64.of_int64
                   (Int64.of_int32
                      (Unsigned.UInt32.to_int32 rdev))));
    respond_with_entry pnode name req;
    st

  let mkdir m name req st =
    let ({ Nodes.path } as pnode) = Nodes.get st.nodes (nodeid req) in
    let path = Filename.concat path name in
    let mode = Ctypes.getf m In.Mkdir.mode in
    Unix.mkdir path (Unsigned.UInt32.to_int mode);
    respond_with_entry pnode name req;
    st

  (* TODO: do *)
  let fsyncdir _f = enosys

  (* TODO: do *)
  let getlk _lk = enosys

  (* TODO: do *)
  let setlk _lk = enosys

  (* TODO: do *)
  let setlkw _lk = enosys

  (* TODO: do *)
  let interrupt _i = enosys

  (* TODO: do *)
  let bmap _b = enosys

  let destroy _req _st = ()

  let setattr s req st = In.Setattr.(
    let valid = Ctypes.getf s valid in
    begin
      if Valid.(is_set valid handle)
      then Handles.with_file_fd st.handles (Ctypes.getf s fh) (fun _h fd k ->
        (if Valid.(is_set valid mode)
         then
            let mode = Ctypes.getf s mode in
            let phost = Fuse.(req.chan.host.unix_sys_stat.Unix_sys_stat.mode) in
            let (kind,perm) = Stat.Mode.(
              of_code_exn ~host:phost (Int32.to_int mode)
            ) in
            assert (kind = k); (* TODO: ???!!! *)
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
        let { Nodes.path } = Nodes.get st.nodes (nodeid req) in
        (if Valid.(is_set valid mode)
         then
            let mode = Ctypes.getf s mode in
            let phost = Fuse.(req.chan.host.unix_sys_stat.Unix_sys_stat.mode) in
            let (kind,perm) = Stat.Mode.(
              of_code_exn ~host:phost (Int32.to_int mode)
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
