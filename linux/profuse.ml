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

module Caml = struct
  module Array = Array
end
open Ctypes
open Foreign
open Unsigned
open View

module In   = In.Linux_7_8
type req = In.t In_common.request
type 'a fs_fn = req -> 'a -> 'a

module type RO_SIMPLE = sig
  type t

  val getattr    : t fs_fn
  val opendir    : In.Open.t structure -> t fs_fn
  val forget     : int -> t fs_fn
  val lookup     : string -> t fs_fn
  val readdir    : In.Read.t structure -> t fs_fn
  val readlink   : t fs_fn
  val release    : In.Release.t structure -> t fs_fn
  val releasedir : In.Release.t structure -> t fs_fn
  val open_      : In.Open.t structure -> t fs_fn
  val read       : In.Read.t structure -> t fs_fn
  val access     : In.Access.t structure -> t fs_fn
  val destroy    : req -> t -> unit
end

module type RO_MID = sig
  include RO_SIMPLE

  val statfs : t fs_fn
end

module type RO_FULL = sig
  include RO_MID

  val getxattr  : In.Getxattr.t structure -> t fs_fn
  val listxattr : In.Getxattr.t structure -> t fs_fn
  val interrupt : In.Interrupt.t structure -> t fs_fn
  val bmap      : In.Bmap.t structure -> t fs_fn
end

module type RW_SIMPLE = sig
  include RO_SIMPLE

  val flush   : In.Flush.t structure -> t fs_fn
  val link    : In.Link.t structure -> string -> t fs_fn
  val symlink : string -> string -> t fs_fn
  val rename  : In.Rename.t structure -> string -> string -> t fs_fn
  val unlink  : string -> t fs_fn
  val rmdir   : string -> t fs_fn
  val mknod   : In.Mknod.t structure -> string -> t fs_fn
  val write   : In.Write.t structure -> t fs_fn
  val mkdir   : In.Mkdir.t structure -> string -> t fs_fn
  val setattr : In.Setattr.t structure -> t fs_fn
end

module type RW_MID = sig
  include RO_MID
  include RW_SIMPLE with type t := t

  val create      : In.Create.t structure -> string -> t fs_fn
  val fsync       : In.Fsync.t structure -> t fs_fn
end

module type RW_FULL = sig
  include RO_FULL
  include RW_MID with type t := t

  val fsyncdir    : In.Fsync.t structure -> t fs_fn
  val setxattr    : In.Setxattr.t structure -> t fs_fn
  val removexattr : string -> t fs_fn
  val getlk       : In.Lk.t structure -> t fs_fn (* TODO: RO? *)
  val setlk       : In.Lk.t structure -> t fs_fn (* TODO: RO? *)
  val setlkw      : In.Lk.t structure -> t fs_fn (* TODO: RO? *)
end

module Zero :
  functor (X : sig type t end) -> functor (Out : Out.WRITE) ->
    RW_FULL with type t := X.t =
  functor (X : sig type t end) -> functor (Out : Out.WRITE) ->
struct
  let enosys req st = Out.write_error req Unix.ENOSYS; st

  let getattr      = enosys
  let opendir _    = enosys
  let forget _     = enosys
  let lookup _     = enosys
  let readdir _    = enosys
  let readlink     = enosys
  let release _    = enosys
  let releasedir _ = enosys
  let open_ _      = enosys
  let read _       = enosys
  let access _     = enosys
  let destroy req st = ignore (enosys req st)

  let statfs       = enosys
  let getxattr _   = enosys
  let listxattr _  = enosys
  let interrupt _  = enosys
  let bmap _       = enosys

  let flush _      = enosys
  let link _ _     = enosys
  let symlink _ _  = enosys
  let rename _ _ _ = enosys
  let unlink _     = enosys
  let rmdir _      = enosys
  let mknod _ _    = enosys
  let write _      = enosys
  let mkdir _ _    = enosys
  let setattr _    = enosys

  let create _ _   = enosys
  let fsync _      = enosys
  let fsyncdir _   = enosys
  let setxattr _   = enosys
  let removexattr _= enosys
  let getlk _      = enosys
  let setlk _      = enosys
  let setlkw _     = enosys
end

module Ro_simple :
  functor (X : RO_SIMPLE) -> functor (Out : Out.WRITE) ->
    RW_FULL with type t = X.t =
  functor (X : RO_SIMPLE) -> functor (Out : Out.WRITE) ->
struct
  include Zero(X)(Out)
  include X
end

module Ro_mid :
  functor (X : RO_MID) -> functor (Out : Out.WRITE) ->
    RW_FULL with type t = X.t =
  functor (X : RO_MID) -> functor (Out : Out.WRITE) ->
struct
  include Zero(X)(Out)
  include X
end

module Ro_full :
  functor (X : RO_FULL) -> functor (Out : Out.WRITE) ->
    RW_FULL with type t = X.t =
  functor (X : RO_FULL) -> functor (Out : Out.WRITE) ->
struct
  include Zero(X)(Out)
  include X
end

module Rw_simple :
  functor (X : RW_SIMPLE) -> functor (Out : Out.WRITE) ->
    RW_FULL with type t = X.t =
  functor (X : RW_SIMPLE) -> functor (Out : Out.WRITE) ->
struct
  include Zero(X)(Out)
  include X
end

module Rw_mid :
  functor (X : RW_MID) -> functor (Out : Out.WRITE) ->
    RW_FULL with type t = X.t =
  functor (X : RW_MID) -> functor (Out : Out.WRITE) ->
struct
  include Zero(X)(Out)
  include X
end

module type FULL = sig
  include RW_FULL
  val mount : argv:string array -> mnt:string -> t -> req * t
end

module type FS = sig
  type t

  val trace_channel : out_channel

  val string_of_nodeid : int64 -> t -> string
  val string_of_state  : req -> t -> string

  module Linux_7_8 : functor (Out : Out.LINUX_7_8) -> FULL with type t = t
end

module type SERVER = functor (Fs : FS) -> sig
  module Serve_out : Out.LINUX_7_8
  module Trace_out : Out.LINUX_7_8

  val string_of_request : req -> Fs.t -> string

  val serve : Fuse.chan -> Fs.t -> Fs.t
  val trace : Fuse.chan -> string -> Fs.t -> Fs.t
end

let no_flags = 0l

let onbit = Int32.shift_left 1l
let cap_async_read       = onbit 0
let cap_posix_locks      = onbit 1
let cap_file_ops         = onbit 2
let cap_atomic_o_trunc   = onbit 3
let cap_export_support   = onbit 4
let cap_big_writes       = onbit 5
let cap_dont_mask        = onbit 6
let cap_splice_write     = onbit 7
let cap_splice_move      = onbit 8
let cap_splice_read      = onbit 9
let cap_flock_locks      = onbit 10
let cap_ioctl_dir        = onbit 11
let cap_auto_inval_data  = onbit 12
let cap_readdirplus      = onbit 13
let cap_readdirplus_auto = onbit 14
let is_set flags cap = (Int32.logand flags cap) <> 0l

let fusermount = "fusermount"

let check_status cmd = function
  | Unix.WEXITED k when k <> 0 ->
    raise (Fuse.ExecError (cmd,"exit code "^(string_of_int k)))
  | Unix.WSIGNALED k ->
    raise (Fuse.ExecError (cmd,"ocaml kill signal "^(string_of_int k)))
  | Unix.WSTOPPED k ->
    raise (Fuse.ExecError (cmd,"ocaml stop signal "^(string_of_int k)))
  | Unix.WEXITED _ -> ()
module Linux_7_8(Out : Out.LINUX_7_8) = struct
  (**
     Can raise Fuse.ExecError, Fuse.ProtocolError
     If this fails, you should be sure to unmount the mountpoint.
  *)
  let mount ~argv ~mnt st =
    let max_write = 1 lsl 16 in

    let argv = Caml.Array.append argv [|mnt|] in

    let wsock, rsock = Unix.(socketpair PF_UNIX SOCK_STREAM 0) in
    let wfd = Fd_send_recv.int_of_fd wsock in
    let pid = Unix.(create_process_env fusermount argv
                      [|"_FUSE_COMMFD="^(string_of_int wfd)|]
                      stdin stdout stderr) in

    let _, status = Unix.waitpid [] pid in
    let () = Unix.(shutdown wsock SHUTDOWN_ALL) in
    check_status fusermount status;

    (* We must read at least 1 byte, by POSIX! *)
    let _, _, fd = Fd_send_recv.recv_fd rsock "\000" 0 1 [] in

    let () = Unix.(shutdown rsock SHUTDOWN_ALL) in

    let init_fs = Fuse.({
      mnt; fd; unique=UInt64.zero; version=(0,0);
      max_readahead=0; max_write; flags=no_flags; host;
    }) in
    let req = In.read init_fs () in

    In.(match req with
    | { Fuse.pkt=Init pkt } ->
      let major = getf pkt Init.major in
      if major <> 7 then raise
        (Fuse.ProtocolError
           (init_fs, Printf.sprintf
             "Incompatible FUSE protocol major version %d <> 7" major));
      let minor = getf pkt Init.minor in
      if minor < 8 then raise
        (Fuse.ProtocolError
           (init_fs, Printf.sprintf
             "Incompatible FUSE protocol minor version %d < 8" minor));
      let minor = min minor 8 in (* TODO: track kernel minor *)
      let max_readahead = getf pkt Init.max_readahead in
      let max_readahead = max max_readahead 65536 in (* TODO: ? *)
      let flags = getf pkt Init.flags in (* TODO: ? *)
      let pkt = Out.Init.create ~major ~minor ~max_readahead
        ~flags:(UInt32.of_int 0) ~max_write in
      let fs = Fuse.({
        mnt; fd; unique=init_fs.unique; version = (major, minor);
        max_readahead; max_write; flags = 0l; host;
      }) in
      Out.write_reply req pkt;
      req, st
    | { Fuse.hdr } -> raise
      (Fuse.ProtocolError
         (init_fs, Printf.sprintf "Unexpected opcode %s <> FUSE_INIT"
           (Opcode.to_string (getf hdr Hdr.opcode))))
    )

  let nodeid req = In.(Ctypes.getf req.Fuse.hdr Hdr.nodeid)

  let enosys req st = Out.write_error req Unix.ENOSYS; st

  let store_entry store_attr_of_path parent name = Out.(
    let path = Filename.concat parent.Nodes.path name in
    let store_attr = store_attr_of_path path in (* can raise ENOENT *)
    let node = Nodes.lookup parent name in
    let nodeid = node.Nodes.id in
    let generation = node.Nodes.gen in
    Entry.store ~nodeid ~generation
      ~entry_valid:0L ~entry_valid_nsec:0l
      ~attr_valid:0L ~attr_valid_nsec:0l
      ~store_attr
  )

  let respond_with_entry store_attr_of_path parent name req = Out.(
    let path = Filename.concat parent.Nodes.path name in
    let store_attr = store_attr_of_path path in (* can raise ENOENT *)
    let node = Nodes.lookup parent name in
    let nodeid = node.Nodes.id in
    let generation = node.Nodes.gen in
    write_reply req
      (Entry.create ~nodeid ~generation
         ~entry_valid:0L ~entry_valid_nsec:0l
         ~attr_valid:0L ~attr_valid_nsec:0l
         ~store_attr)
  )
end

let exec_unmount_path args mnt =
  let cmd = Printf.sprintf "%s %s %s" fusermount args mnt in
  check_status cmd (Unix.system cmd)

(** Can raise Fuse.ExecError *)
let unmount_path = exec_unmount_path "-u"

(** Can raise Fuse.ExecError *)
let unmount fs = Fuse.(
  Unix.close fs.fd;
  unmount_path fs.mnt
)

(** Can raise Fuse.ExecError *)
let detach_path = exec_unmount_path "-u -z"

(** Can raise Fuse.ExecError *)
let detach fs = Fuse.(
  Unix.close fs.fd;
  detach_path fs.mnt
)

module Server : SERVER = functor (Fs : FS) -> struct

  module Handler(Out : Out.LINUX_7_8) = struct
    module Fs = Fs.Linux_7_8(Out)

    let dispatch req t =
      try (match req.Fuse.pkt with
      | In.Init _ -> raise (Fuse.ProtocolError (req.Fuse.chan,"INIT after mount"))
      | In.Getattr -> Fs.getattr req t
      | In.Opendir op -> Fs.opendir op req t
      | In.Forget f -> Fs.forget In.(Ctypes.getf f Forget.nlookup) req t
      | In.Lookup name -> Fs.lookup name req t
      | In.Readdir r -> Fs.readdir r req t
      | In.Readlink -> Fs.readlink req t
      | In.Releasedir r -> Fs.releasedir r req t
      | In.Open op -> Fs.open_ op req t
      | In.Read r -> Fs.read r req t
      | In.Flush f -> Fs.flush f req t
      | In.Release r -> Fs.release r req t
      | In.Symlink (name,target) -> Fs.symlink name target req t
      | In.Rename (r,src,dest) -> Fs.rename r src dest req t
      | In.Unlink name -> Fs.unlink name req t
      | In.Rmdir name -> Fs.rmdir name req t
      | In.Statfs -> Fs.statfs req t
      | In.Fsync f -> Fs.fsync f req t
      | In.Write w -> Fs.write w req t
      | In.Link (l,name) -> Fs.link l name req t
      | In.Getxattr g -> Fs.getxattr g req t
      | In.Setxattr s -> Fs.setxattr s req t
      | In.Listxattr g -> Fs.listxattr g req t
      | In.Removexattr name -> Fs.removexattr name req t
      | In.Access a -> Fs.access a req t
      | In.Create (c,name) -> Fs.create c name req t
      | In.Mknod (m,name) -> Fs.mknod m name req t
      | In.Mkdir (m,name) -> Fs.mkdir m name req t
      | In.Fsyncdir f -> Fs.fsyncdir f req t
      | In.Getlk lk  -> Fs.getlk  lk req t
      | In.Setlk lk  -> Fs.setlk  lk req t
      | In.Setlkw lk -> Fs.setlkw lk req t
      | In.Interrupt i -> Fs.interrupt i req t
      | In.Bmap b -> Fs.bmap b req t
      | In.Destroy -> Fs.destroy req t; raise (Fuse.Destroy 0)
      | In.Setattr s -> Fs.setattr s req t
      | In.Other _ | In.Unknown _ -> Out.write_error req Unix.ENOSYS; t
      ) with
      | Unix.Unix_error(e,_,_) -> Out.write_error req e; t
      | (Fuse.Destroy k) as exn -> raise exn
      | exn -> Out.write_error req Unix.EIO; raise exn
  end

  module Serve_out = Out.Linux_7_8

  let serve chan =
    let read = In.read chan in
    let module H = Handler(Serve_out) in
    fun t -> H.dispatch (read ()) t

  let string_of_mode req mode =
    let open Unix_sys_stat.Mode in
    let host = Fuse.(req.chan.host.unix_sys_stat.Unix_sys_stat.mode) in
    to_string ~host (of_code_exn ~host mode)

  let string_of_perms req perms =
    let open Unix_sys_stat.File_perm in
    let host = Fuse.(req.chan.host.unix_sys_stat.Unix_sys_stat.file_perm) in
    to_string ~host (full_of_code ~host perms)

  let string_of_request req t =
    let open Ctypes in
    let open In in
    Printf.sprintf "%s.p%ld.u%ld.g%ld.%Ld (%s) %s"
      (Opcode.to_string (getf req.Fuse.hdr Hdr.opcode))
      (getf req.Fuse.hdr Hdr.pid)
      (getf req.Fuse.hdr Hdr.uid)
      (getf req.Fuse.hdr Hdr.gid)
      (Unsigned.UInt64.to_int64 (getf req.Fuse.hdr Hdr.unique))
      (Fs.string_of_nodeid (getf req.Fuse.hdr Hdr.nodeid) t)
      (match req.Fuse.pkt with
      | Init i ->
        Printf.sprintf "version=%d.%d max_readahead=%d flags=0x%lX"
          (getf i Init.major) (getf i Init.minor)
          (getf i Init.max_readahead)
          (Unsigned.UInt32.to_int32 (getf i Init.flags))
      | Getattr | Readlink | Destroy -> ""
      | Symlink (name,target) -> name ^ " -> " ^ target
      | Forget f -> string_of_int (getf f Forget.nlookup)
      | Lookup name -> name
      | Mkdir (m,name) -> Printf.sprintf "mode=%s %s"
        (string_of_perms req (Int32.to_int (getf m Mkdir.mode))) name
      | Mknod (m,name) -> Printf.sprintf "mode=%s rdev=%ld %s"
        (string_of_mode req (Int32.to_int (getf m Mknod.mode)))
        (Unsigned.UInt32.to_int32 (getf m Mknod.rdev))
        name
      | Create (c,name) -> Printf.sprintf "flags=%ld mode=%s %s"
        (getf c Create.flags)
        (string_of_mode req (Int32.to_int (getf c Create.mode))) name
      | Setattr s -> Printf.sprintf "0x%lX[%s]"
        (getf s Setattr.valid)
        (String.concat " " (SetattrValid.attrs (getf s Setattr.valid)))
      | Access a ->
        let code = Ctypes.getf a In.Access.mask in
        let phost = Fuse.(req.chan.host.unix_unistd.Unix_unistd.access) in
        let perms = Unix_unistd.Access.(of_code ~host:phost code) in
        let uid = Ctypes.getf req.Fuse.hdr In.Hdr.uid in
        let gid = Ctypes.getf req.Fuse.hdr In.Hdr.gid in
        Printf.sprintf "uid:%ld gid:%ld (%s)" uid gid
          (List.fold_left Unix.(fun s -> function
          | R_OK -> s^"R" | W_OK -> s^"W" | X_OK -> s^"X" | F_OK -> s^"F"
           ) "" perms)
      | Unlink name | Rmdir name -> name
      | _ -> "FIX ME"
      )

  module Trace_out = struct
    include Out.Linux_7_8

    let write_reply req arrfn =
      let arr = arrfn req in
      let sz  = CArray.length arr + Hdr.hdrsz in
      let ptr = CArray.start arr -@ Hdr.hdrsz in
      Printf.fprintf Fs.trace_channel "    returning %s from %Ld\n%!"
        (describe_reply (deserialize req sz ptr))
        (Unsigned.UInt64.to_int64 (Ctypes.getf req.Fuse.hdr In.Hdr.unique));
      write_reply_raw req sz ptr

    let write_ack req =
      Printf.fprintf Fs.trace_channel "    returning ack from %Ld\n%!"
        (Unsigned.UInt64.to_int64 (Ctypes.getf req.Fuse.hdr In.Hdr.unique));
      write_ack req

    let write_error req err =
      Printf.fprintf Fs.trace_channel "    returning err %s from %Ld\n%!"
        (Unix_errno.to_string err)
        (Unsigned.UInt64.to_int64 (Ctypes.getf req.Fuse.hdr In.Hdr.unique));
      write_error req err

  end

  let trace chan =
    let read = In.read chan in
    let module H = Handler(Trace_out) in
    fun tag t ->
      let req = read () in
      (* can raise Opcode.Unknown?? *)
      Printf.fprintf Fs.trace_channel "    %s %s\n%!"
        tag (string_of_request req t);
      let t = H.dispatch req t in
      Printf.fprintf Fs.trace_channel "    %s\n%!" (Fs.string_of_state req t);
      t
end
