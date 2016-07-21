(*
 * Copyright (c) 2014-2015 David Sheets <sheets@alum.mit.edu>
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

open Ctypes
open Unsigned
module Types = Profuse_types_7_23.C(Profuse_types_detected_7_23)

type 'a structure = 'a Types.structure

module Flags = struct
  type t = int32

  let empty = 0l
end

module Host = struct
  type t = {
    fcntl    : Fcntl.Host.t;
    errno    : Errno.Host.t;
    sys_stat : Sys_stat.Host.t;
    dirent   : Dirent.Host.t;
    unistd   : Unistd.host;
  }

  let linux_4_0_5 = {
    fcntl    = Fcntl_host.Linux.V4_1_12.Musl.v1_1_12;
    errno    = Errno_host.Linux.v4_4_6;
    sys_stat = Sys_stat_host.Linux.V4_1_12.Musl.v1_1_12;
    dirent   = Dirent_unix.host; (* TODO: FIXME *)
    unistd   = Unistd_unix.host; (* TODO: FIXME *)
  }
end

type chan = {
  id : int;
  mutable unique : Unsigned.uint64;
  mnt : string;
  version : int * int;
  max_readahead : int;
  max_write : int;
  flags : Flags.t;
  host : Host.t;
}

exception ProtocolError of chan * string
exception Destroy of int

type ('hdr, 'body) packet = {
  chan : chan;
  hdr  : 'hdr Ctypes.structure;
  pkt  : 'body;
}

module Struct = struct
  module T = Types.Struct

  module Kstatfs = struct
    module T = T.Kstatfs

    let store
        ~blocks ~bfree ~bavail ~files ~ffree ~bsize ~namelen ~frsize mem =
      setf mem T.blocks  blocks;
      setf mem T.bfree   bfree;
      setf mem T.bavail  bavail;
      setf mem T.files   files;
      setf mem T.ffree   ffree;
      setf mem T.bsize   bsize;
      setf mem T.namelen namelen;
      setf mem T.frsize  frsize;
      ()

    let create
        ~blocks ~bfree ~bavail ~files ~ffree ~bsize ~namelen ~frsize () =
      let kstatfs = make T.t in
      store
        ~blocks ~bfree ~bavail ~files ~ffree ~bsize ~namelen ~frsize kstatfs;
      kstatfs

    let describe pkt =
      let i64 = UInt64.to_int64 in
      let i32 = UInt32.to_int32 in
      let blocks  = getf pkt T.blocks in
      let bfree   = getf pkt T.bfree in
      let bavail  = getf pkt T.bavail in
      let files   = getf pkt T.files in
      let ffree   = getf pkt T.ffree in
      let bsize   = getf pkt T.bsize in
      let frsize  = getf pkt T.frsize in
      let namelen = getf pkt T.namelen in
      Printf.sprintf
        "blocks=%Ld bfree=%Ld bavail=%Ld files=%Ld ffree=%Ld bsize=%ld frsize=%ld namelen=%ld"
        (i64 blocks)
        (i64 bfree)
        (i64 bavail)
        (i64 files)
        (i64 ffree)
        (i32 bsize)
        (i32 frsize)
        (i32 namelen)
  end

  module File_lock = struct
    module T = T.File_lock
  end

  module Attr = struct
    module T = T.Attr

    let store ~ino ~size ~blocks
        ~atime ~mtime ~ctime ~atimensec ~mtimensec ~ctimensec
        ~mode ~nlink ~uid ~gid ~rdev ~blksize mem =
      setf mem T.ino       ino;
      setf mem T.size      size;
      setf mem T.blocks    blocks;
      setf mem T.atime     atime;
      setf mem T.mtime     mtime;
      setf mem T.ctime     ctime;
      setf mem T.atimensec atimensec;
      setf mem T.mtimensec mtimensec;
      setf mem T.ctimensec ctimensec;
      setf mem T.mode      mode;
      setf mem T.nlink     nlink;
      setf mem T.uid       uid;
      setf mem T.gid       gid;
      setf mem T.rdev      rdev;
      setf mem T.blksize   blksize;
      ()

    let create ~ino ~size ~blocks
        ~atime ~mtime ~ctime ~atimensec ~mtimensec ~ctimensec
        ~mode ~nlink ~uid ~gid ~rdev ~blksize () =
      let attr = make T.t in
      store ~ino ~size ~blocks ~atime ~mtime ~ctime
        ~atimensec ~mtimensec ~ctimensec ~mode ~nlink ~uid ~gid ~rdev ~blksize
        attr;
      attr

    let describe ~host pkt =
      let phost = host.Host.sys_stat.Sys_stat.Host.mode in
      let i64 = UInt64.to_int64 in
      let i32 = UInt32.to_int32 in
      let mode = UInt32.to_int (getf pkt T.mode) in
      (* TODO: nsec times? *)
      Printf.sprintf
        "ino=%Ld size=%Ld blocks=%Ld atime=%Ld mtime=%Ld ctime=%Ld mode=%s (0x%x) nlink=%ld uid=%ld gid=%ld rdev=%ld blksize=%ld"
        (i64 (getf pkt T.ino))
        (i64 (getf pkt T.size))
        (i64 (getf pkt T.blocks))
        (i64 (getf pkt T.atime))
        (i64 (getf pkt T.mtime))
        (i64 (getf pkt T.ctime))
        Sys_stat.Mode.(to_string ~host:phost (of_code_exn ~host:phost mode))
        mode
        (i32 (getf pkt T.nlink))
        (i32 (getf pkt T.uid))
        (i32 (getf pkt T.gid))
        (i32 (getf pkt T.rdev))
        (i32 (getf pkt T.blksize))
  end

  module Forget_one = struct
    module T = T.Forget_one
  end
end

module In = struct
  module T = Types.In

  module Opcode = struct
    module T = T.Opcode

    type t = T.t

    let to_string = function
      | `CUSE_INIT -> "CUSE_INIT"
      | `FUSE_ACCESS -> "FUSE_ACCESS"
      | `FUSE_BATCH_FORGET -> "FUSE_BATCH_FORGET"
      | `FUSE_BMAP -> "FUSE_BMAP"
      | `FUSE_CREATE -> "FUSE_CREATE"
      | `FUSE_DESTROY -> "FUSE_DESTROY"
      | `FUSE_FALLOCATE -> "FUSE_FALLOCATE"
      | `FUSE_FLUSH -> "FUSE_FLUSH"
      | `FUSE_FORGET -> "FUSE_FORGET"
      | `FUSE_FSYNC -> "FUSE_FSYNC"
      | `FUSE_FSYNCDIR -> "FUSE_FSYNCDIR"
      | `FUSE_GETATTR -> "FUSE_GETATTR"
      | `FUSE_GETLK -> "FUSE_GETLK"
      | `FUSE_GETXATTR -> "FUSE_GETXATTR"
      | `FUSE_INIT -> "FUSE_INIT"
      | `FUSE_INTERRUPT -> "FUSE_INTERRUPT"
      | `FUSE_IOCTL -> "FUSE_IOCTL"
      | `FUSE_LINK -> "FUSE_LINK"
      | `FUSE_LISTXATTR -> "FUSE_LISTXATTR"
      | `FUSE_LOOKUP -> "FUSE_LOOKUP"
      | `FUSE_MKDIR -> "FUSE_MKDIR"
      | `FUSE_MKNOD -> "FUSE_MKNOD"
      | `FUSE_NOTIFY_REPLY -> "FUSE_NOTIFY_REPLY"
      | `FUSE_OPEN -> "FUSE_OPEN"
      | `FUSE_OPENDIR -> "FUSE_OPENDIR"
      | `FUSE_POLL -> "FUSE_POLL"
      | `FUSE_READ -> "FUSE_READ"
      | `FUSE_READDIR -> "FUSE_READDIR"
      | `FUSE_READDIRPLUS -> "FUSE_READDIRPLUS"
      | `FUSE_READLINK -> "FUSE_READLINK"
      | `FUSE_RELEASE -> "FUSE_RELEASE"
      | `FUSE_RELEASEDIR -> "FUSE_RELEASEDIR"
      | `FUSE_REMOVEXATTR -> "FUSE_REMOVEXATTR"
      | `FUSE_RENAME -> "FUSE_RENAME"
      | `FUSE_RENAME2 -> "FUSE_RENAME2"
      | `FUSE_RMDIR -> "FUSE_RMDIR"
      | `FUSE_SETATTR -> "FUSE_SETATTR"
      | `FUSE_SETLK -> "FUSE_SETLK"
      | `FUSE_SETLKW -> "FUSE_SETLKW"
      | `FUSE_SETXATTR -> "FUSE_SETXATTR"
      | `FUSE_STATFS -> "FUSE_STATFS"
      | `FUSE_SYMLINK -> "FUSE_SYMLINK"
      | `FUSE_UNLINK -> "FUSE_UNLINK"
      | `FUSE_WRITE -> "FUSE_WRITE"
      | `Unknown i -> "UnknownOpcode("^(Int32.to_string i)^")"

    let returns = function
      | `FUSE_FORGET | `FUSE_DESTROY -> false
      | _ -> true

    let of_uint32 : Unsigned.UInt32.t -> Types.In.Opcode.t =
      let l = List.map (fun (k, v) -> (v, k)) Types.In.Opcode.enum_values in
      fun i ->
        try ListLabels.assoc i l
        with Not_found -> `Unknown (Unsigned.UInt32.to_int32 i)

    let to_uint32 v = ListLabels.assoc v Types.In.Opcode.enum_values
  end

  module Hdr = struct
    module T = T.Hdr

    let hdrsz = sizeof T.t
    let sz = hdrsz

    (* Create a headed packet with count buffer sequentially after header *)
    let packet ~opcode ~unique ~nodeid ~uid ~gid ~pid ~count =
      let opcode = Opcode.to_uint32 opcode in
      let bodysz = count in
      let count = hdrsz + bodysz in
      let pkt = allocate_n char ~count in
      let hdr = !@ (coerce (ptr char) (ptr T.t) pkt) in
      setf hdr T.len    (UInt32.of_int count);
      setf hdr T.opcode opcode;
      setf hdr T.unique unique;
      setf hdr T.nodeid nodeid;
      setf hdr T.uid    uid;
      setf hdr T.gid    gid;
      setf hdr T.pid    pid;
      CArray.from_ptr (pkt +@ hdrsz) bodysz

    (* Create a headed packet with st struct sequentially after header *)
    let make ~opcode ~unique ~nodeid ~uid ~gid ~pid st =
      let count = sizeof st in
      let pkt = packet ~opcode ~unique ~nodeid ~uid ~gid ~pid ~count in
      !@ (coerce (ptr char) (ptr st) (CArray.start pkt))

    let memcpy ~dest ~src n =
      let cast p = from_voidp (array n uchar) p in
      cast dest <-@ !@(cast src)

    let packet_from_hdr hdr ~count =
      let bodysz = count in
      let count = hdrsz + bodysz in
      let pkt = allocate_n char ~count in
      let dest = to_voidp pkt in
      memcpy ~dest ~src:(to_voidp (addr hdr)) hdrsz;
      setf hdr T.len (UInt32.of_int count);
      CArray.from_ptr (pkt +@ hdrsz) bodysz

    let make_from_hdr hdr st =
      let count = sizeof st in
      let pkt = packet_from_hdr hdr ~count in
      !@ (coerce (ptr char) (ptr st) (CArray.start pkt))
  end

  module Init = struct
    module T = T.Init
  end

  module Open = struct
    module T = T.Open
  end

  module Read = struct
    module T = T.Read
  end

  module Release = struct
    module T = T.Release
  end

  module Access = struct
    module T = T.Access
  end

  module Forget = struct
    module T = T.Forget
  end

  module Flush = struct
    module T = T.Flush
  end

  module Create = struct
    module T = T.Create

    let struct_size = sizeof T.t

    let name p = coerce (ptr char) string ((from_voidp char p) +@ struct_size)
  end

  module Mknod = struct
    module T = T.Mknod

    let struct_size = sizeof T.t

    let name p = coerce (ptr char) string ((from_voidp char p) +@ struct_size)
  end

  module Mkdir = struct
    module T = T.Mkdir

    let struct_size = sizeof T.t

    let name p = coerce (ptr char) string ((from_voidp char p) +@ struct_size)
  end

  module Rename = struct
    module T = T.Rename

    let struct_size = sizeof T.t

    let source_destination p =
      let p = (from_voidp char p) +@ struct_size in
      let src = coerce (ptr char) string p in
      let p = p +@ (String.length src + 1) in
      let dest = coerce (ptr char) string p in
      (src, dest)
  end

  module Link = struct
    module T = T.Link

    let struct_size = sizeof T.t

    let name p = coerce (ptr char) string ((from_voidp char p) +@ struct_size)
  end

  module Write = struct
    module T = T.Write

    let struct_size = sizeof T.t

    let data p = (from_voidp char p) +@ struct_size
  end

  module Fsync = struct
    module T = T.Fsync
  end

  module Lk = struct
    module T = T.Lk
  end

  module Interrupt = struct
    module T = T.Interrupt
  end

  module Bmap = struct
    module T = T.Bmap
  end

  module Setattr = struct
    module T = T.Setattr

    module Valid = struct
      module T = T.Valid

      type t = {
        mode : bool;
        uid : bool;
        gid : bool;
        size : bool;
        atime : bool;
        mtime : bool;
        fh : bool;
        unknown : int32;
        atime_now : bool;
        mtime_now : bool;
        lock_owner : bool;
        ctime : bool;
      }

      let (&&&) = UInt32.logand
      let (|||) = UInt32.logor

      let to_string_list valid =
        let list = if valid.mode       then ["mode"]            else []   in
        let list = if valid.uid        then "uid"        ::list else list in
        let list = if valid.gid        then "gid"        ::list else list in
        let list = if valid.size       then "size"       ::list else list in
        let list = if valid.atime      then "atime"      ::list else list in
        let list = if valid.mtime      then "mtime"      ::list else list in
        let list = if valid.fh         then "fh"         ::list else list in
        let list = if valid.atime_now  then "atime_now"  ::list else list in
        let list = if valid.mtime_now  then "mtime_now"  ::list else list in
        let list = if valid.lock_owner then "lock_owner" ::list else list in
        let list = if valid.ctime      then "ctime"      ::list else list in
        if valid.unknown = Int32.zero
        then list
        else (Printf.sprintf "unknown[0x%lx]" valid.unknown)::list

      let all = UInt32.zero
                ||| T.fattr_mode
                ||| T.fattr_uid
                ||| T.fattr_gid
                ||| T.fattr_size
                ||| T.fattr_atime
                ||| T.fattr_mtime
                ||| T.fattr_fh
                ||| T.fattr_atime_now
                ||| T.fattr_mtime_now
                ||| T.fattr_lockowner
                ||| T.fattr_ctime


      let of_uint32 i = {
        mode = UInt32.(compare zero (i &&& T.fattr_mode) <> 0);
        uid = UInt32.(compare zero (i &&& T.fattr_uid) <> 0);
        gid = UInt32.(compare zero (i &&& T.fattr_gid) <> 0);
        size = UInt32.(compare zero (i &&& T.fattr_size) <> 0);
        atime = UInt32.(compare zero (i &&& T.fattr_atime) <> 0);
        mtime = UInt32.(compare zero (i &&& T.fattr_mtime) <> 0);
        fh = UInt32.(compare zero (i &&& T.fattr_fh) <> 0);
        unknown = UInt32.(to_int32 (i &&& (lognot all)));
        atime_now = UInt32.(compare zero (i &&& T.fattr_atime_now) <> 0);
        mtime_now = UInt32.(compare zero (i &&& T.fattr_mtime_now) <> 0);
        lock_owner = UInt32.(compare zero (i &&& T.fattr_lockowner) <> 0);
        ctime = UInt32.(compare zero (i &&& T.fattr_ctime) <> 0);
      }

      let to_uint32 {
        mode;
        uid;
        gid;
        size;
        atime;
        mtime;
        fh;
        atime_now;
        mtime_now;
        lock_owner;
        ctime;
      } =
        let open UInt32 in
        (if mode then T.fattr_mode else zero) |||
        (if uid then T.fattr_uid else zero) |||
        (if gid then T.fattr_gid else zero) |||
        (if size then T.fattr_size else zero) |||
        (if atime then T.fattr_atime else zero) |||
        (if mtime then T.fattr_mtime else zero) |||
        (if fh then T.fattr_fh else zero) |||
        (if atime_now then T.fattr_atime_now else zero) |||
        (if mtime_now then T.fattr_mtime_now else zero) |||
        (if lock_owner then T.fattr_lockowner else zero) |||
        (if ctime then T.fattr_ctime else zero)
    end

    let create_from_hdr
        ~valid ~fh ~size
        ~atime ~mtime ~atimensec ~mtimensec
        ~mode ~uid ~gid ~lock_owner
        ~ctime ~ctimensec
        hdr =
      let pkt = Hdr.make_from_hdr hdr T.t in
      setf pkt T.valid      valid;
      setf pkt T.fh         fh;
      setf pkt T.size       size;
      setf pkt T.atime      atime;
      setf pkt T.mtime      mtime;
      setf pkt T.atimensec  atimensec;
      setf pkt T.mtimensec  mtimensec;
      setf pkt T.mode       mode;
      setf pkt T.uid        uid;
      setf pkt T.gid        gid;
      setf pkt T.lock_owner lock_owner;
      setf pkt T.ctime      ctime;
      setf pkt T.ctimensec  ctimensec;
      CArray.from_ptr (coerce (ptr T.t) (ptr char) (addr pkt)) (sizeof T.t)

    let to_string_list ~host t =
      let valid = Valid.of_uint32 (getf t T.valid) in
      let list =
        if valid.Valid.mode
        then
          let mode_code = Unsigned.UInt32.to_int (getf t T.mode) in
          let mode = Sys_stat.File_perm.full_of_code ~host mode_code in
          let mode_string = Sys_stat.File_perm.to_string ~host mode in
          [Printf.sprintf "mode=%s" mode_string]
        else []
      in
      let list =
        if valid.Valid.uid
        then
          let uid = Unsigned.UInt32.to_int (getf t T.uid) in
          (Printf.sprintf "uid=%d" uid)::list
        else list
      in
      let list =
        if valid.Valid.gid
        then
          let gid = Unsigned.UInt32.to_int (getf t T.gid) in
          (Printf.sprintf "gid=%d" gid)::list
        else list
      in
      let list =
        if valid.Valid.size
        then
          let size = Unsigned.UInt64.to_int64 (getf t T.size) in
          (Printf.sprintf "size=%Ld" size)::list
        else list
      in
      let list =
        if valid.Valid.atime
        then
          let atime = Unsigned.UInt64.to_int64 (getf t T.atime) in
          (Printf.sprintf "atime=%Ld" atime)::list
        else list
      in
      let list =
        if valid.Valid.atime_now
        then (Printf.sprintf "atime_now")::list
        else list
      in
      let list =
        if valid.Valid.mtime
        then
          let mtime = Unsigned.UInt64.to_int64 (getf t T.mtime) in
          (Printf.sprintf "mtime=%Ld" mtime)::list
        else list
      in
      let list =
        if valid.Valid.mtime_now
        then (Printf.sprintf "mtime_now")::list
        else list
      in
      let list =
        if valid.Valid.fh
        then
          let fh = Unsigned.UInt64.to_int64 (getf t T.fh) in
          (Printf.sprintf "fh=%Ld" fh)::list
        else list
      in
      let list =
        if valid.Valid.ctime
        then
          let ctime = Unsigned.UInt64.to_int64 (getf t T.ctime) in
          (Printf.sprintf "ctime=%Ld" ctime)::list
        else list
      in
      let list =
        if valid.Valid.lock_owner
        then
          let mtime = Unsigned.UInt64.to_int64 (getf t T.lock_owner) in
          (Printf.sprintf "lock_owner=%Ld" mtime)::list
        else list
      in
      if valid.Valid.unknown = Int32.zero
      then list
      else (Printf.sprintf "unknown[0x%lx]" valid.Valid.unknown)::list
  end

  module Getxattr = struct
    module T = T.Getxattr

    let create_from_hdr ~size hdr =
      let pkt = Hdr.make_from_hdr hdr T.t in
      setf pkt T.size size;
      CArray.from_ptr (coerce (ptr T.t) (ptr char) (addr pkt)) (sizeof T.t)
  end

  module Setxattr = struct
    module T = T.Setxattr

    let create_from_hdr ~size ~flags hdr =
      let pkt = Hdr.make_from_hdr hdr T.t in
      setf pkt T.size  size;
      setf pkt T.flags flags;
      CArray.from_ptr (coerce (ptr T.t) (ptr char) (addr pkt)) (sizeof T.t)
  end

  module Batch_forget = struct
    module T = T.Batch_forget
  end

  module Message = struct
    type t =
      | Init of Init.T.t structure
      | Getattr
      | Lookup of string
      | Opendir of Open.T.t structure
      | Readdir of Read.T.t structure
      | Releasedir of Release.T.t structure
      | Fsyncdir of Fsync.T.t structure
      | Rmdir of string
      | Getxattr of Getxattr.T.t structure
      | Setxattr of Setxattr.T.t structure
      | Listxattr of Getxattr.T.t structure
      | Removexattr of string
      | Access of Access.T.t structure
      | Forget of Forget.T.t structure
      | Readlink
      | Open of Open.T.t structure
      | Read of Read.T.t structure
      | Write of Write.T.t structure * char Ctypes.ptr
      | Statfs
      | Flush of Flush.T.t structure
      | Release of Release.T.t structure
      | Fsync of Fsync.T.t structure
      | Unlink of string
      | Create of Create.T.t structure * string
      | Mknod of Mknod.T.t structure * string
      | Mkdir of Mkdir.T.t structure * string
      | Setattr of Setattr.T.t structure
      | Link of Link.T.t structure * string
      | Symlink of string * string
      | Rename of Rename.T.t structure * string * string
      | Getlk of Lk.T.t structure
      | Setlk of Lk.T.t structure
      | Setlkw of Lk.T.t structure
      | Interrupt of Interrupt.T.t structure
      | Bmap of Bmap.T.t structure
      | Batch_forget of Struct.Forget_one.T.t structure list
      | Destroy
      | Other of Opcode.t
      | Unknown of int32

    let unknown i = Unknown i

    let parse chan hdr len buf =
      let opcode =  Opcode.of_uint32 Hdr.(getf hdr T.opcode) in
      {chan; hdr; pkt=Opcode.(match opcode with
         | `FUSE_INIT        -> Init       (!@ (from_voidp Init.T.t buf))
         | `FUSE_GETATTR     -> Getattr
         | `FUSE_LOOKUP      -> Lookup     (coerce (ptr void) string buf)
         | `FUSE_OPENDIR     -> Opendir    (!@ (from_voidp Open.T.t buf))
         | `FUSE_READDIR     -> Readdir    (!@ (from_voidp Read.T.t buf))
         | `FUSE_RELEASEDIR  -> Releasedir (!@ (from_voidp Release.T.t buf))
         | `FUSE_FSYNCDIR    -> Fsyncdir   (!@ (from_voidp Fsync.T.t buf))
         | `FUSE_RMDIR       -> Rmdir      (coerce (ptr void) string buf)
         | `FUSE_MKDIR       ->
           let name = Mkdir.name buf in
           let s = !@ (from_voidp Mkdir.T.t buf) in
           Mkdir (s, name)
         | `FUSE_GETXATTR    -> Getxattr   (!@ (from_voidp Getxattr.T.t buf))
         | `FUSE_SETXATTR    -> Setxattr   (!@ (from_voidp Setxattr.T.t buf))
         | `FUSE_LISTXATTR   -> Listxattr  (!@ (from_voidp Getxattr.T.t buf))
         | `FUSE_REMOVEXATTR -> Removexattr (coerce (ptr void) string buf)
         | `FUSE_ACCESS      -> Access     (!@ (from_voidp Access.T.t buf))
         | `FUSE_FORGET      -> Forget     (!@ (from_voidp Forget.T.t buf))
         | `FUSE_READLINK    -> Readlink
         | `FUSE_OPEN        -> Open       (!@ (from_voidp Open.T.t buf))
         | `FUSE_READ        -> Read       (!@ (from_voidp Read.T.t buf))
         | `FUSE_WRITE       ->
           let data = Write.data buf in
           Write (!@ (from_voidp Write.T.t buf), data)
         | `FUSE_STATFS      -> Statfs
         | `FUSE_FLUSH       -> Flush      (!@ (from_voidp Flush.T.t buf))
         | `FUSE_RELEASE     -> Release    (!@ (from_voidp Release.T.t buf))
         | `FUSE_FSYNC       -> Fsync      (!@ (from_voidp Fsync.T.t buf))
         | `FUSE_UNLINK      -> Unlink     (coerce (ptr void) string buf)
         | `FUSE_CREATE      ->
           let name = Create.name buf in
           let s = !@ (from_voidp Create.T.t buf) in
           Create (s, name)
         | `FUSE_MKNOD       ->
           let name = Mknod.name buf in
           let s = !@ (from_voidp Mknod.T.t buf) in
           Mknod (s, name)
         | `FUSE_SETATTR     -> Setattr    (!@ (from_voidp Setattr.T.t buf))
         | `FUSE_LINK        ->
           let name = Link.name buf in
           let s = !@ (from_voidp Link.T.t buf) in
           Link (s, name)
         | `FUSE_SYMLINK     ->
           let name = coerce (ptr void) string buf in
           let buf =
             to_voidp ((from_voidp char buf) +@ (String.length name + 1))
           in
           let target = coerce (ptr void) string buf in
           Symlink (name,target)
         | `FUSE_RENAME      ->
           let (src, dest) = Rename.source_destination buf in
           let s = !@ (from_voidp Rename.T.t buf) in
           Rename (s,src,dest)
         | `FUSE_GETLK       -> Getlk      (!@ (from_voidp Lk.T.t buf))
         | `FUSE_SETLK       -> Setlk      (!@ (from_voidp Lk.T.t buf))
         | `FUSE_SETLKW      -> Setlkw     (!@ (from_voidp Lk.T.t buf))
         | `FUSE_INTERRUPT   -> Interrupt  (!@ (from_voidp Interrupt.T.t buf))
         | `FUSE_BMAP        -> Bmap       (!@ (from_voidp Bmap.T.t buf))
         | `FUSE_BATCH_FORGET ->
           let batch_forget = !@ (from_voidp Batch_forget.T.t buf) in
           let forgets =
             match UInt32.to_int (getf batch_forget Batch_forget.T.count) with
             | 0 -> []
             | k ->
               let head = coerce (ptr void) (ptr Batch_forget.T.t) buf in
               let p = to_voidp (head +@ 1) in
               let first = coerce (ptr void) (ptr Struct.Forget_one.T.t) p in
               CArray.(to_list (from_ptr first k))
           in
           Batch_forget forgets
         | `FUSE_DESTROY     -> Destroy

         | `CUSE_INIT         -> Other opcode
         | `FUSE_NOTIFY_REPLY -> Other opcode
         | `FUSE_RENAME2      -> Other opcode
         | `FUSE_READDIRPLUS  -> Other opcode
         | `FUSE_FALLOCATE    -> Other opcode
         | `FUSE_IOCTL        -> Other opcode
         | `FUSE_POLL         -> Other opcode

         | `Unknown i        -> unknown i
       )}

    let string_of_mode req mode =
      let sys_stat = req.chan.host.Host.sys_stat in
      let host = sys_stat.Sys_stat.Host.mode in
      let open Sys_stat.Mode in
      Printf.sprintf "%s (%x)"
        (to_string ~host (of_code_exn ~host mode))
        mode

    let string_of_perms req perms =
      let sys_stat = req.chan.host.Host.sys_stat in
      let host = sys_stat.Sys_stat.Host.file_perm in
      let open Sys_stat.File_perm in
      to_string ~host (full_of_code ~host perms)

    let describe req =
      let module Hdr = Hdr.T in
      Printf.sprintf "%Ld (%Ld) %s.p%ld.u%ld.g%ld %s"
        (UInt64.to_int64 (getf req.hdr Hdr.unique))
        (UInt64.to_int64 (getf req.hdr Hdr.nodeid))
        (Opcode.to_string (Opcode.of_uint32 (getf req.hdr Hdr.opcode)))
        (UInt32.to_int32 (getf req.hdr Hdr.pid))
        (UInt32.to_int32 (getf req.hdr Hdr.uid))
        (UInt32.to_int32 (getf req.hdr Hdr.gid))
        (match req.pkt with
         | Init i ->
           Printf.sprintf "version=%d.%d max_readahead=%d flags=0x%lX"
             (UInt32.to_int (getf i Init.T.major))
             (UInt32.to_int (getf i Init.T.minor))
             (UInt32.to_int (getf i Init.T.max_readahead))
             (Unsigned.UInt32.to_int32 (getf i Init.T.flags))
         | Getattr | Readlink | Statfs | Destroy -> ""
         | Symlink (name,target) -> name ^ " -> " ^ target
         | Forget f ->
           Int64.to_string (UInt64.to_int64 (getf f Forget.T.nlookup))
         | Lookup name -> name
         | Mkdir (m,name) ->
           Printf.sprintf "mode=%s umask=0o%o %s"
             (string_of_perms req (UInt32.to_int (getf m Mkdir.T.mode)))
             (UInt32.to_int (getf m Mkdir.T.umask))
             name
         | Mknod (m,name) ->
           Printf.sprintf "mode=%s rdev=%ld umask=0o%o %s"
             (string_of_mode req (UInt32.to_int (getf m Mknod.T.mode)))
             (Unsigned.UInt32.to_int32 (getf m Mknod.T.rdev))
             (UInt32.to_int (getf m Mknod.T.umask))
             name
         | Create (c,name) ->
           let host = req.chan.host.Host.fcntl.Fcntl.Host.oflags in
           let flags_code = UInt32.to_int (getf c Create.T.flags) in
           let flags = Fcntl.Oflags.of_code ~host flags_code in
           let flags_s =
             String.concat " " (List.map Fcntl.Oflags.to_string flags)
           in
           Printf.sprintf "flags=[%s] mode=%s %s"
             flags_s
             (string_of_mode req (UInt32.to_int (getf c Create.T.mode)))
             name
         | Opendir o
         | Open o ->
           let host = req.chan.host.Host.fcntl.Fcntl.Host.oflags in
           let flags_code = UInt32.to_int (getf o Open.T.flags) in
           let flags = Fcntl.Oflags.of_code ~host flags_code in
           let flags_s =
             String.concat " " (List.map Fcntl.Oflags.to_string flags)
           in
           Printf.sprintf "flags=[%s]" flags_s
         | Setattr s ->
           let host = req.chan.host.Host.sys_stat.Sys_stat.Host.file_perm in
           Printf.sprintf "0x%lX[%s]"
             (UInt32.to_int32 (getf s Setattr.T.valid))
             (String.concat " " (Setattr.to_string_list ~host s))
         | Access a ->
           let code = getf a Access.T.mask in
           (*let phost = Fuse.(req.chan.host.unistd.Unix_unistd.access) in
                let perms = Unix_unistd.Access.(of_code ~host:phost code) in
                (List.fold_left Unix.(fun s -> function
                | R_OK -> s^"R" | W_OK -> s^"W" | X_OK -> s^"X" | F_OK -> s^"F"
                ) "" perms)
           *)
           (* TODO: fix symbolic host map *)
           let perms = string_of_int (UInt32.to_int code) in
           let uid = getf req.hdr Hdr.uid in
           let gid = getf req.hdr Hdr.gid in
           Printf.sprintf "uid:%ld gid:%ld (%s)"
             (UInt32.to_int32 uid)
             (UInt32.to_int32 gid)
             perms
         | Unlink name | Rmdir name -> name
         | Rename (r,src,dest) ->
           let newdir = UInt64.to_string (getf r Rename.T.newdir) in
           Printf.sprintf "%s -> %s %s" src newdir dest
         | Read r ->
           let fh = getf r Read.T.fh in
           let offset = getf r Read.T.offset in
           let size = getf r Read.T.size in
           Printf.sprintf "fh=%Ld offset=%Ld size=%ld"
             (UInt64.to_int64 fh)
             (UInt64.to_int64 offset)
             (UInt32.to_int32 size)
         | Write (w,_) ->
           let fh = getf w Write.T.fh in
           let offset = getf w Write.T.offset in
           let size = getf w Write.T.size in
           let flags = getf w Write.T.write_flags in
           Printf.sprintf "fh=%Ld offset=%Ld size=%ld flags=0x%lx"
             (UInt64.to_int64 fh)
             (UInt64.to_int64 offset)
             (UInt32.to_int32 size)
             (UInt32.to_int32 flags)
         | Interrupt i ->
           let unique = UInt64.to_int64 (getf i Interrupt.T.unique) in
           Printf.sprintf "request %Ld" unique
         | Readdir r ->
           let fh = getf r Read.T.fh in
           let offset = getf r Read.T.offset in
           let size = getf r Read.T.size in
           Printf.sprintf "fh=%Ld offset=%Ld size=%ld"
             (UInt64.to_int64 fh)
             (UInt64.to_int64 offset)
             (UInt32.to_int32 size)
         | Release r
         | Releasedir r ->
           let fh = getf r Release.T.fh in
           let flags = getf r Release.T.flags in
           let rflags = getf r Release.T.release_flags in
           let lock_owner = getf r Release.T.lock_owner in
           Printf.sprintf "fh=%s flags=%s release_flags=%s lock_owner=%s"
             (UInt64.to_string fh)
             (UInt32.to_string flags)
             (UInt32.to_string rflags)
             (UInt64.to_string lock_owner)
         | Getxattr _
         | Setxattr _
         | Listxattr _
         | Removexattr _
         | Getlk _
         | Setlk _
         | Setlkw _
         | Link (_,_)
         | Flush _
         | Fsyncdir _
         | Fsync _
         | Bmap _ -> "FIX ME"
         | Batch_forget b ->
           let forgets = String.concat ", " (List.map (fun forget ->
             let id = getf forget Struct.Forget_one.T.nodeid in
             let n  = getf forget Struct.Forget_one.T.nlookup in
             Printf.sprintf "%Ld: %Ld"
               (UInt64.to_int64 id)
               (UInt64.to_int64 n)
           ) b) in
           Printf.sprintf "[%s]" forgets
         | Other opcode -> "OTHER "^(Opcode.to_string opcode)
         | Unknown i -> "UNKNOWN "^(Int32.to_string i)
        )

  end
end

module Out = struct
  module T = Types.Out

  module Hdr = struct
    module T = T.Hdr

    module Notify_code =
    struct
      module T = T.Notify_code

      type t = T.t

      let to_string = function
        | `FUSE_NOTIFY_DELETE -> "FUSE_NOTIFY_DELETE"
        | `FUSE_NOTIFY_INVAL_ENTRY -> "FUSE_NOTIFY_INVAL_ENTRY"
        | `FUSE_NOTIFY_INVAL_INODE -> "FUSE_NOTIFY_INVAL_INODE"
        | `FUSE_NOTIFY_POLL -> "FUSE_NOTIFY_POLL"
        | `FUSE_NOTIFY_RETRIEVE -> "FUSE_NOTIFY_RETRIEVE"
        | `FUSE_NOTIFY_STORE -> "FUSE_NOTIFY_STORE"

      let of_int32 =
        let l = List.map (fun (k, v) -> (v, k)) T.enum_values in
        fun i -> ListLabels.assoc i l
            
      let to_int32 v = ListLabels.assoc v T.enum_values
    end

    let hdrsz = sizeof T.t
    let sz = hdrsz

    let packet ?(nerrno=0l) ~count req =
      let bodysz = count in
      let count = hdrsz + bodysz in
      let pkt = allocate_n char ~count in
      let hdr = !@ (coerce (ptr char) (ptr T.t) pkt) in
      setf hdr T.len    (UInt32.of_int count);
      setf hdr T.error  nerrno;
      setf hdr T.unique (getf req.hdr In.Hdr.T.unique);
      CArray.from_ptr (pkt +@ hdrsz) bodysz

    let make req st =
      let count = sizeof st in
      let pkt = packet ~count req in
      !@ (coerce (ptr char) (ptr st) (CArray.start pkt))

    let set_size pkt sz =
      let pktsz = hdrsz + sz in
      let hdr =
        !@ (coerce (ptr char) (ptr T.t) ((CArray.start pkt) -@ hdrsz))
      in
      setf hdr T.len (UInt32.of_int pktsz);
      CArray.from_ptr (CArray.start pkt) sz
  end

  module Notify = struct
    let packet ~code ~count =
      let bodysz = count in
      let count = Hdr.sz + bodysz in
      let pkt = allocate_n char ~count in
      let hdr = !@ (coerce (ptr char) (ptr Hdr.T.t) pkt) in
      setf hdr Hdr.T.len    (UInt32.of_int count);
      setf hdr Hdr.T.error  code;
      setf hdr Hdr.T.unique UInt64.zero;
      CArray.from_ptr (pkt +@ Hdr.sz) bodysz

    module Inval_entry = struct
      module T = T.Notify_inval_entry

      let hdrsz = sizeof T.t
      let struct_size = hdrsz
      let size name = hdrsz + (String.length name) + 1

      let create parent filename =
        let code = Hdr.T.Notify_code.fuse_notify_inval_entry in
        let pkt = packet ~code ~count:(size filename) in
        let p = CArray.start pkt in
        let s = !@ (coerce (ptr char) (ptr T.t) p) in
        setf s T.parent  parent;
        setf s T.namelen (UInt32.of_int (String.length filename));
        let sp = ref (p +@ hdrsz) in
        (* TODO: better copy *)
        String.iter (fun c -> !sp <-@ c; sp := !sp +@ 1) filename;
        pkt

      let name p =
        (* TODO: check namelen valid? *)
        coerce (ptr char) string ((from_voidp char p) +@ struct_size)

      let describe pkt =
        Printf.sprintf "under %Ld" (UInt64.to_int64 (getf pkt T.parent))
    end

    module Delete = struct
      module T = T.Notify_delete

      let hdrsz = sizeof T.t
      let struct_size = hdrsz
      let size name = hdrsz + (String.length name) + 1

      let create parent child filename =
        let code = Hdr.T.Notify_code.fuse_notify_delete in
        let pkt = packet ~code ~count:(size filename) in
        let p = CArray.start pkt in
        let s = !@ (coerce (ptr char) (ptr T.t) p) in
        setf s T.parent  parent;
        setf s T.child   child;
        setf s T.namelen (UInt32.of_int (String.length filename));
        let sp = ref (p +@ hdrsz) in
        (* TODO: better copy *)
        String.iter (fun c -> !sp <-@ c; sp := !sp +@ 1) filename;
        pkt

      let name p =
        (* TODO: check namelen valid? *)
        coerce (ptr char) string ((from_voidp char p) +@ struct_size)

      let describe pkt =
        Printf.sprintf "as %Ld / %Ld"
          (UInt64.to_int64 (getf pkt T.parent))
          (UInt64.to_int64 (getf pkt T.child))
    end

    type t =
      | Delete of string * Delete.T.t structure
      | Inval_entry of string * Inval_entry.T.t structure
      | Inval_inode (* TODO: do *)
      | Poll (* TODO: do *)
      | Retrieve (* TODO: do *)
      | Store (* TODO: do *)

    let parse chan hdr len p =
      let unique = UInt64.to_int64 (getf hdr Hdr.T.unique) in
      assert (unique = 0_L); (* TODO: really?? *)
      let code = Hdr.Notify_code.of_int32 (getf hdr Hdr.T.error) in
      { chan; hdr; pkt=(match code with
          | `FUSE_NOTIFY_DELETE ->
            let name = Delete.name p in
            let s = !@ (from_voidp Delete.T.t p) in
            Delete (name, s)
          | `FUSE_NOTIFY_INVAL_INODE -> Inval_inode
          | `FUSE_NOTIFY_POLL -> Poll
          | `FUSE_NOTIFY_RETRIEVE -> Retrieve
          | `FUSE_NOTIFY_STORE -> Store
          | `FUSE_NOTIFY_INVAL_ENTRY ->
            let name = Inval_entry.name p in
            let s = !@ (from_voidp Inval_entry.T.t p) in
            Inval_entry (name, s)
        )}

    let describe ({ chan; pkt }) =
      match pkt with
      | Delete (name, d) ->
        Printf.sprintf "DELETE %s %s" name (Delete.describe d)
      | Inval_entry (name, i) ->
        Printf.sprintf "INVAL_ENTRY %s %s" name (Inval_entry.describe i)
      | Inval_inode -> "INVAL_INODE FIXME" (* TODO: more *)
      | Poll -> "POLL FIXME" (* TODO: more *)
      | Retrieve -> "RETRIEVE FIXME" (* TODO: more *)
      | Store -> "STORE FIXME" (* TODO: more *)
  end

  module Dirent = struct
    module T = Struct.T.Dirent

    let hdrsz = sizeof T.t
    let struct_size = hdrsz
    let size name = hdrsz + 8 * (((String.length name) + 7) / 8)

    let of_list ~host listing offset read_size req =
      let phost = host.Host.dirent.Dirent.Host.file_kind in
      let emit = ref false in
      let count = ref 0 in
      let listing = List.fold_left (fun acc ((off,_,name,_) as ent) ->
        if offset <> 0 && not !emit then (* TODO: fixme this is gross *)
          if off = offset
          then (emit := true; acc)
          else acc
        else
          let next_total = !count + (size name) in
          if next_total > read_size
          then acc
          else begin
            count := next_total;
            ent::acc
          end
      ) [] listing in
      let count = !count in
      let pkt = Hdr.packet ~count req in
      let buf = CArray.start pkt in
      let ep = List.fold_left (fun p (off,ino,name,typ) ->
        let sz = size name in
        let dirent = !@ (coerce (ptr char) (ptr T.t) p) in
        let typ = Dirent.File_kind.(to_code ~host:phost typ) in
        setf dirent T.ino     (UInt64.of_int64 ino);
        setf dirent T.off     (UInt64.of_int off);
        setf dirent T.namelen (UInt32.of_int (String.length name));
        setf dirent T.typ     (UInt32.of_int (int_of_char typ));
        let sp = ref (p +@ hdrsz) in
        (* TODO: better copy *)
        String.iter (fun c -> !sp <-@ c; sp := !sp +@ 1) name;
        (* Printf.eprintf "dirent serialized %s\n%!" name; *)
        p +@ sz
      ) buf (List.rev listing) in
      assert (ptr_diff buf ep = count);
      pkt

    let to_string ~host dirent =
      let phost = host.Host.dirent.Dirent.Host.file_kind in
      let ino = UInt64.to_string (getf dirent T.ino) in
      let off = UInt64.to_string (getf dirent T.off) in
      let typ_i = char_of_int (UInt32.to_int (getf dirent T.typ)) in
      let typ = Dirent.File_kind.(to_string (of_code_exn ~host:phost typ_i)) in
      let length = UInt32.to_int (getf dirent T.namelen) in
      let name = string_from_ptr (CArray.start (getf dirent T.name)) ~length in
      Printf.sprintf "{ ino=%s; off=%s; typ=%s; name=\"%s\" }" ino off typ name

    let describe ~host p =
      let size = CArray.length p in
      let rec collect_dirents dirents p off =
        let dirent = !@ (coerce (ptr char) (ptr T.t) p) in
        let namelen = UInt32.to_int (getf dirent T.namelen) in
        if off = size
        then List.rev dirents
        else
          let length = hdrsz + 8 * ((namelen + 7) / 8) in
          collect_dirents (dirent::dirents) (p +@ length) (off + length)
      in
      let dirents = collect_dirents [] (CArray.start p) 0 in
      Printf.sprintf "%d:[ %s ]"
        size (String.concat "; " (List.map (to_string ~host) dirents))
  end

  module Readlink = struct
    let create ~target req =
      let count = String.length target in
      let pkt = Hdr.packet ~count req in
      let sp = ref (CArray.start pkt) in
      (* TODO: FIXME should not iterate! *)
      String.iter (fun c -> !sp <-@ c; sp := !sp +@ 1) target;
      pkt
  end

  module Read = struct
    let allocate ~size req =
      Hdr.packet ~count:size req

    let finalize ~size pkt _req =
      Hdr.set_size pkt size

    let describe carray = string_of_int (CArray.length carray)
  end

  module Write = struct
    module T = T.Write

    let create ~size req =
      let pkt = Hdr.make req T.t in
      setf pkt T.size    size;
      CArray.from_ptr (coerce (ptr T.t) (ptr char) (addr pkt)) (sizeof T.t)

    let describe pkt =
      let size = getf pkt T.size in
      Printf.sprintf "size=%ld" (UInt32.to_int32 size)
  end

  module Statfs = struct
    module T = T.Statfs

    let create
        ~blocks ~bfree ~bavail ~files ~ffree ~bsize ~namelen ~frsize req =
      let pkt = Hdr.make req T.t in
      let st = getf pkt T.st in
      Struct.Kstatfs.store
        ~blocks ~bfree ~bavail ~files ~ffree ~bsize ~namelen ~frsize st;
      CArray.from_ptr (coerce (ptr T.t) (ptr char) (addr pkt)) (sizeof T.t)

    let describe pkt =
      let kstatfs = getf pkt T.st in
      Struct.Kstatfs.describe kstatfs
  end

  module Open = struct
    module T = T.Open

    module Flags = struct
      module T = T.Flags
      type t = {
        direct_io   : bool;
        keep_cache  : bool;
        nonseekable : bool;
      }

      let zero = { direct_io = false; keep_cache = false; nonseekable = false }

      let to_string { direct_io; keep_cache; nonseekable } =
        Printf.sprintf "{ direct_io = %b; keep_cache = %b; nonseekable = %b }"
          direct_io keep_cache nonseekable

      let to_uint32 { direct_io; keep_cache; nonseekable } =
        let open Unsigned.UInt32 in
        let open Infix in
        (if direct_io  then T.fopen_direct_io else zero) lor
        (if keep_cache then T.fopen_keep_cache else zero) lor
        (if nonseekable then T.fopen_nonseekable else zero)

      let of_uint32 i =
        let open Unsigned in
        let open UInt32.Infix in
        {
          direct_io   = UInt32.(compare zero (i land T.fopen_direct_io)) = 0;
          keep_cache  = UInt32.(compare zero (i land T.fopen_keep_cache)) = 0;
          nonseekable = UInt32.(compare zero (i land T.fopen_nonseekable)) = 0;
        }
    end

    let store ~fh ~open_flags mem req =
      setf mem T.fh         fh;
      setf mem T.open_flags (Flags.to_uint32 open_flags);
      ()

    let create ~fh ~open_flags req =
      let pkt = Hdr.make req T.t in
      store ~fh ~open_flags pkt req;
      CArray.from_ptr (coerce (ptr T.t) (ptr char) (addr pkt)) (sizeof T.t)

    let describe pkt =
      Printf.sprintf
        "fh=%Ld flags=0x%x"
        (UInt64.to_int64 (getf pkt T.fh))
        (UInt32.to_int (getf pkt T.open_flags))
  end

  module Init = struct
    module T = T.Init

    let create ~major ~minor ~max_readahead ~flags ~max_write req =
      let pkt = Hdr.make req T.t in
      setf pkt T.major         major;
      setf pkt T.minor         minor;
      setf pkt T.max_readahead max_readahead;
      setf pkt T.flags         flags;
      setf pkt T.max_write     max_write;
      CArray.from_ptr (coerce (ptr T.t) (ptr char) (addr pkt)) (sizeof T.t)

    let describe pkt =
      Printf.sprintf
        "version=%d.%d max_readahead=%d flags=0x%lX max_write=%d"
        (UInt32.to_int (getf pkt T.major))
        (UInt32.to_int (getf pkt T.minor))
        (UInt32.to_int (getf pkt T.max_readahead))
        (UInt32.to_int32 (getf pkt T.flags))
        (UInt32.to_int (getf pkt T.max_write))
  end

  module Entry = struct
    module T = T.Entry

    let sz = sizeof T.t

    let store ~nodeid ~generation ~entry_valid ~attr_valid
        ~entry_valid_nsec ~attr_valid_nsec ~store_attr mem req =
      setf mem T.nodeid nodeid;
      setf mem T.generation generation;
      setf mem T.entry_valid entry_valid;
      setf mem T.attr_valid attr_valid;
      setf mem T.entry_valid_nsec entry_valid_nsec;
      setf mem T.attr_valid_nsec attr_valid_nsec;
      store_attr (getf mem T.attr);
      ()

    let create ~nodeid ~generation ~entry_valid ~attr_valid
        ~entry_valid_nsec ~attr_valid_nsec ~store_attr req =
      let pkt = Hdr.make req T.t in
      store ~nodeid ~generation ~entry_valid ~attr_valid
        ~entry_valid_nsec ~attr_valid_nsec ~store_attr pkt req;
      CArray.from_ptr (coerce (ptr T.t) (ptr char) (addr pkt)) (sizeof T.t)

    let describe ~host pkt =
      (* TODO: times? *)
      Printf.sprintf
        "nodeid=%Ld.%Ld valid=%Ld.%09lds attr={%s}"
        (UInt64.to_int64 (getf pkt T.generation))
        (UInt64.to_int64 (getf pkt T.nodeid))
        (UInt64.to_int64 (getf pkt T.entry_valid))
        (UInt32.to_int32 (getf pkt T.entry_valid_nsec))
        (Struct.Attr.describe ~host (getf pkt T.attr))
  end

  module Attr = struct
    module T = T.Attr

    let create ~attr_valid ~attr_valid_nsec ~store_attr req =
      let pkt = Hdr.make req T.t in
      setf pkt T.attr_valid      attr_valid;
      setf pkt T.attr_valid_nsec attr_valid_nsec;
      store_attr (getf pkt T.attr);
      CArray.from_ptr (coerce (ptr T.t) (ptr char) (addr pkt)) (sizeof T.t)

    let describe ~host pkt =
      Printf.sprintf
        "attr_valid=%Ld.%09lds attr={%s}"
        (Unsigned.UInt64.to_int64 (getf pkt T.attr_valid))
        (Unsigned.UInt32.to_int32 (getf pkt T.attr_valid_nsec))
        (Struct.Attr.describe ~host (getf pkt T.attr))
  end

  module Create = struct
    module T = struct
      (* No fuse_create_out structure exists so we synthesize it. *)
      type t
      let t : t structure typ = structure "fuse_create_out"
      let ( -:* ) s x = field t s x
      let entry = "entry" -:* Entry.T.t
      let open_ = "open"  -:* Open.T.t
      let () = seal t
    end

    let create ~store_entry ~store_open req =
      let pkt = Hdr.make req T.t in
      store_entry (getf pkt T.entry) req;
      store_open  (getf pkt T.open_) req;
      CArray.from_ptr (coerce (ptr T.t) (ptr char) (addr pkt)) (sizeof T.t)
  end

  module Message = struct
    type t =
      | Init    of Init.T.t  structure
      | Getattr of Attr.T.t  structure
      | Lookup  of Entry.T.t structure
      | Opendir of Open.T.t  structure
      | Readdir of char CArray.t
      | Releasedir
      | Fsyncdir (* TODO: do *)
      | Rmdir
      | Mkdir   of Entry.T.t structure
      | Getxattr (* TODO: do *)
      | Setxattr (* TODO: do *)
      | Listxattr (* TODO: do *)
      | Removexattr (* TODO: do *)
      | Access
      | Forget (* TODO: should never happen? *)
      | Readlink of string
      | Open     of Open.T.t           structure
      | Read     of char CArray.t
      | Write    of Write.T.t          structure
      | Statfs   of Struct.Kstatfs.T.t structure
      | Flush
      | Release
      | Fsync
      | Unlink
      | Create   of Entry.T.t structure * Open.T.t structure
      | Mknod    of Entry.T.t structure
      | Setattr  of Attr.T.t  structure
      | Link     of Entry.T.t structure
      | Symlink  of Entry.T.t structure
      | Rename
      | Getlk (* TODO: do *)
      | Setlk (* TODO: do *)
      | Setlkw (* TODO: do *)
      | Interrupt (* TODO: do *)
      | Bmap (* TODO: do *)
      | Destroy
      | Other    of In.Opcode.t
      | Unknown  of int32 * int * unit ptr

    let unknown opcode len buf = Unknown (opcode, len, buf)

    let parse ({ chan } as req) hdr len buf =
      let opcode = In.Opcode.of_uint32 In.Hdr.(getf req.hdr T.opcode) in
      {chan; hdr; pkt=In.Opcode.(match opcode with
         | `FUSE_INIT        -> Init       (!@ (from_voidp Init.T.t buf))
         | `FUSE_GETATTR     -> Getattr    (!@ (from_voidp Attr.T.t buf))
         | `FUSE_LOOKUP      -> Lookup     (!@ (from_voidp Entry.T.t buf))
         | `FUSE_OPENDIR     -> Opendir    (!@ (from_voidp Open.T.t buf))
         | `FUSE_READDIR     ->
           Readdir (CArray.from_ptr (from_voidp char buf) len)
         | `FUSE_RELEASEDIR  -> Releasedir
         | `FUSE_FSYNCDIR    -> Fsyncdir
         | `FUSE_RMDIR       -> Rmdir
         | `FUSE_MKDIR       -> Mkdir      (!@ (from_voidp Entry.T.t buf))
         | `FUSE_GETXATTR    -> Getxattr
         | `FUSE_SETXATTR    -> Setxattr
         | `FUSE_LISTXATTR   -> Listxattr
         | `FUSE_REMOVEXATTR -> Removexattr
         | `FUSE_ACCESS      -> Access
         | `FUSE_FORGET      -> Forget
         | `FUSE_READLINK    -> Readlink   (coerce (ptr void) string buf)
         | `FUSE_OPEN        -> Open       (!@ (from_voidp Open.T.t buf))
         | `FUSE_READ        ->
           Read (CArray.from_ptr (from_voidp char buf) len)
         | `FUSE_WRITE       -> Write      (!@ (from_voidp Write.T.t buf))
         | `FUSE_STATFS      ->
           let statfs_pkt = !@ (from_voidp Statfs.T.t buf) in
           Statfs (getf statfs_pkt Statfs.T.st)
         | `FUSE_FLUSH       -> Flush
         | `FUSE_RELEASE     -> Release
         | `FUSE_FSYNC       -> Fsync
         | `FUSE_UNLINK      -> Unlink
         | `FUSE_CREATE      ->
           let entry = !@ (from_voidp Entry.T.t buf) in
           let ptr = (coerce (ptr void) (ptr char) buf) +@ Entry.sz in
           let open_ = !@ (from_voidp Open.T.t (to_voidp ptr)) in
           Create (entry, open_)
         | `FUSE_MKNOD       -> Mknod      (!@ (from_voidp Entry.T.t buf))
         | `FUSE_SETATTR     -> Setattr    (!@ (from_voidp Attr.T.t buf))
         | `FUSE_LINK        -> Link       (!@ (from_voidp Entry.T.t buf))
         | `FUSE_SYMLINK     -> Symlink    (!@ (from_voidp Entry.T.t buf))
         | `FUSE_RENAME      -> Rename
         | `FUSE_GETLK       -> Getlk
         | `FUSE_SETLK       -> Setlk
         | `FUSE_SETLKW      -> Setlkw
         | `FUSE_INTERRUPT   -> Interrupt
         | `FUSE_BMAP        -> Bmap
         | `FUSE_DESTROY     -> Destroy

         | `CUSE_INIT         -> Other opcode
         | `FUSE_NOTIFY_REPLY -> Other opcode
         | `FUSE_BATCH_FORGET -> Other opcode
         | `FUSE_RENAME2      -> Other opcode
         | `FUSE_READDIRPLUS  -> Other opcode
         | `FUSE_FALLOCATE    -> Other opcode
         | `FUSE_IOCTL        -> Other opcode
         | `FUSE_POLL         -> Other opcode

         | `Unknown opcode   -> unknown opcode len buf
       )}

    let pnum = ref 0
    let deserialize req len buf =
      (*let ca = CArray.from_ptr buf len in
      let str = Bytes.create len in
      for i = 0 to len - 1 do Bytes.set str i (CArray.get ca i) done;
      let fd = Unix.(
        openfile ("deserialized_packet_"^(string_of_int !pnum))
          [O_WRONLY;O_CREAT] 0o600
      ) in
      let logn = Unix.write fd str 0 len in
      assert (logn = len);
      let () = Unix.close fd in
      incr pnum;
      *)

      let hdr_ptr = coerce (ptr char) (ptr Hdr.T.t) buf in
      let hdr = !@ hdr_ptr in
      let sz = UInt32.to_int (getf hdr Hdr.T.len) in
      if len <> sz
      then raise (
        ProtocolError
          (req.chan,
           (Printf.sprintf "Packet has %d bytes but only provided %d"
              sz len))
      );
      parse req hdr (sz - Hdr.sz) (to_voidp (buf +@ Hdr.sz))

    let describe ({ chan; pkt }) =
      let host = chan.host in
      match pkt with
      | Init i -> Init.describe i
      | Getattr a -> Attr.describe ~host a
      | Lookup e -> Entry.describe ~host e
      | Opendir o -> Open.describe o
      | Readdir r -> Dirent.describe ~host r
      | Releasedir -> "RELEASEDIR"
      | Fsyncdir -> "FSYNCDIR"
      | Rmdir -> "RMDIR"
      | Mkdir e -> Entry.describe ~host e
      | Getxattr -> "GETXATTR"
      | Setxattr -> "SETXATTR"
      | Listxattr -> "LISTXATTR"
      | Removexattr -> "REMOVEXATTR"
      | Access -> "ACCESS"
      | Forget -> "FORGET"
      | Readlink r -> r
      | Open o -> Open.describe o
      | Read r -> Read.describe r
      | Write w -> Write.describe w
      | Statfs s -> Struct.Kstatfs.describe s
      | Flush -> "FLUSH"
      | Release -> "RELEASE"
      | Fsync -> "FSYNC"
      | Unlink -> "UNLINK"
      | Create (entry,open_) ->
        Printf.sprintf "[%s][%s]"
          (Entry.describe ~host entry) (Open.describe open_)
      | Mknod e -> Entry.describe ~host e
      | Setattr a -> Attr.describe ~host a
      | Link e -> Entry.describe ~host e
      | Symlink e -> Entry.describe ~host e
      | Rename -> "RENAME"
      | Getlk -> "GETLK"
      | Setlk -> "SETLK"
      | Setlkw -> "SETLKW"
      | Interrupt -> "INTERRUPT"
      | Bmap -> "BMAP"
      | Destroy -> "DESTROY"
      | Other opcode -> "OTHER ("^(In.Opcode.to_string opcode)^")"
      | Unknown (o,l,b) -> "UNKNOWN FIXME" (* TODO: more *)

  end
end

type 'a request = (In.Hdr.T.t, 'a) packet
