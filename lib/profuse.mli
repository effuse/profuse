
module Flags : sig
  type t = int32

  val empty : t
end

module Host : sig
  type t = {
    fcntl : Fcntl.Host.t;
    errno : Errno.Host.t;
    sys_stat : Sys_stat.Host.t;
    dirent : Dirent.Host.t;
    unistd : Unix_unistd.host;
  }

  val linux_4_0_5 : t
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
  hdr : 'hdr Ctypes.structure;
  pkt : 'body;
}

module Types : sig
  type 'a structure = 'a Ctypes_static.structure

  module Struct : sig
    module Kstatfs : sig
      type t
      val t : t structure Ctypes.typ

      val blocks : (Unsigned.uint64, t structure) Ctypes.field
      val bfree : (Unsigned.uint64, t structure) Ctypes.field
      val bavail : (Unsigned.uint64, t structure) Ctypes.field
      val files : (Unsigned.uint64, t structure) Ctypes.field
      val ffree : (Unsigned.uint64, t structure) Ctypes.field
      val bsize : (Unsigned.uint32, t structure) Ctypes.field
      val namelen :
        (Unsigned.uint32, t structure) Ctypes.field
      val frsize : (Unsigned.uint32, t structure) Ctypes.field
    end

    module File_lock : sig
      type t
      val t : t structure Ctypes.typ

      val start : (Unsigned.uint64, t structure) Ctypes.field
      val end_ : (Unsigned.uint64, t structure) Ctypes.field
      val type_ : (Unsigned.uint32, t structure) Ctypes.field
      val pid : (Unsigned.uint32, t structure) Ctypes.field
    end

    module Attr : sig
      type t
      val t : t structure Ctypes.typ

      val ino : (Unsigned.uint64, t structure) Ctypes.field
      val size : (Unsigned.uint64, t structure) Ctypes.field
      val blocks : (Unsigned.uint64, t structure) Ctypes.field
      val atime : (Unsigned.uint64, t structure) Ctypes.field
      val mtime : (Unsigned.uint64, t structure) Ctypes.field
      val ctime : (Unsigned.uint64, t structure) Ctypes.field
      val atimensec :
        (Unsigned.uint32, t structure) Ctypes.field
      val mtimensec :
        (Unsigned.uint32, t structure) Ctypes.field
      val ctimensec :
        (Unsigned.uint32, t structure) Ctypes.field
      val mode : (Unsigned.uint32, t structure) Ctypes.field
      val nlink : (Unsigned.uint32, t structure) Ctypes.field
      val uid : (Unsigned.uint32, t structure) Ctypes.field
      val gid : (Unsigned.uint32, t structure) Ctypes.field
      val rdev : (Unsigned.uint32, t structure) Ctypes.field
    end

    module Dirent : sig
      type t
      val t : t structure Ctypes.typ

      val ino : (Unsigned.uint64, t structure) Ctypes.field
      val off : (Unsigned.uint64, t structure) Ctypes.field
      val namelen :
        (Unsigned.uint32, t structure) Ctypes.field
      val typ : (Unsigned.uint32, t structure) Ctypes.field
      val name :
        (char Ctypes_static.carray, t structure) Ctypes.field
    end
  end

  module Out : sig

    module Hdr : sig
      type t
      val t : t structure Ctypes.typ

      val len : (Unsigned.uint32, t structure) Ctypes.field
      val error : (int32, t structure) Ctypes.field
      val unique : (Unsigned.uint64, t structure) Ctypes.field
    end

    module Write : sig
      type t
      val t : t structure Ctypes.typ

      val size : (Unsigned.uint32, t structure) Ctypes.field
    end

    module Open : sig
      type t
      val t : t structure Ctypes.typ

      val fh : (Unsigned.uint64, t structure) Ctypes.field
      val open_flags :
        (Unsigned.uint32, t structure) Ctypes.field
    end

    module Init : sig
      type t
      val t : t structure Ctypes.typ

      val major : (Unsigned.uint32, t structure) Ctypes.field
      val minor : (Unsigned.uint32, t structure) Ctypes.field
      val max_readahead :
        (Unsigned.uint32, t structure) Ctypes.field
      val flags : (Unsigned.uint32, t structure) Ctypes.field
      val max_write :
        (Unsigned.uint32, t structure) Ctypes.field
    end

    module Entry : sig
      type t
      val t : t structure Ctypes.typ

      val nodeid : (Unsigned.uint64, t structure) Ctypes.field
      val generation :
        (Unsigned.uint64, t structure) Ctypes.field
      val entry_valid :
        (Unsigned.uint64, t structure) Ctypes.field
      val attr_valid :
        (Unsigned.uint64, t structure) Ctypes.field
      val entry_valid_nsec :
        (Unsigned.uint32, t structure) Ctypes.field
      val attr_valid_nsec :
        (Unsigned.uint32, t structure) Ctypes.field
      val attr :
        (Struct.Attr.t structure,
         t structure)
          Ctypes.field
    end

    module Attr : sig
      type t
      val t : t structure Ctypes.typ

      val attr_valid :
        (Unsigned.uint64, t structure) Ctypes.field
      val attr_valid_nsec :
        (Unsigned.uint32, t structure) Ctypes.field
      val attr :
        (Struct.Attr.t structure,
         t structure)
          Ctypes.field
    end

    module Statfs : sig
      type t
      val t : t structure Ctypes.typ

      val st :
        (Struct.Kstatfs.t structure,
         t structure)
          Ctypes.field
    end

    module Getxattr : sig
      type t
      val t : t structure Ctypes.typ

      val size : (Unsigned.uint32, t structure) Ctypes.field
    end

    module Lk : sig
      type t
      val t : t structure Ctypes.typ

      val lk :
        (Struct.File_lock.t structure,
         t structure)
          Ctypes.field
    end

    module Bmap : sig
      type t
      val t : t structure Ctypes.typ

      val block : (Unsigned.uint64, t structure) Ctypes.field
    end
  end

  module In : sig
    module Opcode : sig
      type t =
        | FUSE_LOOKUP
        | FUSE_FORGET
        | FUSE_GETATTR
        | FUSE_SETATTR
        | FUSE_READLINK
        | FUSE_SYMLINK
        | FUSE_MKNOD
        | FUSE_MKDIR
        | FUSE_UNLINK
        | FUSE_RMDIR
        | FUSE_RENAME
        | FUSE_LINK
        | FUSE_OPEN
        | FUSE_READ
        | FUSE_WRITE
        | FUSE_STATFS
        | FUSE_RELEASE
        | FUSE_FSYNC
        | FUSE_SETXATTR
        | FUSE_GETXATTR
        | FUSE_LISTXATTR
        | FUSE_REMOVEXATTR
        | FUSE_FLUSH
        | FUSE_INIT
        | FUSE_OPENDIR
        | FUSE_READDIR
        | FUSE_RELEASEDIR
        | FUSE_FSYNCDIR
        | FUSE_GETLK
        | FUSE_SETLK
        | FUSE_SETLKW
        | FUSE_ACCESS
        | FUSE_CREATE
        | FUSE_INTERRUPT
        | FUSE_BMAP
        | FUSE_DESTROY
        (*| FUSE_IOCTL
        | FUSE_POLL
        | FUSE_NOTIFY_REPLY
        | FUSE_BATCH_FORGET
        | FUSE_FALLOCATE
        | CUSE_INIT*)
        | Unknown of int32

      val fuse_lookup : int64
      val fuse_forget : int64
      val fuse_getattr : int64
      val fuse_setattr : int64
      val fuse_readlink : int64
      val fuse_symlink : int64
      val fuse_mknod : int64
      val fuse_mkdir : int64
      val fuse_unlink : int64
      val fuse_rmdir : int64
      val fuse_rename : int64
      val fuse_link : int64
      val fuse_open : int64
      val fuse_read : int64
      val fuse_write : int64
      val fuse_statfs : int64
      val fuse_release : int64
      val fuse_fsync : int64
      val fuse_setxattr : int64
      val fuse_getxattr : int64
      val fuse_listxattr : int64
      val fuse_removexattr : int64
      val fuse_flush : int64
      val fuse_init : int64
      val fuse_opendir : int64
      val fuse_readdir : int64
      val fuse_releasedir : int64
      val fuse_fsyncdir : int64
      val fuse_getlk : int64
      val fuse_setlk : int64
      val fuse_setlkw : int64
      val fuse_access : int64
      val fuse_create : int64
      val fuse_interrupt : int64
      val fuse_bmap : int64
      val fuse_destroy : int64
      (*val fuse_ioctl : int64
      val fuse_poll : int64
      val fuse_notify_reply : int64
      val fuse_batch_forget : int64
      val fuse_fallocate : int64
      val cuse_init : int64*)
      val t : t Ctypes.typ
    end

    module Hdr : sig
      type t
      val t : t structure Ctypes.typ

      val len : (Unsigned.uint32, t structure) Ctypes.field
      val opcode : (Opcode.t, t structure) Ctypes.field
      val unique : (Unsigned.uint64, t structure) Ctypes.field
      val nodeid : (Unsigned.uint64, t structure) Ctypes.field
      val uid : (Unsigned.uint32, t structure) Ctypes.field
      val gid : (Unsigned.uint32, t structure) Ctypes.field
      val pid : (Unsigned.uint32, t structure) Ctypes.field
    end

    module Init : sig
      type t
      val t : t structure Ctypes.typ

      val major : (Unsigned.uint32, t structure) Ctypes.field
      val minor : (Unsigned.uint32, t structure) Ctypes.field
      val max_readahead :
        (Unsigned.uint32, t structure) Ctypes.field
      val flags : (Unsigned.uint32, t structure) Ctypes.field
    end

    module Open : sig
      type t
      val t : t structure Ctypes.typ

      val flags : (Unsigned.uint32, t structure) Ctypes.field
    end

    module Read : sig
      type t
      val t : t structure Ctypes.typ

      val fh : (Unsigned.uint64, t structure) Ctypes.field
      val offset : (Unsigned.uint64, t structure) Ctypes.field
      val size : (Unsigned.uint32, t structure) Ctypes.field
    end

    module Release : sig
      type t
      val t : t structure Ctypes.typ

      val fh : (Unsigned.uint64, t structure) Ctypes.field
      val flags : (Unsigned.uint32, t structure) Ctypes.field
      val release_flags :
        (Unsigned.uint32, t structure) Ctypes.field
      val lock_owner :
        (Unsigned.uint64, t structure) Ctypes.field
    end

    module Access : sig
      type t
      val t : t structure Ctypes.typ

      val mask : (Unsigned.uint32, t structure) Ctypes.field
    end

    module Forget : sig
      type t
      val t : t structure Ctypes.typ

      val nlookup :
        (Unsigned.uint64, t structure) Ctypes.field
    end

    module Flush : sig
      type t
      val t : t structure Ctypes.typ

      val fh : (Unsigned.uint64, t structure) Ctypes.field
      val lock_owner :
        (Unsigned.uint64, t structure) Ctypes.field
    end

    module Create : sig
      type t
      val t : t structure Ctypes.typ

      val flags : (Unsigned.uint32, t structure) Ctypes.field
      val mode : (Unsigned.uint32, t structure) Ctypes.field
    end

    module Mknod : sig
      type t
      val t : t structure Ctypes.typ

      val mode : (Unsigned.uint32, t structure) Ctypes.field
      val rdev : (Unsigned.uint32, t structure) Ctypes.field
    end

    module Mkdir : sig
      type t
      val t : t structure Ctypes.typ

      val mode : (Unsigned.uint32, t structure) Ctypes.field
    end

    module Rename : sig
      type t
      val t : t structure Ctypes.typ

      val newdir : (Unsigned.uint64, t structure) Ctypes.field
    end

    module Link : sig
      type t
      val t : t structure Ctypes.typ

      val oldnodeid :
        (Unsigned.uint64, t structure) Ctypes.field
    end

    module Write : sig
      type t
      val t : t structure Ctypes.typ

      val fh : (Unsigned.uint64, t structure) Ctypes.field
      val offset : (Unsigned.uint64, t structure) Ctypes.field
      val size : (Unsigned.uint32, t structure) Ctypes.field
      val write_flags :
        (Unsigned.uint32, t structure) Ctypes.field
    end

    module Fsync : sig
      type t
      val t : t structure Ctypes.typ

      val fh : (Unsigned.uint64, t structure) Ctypes.field
      val fsync_flags :
        (Unsigned.uint32, t structure) Ctypes.field
    end

    module Lk : sig
      type t
      val t : t structure Ctypes.typ

      val fh : (int64, t structure) Ctypes.field
      val owner : (Unsigned.uint64, t structure) Ctypes.field
      val lk :
        (Struct.File_lock.t structure,
         t structure)
          Ctypes.field
    end

    module Interrupt : sig
      type t
      val t : t structure Ctypes.typ

      val unique : (Unsigned.uint64, t structure) Ctypes.field
    end

    module Bmap : sig
      type t
      val t : t structure Ctypes.typ

      val block : (Unsigned.uint64, t structure) Ctypes.field
      val blocksize :
        (Unsigned.uint32, t structure) Ctypes.field
    end

    module Setattr : sig

      module Valid : sig
        val t : Unsigned.uint32 Ctypes.typ
        val fattr_mode : Unsigned.uint32
        val fattr_uid : Unsigned.uint32
        val fattr_gid : Unsigned.uint32
        val fattr_size : Unsigned.uint32
        val fattr_atime : Unsigned.uint32
        val fattr_mtime : Unsigned.uint32
        val fattr_fh : Unsigned.uint32
        (*val fattr_atime_now : Unsigned.uint32
        val fattr_mtime_now : Unsigned.uint32
        val fattr_lockowner : Unsigned.uint32*)
      end

      type t
      val t : t structure Ctypes.typ

      val valid : (Unsigned.uint32, t structure) Ctypes.field
      val fh : (Unsigned.uint64, t structure) Ctypes.field
      val size : (Unsigned.uint64, t structure) Ctypes.field
      val atime : (Unsigned.uint64, t structure) Ctypes.field
      val mtime : (Unsigned.uint64, t structure) Ctypes.field
      val atimensec :
        (Unsigned.uint32, t structure) Ctypes.field
      val mtimensec :
        (Unsigned.uint32, t structure) Ctypes.field
      val mode : (Unsigned.uint32, t structure) Ctypes.field
      val uid : (Unsigned.uint32, t structure) Ctypes.field
      val gid : (Unsigned.uint32, t structure) Ctypes.field
    end

    module Getxattr : sig
      type t
      val t : t structure Ctypes.typ

      val size : (Unsigned.uint32, t structure) Ctypes.field
    end

    module Setxattr : sig
      type t
      val t : t structure Ctypes.typ

      val size : (Unsigned.uint32, t structure) Ctypes.field
      val flags : (Unsigned.uint32, t structure) Ctypes.field
    end
  end
end

type 'a structure = 'a Types.structure

module Struct : sig
  module T = Types.Struct
  module Kstatfs : sig
    module T = T.Kstatfs

    val store :
      blocks:Unsigned.uint64 ->
      bfree:Unsigned.uint64 ->
      bavail:Unsigned.uint64 ->
      files:Unsigned.uint64 ->
      ffree:Unsigned.uint64 ->
      bsize:Unsigned.uint32 ->
      namelen:Unsigned.uint32 ->
      frsize:Unsigned.uint32 ->
      T.t structure -> unit

    val create :
      blocks:Unsigned.uint64 ->
      bfree:Unsigned.uint64 ->
      bavail:Unsigned.uint64 ->
      files:Unsigned.uint64 ->
      ffree:Unsigned.uint64 ->
      bsize:Unsigned.uint32 ->
      namelen:Unsigned.uint32 ->
      frsize:Unsigned.uint32 ->
      unit -> T.t structure
  end

  module File_lock : sig
    module T = T.File_lock
  end

  module Attr : sig
    module T = T.Attr

    val store :
      ino:Unsigned.uint64 ->
      size:Unsigned.uint64 ->
      blocks:Unsigned.uint64 ->
      atime:Unsigned.uint64 ->
      mtime:Unsigned.uint64 ->
      ctime:Unsigned.uint64 ->
      atimensec:Unsigned.uint32 ->
      mtimensec:Unsigned.uint32 ->
      ctimensec:Unsigned.uint32 ->
      mode:Unsigned.uint32 ->
      nlink:Unsigned.uint32 ->
      uid:Unsigned.uint32 ->
      gid:Unsigned.uint32 ->
      rdev:Unsigned.uint32 ->
      T.t structure -> unit

    val create :
      ino:Unsigned.uint64 ->
      size:Unsigned.uint64 ->
      blocks:Unsigned.uint64 ->
      atime:Unsigned.uint64 ->
      mtime:Unsigned.uint64 ->
      ctime:Unsigned.uint64 ->
      atimensec:Unsigned.uint32 ->
      mtimensec:Unsigned.uint32 ->
      ctimensec:Unsigned.uint32 ->
      mode:Unsigned.uint32 ->
      nlink:Unsigned.uint32 ->
      uid:Unsigned.uint32 ->
      gid:Unsigned.uint32 ->
      rdev:Unsigned.uint32 ->
      unit -> T.t structure

    val describe : host:Host.t -> T.t structure -> string

  end
end

module In : sig
  module T = Types.In

  module Opcode : sig
    module T = T.Opcode

    type t = T.t =
      (* ro *)
      | FUSE_LOOKUP (* = 1 *)
      | FUSE_FORGET (* no reply *)
      | FUSE_GETATTR
      (* end ro *)

      | FUSE_SETATTR
      | FUSE_READLINK
      | FUSE_SYMLINK

      | FUSE_MKNOD (* = 8 *)
      | FUSE_MKDIR
      | FUSE_UNLINK
      | FUSE_RMDIR
      | FUSE_RENAME
      | FUSE_LINK

      (* ro *)
      | FUSE_OPEN (* = 14 *)
      | FUSE_READ
      (* end ro *)

      | FUSE_WRITE
      | FUSE_STATFS

      (* ro *)
      | FUSE_RELEASE (* = 18 *) (* 0 reply? *)
      (* end ro *)

      | FUSE_FSYNC (* = 20 *)
      | FUSE_SETXATTR
      | FUSE_GETXATTR
      | FUSE_LISTXATTR
      | FUSE_REMOVEXATTR
      | FUSE_FLUSH (* 0 reply *)

      (* ro *)
      | FUSE_INIT (* = 26 *)
      | FUSE_OPENDIR
      | FUSE_READDIR
      | FUSE_RELEASEDIR (* 0 reply *)
      (* end ro *)

      | FUSE_FSYNCDIR (* = 30 *)
      | FUSE_GETLK
      | FUSE_SETLK
      | FUSE_SETLKW
      | FUSE_ACCESS
      | FUSE_CREATE
      | FUSE_INTERRUPT
      | FUSE_BMAP

      (* ro *)
      | FUSE_DESTROY (* = 38 *) (* no reply *)
      (* end ro *)

      (* > 7.8 *)
      (*| FUSE_IOCTL
      | FUSE_POLL
      | FUSE_NOTIFY_REPLY
      | FUSE_BATCH_FORGET
      | FUSE_FALLOCATE

      | CUSE_INIT*)

      | Unknown of int32

    val to_string : t -> string

    val returns : t -> bool
  end

  module Hdr : sig
    module T = T.Hdr

    val sz : int

    val packet :
      opcode:Opcode.t ->
      unique:Unsigned.uint64 ->
      nodeid:Unsigned.uint64 ->
      uid:Unsigned.uint32 ->
      gid:Unsigned.uint32 ->
      pid:Unsigned.uint32 -> count:int -> char Ctypes.CArray.t

    val make :
      opcode:Opcode.t ->
      unique:Unsigned.uint64 ->
      nodeid:Unsigned.uint64 ->
      uid:Unsigned.uint32 ->
      gid:Unsigned.uint32 -> pid:Unsigned.uint32 -> 'a Ctypes.typ -> 'a

    val memcpy :
      dest:unit Ctypes.ptr -> src:unit Ctypes.ptr -> int -> unit

    val packet_from_hdr :
      T.t structure ->
      count:int -> char Ctypes.CArray.t

    val make_from_hdr :
      T.t structure -> 'a Ctypes.typ -> 'a
  end

  module Init : sig
    module T = T.Init
  end

  module Open : sig
    module T = T.Open
  end

  module Read : sig
    module T = T.Read
  end

  module Release : sig
    module T = T.Release
  end

  module Access : sig
    module T = T.Access
  end

  module Forget : sig
    module T = T.Forget
  end

  module Flush : sig
    module T = T.Flush
  end

  module Create : sig
    module T = T.Create

    val name : unit Ctypes.ptr -> string
  end

  module Mknod : sig
    module T = T.Mknod

    val name : unit Ctypes.ptr -> string
  end

  module Mkdir : sig
    module T = T.Mkdir

    val name : unit Ctypes.ptr -> string
  end

  module Rename : sig
    module T = T.Rename

    val source_destination : unit Ctypes.ptr -> string * string
  end

  module Link : sig
    module T = T.Link

    val name : unit Ctypes.ptr -> string
  end

  module Write : sig
    module T = T.Write
  end

  module Fsync : sig
    module T = T.Fsync
  end

  module Lk : sig
    module T = T.Lk
  end

  module Interrupt : sig
    module T = T.Interrupt
  end

  module Bmap : sig
    module T = T.Bmap
  end

  module Setattr : sig
    module T = T.Setattr

    module Valid : sig
      module T = T.Valid

      type t = {
        mode : bool;
        uid : bool;
        gid : bool;
        size : bool;
        atime : bool;
        mtime : bool;
        fh : bool;
        (*atime_now : bool;
        mtime_now : bool;
          lockowner : bool;*)
      }

      val of_uint32 : Unsigned.uint32 -> t

      val to_uint32 : t -> Unsigned.uint32
    end
    
    val create_from_hdr :
      valid:Unsigned.uint32 ->
      fh:Unsigned.uint64 ->
      size:Unsigned.uint64 ->
      atime:Unsigned.uint64 ->
      mtime:Unsigned.uint64 ->
      atimensec:Unsigned.uint32 ->
      mtimensec:Unsigned.uint32 ->
      mode:Unsigned.uint32 ->
      uid:Unsigned.uint32 ->
      gid:Unsigned.uint32 ->
      Hdr.T.t structure -> char Ctypes.CArray.t
  end

  module Getxattr : sig
    module T = T.Getxattr

    val create_from_hdr :
      size:Unsigned.uint32 ->
      Hdr.T.t structure -> char Ctypes.CArray.t
  end

  module Setxattr : sig
    module T = T.Setxattr

    val create_from_hdr :
      size:Unsigned.uint32 ->
      flags:Unsigned.uint32 ->
      Hdr.T.t structure -> char Ctypes.CArray.t
  end

  module Message : sig
    type t =
      | Init of Init.T.t Ctypes.structure
      | Getattr
      | Lookup of string
      | Opendir of Open.T.t Ctypes.structure
      | Readdir of Read.T.t Ctypes.structure
      | Releasedir of Release.T.t Ctypes.structure
      | Fsyncdir of Fsync.T.t Ctypes.structure
      | Rmdir of string
      | Getxattr of Getxattr.T.t Ctypes.structure
      | Setxattr of Setxattr.T.t Ctypes.structure
      | Listxattr of Getxattr.T.t Ctypes.structure
      | Removexattr of string
      | Access of Access.T.t Ctypes.structure
      | Forget of Forget.T.t Ctypes.structure
      | Readlink
      | Open of Open.T.t Ctypes.structure
      | Read of Read.T.t Ctypes.structure
      | Write of Write.T.t structure * char Ctypes.ptr
      | Statfs
      | Flush of Flush.T.t Ctypes.structure
      | Release of Release.T.t Ctypes.structure
      | Fsync of Fsync.T.t Ctypes.structure
      | Unlink of string
      | Create of Create.T.t Ctypes.structure * string
      | Mknod of Mknod.T.t Ctypes.structure * string
      | Mkdir of Mkdir.T.t Ctypes.structure * string
      | Setattr of Setattr.T.t Ctypes.structure
      | Link of Link.T.t Ctypes.structure * string
      | Symlink of string * string
      | Rename of Rename.T.t Ctypes.structure * string * string
      | Getlk of Lk.T.t Ctypes.structure
      | Setlk of Lk.T.t Ctypes.structure
      | Setlkw of Lk.T.t Ctypes.structure
      | Interrupt of Interrupt.T.t Ctypes.structure
      | Bmap of Bmap.T.t Ctypes.structure
      | Destroy
      | Other of Opcode.t
      | Unknown of int32

    val parse :
      chan -> Hdr.T.t Ctypes.structure -> int -> unit Ctypes.ptr
      -> (Hdr.T.t, t) packet
  end
end

type 'a request = (In.Hdr.T.t, 'a) packet

module Out : sig
  module T = Types.Out

  module Hdr : sig
    module T = T.Hdr

    val sz : int

    val packet :
      ?nerrno:int32 ->
      count:int -> 'a request -> char Ctypes.CArray.t

    val make : 'a request -> 'b Ctypes.typ -> 'b

    val set_size : char Ctypes.CArray.t -> int -> char Ctypes.CArray.t
  end

  module Dirent : sig
    module T = Struct.T.Dirent

    val struct_size : int

    val size : string -> int

    val of_list :
      host:Host.t ->
      (int * int64 * string * Dirent.File_kind.t) list ->
      int -> 'a request -> char Ctypes.CArray.t
  end

  module Readlink : sig
    val create :
      target:string -> 'a request -> char Ctypes.CArray.t
  end

  module Read : sig
    val create :
      size:int ->
      data_fn:((char, Bigarray.int8_unsigned_elt, Bigarray.c_layout)
                 Bigarray.Array1.t -> int) ->
      'a request -> char Ctypes.CArray.t
  end

  module Write : sig
    module T = T.Write

    val create : size:Unsigned.uint32 -> 'a request -> char Ctypes.CArray.t
  end

  module Open : sig
    module T = T.Open

    val store :
      fh:Unsigned.uint64 ->
      open_flags:Unsigned.uint32 ->
      T.t structure -> 'a -> unit

    val create :
      fh:Unsigned.uint64 ->
      open_flags:Unsigned.uint32 ->
      'a request -> char Ctypes.CArray.t
  end

  module Init : sig
    module T = T.Init

    val create :
      major:Unsigned.uint32 ->
      minor:Unsigned.uint32 ->
      max_readahead:Unsigned.uint32 ->
      flags:Unsigned.uint32 ->
      max_write:Unsigned.uint32 ->
      'a request -> char Ctypes.CArray.t
    val describe : T.t structure -> string
  end

  module Entry : sig
    module T = T.Entry

    val store :
      nodeid:Unsigned.uint64 ->
      generation:Unsigned.uint64 ->
      entry_valid:Unsigned.uint64 ->
      attr_valid:Unsigned.uint64 ->
      entry_valid_nsec:Unsigned.uint32 ->
      attr_valid_nsec:Unsigned.uint32 ->
      store_attr:(Types.Struct.Attr.t structure -> unit) ->
      T.t structure -> 'a -> unit

    val create :
      nodeid:Unsigned.uint64 ->
      generation:Unsigned.uint64 ->
      entry_valid:Unsigned.uint64 ->
      attr_valid:Unsigned.uint64 ->
      entry_valid_nsec:Unsigned.uint32 ->
      attr_valid_nsec:Unsigned.uint32 ->
      store_attr:(Types.Struct.Attr.t structure -> unit) ->
      'a request -> char Ctypes.CArray.t

    val describe : host:Host.t -> T.t structure -> string
  end

  module Attr : sig
    module T = T.Attr

    val create :
      attr_valid:Unsigned.uint64 ->
      attr_valid_nsec:Unsigned.uint32 ->
      store_attr:(Types.Struct.Attr.t structure -> unit) ->
      'a request -> char Ctypes.CArray.t
  end

  module Create : sig
    module T : sig
      type t
      val t : t structure Ctypes.typ

      val entry : (Types.Out.Entry.t structure, t structure) Ctypes.field
      val open_ : (Types.Out.Open.t structure, t structure) Ctypes.field
    end

    val create :
      store_entry:(Entry.T.t structure -> 'a request -> unit)
      -> store_open:(Open.T.t structure -> 'a request -> unit)
      -> 'a request -> char Ctypes.CArray.t
  end

  module Message : sig
    type t =
      | Init    of Init.T.t  structure
      | Getattr of Attr.T.t  structure
      | Lookup  of Entry.T.t structure
      | Opendir of Open.T.t  structure
      | Readdir of char Ctypes.CArray.t
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
      | Read     of char Ctypes.CArray.t
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
      | Rename   of Entry.T.t structure
      | Getlk (* TODO: do *)
      | Setlk (* TODO: do *)
      | Setlkw (* TODO: do *)
      | Interrupt (* TODO: do *)
      | Bmap (* TODO: do *)
      | Destroy
      | Other    of In.Opcode.t
      | Unknown  of int32 * int * unit Ctypes.ptr

    val deserialize :
      (In.Hdr.T.t, 'a) packet -> int -> char Ctypes.ptr
      -> (Hdr.T.t, t) packet

    val describe_reply : ('a,t) packet -> string
  end
end
