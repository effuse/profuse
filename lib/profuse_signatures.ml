module type S = sig

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
    unistd : Unistd.host;
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

end

module Types_7_8 = struct
  type 'a structure = 'a Ctypes_static.structure

  module Struct = struct
    module type Kstatfs = sig
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

    module type File_lock = sig
      type t
      val t : t structure Ctypes.typ

      val start : (Unsigned.uint64, t structure) Ctypes.field
      val end_ : (Unsigned.uint64, t structure) Ctypes.field
      val type_ : (Unsigned.uint32, t structure) Ctypes.field
      val pid : (Unsigned.uint32, t structure) Ctypes.field
    end

    module type Attr = sig
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

    module type Dirent = sig
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

  module Out = struct

    module type Hdr = sig
      type t
      val t : t structure Ctypes.typ

      val len : (Unsigned.uint32, t structure) Ctypes.field
      val error : (int32, t structure) Ctypes.field
      val unique : (Unsigned.uint64, t structure) Ctypes.field
    end

    module type Write = sig
      type t
      val t : t structure Ctypes.typ

      val size : (Unsigned.uint32, t structure) Ctypes.field
    end

    module type Open_flags = sig
      val fopen_direct_io   : Unsigned.uint32
      val fopen_keep_cache  : Unsigned.uint32
    end
        
    module type Open_ = sig
      type t
      val t : t structure Ctypes.typ

      val fh : (Unsigned.uint64, t structure) Ctypes.field
      val open_flags :
        (Unsigned.uint32, t structure) Ctypes.field
    end
    module type Open = sig
      module Flags : Open_flags
      include Open_
    end

    module type Init = sig
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

    module type Entry = sig
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
      type struct_attr_t
      val attr :
        (struct_attr_t structure,
         t structure)
          Ctypes.field
    end

    module type Attr = sig
      type t
      val t : t structure Ctypes.typ

      val attr_valid :
        (Unsigned.uint64, t structure) Ctypes.field
      val attr_valid_nsec :
        (Unsigned.uint32, t structure) Ctypes.field

      type struct_attr_t
      val attr :
        (struct_attr_t structure,
         t structure)
          Ctypes.field
    end

    module type Statfs = sig
      type t
      val t : t structure Ctypes.typ

      type struct_kstatfs_t
      val st :
        (struct_kstatfs_t structure,
         t structure)
          Ctypes.field
    end

    module type Getxattr = sig
      type t
      val t : t structure Ctypes.typ

      val size : (Unsigned.uint32, t structure) Ctypes.field
    end

    module type Lk = sig
      type t
      val t : t structure Ctypes.typ

      type struct_file_lock_t
      val lk :
        (struct_file_lock_t structure,
         t structure)
          Ctypes.field
    end

    module type Bmap = sig
      type t
      val t : t structure Ctypes.typ

      val block : (Unsigned.uint64, t structure) Ctypes.field
    end
  end

  module In = struct
    module type Opcode_ops = sig
      val fuse_lookup : Unsigned.uint32
      val fuse_forget : Unsigned.uint32
      val fuse_getattr : Unsigned.uint32
      val fuse_setattr : Unsigned.uint32
      val fuse_readlink : Unsigned.uint32
      val fuse_symlink : Unsigned.uint32
      val fuse_mknod : Unsigned.uint32
      val fuse_mkdir : Unsigned.uint32
      val fuse_unlink : Unsigned.uint32
      val fuse_rmdir : Unsigned.uint32
      val fuse_rename : Unsigned.uint32
      val fuse_link : Unsigned.uint32
      val fuse_open : Unsigned.uint32
      val fuse_read : Unsigned.uint32
      val fuse_write : Unsigned.uint32
      val fuse_statfs : Unsigned.uint32
      val fuse_release : Unsigned.uint32
      val fuse_fsync : Unsigned.uint32
      val fuse_setxattr : Unsigned.uint32
      val fuse_getxattr : Unsigned.uint32
      val fuse_listxattr : Unsigned.uint32
      val fuse_removexattr : Unsigned.uint32
      val fuse_flush : Unsigned.uint32
      val fuse_init : Unsigned.uint32
      val fuse_opendir : Unsigned.uint32
      val fuse_readdir : Unsigned.uint32
      val fuse_releasedir : Unsigned.uint32
      val fuse_fsyncdir : Unsigned.uint32
      val fuse_getlk : Unsigned.uint32
      val fuse_setlk : Unsigned.uint32
      val fuse_setlkw : Unsigned.uint32
      val fuse_access : Unsigned.uint32
      val fuse_create : Unsigned.uint32
      val fuse_interrupt : Unsigned.uint32
      val fuse_bmap : Unsigned.uint32
      val fuse_destroy : Unsigned.uint32
    end
     
    type opcode_t = [
        | `FUSE_LOOKUP
        | `FUSE_FORGET
        | `FUSE_GETATTR
        | `FUSE_SETATTR
        | `FUSE_READLINK
        | `FUSE_SYMLINK
        | `FUSE_MKNOD
        | `FUSE_MKDIR
        | `FUSE_UNLINK
        | `FUSE_RMDIR
        | `FUSE_RENAME
        | `FUSE_LINK
        | `FUSE_OPEN
        | `FUSE_READ
        | `FUSE_WRITE
        | `FUSE_STATFS
        | `FUSE_RELEASE
        | `FUSE_FSYNC
        | `FUSE_SETXATTR
        | `FUSE_GETXATTR
        | `FUSE_LISTXATTR
        | `FUSE_REMOVEXATTR
        | `FUSE_FLUSH
        | `FUSE_INIT
        | `FUSE_OPENDIR
        | `FUSE_READDIR
        | `FUSE_RELEASEDIR
        | `FUSE_FSYNCDIR
        | `FUSE_GETLK
        | `FUSE_SETLK
        | `FUSE_SETLKW
        | `FUSE_ACCESS
        | `FUSE_CREATE
        | `FUSE_INTERRUPT
        | `FUSE_BMAP
        | `FUSE_DESTROY
        | `Unknown of int32
      ]
    module type Opcode = sig
      type t = opcode_t

      include Opcode_ops
    end

    module type Hdr = sig
      type t
      val t : t structure Ctypes.typ

      val len : (Unsigned.uint32, t structure) Ctypes.field
      val opcode : (Unsigned.uint32, t structure) Ctypes.field
      val unique : (Unsigned.uint64, t structure) Ctypes.field
      val nodeid : (Unsigned.uint64, t structure) Ctypes.field
      val uid : (Unsigned.uint32, t structure) Ctypes.field
      val gid : (Unsigned.uint32, t structure) Ctypes.field
      val pid : (Unsigned.uint32, t structure) Ctypes.field
    end

    module type Init_flags = sig
      val async_read : Unsigned.uint32
      val posix_locks : Unsigned.uint32
    end
    module type Init_ = sig
      type t
      val t : t structure Ctypes.typ

      val major : (Unsigned.uint32, t structure) Ctypes.field
      val minor : (Unsigned.uint32, t structure) Ctypes.field
      val max_readahead :
        (Unsigned.uint32, t structure) Ctypes.field
      val flags : (Unsigned.uint32, t structure) Ctypes.field
    end

    module type Init = sig
      module Flags : Init_flags
      include Init_
    end

    module type Open = sig
      type t
      val t : t structure Ctypes.typ

      val flags : (Unsigned.uint32, t structure) Ctypes.field
    end

    module type Read = sig
      type t
      val t : t structure Ctypes.typ

      val fh : (Unsigned.uint64, t structure) Ctypes.field
      val offset : (Unsigned.uint64, t structure) Ctypes.field
      val size : (Unsigned.uint32, t structure) Ctypes.field
    end

    module type Release = sig
      type t
      val t : t structure Ctypes.typ

      val fh : (Unsigned.uint64, t structure) Ctypes.field
      val flags : (Unsigned.uint32, t structure) Ctypes.field
      val release_flags :
        (Unsigned.uint32, t structure) Ctypes.field
      val lock_owner :
        (Unsigned.uint64, t structure) Ctypes.field
    end

    module type Access = sig
      type t
      val t : t structure Ctypes.typ

      val mask : (Unsigned.uint32, t structure) Ctypes.field
    end

    module type Forget = sig
      type t
      val t : t structure Ctypes.typ

      val nlookup :
        (Unsigned.uint64, t structure) Ctypes.field
    end

    module type Flush = sig
      type t
      val t : t structure Ctypes.typ

      val fh : (Unsigned.uint64, t structure) Ctypes.field
      val lock_owner :
        (Unsigned.uint64, t structure) Ctypes.field
    end

    module type Create = sig
      type t
      val t : t structure Ctypes.typ

      val flags : (Unsigned.uint32, t structure) Ctypes.field
      val mode : (Unsigned.uint32, t structure) Ctypes.field
    end

    module type Mknod = sig
      type t
      val t : t structure Ctypes.typ

      val mode : (Unsigned.uint32, t structure) Ctypes.field
      val rdev : (Unsigned.uint32, t structure) Ctypes.field
    end

    module type Mkdir = sig
      type t
      val t : t structure Ctypes.typ

      val mode : (Unsigned.uint32, t structure) Ctypes.field
    end

    module type Rename = sig
      type t
      val t : t structure Ctypes.typ

      val newdir : (Unsigned.uint64, t structure) Ctypes.field
    end

    module type Link = sig
      type t
      val t : t structure Ctypes.typ

      val oldnodeid :
        (Unsigned.uint64, t structure) Ctypes.field
    end

    module type Write = sig
      type t
      val t : t structure Ctypes.typ

      val fh : (Unsigned.uint64, t structure) Ctypes.field
      val offset : (Unsigned.uint64, t structure) Ctypes.field
      val size : (Unsigned.uint32, t structure) Ctypes.field
      val write_flags :
        (Unsigned.uint32, t structure) Ctypes.field
    end

    module type Fsync = sig
      type t
      val t : t structure Ctypes.typ

      val fh : (Unsigned.uint64, t structure) Ctypes.field
      val fsync_flags :
        (Unsigned.uint32, t structure) Ctypes.field
    end

    module type Lk = sig
      type t
      val t : t structure Ctypes.typ

      val fh : (int64, t structure) Ctypes.field
      val owner : (Unsigned.uint64, t structure) Ctypes.field
      type struct_file_lock_t
      val lk :
        (struct_file_lock_t structure,
         t structure)
          Ctypes.field
    end

    module type Interrupt = sig
      type t
      val t : t structure Ctypes.typ

      val unique : (Unsigned.uint64, t structure) Ctypes.field
    end

    module type Bmap = sig
      type t
      val t : t structure Ctypes.typ

      val block : (Unsigned.uint64, t structure) Ctypes.field
      val blocksize :
        (Unsigned.uint32, t structure) Ctypes.field
    end

    module type Setattr_valid = sig
        val t : Unsigned.uint32 Ctypes.typ
        val fattr_mode : Unsigned.uint32
        val fattr_uid : Unsigned.uint32
        val fattr_gid : Unsigned.uint32
        val fattr_size : Unsigned.uint32
        val fattr_atime : Unsigned.uint32
        val fattr_mtime : Unsigned.uint32
        val fattr_fh : Unsigned.uint32
    end
    module type Setattr_ = sig
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
    module type Setattr = sig
      module Valid : Setattr_valid
      include Setattr_
    end

    module type Getxattr = sig
      type t
      val t : t structure Ctypes.typ

      val size : (Unsigned.uint32, t structure) Ctypes.field
    end

    module type Setxattr = sig
      type t
      val t : t structure Ctypes.typ

      val size : (Unsigned.uint32, t structure) Ctypes.field
      val flags : (Unsigned.uint32, t structure) Ctypes.field
    end
  end
end

module type Types_7_8_struct = sig
  open Types_7_8.Struct
  module Kstatfs : Kstatfs
  module File_lock : File_lock
  module Attr : Attr
  module Dirent : Dirent
end

module type Types_7_8_out = sig
  module Struct : Types_7_8_struct
  open Types_7_8.Out
  module Hdr : Hdr
  module Write : Write
  module Open : Open
  module Init : Init
  module Entry : Entry with type struct_attr_t := Struct.Attr.t
  module Attr : Attr with type struct_attr_t := Struct.Attr.t
  module Statfs : Statfs with type struct_kstatfs_t := Struct.Kstatfs.t
  module Getxattr : Getxattr
  module Lk : Lk with type struct_file_lock_t := Struct.File_lock.t
  module Bmap : Bmap
end

module type Types_7_8_in = sig
  module Struct : Types_7_8_struct
  open Types_7_8.In
  module Opcode : Opcode
  module Hdr : Hdr
  module Init : Init
  module Open : Open
  module Read : Read
  module Release : Release
  module Access : Access
  module Forget : Forget
  module Flush : Flush
  module Create : Create
  module Mknod : Mknod
  module Mkdir : Mkdir
  module Rename : Rename
  module Link : Link
  module Write : Write
  module Fsync : Fsync
  module Lk : Lk with type struct_file_lock_t := Struct.File_lock.t
  module Interrupt : Interrupt
  module Bmap : Bmap
  module Setattr : Setattr
  module Getxattr : Getxattr
  module Setxattr : Setxattr
end

module Types_7_23 =
struct
  type 'a structure = 'a Ctypes_static.structure
  module Struct =
  struct
    open Types_7_8.Struct
    module type Kstatfs = Kstatfs
    module type File_lock = File_lock
    module type Attr = sig
      include Attr
      val blksize : (Unsigned.uint32, t structure) Ctypes.field
    end
    module type Dirent = Dirent
    module type Forget_one = sig
      type t
      val t : t structure Ctypes.typ

      val nodeid  : (Unsigned.uint64, t structure) Ctypes.field
      val nlookup : (Unsigned.uint64, t structure) Ctypes.field
    end
  end
  module In =
  struct
    open Types_7_8.In
    type opcode_t = [
      Types_7_8.In.opcode_t
      | `FUSE_IOCTL
      | `FUSE_POLL
      | `FUSE_NOTIFY_REPLY
      | `FUSE_BATCH_FORGET
      | `FUSE_FALLOCATE
      | `CUSE_INIT
      | `FUSE_READDIRPLUS
      | `FUSE_RENAME2
    ]
    module type Opcode_ops =
    sig
      include Opcode_ops
      val fuse_ioctl : Unsigned.uint32
      val fuse_poll : Unsigned.uint32
      val fuse_notify_reply : Unsigned.uint32
      val fuse_batch_forget : Unsigned.uint32
      val fuse_fallocate : Unsigned.uint32
      val fuse_rename2 : Unsigned.uint32
      val fuse_readdirplus : Unsigned.uint32
      val cuse_init : Unsigned.uint32
    end
    module type Opcode = sig
      type t = opcode_t
      include Opcode_ops

    end
    module type Hdr = Hdr
    module type Init_flags = sig
      include Init_flags
                val fuse_file_ops : Unsigned.uint32
      val fuse_atomic_o_trunc : Unsigned.uint32
      val fuse_big_writes : Unsigned.uint32
      val fuse_export_support : Unsigned.uint32
      val fuse_dont_mask : Unsigned.uint32
      val fuse_flock_locks : Unsigned.uint32
      val fuse_splice_write : Unsigned.uint32
      val fuse_splice_move : Unsigned.uint32
      val fuse_splice_read : Unsigned.uint32
      val fuse_has_ioctl_dir : Unsigned.uint32
      val fuse_auto_inval_data : Unsigned.uint32
      val fuse_do_readdirplus : Unsigned.uint32
      val fuse_readdirplus_auto : Unsigned.uint32
      val fuse_async_dio : Unsigned.uint32
      val fuse_writeback_cache : Unsigned.uint32
      val fuse_no_open_support : Unsigned.uint32
    end
    module type Init = sig
      module Flags : Init_flags
      include Init_
    end
    module type Open = Open
    module type Read = Read
    module type Release = Release
    module type Access = Access
    module type Forget = Forget
    module type Flush = Flush
    module type Create = Create
    module type Mknod = sig
      include Mknod
      val umask : (Unsigned.uint32, t structure) Ctypes.field
    end
    module type Mkdir = sig
      include Mkdir
      val umask : (Unsigned.uint32, t structure) Ctypes.field
    end
    module type Rename = Rename
    module type Link = Link
    module type Write = Write
    module type Fsync = Fsync
    module type Lk = Lk
    module type Interrupt = Interrupt
    module type Bmap = Bmap
    module type Setattr_valid = sig
      include Setattr_valid
      val fattr_ctime : Unsigned.uint32
      val fattr_atime_now : Unsigned.uint32
      val fattr_mtime_now : Unsigned.uint32
      val fattr_lockowner : Unsigned.uint32
    end
    module type Setattr = sig
      module Valid : Setattr_valid
      include Setattr_
      val lock_owner : (Unsigned.uint64, t structure) Ctypes.field
    end
    module type Getxattr = Getxattr
    module type Setxattr = Setxattr
  end
  module Out =
  struct
    open Types_7_8.Out

    module type Hdr = sig
      module Notify_code : sig
        type t =
          [ `FUSE_NOTIFY_DELETE
          | `FUSE_NOTIFY_INVAL_ENTRY
          | `FUSE_NOTIFY_INVAL_INODE
          | `FUSE_NOTIFY_POLL
          | `FUSE_NOTIFY_RETRIEVE
          | `FUSE_NOTIFY_STORE ]
      end
      include Hdr
    end

    module type Notify_inval_entry = sig
      type t
      val t : t structure Ctypes.typ

      val parent  : (Unsigned.UInt64.t, t structure) Ctypes.field
      val namelen : (Unsigned.UInt32.t, t structure) Ctypes.field
    end

    module type Notify_inval_inode = sig
      type t
      val t : t structure Ctypes.typ

      val ino : (Unsigned.UInt64.t, t structure) Ctypes.field
      val off : (Signed.Int64.t, t structure) Ctypes.field
      val len : (Signed.Int64.t, t structure) Ctypes.field
    end

    module type Notify_delete = sig
      type t
      val t : t structure Ctypes.typ

      val parent  : (Unsigned.UInt64.t, t structure) Ctypes.field
      val child   : (Unsigned.UInt64.t, t structure) Ctypes.field
      val namelen : (Unsigned.UInt32.t, t structure) Ctypes.field
    end

    module type Write = Write
    module type Open_flags = sig
      include Open_flags
      val fopen_nonseekable : Unsigned.uint32
    end
    module type Open = sig
      module Flags : Open_flags
      include Open_
    end
    module type Init = Init
    module type Entry = Entry
    module type Attr = Attr
    module type Statfs = Statfs
    module type Getxattr = Getxattr
    module type Lk = Lk
    module type Bmap = Bmap
  end
end

module Signatures_7_8 =
struct
  type 'a structure = 'a Ctypes.structure

  module Struct = struct
    module type Kstatfs = sig
      module Types : Types_7_8_struct
      module T = Types.Kstatfs

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

    module type File_lock = sig
      module Types : Types_7_8_struct
      module T = Types.File_lock
    end

    module type Attr = sig
      module Types : Types_7_8_struct
      module T = Types.Attr

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

      type host_t
      val describe : host:host_t -> T.t structure -> string
    end
  end

  module In = struct
    module type Opcode = sig
      module Struct : Types_7_8_struct
      module Types : Types_7_8_in with module Struct := Struct
      module T = Types.Opcode

      type t = T.t

      val to_string : t -> string

      val returns : t -> bool

      val of_uint32 : Unsigned.uint32 -> t

      val to_uint32 : t -> Unsigned.uint32
    end

    module type Hdr = sig
      module Struct : Types_7_8_struct
      module Types : Types_7_8_in with module Struct := Struct
      module T = Types.Hdr

      type opcode_t

      val sz : int

      val packet :
        opcode:opcode_t ->
        unique:Unsigned.uint64 ->
        nodeid:Unsigned.uint64 ->
        uid:Unsigned.uint32 ->
        gid:Unsigned.uint32 ->
        pid:Unsigned.uint32 -> count:int -> char Ctypes.CArray.t

      val make :
        opcode:opcode_t ->
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

    module type Init = sig
      module Struct : Types_7_8_struct
      module Types : Types_7_8_in with module Struct := Struct

      module T = Types.Init
   end

    module type Open = sig
      module Struct : Types_7_8_struct
      module Types : Types_7_8_in with module Struct := Struct
      module T = Types.Open

   end

    module type Read = sig
      module Struct : Types_7_8_struct
      module Types : Types_7_8_in with module Struct := Struct
      module T = Types.Read

   end

    module type Release = sig
      module Struct : Types_7_8_struct
      module Types : Types_7_8_in with module Struct := Struct
      module T = Types.Release

   end

    module type Access = sig
      module Struct : Types_7_8_struct
      module Types : Types_7_8_in with module Struct := Struct
      module T = Types.Access

   end

    module type Forget = sig
      module Struct : Types_7_8_struct
      module Types : Types_7_8_in with module Struct := Struct
      module T = Types.Forget

   end

    module type Flush = sig
      module Struct : Types_7_8_struct
      module Types : Types_7_8_in with module Struct := Struct
      module T = Types.Flush

   end

    module type Create = sig
      module Struct : Types_7_8_struct
      module Types : Types_7_8_in with module Struct := Struct
      module T = Types.Create

      val name : unit Ctypes.ptr -> string
    end

    module type Mknod = sig
      module Struct : Types_7_8_struct
      module Types : Types_7_8_in with module Struct := Struct
      module T = Types.Mknod

      val name : unit Ctypes.ptr -> string
    end

    module type Mkdir = sig
      module Struct : Types_7_8_struct
      module Types : Types_7_8_in with module Struct := Struct
      module T = Types.Mkdir

      val name : unit Ctypes.ptr -> string
    end

    module type Rename = sig
      module Struct : Types_7_8_struct
      module Types : Types_7_8_in with module Struct := Struct
      module T = Types.Rename

      val source_destination : unit Ctypes.ptr -> string * string
    end

    module type Link = sig
      module Struct : Types_7_8_struct
      module Types : Types_7_8_in with module Struct := Struct
      module T = Types.Link

      val name : unit Ctypes.ptr -> string
    end

    module type Write = sig
      module Struct : Types_7_8_struct
      module Types : Types_7_8_in with module Struct := Struct
      module T = Types.Write

   end

    module type Fsync = sig
      module Struct : Types_7_8_struct
      module Types : Types_7_8_in with module Struct := Struct
      module T = Types.Fsync

   end

    module type Lk = sig
      module Struct : Types_7_8_struct
      module Types : Types_7_8_in with module Struct := Struct
      module T = Types.Lk

   end

    module type Interrupt = sig
      module Struct : Types_7_8_struct
      module Types : Types_7_8_in with module Struct := Struct
      module T = Types.Interrupt

    end
    module type Bmap = sig
      module Struct : Types_7_8_struct
      module Types : Types_7_8_in with module Struct := Struct
      module T = Types.Bmap

   end

    module type Setattr = sig
      module Struct : Types_7_8_struct
      module Types : Types_7_8_in with module Struct := Struct
      module T = Types.Setattr

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
          unknown : int32;
          (*atime_now : bool;
          mtime_now : bool;
            lockowner : bool;*)
        }

        val to_string_list : t -> string list

        val of_uint32 : Unsigned.uint32 -> t

        val to_uint32 : t -> Unsigned.uint32
      end

      type hdr_t

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
        hdr_t structure -> char Ctypes.CArray.t
    end

    module type Getxattr = sig
      module Struct : Types_7_8_struct
      module Types : Types_7_8_in with module Struct := Struct
      module T = Types.Getxattr

      type hdr_t

      val create_from_hdr :
        size:Unsigned.uint32 ->
        hdr_t structure -> char Ctypes.CArray.t
    end

    module type Setxattr = sig
      module Struct : Types_7_8_struct
      module Types : Types_7_8_in with module Struct := Struct
      module T = Types.Setxattr

      type hdr_t

      val create_from_hdr :
        size:Unsigned.uint32 ->
        flags:Unsigned.uint32 ->
        hdr_t structure -> char Ctypes.CArray.t
    end

    module type Message = sig
      type t
      type hdr_t
      type chan
      type (_,_) packet

      val parse :
        chan -> hdr_t Ctypes.structure -> int -> unit Ctypes.ptr
        -> (hdr_t, t) packet

      val describe : (hdr_t, t) packet -> string
    end
  end

  module Out = struct
    module type Hdr = sig
      type 'a request
      module Struct : Types_7_8_struct
      module Types : Types_7_8_out with module Struct := Struct
      module T = Types.Hdr

      val sz : int

      val packet :
        ?nerrno:int32 ->
        count:int -> 'a request -> char Ctypes.CArray.t

      val make : 'a request -> 'b Ctypes.typ -> 'b

      val set_size : char Ctypes.CArray.t -> int -> char Ctypes.CArray.t
    end

    module type Dirent = sig
      type 'a request
      type host_t
      module Struct : Types_7_8_struct
      module T = Struct.Dirent

      val struct_size : int

      val size : string -> int

      val of_list :
        host:host_t ->
        (int * int64 * string * Dirent.File_kind.t) list ->
        int -> int -> 'a request -> char Ctypes.CArray.t
    end

    module type Readlink = sig
      type 'a request
      val create :
        target:string -> 'a request -> char Ctypes.CArray.t
    end

    module type Read = sig
      type 'a request
      val allocate : size:int -> 'a request -> char Ctypes.CArray.t

      val finalize :
        size:int -> char Ctypes.CArray.t -> 'a request -> char Ctypes.CArray.t

      val describe : char Ctypes.CArray.t -> string
    end

    module type Write = sig
      type 'a request
      module Struct : Types_7_8_struct
      module Types : Types_7_8_out with module Struct := Struct
      module T = Types.Write

      val create : size:Unsigned.uint32 -> 'a request -> char Ctypes.CArray.t
    end

    module type Statfs = sig
      type 'a request
      module Struct : Types_7_8_struct
      module Types : Types_7_8_out with module Struct := Struct
      module T = Types.Statfs

      val create :
        blocks:Unsigned.uint64 ->
        bfree:Unsigned.uint64 ->
        bavail:Unsigned.uint64 ->
        files:Unsigned.uint64 ->
        ffree:Unsigned.uint64 ->
        bsize:Unsigned.uint32 ->
        namelen:Unsigned.uint32 ->
        frsize:Unsigned.uint32 ->
        'a request -> char Ctypes.CArray.t
    end

    module type Open_flags = sig
      type t = {
        direct_io  : bool;
        keep_cache : bool;
      }

      val zero : t

      val of_uint32 : Unsigned.uint32 -> t
      val to_uint32 : t -> Unsigned.uint32
      val to_string : t -> string
    end

    module type Open = sig
      type 'a request
      module Struct : Types_7_8_struct
      module Types : Types_7_8_out with module Struct := Struct
      module T = Types.Open

      module Flags : Open_flags

      val store :
        fh:Unsigned.uint64 ->
        open_flags:Flags.t ->
        T.t structure -> 'a -> unit

      val create :
        fh:Unsigned.uint64 ->
        open_flags:Flags.t ->
        'a request -> char Ctypes.CArray.t
    end

    module type Init = sig
      type 'a request
      module Struct : Types_7_8_struct
      module Types : Types_7_8_out with module Struct := Struct
      module T = Types.Init

      val create :
        major:Unsigned.uint32 ->
        minor:Unsigned.uint32 ->
        max_readahead:Unsigned.uint32 ->
        flags:Unsigned.uint32 ->
        max_write:Unsigned.uint32 ->
        'a request -> char Ctypes.CArray.t
      val describe : T.t structure -> string
    end

    module type Entry = sig
      type 'a request
      module Struct : Types_7_8_struct
      module Types : Types_7_8_out with module Struct := Struct
      module T = Types.Entry

      val store :
        nodeid:Unsigned.uint64 ->
        generation:Unsigned.uint64 ->
        entry_valid:Unsigned.uint64 ->
        attr_valid:Unsigned.uint64 ->
        entry_valid_nsec:Unsigned.uint32 ->
        attr_valid_nsec:Unsigned.uint32 ->
        store_attr:(Struct.Attr.t structure -> unit) ->
        T.t structure -> 'a -> unit

      val create :
        nodeid:Unsigned.uint64 ->
        generation:Unsigned.uint64 ->
        entry_valid:Unsigned.uint64 ->
        attr_valid:Unsigned.uint64 ->
        entry_valid_nsec:Unsigned.uint32 ->
        attr_valid_nsec:Unsigned.uint32 ->
        store_attr:(Struct.Attr.t structure -> unit) ->
        'a request -> char Ctypes.CArray.t

      type host_t
      val describe : host:host_t -> T.t structure -> string
    end

    module type Attr = sig
      type 'a request
      module Struct : Types_7_8_struct
      module Types : Types_7_8_out with module Struct := Struct
      module T = Types.Attr

      val create :
        attr_valid:Unsigned.uint64 ->
        attr_valid_nsec:Unsigned.uint32 ->
        store_attr:(Struct.Attr.t structure -> unit) ->
        'a request -> char Ctypes.CArray.t
    end

    module type Create_t = sig
      module Struct : Types_7_8_struct
      module Types : Types_7_8_out with module Struct := Struct
      type t
      val t : t structure Ctypes.typ

      val entry : (Types.Entry.t structure, t structure) Ctypes.field
      val open_ : (Types.Open.t structure, t structure) Ctypes.field
    end

    module type Create = sig
      type 'a request
      type host_t
      module Struct : Types_7_8_struct
      module Types : Types_7_8_out with module Struct := Struct
      module Entry : Entry
        with module Struct := Struct
        with module Types := Types
        with type 'a request := 'a request
        with type host_t := host_t
      module Open : Open with module Struct := Struct with module Types := Types with type 'a request := 'a request
      module T : Create_t with module Struct := Struct with module Types := Types

      val create :
        store_entry:(Entry.T.t structure -> 'a request -> unit)
        -> store_open:(Open.T.t structure -> 'a request -> unit)
        -> 'a request -> char Ctypes.CArray.t
    end

    module type Message = sig
      type t
      type in_hdr_t
      type hdr_t
      type (_,_) packet

      val deserialize :
        (in_hdr_t, 'a) packet -> int -> char Ctypes.ptr
        -> (hdr_t, t) packet

      val describe : ('a,t) packet -> string
    end
  end
end

module type Signatures_7_8_struct = sig
  module T : Types_7_8_struct
  type host_t

  module Kstatfs : Signatures_7_8.Struct.Kstatfs
    with module Types := T
  module File_lock : Signatures_7_8.Struct.File_lock
    with module Types := T
  module Attr : Signatures_7_8.Struct.Attr
    with module Types := T
    with type host_t := host_t
end


module type Signatures_7_8_in = sig
  module Struct : Types_7_8_struct
  module T : Types_7_8_in with module Struct := Struct

  type chan
  type ('h, 'b) packet

  module Opcode : Signatures_7_8.In.Opcode
    with module Struct := Struct
    with module Types := T
  module Hdr : Signatures_7_8.In.Hdr with type opcode_t := Opcode.t
    with module Struct := Struct
    with module Types := T
  module Init : Signatures_7_8.In.Init
    with module Struct := Struct
    with module Types := T
  module Open : Signatures_7_8.In.Open
    with module Struct := Struct
    with module Types := T
  module Read : Signatures_7_8.In.Read
    with module Struct := Struct
    with module Types := T
  module Release : Signatures_7_8.In.Release
    with module Struct := Struct
    with module Types := T
  module Access : Signatures_7_8.In.Access
    with module Struct := Struct
    with module Types := T
  module Forget : Signatures_7_8.In.Forget
    with module Struct := Struct
    with module Types := T
  module Flush : Signatures_7_8.In.Flush
    with module Struct := Struct
    with module Types := T
  module Create : Signatures_7_8.In.Create
    with module Struct := Struct
    with module Types := T
  module Mknod : Signatures_7_8.In.Mknod
    with module Struct := Struct
    with module Types := T
  module Mkdir : Signatures_7_8.In.Mkdir
    with module Struct := Struct
    with module Types := T
  module Rename : Signatures_7_8.In.Rename
    with module Struct := Struct
    with module Types := T
  module Link : Signatures_7_8.In.Link
    with module Struct := Struct
    with module Types := T
  module Write : Signatures_7_8.In.Write
    with module Struct := Struct
    with module Types := T
  module Fsync : Signatures_7_8.In.Fsync
    with module Struct := Struct
    with module Types := T
  module Lk : Signatures_7_8.In.Lk
    with module Struct := Struct
    with module Types := T
  module Interrupt : Signatures_7_8.In.Interrupt
    with module Struct := Struct
    with module Types := T
  module Bmap : Signatures_7_8.In.Bmap
    with module Struct := Struct
    with module Types := T
  module Setattr : Signatures_7_8.In.Setattr with type hdr_t := Hdr.T.t
    with module Struct := Struct
    with module Types := T
  module Getxattr : Signatures_7_8.In.Getxattr with type hdr_t := Hdr.T.t
    with module Struct := Struct
    with module Types := T
  module Setxattr : Signatures_7_8.In.Setxattr with type hdr_t := Hdr.T.t
    with module Struct := Struct
    with module Types := T
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
      | Getxattr of Getxattr.T.t Ctypes.structure * string
      | Setxattr of Setxattr.T.t Ctypes.structure * string
      | Listxattr of Getxattr.T.t Ctypes.structure
      | Removexattr of string
      | Access of Access.T.t Ctypes.structure
      | Forget of Forget.T.t Ctypes.structure
      | Readlink
      | Open of Open.T.t Ctypes.structure
      | Read of Read.T.t Ctypes.structure
      | Write of Write.T.t Ctypes.structure * char Ctypes.ptr
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
    include Signatures_7_8.In.Message with type t := t
                     and type hdr_t := Hdr.T.t
                     and type chan := chan
                     and type ('h, 'b) packet := ('h, 'b) packet
  end
end

module type Signatures_7_8_out = sig
  module Struct : Types_7_8_struct
  module In : Types_7_8_in with module Struct := Struct
  module T : Types_7_8_out with module Struct := Struct
  type 'a request
  type host_t
  type ('h, 'b) packet

  module Hdr : Signatures_7_8.Out.Hdr
    with module Struct := Struct
     and module Types := T
     and type 'a request := 'a request
  module Dirent : Signatures_7_8.Out.Dirent
    with module Struct := Struct
    and type 'a request := 'a request
    and type host_t := host_t
  module Readlink : Signatures_7_8.Out.Readlink
    with type 'a request := 'a request
  module Read : Signatures_7_8.Out.Read
    with type 'a request := 'a request
  module Write : Signatures_7_8.Out.Write
    with module Struct := Struct
     and module Types := T
     and type 'a request := 'a request
  module Statfs : Signatures_7_8.Out.Statfs
    with module Struct := Struct
     and module Types := T
     and type 'a request := 'a request
  module Open : Signatures_7_8.Out.Open
    with module Struct := Struct
     and module Types := T
     and type 'a request := 'a request
  module Init : Signatures_7_8.Out.Init
    with module Struct := Struct
     and module Types := T
     and type 'a request := 'a request
  module Entry : Signatures_7_8.Out.Entry
    with module Struct := Struct
     and module Types := T
     and type 'a request := 'a request
     and type host_t := host_t
  module Attr : Signatures_7_8.Out.Attr
    with module Struct := Struct
     and module Types := T
     and type 'a request := 'a request
  module Create : Signatures_7_8.Out.Create
    with module Struct := Struct
     and module Types := T
     and type 'a request := 'a request
     and type host_t := host_t
     and module Open := Open
     and module Entry := Entry
  module Message : sig
    type t =
      | Init    of Init.T.t  Ctypes.structure
      | Getattr of Attr.T.t  Ctypes.structure
      | Lookup  of Entry.T.t Ctypes.structure
      | Opendir of Open.T.t  Ctypes.structure
      | Readdir of char Ctypes.CArray.t
      | Releasedir
      | Fsyncdir (* TODO: do *)
      | Rmdir
      | Mkdir   of Entry.T.t Ctypes.structure
      | Getxattr (* TODO: do *)
      | Setxattr (* TODO: do *)
      | Listxattr (* TODO: do *)
      | Removexattr (* TODO: do *)
      | Access
      | Forget (* TODO: should never happen? *)
      | Readlink of string
      | Open     of Open.T.t           Ctypes.structure
      | Read     of char Ctypes.CArray.t
      | Write    of Write.T.t          Ctypes.structure
      | Statfs   of Struct.Kstatfs.t Ctypes.structure
      | Flush
      | Release
      | Fsync
      | Unlink
      | Create   of Entry.T.t Ctypes.structure * Open.T.t Ctypes.structure
      | Mknod    of Entry.T.t Ctypes.structure
      | Setattr  of Attr.T.t  Ctypes.structure
      | Link     of Entry.T.t Ctypes.structure
      | Symlink  of Entry.T.t Ctypes.structure
      | Rename
      | Getlk (* TODO: do *)
      | Setlk (* TODO: do *)
      | Setlkw (* TODO: do *)
      | Interrupt (* TODO: do *)
      | Bmap (* TODO: do *)
      | Destroy
      | Other    of In.Opcode.t
      | Unknown  of int32 * int * unit Ctypes.ptr
    include Signatures_7_8.Out.Message
      with type t := t
       and type in_hdr_t := In.Hdr.t
       and type hdr_t := T.Hdr.t
       and type ('h, 'b) packet := ('h, 'b) packet
  end
end
