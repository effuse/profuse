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
