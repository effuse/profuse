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

(* Forwards-compatible type and constant bindings. *)
module C_compatible(F: Cstubs.Types.TYPE) = struct
  open F
  type 'a structure = 'a Ctypes_static.structure

  module Struct = struct
    module Kstatfs = struct
      type t
      let t : t structure typ = structure "fuse_kstatfs"
      let ( -:* ) s x = field t s x
      let blocks  = "blocks"  -:* uint64_t
      let bfree   = "bfree"   -:* uint64_t
      let bavail  = "bavail"  -:* uint64_t
      let files   = "files"   -:* uint64_t
      let ffree   = "ffree"   -:* uint64_t
      let bsize   = "bsize"   -:* uint32_t
      let namelen = "namelen" -:* uint32_t
      let frsize  = "frsize"  -:* uint32_t
      let () = seal t
    end

    module File_lock = struct
      type t
      let t : t structure typ = structure "fuse_file_lock"
      let ( -:* ) s x = field t s x
      let start  = "start" -:* uint64_t
      let end_   = "end"   -:* uint64_t
      let type_  = "type"  -:* uint32_t
      let pid    = "pid"   -:* uint32_t (* tgid *)
      let () = seal t
    end

    module Attr = struct
      type t
      let t : t structure typ = structure "fuse_attr"
      let ( -:* ) s x = field t s x
      let ino       = "ino"       -:* uint64_t
      let size      = "size"      -:* uint64_t
      let blocks    = "blocks"    -:* uint64_t
      let atime     = "atime"     -:* uint64_t
      let mtime     = "mtime"     -:* uint64_t
      let ctime     = "ctime"     -:* uint64_t
      let atimensec = "atimensec" -:* uint32_t
      let mtimensec = "mtimensec" -:* uint32_t
      let ctimensec = "ctimensec" -:* uint32_t
      let mode      = "mode"      -:* uint32_t
      let nlink     = "nlink"     -:* uint32_t
      let uid       = "uid"       -:* uint32_t
      let gid       = "gid"       -:* uint32_t
      let rdev      = "rdev"      -:* uint32_t
      let () = seal t
    end

    module Dirent = struct
      type t
      let t : t structure typ = structure "fuse_dirent"
      let ( -:* ) s x = field t s x
      let ino     = "ino"     -:* uint64_t
      let off     = "off"     -:* uint64_t
      let namelen = "namelen" -:* uint32_t
      let typ     = "type"    -:* uint32_t
      let name    = "name"    -:* array 0 char
      let () = seal t
    end

  end

  module Out = struct
    module Hdr = struct
      type t
      let t : t structure typ = structure "fuse_out_header"
      let ( -:* ) s x = field t s x
      let len    = "len"    -:* uint32_t
      let error  = "error"  -:* int32_t
      let unique = "unique" -:* uint64_t
      let () = seal t
    end

    module Write = struct
      type t
      let t : t structure typ = structure "fuse_write_out"
      let ( -:* ) s x = field t s x
      let size    = "size"    -:* uint32_t
      let () = seal t
    end

    module Open = struct
      module Flags =
      struct
        type t = [ `FOPEN_DIRECT_IO
                 | `FOPEN_KEEP_CACHE ]
        let t = uint32_t
        let fopen_direct_io = constant "FOPEN_DIRECT_IO" t
        let fopen_keep_cache = constant "FOPEN_KEEP_CACHE" t
        let enum_values : (t * _) list =
          [ `FOPEN_DIRECT_IO, fopen_direct_io;
            `FOPEN_KEEP_CACHE, fopen_keep_cache ]
      end

      type t
      let t : t structure typ = structure "fuse_open_out"
      let ( -:* ) s x = field t s x
      let fh         = "fh"         -:* uint64_t
      let open_flags = "open_flags" -:* uint32_t
      let () = seal t
    end

    module Init = struct
      type t
      let t : t structure typ = structure "fuse_init_out"
      let ( -:* ) s x = field t s x
      let major         = "major"         -:* uint32_t
      let minor         = "minor"         -:* uint32_t
      let max_readahead = "max_readahead" -:* uint32_t
      let flags         = "flags"         -:* uint32_t
      let max_write     = "max_write"     -:* uint32_t
      let () = seal t
    end

    module Entry = struct
      type t
      let t : t structure typ = structure "fuse_entry_out"
      let ( -:* ) s x = field t s x
      let nodeid           = "nodeid"           -:* uint64_t (* inode *)
      let generation       = "generation"       -:* uint64_t
      (* (inode, generation) must be unique over fs life *)
      let entry_valid      = "entry_valid"      -:* uint64_t
      (* name cache timeout *)
      let attr_valid       = "attr_valid"       -:* uint64_t
      (* attribute cache timeout *)
      let entry_valid_nsec = "entry_valid_nsec" -:* uint32_t
      let attr_valid_nsec  = "attr_valid_nsec"  -:* uint32_t
      let attr             = "attr"             -:* Struct.Attr.t
      let () = seal t
    end

    module Attr = struct
      type t
      let t : t structure typ = structure "fuse_attr_out"
      let ( -:* ) s x = field t s x
      let attr_valid      = "attr_valid"      -:* uint64_t
      let attr_valid_nsec = "attr_valid_nsec" -:* uint32_t
      let attr            = "attr"            -:* Struct.Attr.t
      let () = seal t
    end

    module Statfs = struct
      type t
      let t : t structure typ = structure "fuse_statfs_out"
      let ( -:* ) s x = field t s x
      let st = "st" -:* Struct.Kstatfs.t
      let () = seal t
    end

    module Getxattr = struct
      type t
      let t : t structure typ = structure "fuse_getxattr_out"
      let ( -:* ) s x = field t s x
      let size = "size" -:* uint32_t
      let () = seal t
    end

    module Lk = struct
      type t
      let t : t structure typ = structure "fuse_lk_out"
      let ( -:* ) s x = field t s x
      let lk = "lk" -:* Struct.File_lock.t
      let () = seal t
    end

    module Bmap = struct
      type t
      let t : t structure typ = structure "fuse_bmap_out"
      let ( -:* ) s x = field t s x
      let block = "block" -:* uint64_t
      let () = seal t
    end
  end

  module In = struct

    module Opcode = struct
      type t =
        [ `FUSE_LOOKUP
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
        | `Unknown of int32 ]

      let t = uint32_t

      let fuse_lookup = constant "FUSE_LOOKUP" t
      let fuse_forget = constant "FUSE_FORGET" t
      let fuse_getattr = constant "FUSE_GETATTR" t
      let fuse_setattr = constant "FUSE_SETATTR" t
      let fuse_readlink = constant "FUSE_READLINK" t
      let fuse_symlink = constant "FUSE_SYMLINK" t
      let fuse_mknod = constant "FUSE_MKNOD" t
      let fuse_mkdir = constant "FUSE_MKDIR" t
      let fuse_unlink = constant "FUSE_UNLINK" t
      let fuse_rmdir = constant "FUSE_RMDIR" t
      let fuse_rename = constant "FUSE_RENAME" t
      let fuse_link = constant "FUSE_LINK" t
      let fuse_open = constant "FUSE_OPEN" t
      let fuse_read = constant "FUSE_READ" t
      let fuse_write = constant "FUSE_WRITE" t
      let fuse_statfs = constant "FUSE_STATFS" t
      let fuse_release = constant "FUSE_RELEASE" t
      let fuse_fsync = constant "FUSE_FSYNC" t
      let fuse_setxattr = constant "FUSE_SETXATTR" t
      let fuse_getxattr = constant "FUSE_GETXATTR" t
      let fuse_listxattr = constant "FUSE_LISTXATTR" t
      let fuse_removexattr = constant "FUSE_REMOVEXATTR" t
      let fuse_flush = constant "FUSE_FLUSH" t
      let fuse_init = constant "FUSE_INIT" t
      let fuse_opendir = constant "FUSE_OPENDIR" t
      let fuse_readdir = constant "FUSE_READDIR" t
      let fuse_releasedir = constant "FUSE_RELEASEDIR" t
      let fuse_fsyncdir = constant "FUSE_FSYNCDIR" t
      let fuse_getlk = constant "FUSE_GETLK" t
      let fuse_setlk = constant "FUSE_SETLK" t
      let fuse_setlkw = constant "FUSE_SETLKW" t
      let fuse_access = constant "FUSE_ACCESS" t
      let fuse_create = constant "FUSE_CREATE" t
      let fuse_interrupt = constant "FUSE_INTERRUPT" t
      let fuse_bmap = constant "FUSE_BMAP" t
      let fuse_destroy = constant "FUSE_DESTROY" t

      let enum_values : (t * _) list = 
        [ `FUSE_LOOKUP, fuse_lookup;
          `FUSE_FORGET, fuse_forget;
          `FUSE_GETATTR, fuse_getattr;
          `FUSE_SETATTR, fuse_setattr;
          `FUSE_READLINK, fuse_readlink;
          `FUSE_SYMLINK, fuse_symlink;
          `FUSE_MKNOD, fuse_mknod;
          `FUSE_MKDIR, fuse_mkdir;
          `FUSE_UNLINK, fuse_unlink;
          `FUSE_RMDIR, fuse_rmdir;
          `FUSE_RENAME, fuse_rename;
          `FUSE_LINK, fuse_link;
          `FUSE_OPEN, fuse_open;
          `FUSE_READ, fuse_read;
          `FUSE_WRITE, fuse_write;
          `FUSE_STATFS, fuse_statfs;
          `FUSE_RELEASE, fuse_release;
          `FUSE_FSYNC, fuse_fsync;
          `FUSE_SETXATTR, fuse_setxattr;
          `FUSE_GETXATTR, fuse_getxattr;
          `FUSE_LISTXATTR, fuse_listxattr;
          `FUSE_REMOVEXATTR, fuse_removexattr;
          `FUSE_FLUSH, fuse_flush;
          `FUSE_INIT, fuse_init;
          `FUSE_OPENDIR, fuse_opendir;
          `FUSE_READDIR, fuse_readdir;
          `FUSE_RELEASEDIR, fuse_releasedir;
          `FUSE_FSYNCDIR, fuse_fsyncdir;
          `FUSE_GETLK, fuse_getlk;
          `FUSE_SETLK, fuse_setlk;
          `FUSE_SETLKW, fuse_setlkw;
          `FUSE_ACCESS, fuse_access;
          `FUSE_CREATE, fuse_create;
          `FUSE_INTERRUPT, fuse_interrupt;
          `FUSE_BMAP, fuse_bmap;
          `FUSE_DESTROY, fuse_destroy;
        ]
    end

    module Hdr = struct
      type t
      let t : t structure typ = structure "fuse_in_header"
      let ( -:* ) s x = field t s x
      let len     = "len"     -:* uint32_t
      let opcode  = "opcode"  -:* uint32_t
      let unique  = "unique"  -:* uint64_t
      let nodeid  = "nodeid"  -:* uint64_t
      let uid     = "uid"     -:* uint32_t
      let gid     = "gid"     -:* uint32_t
      let pid     = "pid"     -:* uint32_t
      let () = seal t
    end

    module Init = struct
      module Flags =
      struct
        let t = uint32_t
        let async_read = constant "FUSE_ASYNC_READ" t
        let posix_locks = constant "FUSE_POSIX_LOCKS" t
      end
      type t
      let t : t structure typ = structure "fuse_init_in"
      let ( -:* ) s x = field t s x
      let major         = "major"         -:* uint32_t
      let minor         = "minor"         -:* uint32_t
      let max_readahead = "max_readahead" -:* uint32_t
      let flags         = "flags"         -:* uint32_t
      let () = seal t
    end

    module Read = struct
      type t
      let t : t structure typ = structure "fuse_read_in"
      let ( -:* ) s x = field t s x
      let fh      = "fh"      -:* uint64_t
      let offset  = "offset"  -:* uint64_t
      let size    = "size"    -:* uint32_t
      let () = seal t
    end

    module Release = struct
      type t
      let t : t structure typ = structure "fuse_release_in"
      let ( -:* ) s x = field t s x
      let fh            = "fh"            -:* uint64_t
      let flags         = "flags"         -:* uint32_t
      let release_flags = "release_flags" -:* uint32_t
      let lock_owner    = "lock_owner"    -:* uint64_t
      let () = seal t
    end

    module Access = struct
      type t
      let t : t structure typ = structure "fuse_access_in"
      let ( -:* ) s x = field t s x
      let mask    = "mask"    -:* uint32_t
      let () = seal t
    end

    module Forget = struct
      type t
      let t : t structure typ = structure "fuse_forget_in"
      let ( -:* ) s x = field t s x
      let nlookup = "nlookup" -:* uint64_t
      let () = seal t
    end

    module Flush = struct
      type t
      let t : t structure typ = structure "fuse_flush_in"
      let ( -:* ) s x = field t s x
      let fh         = "fh"         -:* uint64_t
      let lock_owner = "lock_owner" -:* uint64_t
      let () = seal t
    end

    module Mknod = struct
      type t
      let t : t structure typ = structure "fuse_mknod_in"
      let ( -:* ) s x = field t s x
      let mode = "mode" -:* uint32_t
      let rdev = "rdev" -:* uint32_t
      let () = seal t
    end

    module Mkdir = struct
      type t
      let t : t structure typ = structure "fuse_mkdir_in"
      let ( -:* ) s x = field t s x
      let mode    = "mode"    -:* uint32_t
      let () = seal t
    end

    module Rename = struct
      type t
      let t : t structure typ = structure "fuse_rename_in"
      let ( -:* ) s x = field t s x
      let newdir = "newdir" -:* uint64_t
      let () = seal t
    end

    module Link = struct
      type t
      let t : t structure typ = structure "fuse_link_in"
      let ( -:* ) s x = field t s x
      let oldnodeid = "oldnodeid" -:* uint64_t
      let () = seal t
    end

    module Write = struct
      type t
      let t : t structure typ = structure "fuse_write_in"
      let ( -:* ) s x = field t s x
      let fh          = "fh"          -:* uint64_t
      let offset      = "offset"      -:* uint64_t
      let size        = "size"        -:* uint32_t
      let write_flags = "write_flags" -:* uint32_t
      let () = seal t
    end

    module Fsync = struct
      type t
      let t : t structure typ = structure "fuse_fsync_in"
      let ( -:* ) s x = field t s x
      let fh          = "fh"          -:* uint64_t
      let fsync_flags = "fsync_flags" -:* uint32_t
      let () = seal t
    end

    module Lk = struct
      type t
      let t : t structure typ = structure "fuse_lk_in"
      let ( -:* ) s x = field t s x
      let fh    = "fh"    -:* int64_t
      let owner = "owner" -:* uint64_t
      let lk    = "lk"    -:* Struct.File_lock.t
      let () = seal t
    end

    module Interrupt = struct
      type t
      let t : t structure typ = structure "fuse_interrupt_in"
      let ( -:* ) s x = field t s x
      let unique = "unique" -:* uint64_t
      let () = seal t
    end

    module Bmap = struct
      type t
      let t : t structure typ = structure "fuse_bmap_in"
      let ( -:* ) s x = field t s x
      let block     = "block"     -:* uint64_t
      let blocksize = "blocksize" -:* uint32_t
      let () = seal t
    end

    module Setattr = struct

      module Valid = struct
        let t = uint32_t

        let fattr_mode = constant "FATTR_MODE" t
        let fattr_uid = constant "FATTR_UID" t
        let fattr_gid = constant "FATTR_GID" t
        let fattr_size = constant "FATTR_SIZE" t
        let fattr_atime = constant "FATTR_ATIME" t
        let fattr_mtime = constant "FATTR_MTIME" t
        let fattr_fh = constant "FATTR_FH" t
      end

      type t
      let t : t structure typ = structure "fuse_setattr_in"
      let ( -:* ) s x = field t s x
      let valid      = "valid"      -:* uint32_t
      let fh         = "fh"         -:* uint64_t
      let size       = "size"       -:* uint64_t
      let atime      = "atime"      -:* uint64_t
      let mtime      = "mtime"      -:* uint64_t
      let atimensec  = "atimensec"  -:* uint32_t
      let mtimensec  = "mtimensec"  -:* uint32_t
      let mode       = "mode"       -:* uint32_t
      let uid        = "uid"        -:* uint32_t
      let gid        = "gid"        -:* uint32_t
      let () = seal t
    end

    module Getxattr = struct
      type t
      let t : t structure typ = structure "fuse_getxattr_in"
      let ( -:* ) s x = field t s x
      let size = "size" -:* uint32_t
      let () = seal t
    end

    module Setxattr = struct
      type t
      let t : t structure typ = structure "fuse_setxattr_in"
      let ( -:* ) s x = field t s x
      let size  = "size"  -:* uint32_t
      let flags = "flags" -:* uint32_t
      let () = seal t
    end
  end
end

(* Forwards-incompatible type and constant bindings *)
module C_incompatible(F: Cstubs.Types.TYPE) = struct
  open F
  type 'a structure = 'a Ctypes_static.structure

  module In = struct
    module Open = struct
      type t
      let t : t structure typ = structure "fuse_open_in"
      let ( -:* ) s x = field t s x
      let flags = "flags" -:* uint32_t
      let mode  = "mode"  -:* uint32_t
      let () = seal t
    end

    module Create = struct
      type t = Open.t
      let t = Open.t
      let flags = Open.flags
      let mode = Open.mode
        (*
      let t : t structure typ = structure "fuse_open_in" (*"fuse_create_in"*)
      let ( -:* ) s x = field t s x
      let flags = "flags" -:* uint32_t
      let mode  = "mode"  -:* uint32_t
      let () = seal t*)
    end
  end
end

module C(F: Cstubs.Types.TYPE) = struct
  type 'a structure = 'a Ctypes_static.structure
  module Compatible = C_compatible(F)
  module Incompatible = C_incompatible(F)
  module Struct = Compatible.Struct
  module Out = Compatible.Out
  module In =
  struct
    include Compatible.In
    include Incompatible.In
  end
end
