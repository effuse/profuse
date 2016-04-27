(*
 * Copyright (c) 2016 Jeremy Yallop <yallop@gmail.com>
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

  (* Build the types for FUSE 7.8, but don't seal the structs *)
  module Version_7_8 = Profuse_types_7_8.C_compatible
      (struct
        include F
        let seal _ = ()
      end)

  (* Extend and seal the structs *)
  module Struct = struct
    module V_7_8 = Version_7_8.Struct
    module Kstatfs   = struct include V_7_8.Kstatfs   let () = seal t end
    module File_lock = struct include V_7_8.File_lock let () = seal t end
    module Dirent    = struct include V_7_8.Dirent    let () = seal t end
    module Attr      = struct
      include V_7_8.Attr
      let blksize     = "blksize"   -:* uint32_t
      let () = seal t
    end
  end

  module Out = struct
    module V_7_8 = Version_7_8.Out
    module Hdr      = struct include V_7_8.Hdr      let () = seal t end
    module Write    = struct include V_7_8.Write    let () = seal t end
    module Open     = struct include V_7_8.Open     let () = seal t end
    module Init     = struct include V_7_8.Init     let () = seal t end
    module Entry    = struct include V_7_8.Entry    let () = seal t end
    module Attr     = struct include V_7_8.Attr     let () = seal t end
    module Statfs   = struct include V_7_8.Statfs   let () = seal t end
    module Getxattr = struct include V_7_8.Getxattr let () = seal t end
    module Lk       = struct include V_7_8.Lk       let () = seal t end
    module Bmap     = struct include V_7_8.Bmap     let () = seal t end
  end

  module In = struct
    module V_7_8 = Version_7_8.In
    module Opcode = V_7_8.Opcode
    module Hdr       = struct include V_7_8.Hdr       let () = seal t end
    module Release   = struct include V_7_8.Release   let () = seal t end
    module Access    = struct include V_7_8.Access    let () = seal t end
    module Forget    = struct include V_7_8.Forget    let () = seal t end
    module Flush     = struct include V_7_8.Flush     let () = seal t end
    module Mknod     = struct include V_7_8.Mknod     let () = seal t end
    module Mkdir     = struct include V_7_8.Mkdir     let () = seal t end
    module Rename    = struct include V_7_8.Rename    let () = seal t end
    module Link      = struct include V_7_8.Link      let () = seal t end
    module Fsync     = struct include V_7_8.Fsync     let () = seal t end
    module Interrupt = struct include V_7_8.Interrupt let () = seal t end
    module Bmap      = struct include V_7_8.Bmap      let () = seal t end
    module Getxattr  = struct include V_7_8.Getxattr  let () = seal t end
    module Setxattr  = struct include V_7_8.Setxattr  let () = seal t end
    module Init      = struct
      module Flags =
      struct
        include V_7_8.Init.Flags
        let fuse_file_ops = constant "FUSE_FILE_OPS" t
        let fuse_atomic_o_trunc = constant "FUSE_ATOMIC_O_TRUNC" t
        let fuse_big_writes = constant "FUSE_BIG_WRITES" t
      end
      include (V_7_8.Init
               : module type of V_7_8.Init with module Flags := Flags)
      let () = seal t
    end
    module Read      = struct
      module Flags =
      struct
        let t = uint32_t
        let fuse_read_lockowner = constant "FUSE_READ_LOCKOWNER" t
      end
      include V_7_8.Read
      let read_flags = "read_flags" -:* uint32_t
      let lock_owner = "lock_owner" -:* uint64_t
      let flags      = "flags"      -:* uint32_t
      let () = seal t
    end
    module Write     = struct
      module Flags =
      struct
        let t = uint32_t
        let fuse_write_cache = constant "FUSE_WRITE_CACHE" t
        let fuse_write_lockowner = constant "FUSE_WRITE_LOCKOWNER" t
      end
      include V_7_8.Write
      let lock_owner = "lock_owner" -:* uint64_t
      let flags      = "flags"      -:* uint32_t
      let () = seal t
    end
    module Lk        = struct
      module Flags =
      struct
        let t = uint32_t
        let fuse_lk_flock = constant "FUSE_LK_FLOCK" t
      end
      include V_7_8.Lk
      let lk_flags   = "lk_flags"   -:* uint32_t
      let () = seal t
    end
    module Setattr   = struct
      module Valid =
      struct
        include V_7_8.Setattr.Valid
        let fattr_atime_now = constant "FATTR_ATIME_NOW" t
        let fattr_mtime_now = constant "FATTR_MTIME_NOW" t
        let fattr_lockowner = constant "FATTR_LOCKOWNER" t
      end
      include (V_7_8.Setattr
               : module type of V_7_8.Setattr with module Valid := Valid)
      let lock_owner     = "lock_owner" -:* uint64_t
      let () = seal t
    end
    module Getattr   = struct
      module Flags =
      struct
        let t = uint32_t
        let fuse_getattr_fh = constant "FUSE_GETATTR_FH" t
      end
      type t
      let t : t structure typ = structure "fuse_getattr_in"
      let ( -:* ) s x = field t s x
      let getattr_flags   = "getattr_flags" -:* uint32_t
      let fh              = "fh"            -:* uint64_t
      let () = seal t
    end
  end
end

(* Forwards-incompatible type and constant bindings *)
module C_incompatible(F: Cstubs.Types.TYPE) = struct
  (* Build the types for FUSE 7.8, but don't seal the structs *)
  module Version_7_8 = Profuse_types_7_8.C_incompatible
      (struct
        include F
        let seal _ = ()
      end)

  include Version_7_8
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
