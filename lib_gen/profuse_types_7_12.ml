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

module C(F: Cstubs.Types.TYPE) = struct
  open F
  type 'a structure = 'a Ctypes_static.structure

  (* Build the types for FUSE 7.11, but don't seal the structs *)
  module Version_7_11 = Profuse_types_7_11.C_compatible
      (struct
        include F
        let seal _ = ()
      end)

  (* Extend and seal the structs *)
  module Struct = struct
    module V_7_11 = Version_7_11.Struct
    module Kstatfs   = struct include V_7_11.Kstatfs   let () = seal t end
    module File_lock = struct include V_7_11.File_lock let () = seal t end
    module Dirent    = struct include V_7_11.Dirent    let () = seal t end
    module Attr      = struct include V_7_11.Attr      let () = seal t end
  end

  module Out = struct
    module V_7_11 = Version_7_11.Out
    module Write              = struct include V_7_11.Write              let () = seal t end
    module Open               = struct include V_7_11.Open               let () = seal t end
    module Init               = struct include V_7_11.Init               let () = seal t end
    module Entry              = struct include V_7_11.Entry              let () = seal t end
    module Attr               = struct include V_7_11.Attr               let () = seal t end
    module Statfs             = struct include V_7_11.Statfs             let () = seal t end
    module Getxattr           = struct include V_7_11.Getxattr           let () = seal t end
    module Lk                 = struct include V_7_11.Lk                 let () = seal t end
    module Bmap               = struct include V_7_11.Bmap               let () = seal t end
    module Ioctl              = struct include V_7_11.Ioctl              let () = seal t end
    module Poll               = struct include V_7_11.Poll               let () = seal t end
    module Notify_poll_wakeup = struct include V_7_11.Notify_poll_wakeup let () = seal t end
    module Hdr                = struct
      module Notify_code = struct
        include V_7_11.Hdr.Notify_code
        let fuse_notify_inval_inode = constant "FUSE_NOTIFY_INVAL_INODE" t
        let fuse_notify_inval_entry = constant "FUSE_NOTIFY_INVAL_ENTRY" t
      end
      include (V_7_11.Hdr
                 : module type of V_7_11.Hdr with module Notify_code := Notify_code)
      let () = seal t
    end
    module Cuse_init          = struct
      type t
      let t : t structure typ = structure "cuse_init_out"
      let ( -:* ) s x = field t s x
      let major     = "major"     -:* uint32_t
      let minor     = "minor"     -:* uint32_t
      let unused    = "unused"    -:* uint32_t
      let flags     = "flags"     -:* uint32_t
      let max_read  = "max_read"  -:* uint32_t
      let max_write = "max_write" -:* uint32_t
      let dev_major = "dev_major" -:* uint32_t
      let dev_minor = "dev_minor" -:* uint32_t
      let spare     = "spare"     -:* array 10 uint32_t
      let () = seal t
    end
    module Notify_inval_inode = struct
      type t
      let t : t structure typ = structure "fuse_notify_inval_inode_out"
      let ( -:* ) s x = field t s x
      let ino       = "ino"       -:* uint64_t
      let off       = "off"       -:* int64_t
      let len       = "len"       -:* int64_t
      let () = seal t
    end
    module Notify_inval_entry = struct
      type t
      let t : t structure typ = structure "fuse_notify_inval_entry_out"
      let ( -:* ) s x = field t s x
      let parent    = "parent"    -:* uint64_t
      let namelen   = "namelen"   -:* uint32_t
      let () = seal t
    end
  end

  module In = struct
    module V_7_11 = Version_7_11.In
    module Opcode = V_7_11.Opcode
    module Hdr       = struct include V_7_11.Hdr       let () = seal t end
    module Release   = struct include V_7_11.Release   let () = seal t end
    module Access    = struct include V_7_11.Access    let () = seal t end
    module Forget    = struct include V_7_11.Forget    let () = seal t end
    module Flush     = struct include V_7_11.Flush     let () = seal t end
    module Rename    = struct include V_7_11.Rename    let () = seal t end
    module Link      = struct include V_7_11.Link      let () = seal t end
    module Fsync     = struct include V_7_11.Fsync     let () = seal t end
    module Interrupt = struct include V_7_11.Interrupt let () = seal t end
    module Bmap      = struct include V_7_11.Bmap      let () = seal t end
    module Getxattr  = struct include V_7_11.Getxattr  let () = seal t end
    module Setxattr  = struct include V_7_11.Setxattr  let () = seal t end
    module Read      = struct include V_7_11.Read      let () = seal t end
    module Write     = struct include V_7_11.Write     let () = seal t end
    module Lk        = struct include V_7_11.Lk        let () = seal t end
    module Setattr   = struct include V_7_11.Setattr   let () = seal t end
    module Getattr   = struct include V_7_11.Getattr   let () = seal t end
    module Ioctl     = struct include V_7_11.Ioctl     let () = seal t end
    module Poll      = struct include V_7_11.Poll      let () = seal t end
    module Init      = struct
      module Flags = struct
        include V_7_11.Init.Flags
        let fuse_dont_mask = constant "FUSE_DONT_MASK" t
      end
      include (V_7_11.Init
               : module type of V_7_11.Init with module Flags := Flags)
      let () = seal t
    end
    module Mknod     = struct
      include V_7_11.Mknod
      let umask     = "umask"     -:* uint32_t
      let () = seal t
    end
    module Mkdir     = struct
      include V_7_11.Mkdir
      let umask     = "umask"     -:* uint32_t
      let () = seal t
    end
    module Open = struct
      (* The 'mode' field from FUSE 7.8-7.11 was renamed 'unused' *)
      type t
      let t : t structure typ = structure "fuse_open_in"
      let ( -:* ) s x = field t s x
      let flags     = "flags"     -:* uint32_t
      let () = seal t
    end
    module Create    = struct
      type t
      let t : t structure typ = structure "fuse_create_in"
      let ( -:* ) s x = field t s x
      let flags     = "flags"     -:* uint32_t
      let mode      = "mode"      -:* uint32_t
      let umask     = "umask"     -:* uint32_t
      let () = seal t
    end
    module Cuse_init = struct
      type t
      let t : t structure typ = structure "cuse_init_in"
      let ( -:* ) s x = field t s x
      let major     = "major"     -:* uint32_t
      let minor     = "minor"     -:* uint32_t
      let unused    = "unused"    -:* uint32_t
      let flags     = "flags"     -:* uint32_t
      let () = seal t
    end
  end
end
