0(*
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

  (* Build the types for FUSE 7.14, but don't seal the structs *)
  module Version_7_14 = Profuse_types_7_14.C
      (struct
        include F
        let seal _ = ()
      end)

  (* Extend and seal the structs *)
  module Struct = struct
    module V_7_14 = Version_7_14.Struct
    module Kstatfs   = struct include V_7_14.Kstatfs   let () = seal t end
    module File_lock = struct include V_7_14.File_lock let () = seal t end
    module Dirent    = struct include V_7_14.Dirent    let () = seal t end
    module Attr      = struct include V_7_14.Attr      let () = seal t end
  end

  module Out = struct
    module V_7_14 = Version_7_14.Out
    module Write              = struct include V_7_14.Write              let () = seal t end
    module Open               = struct include V_7_14.Open               let () = seal t end
    module Entry              = struct include V_7_14.Entry              let () = seal t end
    module Attr               = struct include V_7_14.Attr               let () = seal t end
    module Statfs             = struct include V_7_14.Statfs             let () = seal t end
    module Getxattr           = struct include V_7_14.Getxattr           let () = seal t end
    module Lk                 = struct include V_7_14.Lk                 let () = seal t end
    module Bmap               = struct include V_7_14.Bmap               let () = seal t end
    module Init               = struct include V_7_14.Init               let () = seal t end
    module Ioctl              = struct include V_7_14.Ioctl              let () = seal t end
    module Poll               = struct include V_7_14.Poll               let () = seal t end
    module Notify_poll_wakeup = struct include V_7_14.Notify_poll_wakeup let () = seal t end
    module Cuse_init          = struct include V_7_14.Cuse_init          let () = seal t end
    module Notify_inval_inode = struct include V_7_14.Notify_inval_inode let () = seal t end
    module Notify_inval_entry = struct include V_7_14.Notify_inval_entry let () = seal t end
    module Hdr                = struct
      module Notify_code = struct
        include V_7_14.Hdr.Notify_code
        let fuse_notify_store = constant "FUSE_NOTIFY_STORE" t
        let fuse_notify_retrieve = constant "FUSE_NOTIFY_RETRIEVE" t
      end
      include (V_7_14.Hdr
                 : module type of V_7_14.Hdr with module Notify_code := Notify_code)
      let () = seal t
    end
    module Notify_store       = struct
      type t
      let t : t structure typ = structure "fuse_notify_store_out"
      let ( -:* ) s x = field t s x
      let nodeid        = "nodeid"        -:* uint64_t
      let offset        = "offset"        -:* uint64_t
      let size          = "size"          -:* uint32_t
      let padding       = "padding"       -:* uint32_t
      let () = seal t
    end

    module Notify_retrieve    = struct
      type t
      let t : t structure typ = structure "fuse_notify_retrieve_out"
      let ( -:* ) s x = field t s x
      let notify_unique = "notify_unique" -:* uint64_t
      let nodeid        = "nodeid"        -:* uint64_t
      let offset        = "offset"        -:* uint64_t
      let size          = "size"          -:* uint32_t
      let padding       = "padding"       -:* uint32_t
      let () = seal t
    end
  end

  module In = struct
    module V_7_14 = Version_7_14.In
    module Hdr       = struct include V_7_14.Hdr       let () = seal t end
    module Init      = struct include V_7_14.Init      let () = seal t end
    module Open      = struct include V_7_14.Open      let () = seal t end
    module Release   = struct include V_7_14.Release   let () = seal t end
    module Access    = struct include V_7_14.Access    let () = seal t end
    module Forget    = struct include V_7_14.Forget    let () = seal t end
    module Flush     = struct include V_7_14.Flush     let () = seal t end
    module Create    = struct include V_7_14.Create    let () = seal t end
    module Mknod     = struct include V_7_14.Mknod     let () = seal t end
    module Mkdir     = struct include V_7_14.Mkdir     let () = seal t end
    module Rename    = struct include V_7_14.Rename    let () = seal t end
    module Link      = struct include V_7_14.Link      let () = seal t end
    module Fsync     = struct include V_7_14.Fsync     let () = seal t end
    module Interrupt = struct include V_7_14.Interrupt let () = seal t end
    module Bmap      = struct include V_7_14.Bmap      let () = seal t end
    module Getxattr  = struct include V_7_14.Getxattr  let () = seal t end
    module Setxattr  = struct include V_7_14.Setxattr  let () = seal t end
    module Read      = struct include V_7_14.Read      let () = seal t end
    module Write     = struct include V_7_14.Write     let () = seal t end
    module Lk        = struct include V_7_14.Lk        let () = seal t end
    module Setattr   = struct include V_7_14.Setattr   let () = seal t end
    module Getattr   = struct include V_7_14.Getattr   let () = seal t end
    module Ioctl     = struct include V_7_14.Ioctl     let () = seal t end
    module Poll      = struct include V_7_14.Poll      let () = seal t end
    module Cuse_init = struct include V_7_14.Cuse_init let () = seal t end
    module Opcode =
    struct
      include (V_7_14.Opcode
               : module type of V_7_14.Opcode
               with type t := V_7_14.Opcode.t)

      let fuse_notify_reply = constant "FUSE_NOTIFY_REPLY" uint32_t

      type t = [ V_7_14.Opcode.t
               | `FUSE_NOTIFY_REPLY ]

      let enum_values =
        (`FUSE_NOTIFY_REPLY, fuse_notify_reply) ::
        (V_7_14.Opcode.enum_values :> (t * _) list)
    end
    module Notify_retrieve = struct
      type t
      let t : t structure typ = structure "fuse_notify_retrieve_in"
      let ( -:* ) s x = field t s x
      let offset        = "offset"        -:* uint64_t
      let size          = "size"          -:* uint32_t
      let () = seal t
    end
  end
end
