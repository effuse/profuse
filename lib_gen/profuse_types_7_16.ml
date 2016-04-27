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

  (* Build the types for FUSE 7.15, but don't seal the structs *)
  module Version_7_15 = Profuse_types_7_15.C
      (struct
        include F
        let seal _ = ()
      end)

  (* Extend and seal the structs *)
  module Struct = struct
    module V_7_15 = Version_7_15.Struct
    module Kstatfs   = struct include V_7_15.Kstatfs   let () = seal t end
    module File_lock = struct include V_7_15.File_lock let () = seal t end
    module Dirent    = struct include V_7_15.Dirent    let () = seal t end
    module Attr      = struct include V_7_15.Attr      let () = seal t end
    module Forget_one = struct
      type t
      let t : t structure typ = structure "fuse_forget_one"
      let ( -:* ) s x = field t s x
      let nodeid        = "nodeid"        -:* uint64_t
      let nlookup       = "nlookup"       -:* uint64_t
      let () = seal t
    end
    module Ioctl_iovec = struct
      type t
      let t : t structure typ = structure "fuse_ioctl_iovec"
      let ( -:* ) s x = field t s x
      let base          = "base"          -:* uint64_t
      let len           = "len"           -:* uint64_t
      let () = seal t
    end
  end

  module Out = struct
    module V_7_15 = Version_7_15.Out
    module Hdr                = struct include V_7_15.Hdr                let () = seal t end
    module Write              = struct include V_7_15.Write              let () = seal t end
    module Open               = struct include V_7_15.Open               let () = seal t end
    module Init               = struct include V_7_15.Init               let () = seal t end
    module Entry              = struct include V_7_15.Entry              let () = seal t end
    module Attr               = struct include V_7_15.Attr               let () = seal t end
    module Statfs             = struct include V_7_15.Statfs             let () = seal t end
    module Getxattr           = struct include V_7_15.Getxattr           let () = seal t end
    module Lk                 = struct include V_7_15.Lk                 let () = seal t end
    module Bmap               = struct include V_7_15.Bmap               let () = seal t end
    module Ioctl              = struct include V_7_15.Ioctl              let () = seal t end
    module Poll               = struct include V_7_15.Poll               let () = seal t end
    module Notify_poll_wakeup = struct include V_7_15.Notify_poll_wakeup let () = seal t end
    module Cuse_init          = struct include V_7_15.Cuse_init          let () = seal t end
    module Notify_inval_inode = struct include V_7_15.Notify_inval_inode let () = seal t end
    module Notify_inval_entry = struct include V_7_15.Notify_inval_entry let () = seal t end
    module Notify_store       = struct include V_7_15.Notify_store       let () = seal t end
    module Notify_retrieve    = struct include V_7_15.Notify_retrieve    let () = seal t end
  end

  module In = struct
    module V_7_15 = Version_7_15.In
    module Opcode = V_7_15.Opcode
    module Hdr             = struct include V_7_15.Hdr             let () = seal t end
    module Init            = struct include V_7_15.Init            let () = seal t end
    module Open            = struct include V_7_15.Open            let () = seal t end
    module Release         = struct include V_7_15.Release         let () = seal t end
    module Access          = struct include V_7_15.Access          let () = seal t end
    module Forget          = struct include V_7_15.Forget          let () = seal t end
    module Flush           = struct include V_7_15.Flush           let () = seal t end
    module Create          = struct include V_7_15.Create          let () = seal t end
    module Mknod           = struct include V_7_15.Mknod           let () = seal t end
    module Mkdir           = struct include V_7_15.Mkdir           let () = seal t end
    module Rename          = struct include V_7_15.Rename          let () = seal t end
    module Link            = struct include V_7_15.Link            let () = seal t end
    module Fsync           = struct include V_7_15.Fsync           let () = seal t end
    module Interrupt       = struct include V_7_15.Interrupt       let () = seal t end
    module Bmap            = struct include V_7_15.Bmap            let () = seal t end
    module Getxattr        = struct include V_7_15.Getxattr        let () = seal t end
    module Setxattr        = struct include V_7_15.Setxattr        let () = seal t end
    module Read            = struct include V_7_15.Read            let () = seal t end
    module Write           = struct include V_7_15.Write           let () = seal t end
    module Lk              = struct include V_7_15.Lk              let () = seal t end
    module Setattr         = struct include V_7_15.Setattr         let () = seal t end
    module Getattr         = struct include V_7_15.Getattr         let () = seal t end
    module Ioctl           = struct include V_7_15.Ioctl           let () = seal t end
    module Poll            = struct include V_7_15.Poll            let () = seal t end
    module Cuse_init       = struct include V_7_15.Cuse_init       let () = seal t end
    module Notify_retrieve = struct include V_7_15.Notify_retrieve let () = seal t end
    module Batch_forget    = struct
      type t
      let t : t structure typ = structure "fuse_batch_forget_in"
      let ( -:* ) s x = field t s x
      let count         = "count"         -:* uint32_t
      let () = seal t
    end
  end
end
