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

  (* Build the types for FUSE 7.19, but don't seal the structs *)
  module Version_7_19 = Profuse_types_7_19.C
      (struct
        include F
        let seal _ = ()
      end)

  (* Extend and seal the structs *)
  module Struct = struct
    module V_7_19 = Version_7_19.Struct
    module Kstatfs     = struct include V_7_19.Kstatfs     let () = seal t end
    module File_lock   = struct include V_7_19.File_lock   let () = seal t end
    module Dirent      = struct include V_7_19.Dirent      let () = seal t end
    module Attr        = struct include V_7_19.Attr        let () = seal t end
    module Forget_one  = struct include V_7_19.Forget_one  let () = seal t end
    module Ioctl_iovec = struct include V_7_19.Ioctl_iovec let () = seal t end
  end

  module Out = struct
    module V_7_19 = Version_7_19.Out
    module Hdr                = struct include V_7_19.Hdr                let () = seal t end
    module Write              = struct include V_7_19.Write              let () = seal t end
    module Open               = struct include V_7_19.Open               let () = seal t end
    module Init               = struct include V_7_19.Init               let () = seal t end
    module Entry              = struct include V_7_19.Entry              let () = seal t end
    module Attr               = struct include V_7_19.Attr               let () = seal t end
    module Statfs             = struct include V_7_19.Statfs             let () = seal t end
    module Getxattr           = struct include V_7_19.Getxattr           let () = seal t end
    module Lk                 = struct include V_7_19.Lk                 let () = seal t end
    module Bmap               = struct include V_7_19.Bmap               let () = seal t end
    module Ioctl              = struct include V_7_19.Ioctl              let () = seal t end
    module Poll               = struct include V_7_19.Poll               let () = seal t end
    module Notify_poll_wakeup = struct include V_7_19.Notify_poll_wakeup let () = seal t end
    module Cuse_init          = struct include V_7_19.Cuse_init          let () = seal t end
    module Notify_inval_inode = struct include V_7_19.Notify_inval_inode let () = seal t end
    module Notify_inval_entry = struct include V_7_19.Notify_inval_entry let () = seal t end
    module Notify_store       = struct include V_7_19.Notify_store       let () = seal t end
    module Notify_delete      = struct include V_7_19.Notify_delete      let () = seal t end
    module Notify_retrieve    = struct include V_7_19.Notify_retrieve    let () = seal t end
  end

  module In = struct
    module V_7_19 = Version_7_19.In
    module Opcode = V_7_19.Opcode
    module Hdr             = struct include V_7_19.Hdr             let () = seal t end
    module Open            = struct include V_7_19.Open            let () = seal t end
    module Release         = struct include V_7_19.Release         let () = seal t end
    module Access          = struct include V_7_19.Access          let () = seal t end
    module Forget          = struct include V_7_19.Forget          let () = seal t end
    module Flush           = struct include V_7_19.Flush           let () = seal t end
    module Create          = struct include V_7_19.Create          let () = seal t end
    module Mknod           = struct include V_7_19.Mknod           let () = seal t end
    module Mkdir           = struct include V_7_19.Mkdir           let () = seal t end
    module Rename          = struct include V_7_19.Rename          let () = seal t end
    module Link            = struct include V_7_19.Link            let () = seal t end
    module Fsync           = struct include V_7_19.Fsync           let () = seal t end
    module Interrupt       = struct include V_7_19.Interrupt       let () = seal t end
    module Bmap            = struct include V_7_19.Bmap            let () = seal t end
    module Getxattr        = struct include V_7_19.Getxattr        let () = seal t end
    module Setxattr        = struct include V_7_19.Setxattr        let () = seal t end
    module Read            = struct include V_7_19.Read            let () = seal t end
    module Write           = struct include V_7_19.Write           let () = seal t end
    module Lk              = struct include V_7_19.Lk              let () = seal t end
    module Setattr         = struct include V_7_19.Setattr         let () = seal t end
    module Getattr         = struct include V_7_19.Getattr         let () = seal t end
    module Ioctl           = struct include V_7_19.Ioctl           let () = seal t end
    module Poll            = struct include V_7_19.Poll            let () = seal t end
    module Cuse_init       = struct include V_7_19.Cuse_init       let () = seal t end
    module Notify_retrieve = struct include V_7_19.Notify_retrieve let () = seal t end
    module Batch_forget    = struct include V_7_19.Batch_forget    let () = seal t end
    module Fallocate       = struct include V_7_19.Fallocate       let () = seal t end
    module Init      = struct
      module Flags = struct
        include V_7_19.Init.Flags
        let fuse_splice_write = constant "FUSE_SPLICE_WRITE" t
        let fuse_splice_move = constant "FUSE_SPLICE_MOVE" t
        let fuse_splice_read = constant "FUSE_SPLICE_READ" t
        let fuse_has_ioctl_dir = constant "FUSE_HAS_IOCTL_DIR" t
        let fuse_auto_inval_data = constant "FUSE_AUTO_INVAL_DATA" t
      end
      include (V_7_19.Init
               : module type of V_7_19.Init with module Flags := Flags)
      let () = seal t
    end
  end
end
