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

  (* Build the types for FUSE 7.21, but don't seal the structs *)
  module Version_7_21 = Profuse_types_7_21.C
      (struct
        include F
        let seal _ = ()
      end)

  (* Extend and seal the structs *)
  module Out = struct
    module V_7_21 = Version_7_21.Out
    module Hdr                = struct include V_7_21.Hdr                let () = seal t end
    module Write              = struct include V_7_21.Write              let () = seal t end
    module Open               = struct include V_7_21.Open               let () = seal t end
    module Init               = struct include V_7_21.Init               let () = seal t end
    module Entry              = struct include V_7_21.Entry              let () = seal t end
    module Attr               = struct include V_7_21.Attr               let () = seal t end
    module Statfs             = struct include V_7_21.Statfs             let () = seal t end
    module Getxattr           = struct include V_7_21.Getxattr           let () = seal t end
    module Lk                 = struct include V_7_21.Lk                 let () = seal t end
    module Bmap               = struct include V_7_21.Bmap               let () = seal t end
    module Ioctl              = struct include V_7_21.Ioctl              let () = seal t end
    module Poll               = struct include V_7_21.Poll               let () = seal t end
    module Notify_poll_wakeup = struct include V_7_21.Notify_poll_wakeup let () = seal t end
    module Cuse_init          = struct include V_7_21.Cuse_init          let () = seal t end
    module Notify_inval_inode = struct include V_7_21.Notify_inval_inode let () = seal t end
    module Notify_inval_entry = struct include V_7_21.Notify_inval_entry let () = seal t end
    module Notify_delete      = struct include V_7_21.Notify_delete      let () = seal t end
    module Notify_store       = struct include V_7_21.Notify_store       let () = seal t end
    module Notify_retrieve    = struct include V_7_21.Notify_retrieve    let () = seal t end    
  end

  module In = struct
    module V_7_21 = Version_7_21.In
    module Opcode = V_7_21.Opcode
    module Hdr             = struct include V_7_21.Hdr             let () = seal t end
    module Open            = struct include V_7_21.Open            let () = seal t end
    module Release         = struct include V_7_21.Release         let () = seal t end
    module Access          = struct include V_7_21.Access          let () = seal t end
    module Forget          = struct include V_7_21.Forget          let () = seal t end
    module Flush           = struct include V_7_21.Flush           let () = seal t end
    module Create          = struct include V_7_21.Create          let () = seal t end
    module Mknod           = struct include V_7_21.Mknod           let () = seal t end
    module Mkdir           = struct include V_7_21.Mkdir           let () = seal t end
    module Rename          = struct include V_7_21.Rename          let () = seal t end
    module Link            = struct include V_7_21.Link            let () = seal t end
    module Fsync           = struct include V_7_21.Fsync           let () = seal t end
    module Interrupt       = struct include V_7_21.Interrupt       let () = seal t end
    module Bmap            = struct include V_7_21.Bmap            let () = seal t end
    module Getxattr        = struct include V_7_21.Getxattr        let () = seal t end
    module Setxattr        = struct include V_7_21.Setxattr        let () = seal t end
    module Read            = struct include V_7_21.Read            let () = seal t end
    module Write           = struct include V_7_21.Write           let () = seal t end
    module Lk              = struct include V_7_21.Lk              let () = seal t end
    module Setattr         = struct include V_7_21.Setattr         let () = seal t end
    module Getattr         = struct include V_7_21.Getattr         let () = seal t end
    module Ioctl           = struct include V_7_21.Ioctl           let () = seal t end
    module Cuse_init       = struct include V_7_21.Cuse_init       let () = seal t end
    module Notify_retrieve = struct include V_7_21.Notify_retrieve let () = seal t end
    module Batch_forget    = struct include V_7_21.Batch_forget    let () = seal t end
    module Fallocate       = struct include V_7_21.Fallocate       let () = seal t end
    module Poll            = struct include V_7_21.Poll            let () = seal t end
    module Init            = struct
      module Flags = struct
        include V_7_21.Init.Flags
        let fuse_async_dio = constant "FUSE_ASYNC_DIO" t
      end
      include (V_7_21.Init
               : module type of V_7_21.Init with module Flags := Flags)
      let () = seal t
    end
  end
  module Struct = struct
    module V_7_21 = Version_7_21.Struct
    module Kstatfs     = struct include V_7_21.Kstatfs     let () = seal t end
    module File_lock   = struct include V_7_21.File_lock   let () = seal t end
    module Dirent      = struct include V_7_21.Dirent      let () = seal t end
    module Attr        = struct include V_7_21.Attr        let () = seal t end
    module Forget_one  = struct include V_7_21.Forget_one  let () = seal t end
    module Ioctl_iovec = struct include V_7_21.Ioctl_iovec let () = seal t end
    module Direntplus =  struct include V_7_21.Direntplus  let () = seal t end
  end
end
