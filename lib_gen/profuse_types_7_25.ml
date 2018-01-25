(*
 * Copyright (c) 2018 Docker Inc
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

  (* Build the types for FUSE 7.25, but don't seal the structs *)
  module Version_7_24 = Profuse_types_7_24.C
      (struct
        include F
        let seal _ = ()
      end)

  (* Extend and seal the structs *)
  module Struct = struct
    module V_7_24 = Version_7_24.Struct
    module Kstatfs     = struct include V_7_24.Kstatfs     let () = seal t end
    module File_lock   = struct include V_7_24.File_lock   let () = seal t end
    module Dirent      = struct include V_7_24.Dirent      let () = seal t end
    module Attr        = struct include V_7_24.Attr        let () = seal t end
    module Forget_one  = struct include V_7_24.Forget_one  let () = seal t end
    module Ioctl_iovec = struct include V_7_24.Ioctl_iovec let () = seal t end
    module Direntplus  = struct include V_7_24.Direntplus  let () = seal t end
  end

  module Out = struct
    module V_7_24 = Version_7_24.Out
    module Hdr                = struct include V_7_24.Hdr                let () = seal t end
    module Write              = struct include V_7_24.Write              let () = seal t end
    module Open               = struct include V_7_24.Open               let () = seal t end
    module Entry              = struct include V_7_24.Entry              let () = seal t end
    module Attr               = struct include V_7_24.Attr               let () = seal t end
    module Statfs             = struct include V_7_24.Statfs             let () = seal t end
    module Getxattr           = struct include V_7_24.Getxattr           let () = seal t end
    module Lk                 = struct include V_7_24.Lk                 let () = seal t end
    module Bmap               = struct include V_7_24.Bmap               let () = seal t end
    module Ioctl              = struct include V_7_24.Ioctl              let () = seal t end
    module Poll               = struct include V_7_24.Poll               let () = seal t end
    module Notify_poll_wakeup = struct include V_7_24.Notify_poll_wakeup let () = seal t end
    module Cuse_init          = struct include V_7_24.Cuse_init          let () = seal t end
    module Notify_inval_inode = struct include V_7_24.Notify_inval_inode let () = seal t end
    module Notify_inval_entry = struct include V_7_24.Notify_inval_entry let () = seal t end
    module Notify_delete      = struct include V_7_24.Notify_delete      let () = seal t end
    module Notify_store       = struct include V_7_24.Notify_store       let () = seal t end
    module Notify_retrieve    = struct include V_7_24.Notify_retrieve    let () = seal t end    
    module Init               = struct include V_7_24.Init               let () = seal t end
    module Lseek           = struct include V_7_24.Lseek           let () = seal t end

  end

  module In = struct
    module V_7_24 = Version_7_24.In
    module Hdr             = struct include V_7_24.Hdr             let () = seal t end
    module Open            = struct include V_7_24.Open            let () = seal t end
    module Release         = struct include V_7_24.Release         let () = seal t end
    module Access          = struct include V_7_24.Access          let () = seal t end
    module Forget          = struct include V_7_24.Forget          let () = seal t end
    module Flush           = struct include V_7_24.Flush           let () = seal t end
    module Create          = struct include V_7_24.Create          let () = seal t end
    module Mknod           = struct include V_7_24.Mknod           let () = seal t end
    module Mkdir           = struct include V_7_24.Mkdir           let () = seal t end
    module Rename          = struct include V_7_24.Rename          let () = seal t end
    module Link            = struct include V_7_24.Link            let () = seal t end
    module Fsync           = struct include V_7_24.Fsync           let () = seal t end
    module Interrupt       = struct include V_7_24.Interrupt       let () = seal t end
    module Bmap            = struct include V_7_24.Bmap            let () = seal t end
    module Getxattr        = struct include V_7_24.Getxattr        let () = seal t end
    module Setxattr        = struct include V_7_24.Setxattr        let () = seal t end
    module Read            = struct include V_7_24.Read            let () = seal t end
    module Write           = struct include V_7_24.Write           let () = seal t end
    module Lk              = struct include V_7_24.Lk              let () = seal t end
    module Getattr         = struct include V_7_24.Getattr         let () = seal t end
    module Ioctl           = struct include V_7_24.Ioctl           let () = seal t end
    module Poll            = struct include V_7_24.Poll            let () = seal t end
    module Cuse_init       = struct include V_7_24.Cuse_init       let () = seal t end
    module Notify_retrieve = struct include V_7_24.Notify_retrieve let () = seal t end
    module Batch_forget    = struct include V_7_24.Batch_forget    let () = seal t end
    module Fallocate       = struct include V_7_24.Fallocate       let () = seal t end
    module Opcode          = V_7_24.Opcode

    module Init            = struct
      module Flags = struct
        include V_7_24.Init.Flags
        let fuse_parallel_dirops = constant "FUSE_PARALLEL_DIROPS" t
      end
      include (V_7_24.Init
               : module type of V_7_24.Init with module Flags := Flags)
      let () = seal t
    end
    module Setattr         = struct include V_7_24.Setattr         let () = seal t end
    module Rename2         = struct include V_7_24.Rename2         let () = seal t end
    module Lseek           = struct include V_7_24.Lseek           let () = seal t end
  end
end
