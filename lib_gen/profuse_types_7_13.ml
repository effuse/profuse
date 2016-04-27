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

  (* Build the types for FUSE 7.12, but don't seal the structs *)
  module Version_7_12 = Profuse_types_7_12.C
      (struct
        include F
        let seal _ = ()
      end)

  (* Extend and seal the structs *)
  module Struct = struct
    module V_7_12 = Version_7_12.Struct
    module Kstatfs   = struct include V_7_12.Kstatfs   let () = seal t end
    module File_lock = struct include V_7_12.File_lock let () = seal t end
    module Dirent    = struct include V_7_12.Dirent    let () = seal t end
    module Attr      = struct include V_7_12.Attr      let () = seal t end
  end

  module Out = struct
    module V_7_12 = Version_7_12.Out
    module Hdr                = struct include V_7_12.Hdr                let () = seal t end
    module Write              = struct include V_7_12.Write              let () = seal t end
    module Open               = struct include V_7_12.Open               let () = seal t end
    module Entry              = struct include V_7_12.Entry              let () = seal t end
    module Attr               = struct include V_7_12.Attr               let () = seal t end
    module Statfs             = struct include V_7_12.Statfs             let () = seal t end
    module Getxattr           = struct include V_7_12.Getxattr           let () = seal t end
    module Lk                 = struct include V_7_12.Lk                 let () = seal t end
    module Bmap               = struct include V_7_12.Bmap               let () = seal t end
    module Ioctl              = struct include V_7_12.Ioctl              let () = seal t end
    module Poll               = struct include V_7_12.Poll               let () = seal t end
    module Notify_poll_wakeup = struct include V_7_12.Notify_poll_wakeup let () = seal t end
    module Cuse_init          = struct include V_7_12.Cuse_init          let () = seal t end
    module Notify_inval_inode = struct include V_7_12.Notify_inval_inode let () = seal t end
    module Notify_inval_entry = struct include V_7_12.Notify_inval_entry let () = seal t end
    module Init               = struct
      include V_7_12.Init
      let ( -:* ) s x = field t s x
      let max_background       = "max_background"       -:* uint16_t
      let congestion_threshold = "congestion_threshold" -:* uint16_t
      let () = seal t
    end
  end

  module In = struct
    module V_7_12 = Version_7_12.In
    module Opcode = V_7_12.Opcode
    module Hdr       = struct include V_7_12.Hdr       let () = seal t end
    module Init      = struct include V_7_12.Init      let () = seal t end
    module Open      = struct include V_7_12.Open      let () = seal t end
    module Release   = struct include V_7_12.Release   let () = seal t end
    module Access    = struct include V_7_12.Access    let () = seal t end
    module Forget    = struct include V_7_12.Forget    let () = seal t end
    module Flush     = struct include V_7_12.Flush     let () = seal t end
    module Create    = struct include V_7_12.Create    let () = seal t end
    module Mknod     = struct include V_7_12.Mknod     let () = seal t end
    module Mkdir     = struct include V_7_12.Mkdir     let () = seal t end
    module Rename    = struct include V_7_12.Rename    let () = seal t end
    module Link      = struct include V_7_12.Link      let () = seal t end
    module Fsync     = struct include V_7_12.Fsync     let () = seal t end
    module Interrupt = struct include V_7_12.Interrupt let () = seal t end
    module Bmap      = struct include V_7_12.Bmap      let () = seal t end
    module Getxattr  = struct include V_7_12.Getxattr  let () = seal t end
    module Setxattr  = struct include V_7_12.Setxattr  let () = seal t end
    module Read      = struct include V_7_12.Read      let () = seal t end
    module Write     = struct include V_7_12.Write     let () = seal t end
    module Lk        = struct include V_7_12.Lk        let () = seal t end
    module Setattr   = struct include V_7_12.Setattr   let () = seal t end
    module Getattr   = struct include V_7_12.Getattr   let () = seal t end
    module Ioctl     = struct include V_7_12.Ioctl     let () = seal t end
    module Poll      = struct include V_7_12.Poll      let () = seal t end
    module Cuse_init = struct include V_7_12.Cuse_init let () = seal t end
  end
end
