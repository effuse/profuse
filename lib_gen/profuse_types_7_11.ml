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

  (* Build the types for FUSE 7.10, but don't seal the structs *)
  module Version_7_10 = Profuse_types_7_10.C_compatible
      (struct
        include F
        let seal _ = ()
      end)

  (* Extend and seal the structs *)
  module Struct = struct
    module V_7_10 = Version_7_10.Struct
    module Kstatfs   = struct include V_7_10.Kstatfs   let () = seal t end
    module File_lock = struct include V_7_10.File_lock let () = seal t end
    module Dirent    = struct include V_7_10.Dirent    let () = seal t end
    module Attr      = struct include V_7_10.Attr      let () = seal t end
  end

  module Out = struct
    module V_7_10 = Version_7_10.Out
    module Write    = struct include V_7_10.Write    let () = seal t end
    module Open     = struct include V_7_10.Open     let () = seal t end
    module Init     = struct include V_7_10.Init     let () = seal t end
    module Entry    = struct include V_7_10.Entry    let () = seal t end
    module Attr     = struct include V_7_10.Attr     let () = seal t end
    module Statfs   = struct include V_7_10.Statfs   let () = seal t end
    module Getxattr = struct include V_7_10.Getxattr let () = seal t end
    module Lk       = struct include V_7_10.Lk       let () = seal t end
    module Bmap     = struct include V_7_10.Bmap     let () = seal t end
    module Hdr      = struct
      module Notify_code =
      struct
        let t = int32_t
        let fuse_notify_poll = constant "FUSE_NOTIFY_POLL" t
      end
      include V_7_10.Hdr
      let () = seal t
    end
    module Ioctl    = struct
      type t
      let t : t structure typ = structure "fuse_ioctl_out"
      let ( -:* ) s x = field t s x
      let result   = "result"   -:* int32_t
      let flags    = "flags"    -:* uint32_t
      let in_iovs  = "in_iovs"  -:* uint32_t
      let out_iovs = "out_iovs" -:* uint32_t
      let () = seal t
    end
    module Poll     = struct
      type t
      let t : t structure typ = structure "fuse_poll_out"
      let ( -:* ) s x = field t s x
      let revents  = "revents"  -:* uint32_t
      let () = seal t
    end
    module Notify_poll_wakeup = struct
      type t
      let t : t structure typ = structure "fuse_notify_poll_wakeup_out"
      let ( -:* ) s x = field t s x
      let kh       = "kh"       -:* uint64_t
      let () = seal t
    end
  end

  module In = struct
    module V_7_10 = Version_7_10.In
    module Opcode =
    struct
      include (V_7_10.Opcode
               : module type of V_7_10.Opcode
               with type t := V_7_10.Opcode.t)

      let fuse_ioctl = constant "FUSE_IOCTL" uint32_t
      let fuse_poll = constant "FUSE_POLL" uint32_t
      let cuse_init = constant "CUSE_INIT" uint32_t

      type t = [ V_7_10.Opcode.t
               | `FUSE_IOCTL
               | `FUSE_POLL
               | `CUSE_INIT ]

      let enum_values =
        (`FUSE_IOCTL, fuse_ioctl) ::
        (`FUSE_POLL, fuse_poll) ::
        (`CUSE_INIT, cuse_init) ::
        (V_7_10.Opcode.enum_values :> (t * _) list)
    end

    module Hdr       = struct include V_7_10.Hdr       let () = seal t end
    module Init      = struct include V_7_10.Init      let () = seal t end
    module Release   = struct include V_7_10.Release   let () = seal t end
    module Access    = struct include V_7_10.Access    let () = seal t end
    module Forget    = struct include V_7_10.Forget    let () = seal t end
    module Flush     = struct include V_7_10.Flush     let () = seal t end
    module Mknod     = struct include V_7_10.Mknod     let () = seal t end
    module Mkdir     = struct include V_7_10.Mkdir     let () = seal t end
    module Rename    = struct include V_7_10.Rename    let () = seal t end
    module Link      = struct include V_7_10.Link      let () = seal t end
    module Fsync     = struct include V_7_10.Fsync     let () = seal t end
    module Interrupt = struct include V_7_10.Interrupt let () = seal t end
    module Bmap      = struct include V_7_10.Bmap      let () = seal t end
    module Getxattr  = struct include V_7_10.Getxattr  let () = seal t end
    module Setxattr  = struct include V_7_10.Setxattr  let () = seal t end
    module Read      = struct include V_7_10.Read      let () = seal t end
    module Write     = struct include V_7_10.Write     let () = seal t end
    module Lk        = struct include V_7_10.Lk        let () = seal t end
    module Setattr   = struct include V_7_10.Setattr   let () = seal t end
    module Getattr   = struct include V_7_10.Getattr   let () = seal t end
    module Ioctl     = struct
      module Flags =
      struct
        let t = uint32_t
        let fuse_ioctl_compat = constant "FUSE_IOCTL_COMPAT" t
        let fuse_ioctl_unrestricted = constant "FUSE_IOCTL_UNRESTRICTED" t
        let fuse_ioctl_retry = constant "FUSE_IOCTL_RETRY" t
      end
      type t
      let t : t structure typ = structure "fuse_ioctl_in"
      let ( -:* ) s x = field t s x
      let fh       = "fh"       -:* uint64_t
      let flags    = "flags"    -:* uint32_t
      let cmd      = "cmd"      -:* uint32_t
      let arg      = "arg"      -:* uint64_t
      let in_size  = "in_size"  -:* uint32_t
      let out_size = "out_size" -:* uint32_t
      let () = seal t
    end
    module Poll      = struct
      module Flags =
      struct
        let t = uint32_t
        let fuse_poll_schedule_notify = constant "FUSE_POLL_SCHEDULE_NOTIFY" t
      end
      type t
      let t : t structure typ = structure "fuse_poll_in"
      let ( -:* ) s x = field t s x
      let fh       = "fh"       -:* uint64_t
      let kh       = "kh"       -:* uint64_t
      let flags    = "flags"    -:* uint32_t
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
