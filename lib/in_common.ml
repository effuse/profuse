(*
 * Copyright (c) 2014 David Sheets <sheets@alum.mit.edu>
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

open Ctypes
open Unsigned
open View

module Hdr = struct
  type t
  let t : t structure typ = structure "In_Hdr"
  let ( -:* ) s x = field t s x
  let size    = "size"    -:* int_of_32
  let opcode  = "opcode"  -:* Opcode.view
  let unique  = "unique"  -:* uint64_t
  let nodeid  = "nodeid"  -:* uint64_t
  let uid     = "uid"     -:* uint32_t
  let gid     = "gid"     -:* uint32_t
  let pid     = "pid"     -:* uint32_t
  let padding = "padding" -:* uint32_t
  let () = seal t

  let opcode_ = opcode
  let unique_ = unique
  let nodeid_ = nodeid
  let uid_    = uid
  let gid_    = gid
  let pid_    = pid

  let packet ~opcode ~unique ~nodeid ~uid ~gid ~pid ~count =
    let hdrsz = sizeof t in
    let bodysz = count in
    let count = hdrsz + bodysz in
    let pkt = allocate_n char ~count in
    let hdr = !@ (coerce (ptr char) (ptr t) pkt) in
    setf hdr size    count;
    setf hdr opcode_ opcode;
    setf hdr unique_ unique;
    setf hdr nodeid_ nodeid;
    setf hdr uid_    uid;
    setf hdr gid_    gid;
    setf hdr pid_    pid;
    CArray.from_ptr (pkt +@ hdrsz) bodysz

  let make ~opcode ~unique ~nodeid ~uid ~gid ~pid st =
    let count = sizeof st in
    let pkt = packet ~opcode ~unique ~nodeid ~uid ~gid ~pid ~count in
    !@ (coerce (ptr char) (ptr st) (CArray.start pkt))

  let memcpy ~dest ~src n =
    let cast p = from_voidp (array n uchar) p in
    cast dest <-@ !@(cast src)

  let packet_from_hdr hdr ~count =
    let hdrsz = sizeof t in
    let bodysz = count in
    let count = hdrsz + bodysz in
    let pkt = allocate_n char ~count in
    let dest = to_voidp pkt in
    memcpy ~dest ~src:(to_voidp (addr hdr)) (sizeof t);
    setf hdr size count;
    CArray.from_ptr (pkt +@ hdrsz) bodysz

  let make_from_hdr hdr st =
    let count = sizeof st in
    let pkt = packet_from_hdr hdr ~count in
    !@ (coerce (ptr char) (ptr st) (CArray.start pkt))
end

module Init = struct
  type t
  let t : t structure typ = structure "In_Init"
  let ( -:* ) s x = field t s x
  let major         = "major"         -:* int_of_32
  let minor         = "minor"         -:* int_of_32
  let max_readahead = "max_readahead" -:* int_of_32
  let flags         = "flags"         -:* uint32_t
  let () = seal t
end

module Open = struct
  type t
  let t : t structure typ = structure "In_Open"
  let ( -:* ) s x = field t s x
  let flags  = "flags" -:* int32_of_32
  let mode   = "mode"  -:* int32_of_32
  let () = seal t
end

module Read = struct
  type t
  let t : t structure typ = structure "In_Read"
  let ( -:* ) s x = field t s x
  let fh      = "fh"      -:* int64_of_64
  let offset  = "offset"  -:* int64_of_64
  let size    = "size"    -:* int_of_32
  let padding = "padding" -:* uint32_t
  let () = seal t
end

module Release = struct
  type t
  let t : t structure typ = structure "In_Release"
  let ( -:* ) s x = field t s x
  let fh            = "fh"            -:* int64_of_64
  let flags         = "flags"         -:* uint32_t
  let release_flags = "release_flags" -:* uint32_t
  let lock_owner    = "lock_owner"    -:* uint64_t
  let () = seal t
end

module Access = struct
  type t
  let t : t structure typ = structure "In_Access"
  let ( -:* ) s x = field t s x
  let mask    = "mask"    -:* int_of_32 (* TODO: copacetic? *)
  let padding = "padding" -:* uint32_t
  let () = seal t
end

module Forget = struct
  type t
  let t : t structure typ = structure "In_Forget"
  let ( -:* ) s x = field t s x
  let nlookup = "nlookup" -:* int_of_64 (* TODO: is this copacetic??? *)
  let () = seal t
end

module Flush = struct
  type t
  let t : t structure typ = structure "In_Flush"
  let ( -:* ) s x = field t s x
  let fh         = "fh"         -:* int64_of_64
  let unused     = "unused"     -:* uint32_t
  let padding    = "padding"    -:* uint32_t
  let lock_owner = "lock_owner" -:* uint64_t
  let () = seal t
end

module Create = struct
  type t
  let t : t structure typ = structure "In_Create"
  let ( -:* ) s x = field t s x
  let flags = "flags" -:* int32_of_32
  let mode  = "mode"  -:* int32_of_32
  let name  = "name"  -:* array 0 char
  let () = seal t
end

module Mknod = struct
  type t
  let t : t structure typ = structure "In_Mknod"
  let ( -:* ) s x = field t s x
  let mode = "mode" -:* uint32_t
  let rdev = "rdev" -:* uint32_t
  let name = "name" -:* array 0 char
  let () = seal t
end

module Mkdir = struct
  type t
  let t : t structure typ = structure "In_Mkdir"
  let ( -:* ) s x = field t s x
  let mode    = "mode"    -:* uint32_t
  let padding = "padding" -:* uint32_t
  let name    = "name"    -:* array 0 char
  let () = seal t
end

module Rename = struct
  type t
  let t : t structure typ = structure "In_Rename"
  let ( -:* ) s x = field t s x
  let newdir = "newdir" -:* uint64_t
  let oldnew = "oldnew" -:* array 0 char
  let () = seal t
end

module Link = struct
  type t
  let t : t structure typ = structure "In_Link"
  let ( -:* ) s x = field t s x
  let oldnodeid = "oldnodeid" -:* uint64_t
  let name      = "name"      -:* array 0 char
  let () = seal t
end

module Write = struct
  type t
  let t : t structure typ = structure "In_Write"
  let ( -:* ) s x = field t s x
  let fh          = "fh"          -:* int64_of_64
  let offset      = "offset"      -:* int64_of_64
  let size        = "size"        -:* int_of_32
  let write_flags = "write_flags" -:* uint32_t
  let data        = "data"        -:* array 0 char
  let () = seal t
end

module Fsync = struct
  type t
  let t : t structure typ = structure "In_Fsync"
  let ( -:* ) s x = field t s x
  let fh          = "fh"          -:* int64_of_64
  let fsync_flags = "fsync_flags" -:* uint32_t
  let padding     = "padding"     -:* uint32_t
  let () = seal t
end

module Lk = struct
  type t
  let t : t structure typ = structure "In_Lk"
  let ( -:* ) s x = field t s x
  let fh    = "fh"    -:* int64_of_64
  let owner = "owner" -:* uint64_t
  let lk    = "lk"    -:* Struct_common.File_lock.t
  let () = seal t
end

module Interrupt = struct
  type t
  let t : t structure typ = structure "In_Interrupt"
  let ( -:* ) s x = field t s x
  let unique = "unique" -:* uint64_t
  let () = seal t
end

module Bmap = struct
  type t
  let t : t structure typ = structure "In_Bmap"
  let ( -:* ) s x = field t s x
  let block     = "block"     -:* uint64_t
  let blocksize = "blocksize" -:* uint32_t
  let padding   = "padding"   -:* uint32_t
  let () = seal t
end
