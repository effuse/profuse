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

open In_common

module Getxattr = struct
  type t
  let t : t structure typ = structure "In_Getxattr"
  let ( -:* ) s x = field t s x
  let size     = "size"     -:* int32_of_32
  let padding  = "padding"  -:* uint32_t

  let position = "position" -:* int32_of_32
  let padding2 = "padding2" -:* uint32_t

  let () = seal t

  let size_ = size
  let position_ = position
  let create_from_hdr ~size ~position hdr =
    let pkt = Hdr.make_from_hdr hdr t in
    setf pkt size_     size;
    setf pkt position_ position;
    CArray.from_ptr (coerce (ptr t) (ptr char) (addr pkt)) (sizeof t)
end

module Setxattr = struct
  type t
  let t : t structure typ = structure "In_Setxattr"
  let ( -:* ) s x = field t s x
  let size     = "size"     -:* int32_of_32
  let flags    = "flags"    -:* uint32_t

  let position = "position" -:* int32_of_32
  let padding  = "padding"  -:* uint32_t

  let () = seal t

  let size_ = size
  let flags_ = flags
  let position_ = position
  let create_from_hdr ~size ~flags ~position hdr =
    let pkt = Hdr.make_from_hdr hdr t in
    setf pkt size_     size;
    setf pkt flags_    flags;
    setf pkt position_ position;
    CArray.from_ptr (coerce (ptr t) (ptr char) (addr pkt)) (sizeof t)
end

module Setattr = struct
  type t
  let t : t structure typ = structure "In_Setattr"
  let ( -:* ) s x = field t s x
  let valid      = "valid"      -:* int32_of_32
  let padding    = "padding"    -:* uint32_t
  let fh         = "fh"         -:* int64_of_64
  let size       = "size"       -:* int64_of_64 (* artificial 1EiB limit; wah. *)
  let unused1    = "unused1"    -:* uint64_t
  let atime      = "atime"      -:* uint64_t
  let mtime      = "mtime"      -:* uint64_t
  let unused2    = "unused2"    -:* uint64_t
  let atimensec  = "atimensec"  -:* uint32_t
  let mtimensec  = "mtimensec"  -:* uint32_t
  let unused3    = "unused3"    -:* uint32_t
  let mode       = "mode"       -:* uint32_t
  let unused4    = "unused4"    -:* uint32_t
  let uid        = "uid"        -:* uint32_t
  let gid        = "gid"        -:* uint32_t
  let unused5    = "unused5"    -:* uint32_t

  let bkuptime     = "bkuptime"     -:* int64_of_64
  let chgtime      = "chgtime"      -:* int64_of_64
  let crtime       = "crtime"       -:* int64_of_64
  let bkuptimensec = "bkuptimensec" -:* int32_of_32
  let chgtimensec  = "chgtimensec"  -:* int32_of_32
  let crtimensec   = "crtimensec"   -:* int32_of_32
  let flags        = "flags"        -:* int32_of_32

  let () = seal t

  let valid_        = valid
  let fh_           = fh
  let size_         = size
  let atime_        = atime
  let mtime_        = mtime
  let atimensec_    = atimensec
  let mtimensec_    = mtimensec
  let mode_         = mode
  let uid_          = uid
  let gid_          = gid

  let bkuptime_     = bkuptime
  let chgtime_      = chgtime
  let crtime_       = crtime
  let bkuptimensec_ = bkuptimensec
  let chgtimensec_  = chgtimensec
  let crtimensec_   = crtimensec
  let flags_        = flags

  let create_from_hdr
      ~valid ~fh ~size
      ~atime ~mtime ~atimensec ~mtimensec
      ~mode ~uid ~gid
      ~bkuptime ~chgtime ~crtime ~bkuptimensec ~chgtimensec ~crtimensec
      ~flags hdr =
    let pkt = Hdr.make_from_hdr hdr t in
    setf pkt valid_        valid;
    setf pkt fh_           fh;
    setf pkt size_         size;
    setf pkt atime_        atime;
    setf pkt mtime_        mtime;
    setf pkt atimensec_    atimensec;
    setf pkt mtimensec_    mtimensec;
    setf pkt mode_         mode;
    setf pkt uid_          uid;
    setf pkt gid_          gid;

    setf pkt bkuptime_     bkuptime;
    setf pkt chgtime_      chgtime;
    setf pkt crtime_       crtime;
    setf pkt bkuptimensec_ bkuptimensec;
    setf pkt chgtimensec_  chgtimensec;
    setf pkt crtimensec_   crtimensec;
    setf pkt flags_        flags;

    CArray.from_ptr (coerce (ptr t) (ptr char) (addr pkt)) (sizeof t)
end

module Exchange = struct
  type t
  let t : t structure typ = structure "In_Exchange"
  let ( -:* ) s x = field t s x
  let olddir  = "olddir"  -:* uint64_t
  let newdir  = "newdir"  -:* uint64_t
  let options = "options" -:* uint64_t
  let fromto  = "fromto"  -:* array 0 char
  let () = seal t
end

type t =
| Init of Init.t structure
| Getattr
| Lookup of string
| Opendir of Open.t structure
| Readdir  of Read.t structure
| Releasedir of Release.t structure
| Fsyncdir of Fsync.t structure
| Rmdir of string
| Mkdir of Mkdir.t structure * string
| Getxattr of Getxattr.t structure
| Setxattr of Setxattr.t structure
| Listxattr of Getxattr.t structure
| Removexattr of string
| Access of Access.t structure
| Forget of Forget.t structure
| Readlink
| Open of Open.t structure
| Read of Read.t structure
| Write of Write.t structure
| Statfs
| Flush of Flush.t structure
| Release of Release.t structure
| Fsync of Fsync.t structure
| Unlink of string
| Create of Create.t structure * string
| Mknod of Mknod.t structure * string
| Setattr of Setattr.t structure
| Link of Link.t structure * string
| Symlink of string * string
| Rename of Rename.t structure * string * string
| Getlk of Lk.t structure
| Setlk of Lk.t structure
| Setlkw of Lk.t structure
| Interrupt of Interrupt.t structure
| Bmap of Bmap.t structure
| Destroy
| Setvolname of string
| Getxtimes
| Exchange of Exchange.t structure * string * string
| Other of Opcode.t

let parse chan hdr len buf =
  let opcode = Hdr.(getf hdr opcode) in
  Fuse.({chan; hdr; pkt=Opcode.(match opcode with
  | FUSE_INIT        -> Init        (!@ (from_voidp Init.t buf))
  | FUSE_GETATTR     -> Getattr
  | FUSE_LOOKUP      -> Lookup      (coerce (ptr void) string buf)
  | FUSE_OPENDIR     -> Opendir     (!@ (from_voidp Open.t buf))
  | FUSE_READDIR     -> Readdir     (!@ (from_voidp Read.t buf))
  | FUSE_RELEASEDIR  -> Releasedir  (!@ (from_voidp Release.t buf))
  | FUSE_FSYNCDIR    -> Fsyncdir   (!@ (from_voidp Fsync.t buf))
  | FUSE_RMDIR       -> Rmdir       (coerce (ptr void) string buf)
  | FUSE_MKDIR       ->
    let s = !@ (from_voidp Mkdir.t buf) in
    let name = coerce (ptr char) string (CArray.start (getf s Mkdir.name)) in
    Mkdir (s, name)
  | FUSE_GETXATTR    -> Getxattr    (!@ (from_voidp Getxattr.t buf))
  | FUSE_SETXATTR    -> Setxattr    (!@ (from_voidp Setxattr.t buf))
  | FUSE_LISTXATTR   -> Listxattr   (!@ (from_voidp Getxattr.t buf))
  | FUSE_REMOVEXATTR -> Removexattr (coerce (ptr void) string buf)
  | FUSE_ACCESS      -> Access      (!@ (from_voidp Access.t buf))
  | FUSE_FORGET      -> Forget      (!@ (from_voidp Forget.t buf))
  | FUSE_READLINK    -> Readlink
  | FUSE_OPEN        -> Open        (!@ (from_voidp Open.t buf))
  | FUSE_READ        -> Read        (!@ (from_voidp Read.t buf))
  | FUSE_WRITE       -> Write       (!@ (from_voidp Write.t buf))
  | FUSE_STATFS      -> Statfs
  | FUSE_FLUSH       -> Flush       (!@ (from_voidp Flush.t buf))
  | FUSE_RELEASE     -> Release     (!@ (from_voidp Release.t buf))
  | FUSE_FSYNC       -> Fsync       (!@ (from_voidp Fsync.t buf))
  | FUSE_UNLINK      -> Unlink      (coerce (ptr void) string buf)
  | FUSE_CREATE      ->
    let s = !@ (from_voidp Create.t buf) in
    let name = coerce (ptr char) string (CArray.start (getf s Create.name)) in
    Create (s, name)
  | FUSE_MKNOD       ->
    let s = !@ (from_voidp Mknod.t buf) in
    let name = coerce (ptr char) string (CArray.start (getf s Mknod.name)) in
    Mknod (s, name)
  | FUSE_SETATTR     -> Setattr     (!@ (from_voidp Setattr.t buf))
  | FUSE_LINK        ->
    let s = !@ (from_voidp Link.t buf) in
    let name = coerce (ptr char) string (CArray.start (getf s Link.name)) in
    Link (s, name)
  | FUSE_SYMLINK     ->
    let name = coerce (ptr void) string buf in
    let buf = to_voidp ((from_voidp char buf) +@ (String.length name + 1)) in
    let target = coerce (ptr void) string buf in
    Symlink (name,target)
  | FUSE_RENAME      ->
    let s = !@ (from_voidp Rename.t buf) in
    let buf = CArray.start (getf s Rename.oldnew) in
    let src = coerce (ptr char) string buf in
    let buf = buf +@ (String.length src + 1) in
    let dest= coerce (ptr char) string buf in
    Rename (s,src,dest)
  | FUSE_GETLK       -> Getlk      (!@ (from_voidp Lk.t buf))
  | FUSE_SETLK       -> Setlk      (!@ (from_voidp Lk.t buf))
  | FUSE_SETLKW      -> Setlkw     (!@ (from_voidp Lk.t buf))
  | FUSE_INTERRUPT   -> Interrupt  (!@ (from_voidp Interrupt.t buf))
  | FUSE_BMAP        -> Bmap       (!@ (from_voidp Bmap.t buf))
  | FUSE_DESTROY     -> Destroy
  | FUSE_IOCTL
  | FUSE_POLL
  | FUSE_NOTIFY_REPLY
  | FUSE_BATCH_FORGET
  | FUSE_FALLOCATE   -> Other opcode
  | FUSE_SETVOLNAME  -> Setvolname (coerce (ptr void) string buf)
  | FUSE_GETXTIMES   -> Getxtimes
  | FUSE_EXCHANGE    ->
    let s = !@ (from_voidp Exchange.t buf) in
    let buf = CArray.start (getf s Exchange.fromto) in
    let src = coerce (ptr char) string buf in
    let buf = buf +@ (String.length src + 1) in
    let dest= coerce (ptr char) string buf in
    Exchange (s,src,dest)
  )})

(** Can raise Opcode.Unknown *)
let read chan =
  let count = chan.Fuse.max_write + Unix_unistd.Sysconf.(pagesize ~host) in
  let hdr_sz = sizeof Hdr.t in
  fun () ->
    let buf = allocate_n uint8_t ~count in (* TODO: pool? *)
    let len = try Unix_unistd.read chan.Fuse.fd (to_voidp buf) count
      with Unix.Unix_error (err,_,_) ->
        raise (Fuse.ProtocolError (chan, "Read error: "^(Unix.error_message err)))
    in
    let hdr_ptr = coerce (ptr uint8_t) (ptr Hdr.t) buf in
    let hdr = !@ hdr_ptr in (* TODO: catch Opcode.Unknown? *)
    let sz = getf hdr Hdr.size in
    if len <> sz then raise
      (Fuse.ProtocolError
         (chan,
          (Printf.sprintf "Packet has %d bytes but only read %d" sz len)));

    parse chan hdr (sz - hdr_sz) (to_voidp (buf +@ hdr_sz))
