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

open Fuse
open In_common

module type READ = sig
  type t

  val read : chan -> unit -> t request
end

module type LINUX_7_8 = sig
  include module type of In_common
  include module type of In_linux_7_8
  include READ with type t := t
end
  with module Hdr = Hdr
  and module Init = Init
  and module Forget = Forget
  and module Read = Read
  and module Release = Release
  and module Open = Open
  and module Rename = Rename
  and module Write = Write
  and module Flush = Flush
  and module Link = Link
  and module Access = Access
  and module Create = Create
  and module Mknod = Mknod
  and module Fsync = Fsync
  and module Mkdir = Mkdir
  and module Lk = Lk
  and module Interrupt = Interrupt
  and module Bmap = Bmap

module type OSX_7_8 = sig
  include module type of In_common
  include module type of In_osx_7_8
  include READ with type t := t
end
  with module Hdr = Hdr
  and module Init = Init
  and module Forget = Forget
  and module Read = Read
  and module Release = Release
  and module Open = Open
  and module Rename = Rename
  and module Write = Write
  and module Flush = Flush
  and module Link = Link
  and module Access = Access
  and module Create = Create
  and module Mknod = Mknod
  and module Fsync = Fsync
  and module Mkdir = Mkdir
  and module Lk = Lk
  and module Interrupt = Interrupt
  and module Bmap = Bmap

let read ~destroy ~parse chan =
  let count = chan.Fuse.max_write + Unix_unistd.Sysconf.(pagesize ~host) in
  let hdr_sz = sizeof Hdr.t in
  fun () ->
    let open Unix in
    let buf = allocate_n uint8_t ~count in (* TODO: pool? *)
    try
      let len = Unix_unistd.read chan.Fuse.fd (to_voidp buf) count in
      let hdr_ptr = coerce (ptr uint8_t) (ptr Hdr.t) buf in
      let hdr = !@ hdr_ptr in
      chan.Fuse.unique <- getf hdr Hdr.unique;
      let sz = getf hdr Hdr.size in
      if len <> sz then raise
        (Fuse.ProtocolError
           (chan,
            (Printf.sprintf "Packet has %d bytes but only read %d" sz len)));

      parse chan hdr (sz - hdr_sz) (to_voidp (buf +@ hdr_sz))
    with Unix_error ((
      EINTR (* SIGINT *) | ENODEV (* umount *) | EBADF (* internal unmount *)
    ),"read",_) ->
      let nodeid = UInt64.zero in
      let uid = UInt32.zero in
      let gid = UInt32.zero in
      let pid = UInt32.zero in
      (* assumes sequentially increasing packet ids *)
      let unique = UInt64.succ chan.Fuse.unique in
      chan.Fuse.unique <- unique;
      let pkt = Hdr.packet ~opcode:Opcode.FUSE_DESTROY ~unique
        ~nodeid ~uid ~gid ~pid ~count:0
      in
      let hdr = !@ (coerce (ptr char) (ptr Hdr.t)
                      ((CArray.start pkt) -@ (sizeof Hdr.t)))
      in
      Fuse.({ chan; hdr; pkt=destroy; })
    | Unix_error (err,call,s) ->
      let msg = Printf.sprintf "%s(%s) error: %s" call s (error_message err) in
      raise (Fuse.ProtocolError (chan, msg))

module Linux_7_8 : LINUX_7_8 = struct
  include In_common
  include In_linux_7_8
  let read = read ~destroy:Destroy ~parse
end

module Osx_7_8 : OSX_7_8 = struct
  include In_common
  include In_osx_7_8
  let read = read ~destroy:Destroy ~parse
end

module Linux_7_8_of_osx_7_8 : LINUX_7_8 = struct
  include Linux_7_8

  let linux_getxattr_of_osx_getxattr cons g req =
    let pkt = Getxattr.create_from_hdr
      ~size:(getf g Osx_7_8.Getxattr.size) req.hdr in
    let p = CArray.start pkt in
    let hdr = !@ (coerce (ptr char) (ptr Hdr.t) (p -@ sizeof Hdr.t)) in
    let g = !@ (coerce (ptr char) (ptr Getxattr.t) p) in
    {req with hdr; pkt = Getxattr g}    

  let read chan =
    let module O = Osx_7_8 in
    let read = O.read chan in
    fun () ->
      let req = read () in
      match req.pkt with
      | O.Init i -> {req with pkt = Init i}
      | O.Getattr -> {req with pkt = Getattr}
      | O.Lookup s -> {req with pkt = Lookup s}
      | O.Opendir o -> {req with pkt = Opendir o}
      | O.Readdir r -> {req with pkt = Readdir r}
      | O.Releasedir r -> {req with pkt = Releasedir r}
      | O.Fsyncdir f -> {req with pkt = Fsyncdir f}
      | O.Rmdir r -> {req with pkt = Rmdir r}
      | O.Mkdir (m,name) -> {req with pkt = Mkdir (m,name)}
      | O.Getxattr g ->
        linux_getxattr_of_osx_getxattr (fun g -> Getxattr g) g req
      | O.Setxattr s ->
        let pkt = Setxattr.create_from_hdr
          ~size:(getf s O.Setxattr.size)
          ~flags:(getf s O.Setxattr.flags)
          req.hdr in
        let p = CArray.start pkt in
        let hdr = !@ (coerce (ptr char) (ptr Hdr.t) (p -@ sizeof Hdr.t)) in
        let s = !@ (coerce (ptr char) (ptr Setxattr.t) p) in
        {req with hdr; pkt = Setxattr s}
      | O.Listxattr l ->
        linux_getxattr_of_osx_getxattr (fun g -> Listxattr g) l req
      | O.Removexattr r -> {req with pkt = Removexattr r}
      | O.Access a -> {req with pkt = Access a}
      | O.Forget f -> {req with pkt = Forget f}
      | O.Readlink -> {req with pkt = Readlink}
      | O.Link (l,name) -> {req with pkt = Link (l,name)}
      | O.Symlink (name, target) -> {req with pkt = Symlink (name, target)}
      | O.Rename (r,src,dest) -> {req with pkt = Rename (r, src, dest)}
      | O.Open o -> {req with pkt = Open o}
      | O.Read r -> {req with pkt = Read r}
      | O.Write w -> {req with pkt = Write w}
      | O.Statfs -> {req with pkt = Statfs}
      | O.Flush f -> {req with pkt = Flush f}
      | O.Release r -> {req with pkt = Release r}
      | O.Fsync f -> {req with pkt = Fsync f}
      | O.Unlink u -> {req with pkt = Unlink u}
      | O.Create (c,name) ->
        {req with pkt = Create (c,name)} (* TODO: exists on OS X FUSE 7.8? *)
      | O.Mknod (m,name) -> {req with pkt = Mknod (m,name)}
      | O.Setattr s ->
        let pkt = Setattr.create_from_hdr
          ~valid:(getf s O.Setattr.valid)
          ~fh:(getf s O.Setattr.fh)
          ~size:(getf s O.Setattr.size)
          ~atime:(getf s O.Setattr.atime)
          ~mtime:(getf s O.Setattr.mtime)
          ~atimensec:(getf s O.Setattr.atimensec)
          ~mtimensec:(getf s O.Setattr.mtimensec)
          ~mode:(getf s O.Setattr.mode)
          ~uid:(getf s O.Setattr.uid)
          ~gid:(getf s O.Setattr.gid)
          req.hdr in
        let p = CArray.start pkt in
        let hdr = !@ (coerce (ptr char) (ptr Hdr.t) (p -@ sizeof Hdr.t)) in
        let s = !@ (coerce (ptr char) (ptr Setattr.t) p) in
        {req with hdr; pkt = Setattr s}
      | O.Getlk l -> {req with pkt = Getlk l}
      | O.Setlk l -> {req with pkt = Setlk l}
      | O.Setlkw l -> {req with pkt = Setlkw l}
      | O.Interrupt i -> {req with pkt = Interrupt i}
      | O.Bmap b -> {req with pkt = Bmap b}
      | O.Destroy -> {req with pkt = Destroy}
      | O.Setvolname _ -> {req with pkt = Other Opcode.FUSE_SETVOLNAME}
      | O.Getxtimes -> {req with pkt = Other Opcode.FUSE_GETXTIMES}
      | O.Exchange _ -> {req with pkt = Other Opcode.FUSE_EXCHANGE}
      | O.Other o -> {req with pkt = Other o}
      | O.Unknown c -> {req with pkt = Unknown c}
end

module Osx_7_8_of_linux_7_8 : OSX_7_8 = struct
  include Osx_7_8

  let osx_getxattr_of_linux_getxattr cons g req =
    let pkt = Getxattr.create_from_hdr
      ~size:(getf g Linux_7_8.Getxattr.size)
      ~position:0l (* TODO: right meaning? *)
      req.hdr in
    let p = CArray.start pkt in
    let hdr = !@ (coerce (ptr char) (ptr Hdr.t) (p -@ sizeof Hdr.t)) in
    let g = !@ (coerce (ptr char) (ptr Getxattr.t) p) in
    {req with hdr; pkt = cons g}

  let read chan =
    let module L = Linux_7_8 in
    let read = L.read chan in
    fun () ->
      let req = read () in
      match req.pkt with
      | L.Init i -> {req with pkt = Init i}
      | L.Getattr -> {req with pkt = Getattr}
      | L.Lookup s -> {req with pkt = Lookup s}
      | L.Opendir o -> {req with pkt = Opendir o}
      | L.Readdir r -> {req with pkt = Readdir r}
      | L.Releasedir r -> {req with pkt = Releasedir r}
      | L.Fsyncdir f -> {req with pkt = Fsyncdir f}
      | L.Rmdir r -> {req with pkt = Rmdir r}
      | L.Mkdir (m,name) -> {req with pkt = Mkdir (m,name)}
      | L.Getxattr g ->
        osx_getxattr_of_linux_getxattr (fun g -> Getxattr g) g req
      | L.Setxattr s ->
        let pkt = Setxattr.create_from_hdr
          ~size:(getf s L.Setxattr.size)
          ~flags:(getf s L.Setxattr.flags)
          ~position:0l (* TODO: right meaning? *)
          req.hdr in
        let p = CArray.start pkt in
        let hdr = !@ (coerce (ptr char) (ptr Hdr.t) (p -@ sizeof Hdr.t)) in
        let s = !@ (coerce (ptr char) (ptr Setxattr.t) p) in
        {req with hdr; pkt = Setxattr s}
      | L.Listxattr l ->
        osx_getxattr_of_linux_getxattr (fun g -> Listxattr g) l req
      | L.Removexattr r -> {req with pkt = Removexattr r}
      | L.Access a -> {req with pkt = Access a}
      | L.Forget f -> {req with pkt = Forget f}
      | L.Readlink -> {req with pkt = Readlink}
      | L.Link (l,name) -> {req with pkt = Link (l,name)}
      | L.Symlink (name,target) -> {req with pkt = Symlink (name, target)}
      | L.Rename (r,src,dest) -> {req with pkt = Rename (r,src,dest)}
      | L.Open o -> {req with pkt = Open o}
      | L.Read r -> {req with pkt = Read r}
      | L.Write w -> {req with pkt = Write w}
      | L.Statfs -> {req with pkt = Statfs}
      | L.Flush f -> {req with pkt = Flush f}
      | L.Release r -> {req with pkt = Release r}
      | L.Fsync f -> {req with pkt = Fsync f}
      | L.Unlink u -> {req with pkt = Unlink u}
      | L.Create (c,name) ->
        {req with pkt = Create (c,name)} (* TODO: exists on OS X FUSE 7.8? *)
      | L.Mknod (m,name) -> {req with pkt = Mknod (m,name)}
      | L.Setattr s ->
        let pkt = Setattr.create_from_hdr
          ~valid:(getf s L.Setattr.valid)
          ~fh:(getf s L.Setattr.fh)
          ~size:(getf s L.Setattr.size)
          ~atime:(getf s L.Setattr.atime)
          ~mtime:(getf s L.Setattr.mtime)
          ~atimensec:(getf s L.Setattr.atimensec)
          ~mtimensec:(getf s L.Setattr.mtimensec)
          ~mode:(getf s L.Setattr.mode)
          ~uid:(getf s L.Setattr.uid)
          ~gid:(getf s L.Setattr.gid)
          ~bkuptime:0L (* TODO: right meaning? *)
          ~chgtime:0L (* TODO: right meaning? *)
          ~crtime:0L (* TODO: right meaning? *)
          ~bkuptimensec:0l (* TODO: right meaning? *)
          ~chgtimensec:0l (* TODO: right meaning? *)
          ~crtimensec:0l (* TODO: right meaning? *)
          ~flags:0l (* TODO: right meaning? *)
          req.hdr in
        let p = CArray.start pkt in
        let hdr = !@ (coerce (ptr char) (ptr Hdr.t) (p -@ sizeof Hdr.t)) in
        let s = !@ (coerce (ptr char) (ptr Setattr.t) p) in
        {req with hdr; pkt = Setattr s}
      | L.Getlk l -> {req with pkt = Getlk l}
      | L.Setlk l -> {req with pkt = Setlk l}
      | L.Setlkw l -> {req with pkt = Setlkw l}
      | L.Interrupt i -> {req with pkt = Interrupt i}
      | L.Bmap b -> {req with pkt = Bmap b}
      | L.Destroy -> {req with pkt = Destroy}
      | L.Other o -> {req with pkt = Other o}
      | L.Unknown c -> {req with pkt = Unknown c}

end
