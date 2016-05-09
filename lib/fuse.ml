(*
 * Copyright (c) 2014-2015 David Sheets <sheets@alum.mit.edu>
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

module Profuse = Profuse_7_23

module Handles = Handles

module Nodes = Nodes

module In = Profuse.In

type 'a structure = 'a Ctypes_static.structure
type request = In.Message.t Profuse.request

module type IO = sig
  type 'a t

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

  val return : 'a -> 'a t

  val fail : exn -> 'a t

  module In : sig
    val read : Profuse.chan -> unit -> request t
  end

  module Out : sig
    val write_reply :
      'a Profuse.request -> ('a Profuse.request -> char Ctypes.CArray.t)
      -> unit t
    val write_ack : request -> unit t
    val write_error : (string -> unit) -> request -> Errno.t -> unit t
  end
end

module type RO_SIMPLE = sig
  module IO : IO
  type 'a req_handler = request -> 'a -> 'a IO.t
  type t

  val getattr    : t req_handler
  val opendir    : In.Open.T.t structure -> t req_handler
  val forget     : Unsigned.UInt64.t -> t req_handler
  val lookup     : string -> t req_handler
  val readdir    : In.Read.T.t structure -> t req_handler
  val readlink   : t req_handler
  val release    : In.Release.T.t structure -> t req_handler
  val releasedir : In.Release.T.t structure -> t req_handler
  val open_      : In.Open.T.t structure -> t req_handler
  val read       : In.Read.T.t structure -> t req_handler
  val access     : In.Access.T.t structure -> t req_handler
  val destroy    : t req_handler
end

module type RO_MID = sig
  include RO_SIMPLE

  val statfs : t req_handler
end

module type RO_FULL = sig
  include RO_MID

  val getxattr  : In.Getxattr.T.t structure -> t req_handler
  val listxattr : In.Getxattr.T.t structure -> t req_handler
  val interrupt : In.Interrupt.T.t structure -> t req_handler
  val bmap      : In.Bmap.T.t structure -> t req_handler
end

module type RW_SIMPLE = sig
  include RO_SIMPLE

  val flush   : In.Flush.T.t structure -> t req_handler
  val link    : In.Link.T.t structure -> string -> t req_handler
  val symlink : string -> string -> t req_handler
  val rename  : In.Rename.T.t structure -> string -> string -> t req_handler
  val unlink  : string -> t req_handler
  val rmdir   : string -> t req_handler
  val mknod   : In.Mknod.T.t structure -> string -> t req_handler
  val write   : In.Write.T.t structure -> char Ctypes.ptr -> t req_handler
  val mkdir   : In.Mkdir.T.t structure -> string -> t req_handler
  val setattr : In.Setattr.T.t structure -> t req_handler
end

module type RW_MID = sig
  include RO_MID
  include RW_SIMPLE
    with module IO := IO
     and type t := t
     and type 'a req_handler := 'a req_handler

  val create      : In.Create.T.t structure -> string -> t req_handler
  val fsync       : In.Fsync.T.t structure -> t req_handler
end

module type RW_FULL = sig
  include RO_FULL
  include RW_MID
    with module IO := IO
     and type t := t
     and type 'a req_handler := 'a req_handler

  val fsyncdir    : In.Fsync.T.t structure -> t req_handler
  val setxattr    : In.Setxattr.T.t structure -> t req_handler
  val removexattr : string -> t req_handler
  val getlk       : In.Lk.T.t structure -> t req_handler (* TODO: RO? *)
  val setlk       : In.Lk.T.t structure -> t req_handler (* TODO: RO? *)
  val setlkw      : In.Lk.T.t structure -> t req_handler (* TODO: RO? *)
end

module type FS_IO = sig
  include RW_FULL

  val negotiate_mount :
    In.Init.T.t structure -> request -> t -> (request * t) IO.t

  val dispatch : request -> t -> t IO.t
end

module type STATE = sig
  type t

  val string_of_nodeid : Unsigned.UInt64.t -> t -> string

  val string_of_state : request -> t -> string
end

module type FS = sig
  include STATE

  module Calls :
    functor(IO : IO) -> FS_IO with type 'a IO.t = 'a IO.t and type t = t
end

module Zero(State : STATE)(IO : IO)
  : RW_FULL with module IO = IO and type t = State.t = struct
  module IO = IO
  type 'a req_handler = In.Message.t Profuse.request -> 'a -> 'a IO.t
  type t = State.t

  let enosys req st =
    (* Throw away error messages during the write. *)
    let log_error _msg = () in
    IO.(Out.write_error log_error req Errno.ENOSYS >>= fun () -> return st)

  let getattr      = enosys
  let opendir _    = enosys
  let forget _     = enosys
  let lookup _     = enosys
  let readdir _    = enosys
  let readlink     = enosys
  let release _    = enosys
  let releasedir _ = enosys
  let open_ _      = enosys
  let read _       = enosys
  let access _     = enosys
  let destroy      = enosys

  let statfs       = enosys
  let getxattr _   = enosys
  let listxattr _  = enosys
  let interrupt _  = enosys
  let bmap _       = enosys

  let flush _      = enosys
  let link _ _     = enosys
  let symlink _ _  = enosys
  let rename _ _ _ = enosys
  let unlink _     = enosys
  let rmdir _      = enosys
  let mknod _ _    = enosys
  let write _ _    = enosys
  let mkdir _ _    = enosys
  let setattr _    = enosys

  let create _ _   = enosys
  let fsync _      = enosys
  let fsyncdir _   = enosys
  let setxattr _   = enosys
  let removexattr _= enosys
  let getlk _      = enosys
  let setlk _      = enosys
  let setlkw _     = enosys
end

module type MOUNT_IO = sig
  module IO : IO
  type t

  val mount :
    (Profuse.In.Init.T.t structure -> request -> t -> (request * t) IO.t) ->
    argv:string array -> mnt:string -> t -> (request * t) IO.t
end

module type MOUNT = functor (FS : FS_IO) ->
  MOUNT_IO with module IO = FS.IO
            and type t = FS.t

module type SERVER = sig
  module IO : IO
  type t

  val mount : argv:string array -> mnt:string -> t -> (request * t) IO.t

  val serve_forever : Profuse.chan -> t -> t IO.t
end

module Support(IO : IO) = struct
  open Profuse

  (* A typical mount negotiation *)
  let negotiate_mount pkt req st =
    let open Unsigned in
    let major = UInt32.to_int (Ctypes.getf pkt In.Init.T.major) in
    if major <> 7
    then IO.fail (
      ProtocolError
        (req.chan, Printf.sprintf
           "Incompatible FUSE protocol major version %d <> 7" major))
    else
      let minor = UInt32.to_int (Ctypes.getf pkt In.Init.T.minor) in
      if minor < 8
      then IO.fail (
        ProtocolError
          (req.chan, Printf.sprintf
             "Incompatible FUSE protocol minor version %d < 8" minor))
      else
        let minor = min minor 8 in (* TODO: track kernel minor *)
        let max_readahead = Ctypes.getf pkt In.Init.T.max_readahead in
        let max_readahead = max (UInt32.to_int max_readahead) 65536 in
        (* TODO: ? *)
        let _flags = Ctypes.getf pkt In.Init.T.flags in (* TODO: ? *)
        let chan = {
          req.chan with version = (major, minor); max_readahead; flags = 0l;
        } in
        let major = UInt32.of_int major in
        let minor = UInt32.of_int minor in
        let max_readahead = UInt32.of_int max_readahead in
        let max_write = UInt32.of_int req.chan.max_write in
        let pkt = Out.Init.create ~major ~minor ~max_readahead
            ~flags:UInt32.zero ~max_write in
        IO.(Out.write_reply req pkt
            >>= fun () ->
            return ({req with chan}, st)
           )

  let nodeid req = In.(Ctypes.getf req.hdr In.Hdr.T.nodeid)

  let enosys req st =
    (* Throw away error messages during the write. *)
    let log_error _msg = () in
    IO.(Out.write_error log_error req Errno.ENOSYS
        >>= fun () ->
        return st
       )

  let store_entry
    ?(entry_valid=Unsigned.UInt64.zero)
    ?(entry_valid_nsec=Unsigned.UInt32.zero)
    ?(attr_valid=Unsigned.UInt64.zero)
    ?(attr_valid_nsec=Unsigned.UInt32.zero)
    store_attr_of_node node = IO.(
    store_attr_of_node node (* can raise ENOENT *)
    >>= fun store_attr ->
    let nodeid = Unsigned.UInt64.of_int64 node.Nodes.id in
    let generation = Unsigned.UInt64.of_int64 node.Nodes.gen in
    return (Profuse.Out.Entry.store ~nodeid ~generation
              ~entry_valid ~entry_valid_nsec
              ~attr_valid ~attr_valid_nsec
              ~store_attr)
  )

  let respond_with_entry
    ?(entry_valid=Unsigned.UInt64.zero)
    ?(entry_valid_nsec=Unsigned.UInt32.zero)
    ?(attr_valid=Unsigned.UInt64.zero)
    ?(attr_valid_nsec=Unsigned.UInt32.zero)
    store_attr_of_node node req = IO.(
    store_attr_of_node node (* can raise ENOENT *)
    >>= fun store_attr ->
    let nodeid = Unsigned.UInt64.of_int64 node.Nodes.id in
    let generation = Unsigned.UInt64.of_int64 node.Nodes.gen in
    IO.Out.write_reply req
      (Profuse.Out.Entry.create ~nodeid ~generation
         ~entry_valid ~entry_valid_nsec
         ~attr_valid ~attr_valid_nsec
         ~store_attr)
  )
end
