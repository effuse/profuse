
module Profuse = Profuse_7_23

module Nodes = Nodes

type 'a structure = 'a Ctypes_static.structure
type request = Profuse.In.Message.t Profuse.request

module type IO = sig
  type 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val return : 'a -> 'a t

  val fail : exn -> 'a t

  module In : sig
    val read : Profuse.chan -> unit -> request t
  end

  module Out : sig
    val write_reply :
      'a Profuse.request ->
      ('a Profuse.request -> char Ctypes.CArray.t) -> unit t

    val write_ack : request -> unit t

    val write_error : (string -> unit) -> request -> Errno.t -> unit t
  end
end

module type RO_SIMPLE = sig
  module IO : IO
  type 'a req_handler = request -> 'a -> 'a IO.t

  type t
    
  val getattr : t req_handler
  val opendir : Profuse.In.Open.T.t structure -> t req_handler
  val forget : Unsigned.UInt64.t -> t req_handler
  val batch_forget :
    Profuse.Struct.Forget_one.T.t structure list -> t req_handler
  val lookup : string -> t req_handler
  val readdir : Profuse.In.Read.T.t structure -> t req_handler
  val readlink : t req_handler
  val release : Profuse.In.Release.T.t structure -> t req_handler
  val releasedir : Profuse.In.Release.T.t structure -> t req_handler
  val open_ : Profuse.In.Open.T.t structure -> t req_handler
  val read : Profuse.In.Read.T.t structure -> t req_handler
  val access : Profuse.In.Access.T.t structure -> t req_handler
  val destroy : t req_handler
end

module type RO_MID = sig
  include RO_SIMPLE

  val statfs : t req_handler
end

module type RO_FULL = sig
  include RO_MID

  val getxattr : Profuse.In.Getxattr.T.t structure -> t req_handler
  val listxattr : Profuse.In.Getxattr.T.t structure -> t req_handler
  val interrupt : Profuse.In.Interrupt.T.t structure -> t req_handler
  val bmap : Profuse.In.Bmap.T.t structure -> t req_handler
end

module type RW_SIMPLE = sig
  include RO_SIMPLE

  val flush : Profuse.In.Flush.T.t structure -> t req_handler
  val link : Profuse.In.Link.T.t structure -> string -> t req_handler
  val symlink : string -> string -> t req_handler
  val rename :
    Profuse.In.Rename.T.t structure -> string -> string -> t req_handler
  val unlink : string -> t req_handler
  val rmdir : string -> t req_handler
  val mknod : Profuse.In.Mknod.T.t structure -> string -> t req_handler
  val write :
    Profuse.In.Write.T.t structure -> char Ctypes.ptr -> t req_handler
  val mkdir : Profuse.In.Mkdir.T.t structure -> string -> t req_handler
  val setattr : Profuse.In.Setattr.T.t structure -> t req_handler
end

module type RW_MID = sig
  include RO_MID
  include RW_SIMPLE
    with module IO := IO
     and type t := t
     and type 'a req_handler := 'a req_handler

  val create : Profuse.In.Create.T.t structure -> string -> t req_handler
  val fsync  : Profuse.In.Fsync.T.t structure -> t req_handler
end

module type RW_FULL = sig
  include RO_FULL
  include RW_MID
    with module IO := IO
     and type t := t
     and type 'a req_handler := 'a req_handler

  val fsyncdir    : Profuse.In.Fsync.T.t structure -> t req_handler
  val setxattr    : Profuse.In.Setxattr.T.t structure -> t req_handler
  val removexattr : string -> t req_handler
  val getlk       : Profuse.In.Lk.T.t structure -> t req_handler
  val setlk       : Profuse.In.Lk.T.t structure -> t req_handler
  val setlkw      : Profuse.In.Lk.T.t structure -> t req_handler
end

module type FS_IO = sig
  include RW_FULL

  val negotiate_mount :
    Profuse.In.Init.T.t structure -> request -> t -> (request * t) IO.t

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

module Zero :
  functor (State : STATE) ->
  functor (IO : IO) ->
    RW_FULL with module IO = IO and type t = State.t

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

module Support : functor(IO : IO) -> sig
  
  val negotiate_mount :
    Profuse.In.Init.T.t structure -> request -> 'b -> (request * 'b) IO.t

  val nodeid : request -> Unsigned.uint64

  val enosys : request -> 'a -> 'a IO.t

  val store_entry :
    ?entry_valid:Unsigned.uint64 ->
    ?entry_valid_nsec:Unsigned.uint32 ->
    ?attr_valid:Unsigned.uint64 ->
    ?attr_valid_nsec:Unsigned.uint32 ->
    ('a Nodes.node -> (Profuse.Struct.Attr.T.t structure -> unit) IO.t)
    -> 'a Nodes.node
    -> (Profuse.Out.Entry.T.t structure -> request -> unit) IO.t

  val respond_with_entry :
    ?entry_valid:Unsigned.uint64 ->
    ?entry_valid_nsec:Unsigned.uint32 ->
    ?attr_valid:Unsigned.uint64 ->
    ?attr_valid_nsec:Unsigned.uint32 ->
    ('a Nodes.node -> (Profuse.Struct.Attr.T.t structure -> unit) IO.t)
    -> 'a Nodes.node -> request -> unit IO.t
end
