include Profuse_signatures.S

module Types : sig
  type 'a structure = 'a Ctypes_static.structure

  open Profuse_signatures.Types_7_8
  module Struct : Profuse_signatures.Types_7_8_struct
  module Out : Profuse_signatures.Types_7_8_out with module Struct := Struct
  module In : Profuse_signatures.Types_7_8_in with module Struct := Struct
end

type 'a structure = 'a Types.structure

module Struct : sig
  module T = Types.Struct
  module Kstatfs : sig
    module T = T.Kstatfs

    val store :
      blocks:Unsigned.uint64 ->
      bfree:Unsigned.uint64 ->
      bavail:Unsigned.uint64 ->
      files:Unsigned.uint64 ->
      ffree:Unsigned.uint64 ->
      bsize:Unsigned.uint32 ->
      namelen:Unsigned.uint32 ->
      frsize:Unsigned.uint32 ->
      T.t structure -> unit

    val create :
      blocks:Unsigned.uint64 ->
      bfree:Unsigned.uint64 ->
      bavail:Unsigned.uint64 ->
      files:Unsigned.uint64 ->
      ffree:Unsigned.uint64 ->
      bsize:Unsigned.uint32 ->
      namelen:Unsigned.uint32 ->
      frsize:Unsigned.uint32 ->
      unit -> T.t structure
  end

  module File_lock : sig
    module T = T.File_lock
  end

  module Attr : sig
    module T = T.Attr

    val store :
      ino:Unsigned.uint64 ->
      size:Unsigned.uint64 ->
      blocks:Unsigned.uint64 ->
      atime:Unsigned.uint64 ->
      mtime:Unsigned.uint64 ->
      ctime:Unsigned.uint64 ->
      atimensec:Unsigned.uint32 ->
      mtimensec:Unsigned.uint32 ->
      ctimensec:Unsigned.uint32 ->
      mode:Unsigned.uint32 ->
      nlink:Unsigned.uint32 ->
      uid:Unsigned.uint32 ->
      gid:Unsigned.uint32 ->
      rdev:Unsigned.uint32 ->
      T.t structure -> unit

    val create :
      ino:Unsigned.uint64 ->
      size:Unsigned.uint64 ->
      blocks:Unsigned.uint64 ->
      atime:Unsigned.uint64 ->
      mtime:Unsigned.uint64 ->
      ctime:Unsigned.uint64 ->
      atimensec:Unsigned.uint32 ->
      mtimensec:Unsigned.uint32 ->
      ctimensec:Unsigned.uint32 ->
      mode:Unsigned.uint32 ->
      nlink:Unsigned.uint32 ->
      uid:Unsigned.uint32 ->
      gid:Unsigned.uint32 ->
      rdev:Unsigned.uint32 ->
      unit -> T.t structure

    val describe : host:Host.t -> T.t structure -> string

  end
end

module In : sig
  module T = Types.In

  module Opcode : sig
    module T = T.Opcode

    type t = T.t

    val to_string : t -> string

    val returns : t -> bool

    val of_uint32 : Unsigned.uint32 -> t

    val to_uint32 : t -> Unsigned.uint32
  end

  module Hdr : sig
    module T = T.Hdr

    val sz : int

    val packet :
      opcode:Opcode.t ->
      unique:Unsigned.uint64 ->
      nodeid:Unsigned.uint64 ->
      uid:Unsigned.uint32 ->
      gid:Unsigned.uint32 ->
      pid:Unsigned.uint32 -> count:int -> char Ctypes.CArray.t

    val make :
      opcode:Opcode.t ->
      unique:Unsigned.uint64 ->
      nodeid:Unsigned.uint64 ->
      uid:Unsigned.uint32 ->
      gid:Unsigned.uint32 -> pid:Unsigned.uint32 -> 'a Ctypes.typ -> 'a

    val memcpy :
      dest:unit Ctypes.ptr -> src:unit Ctypes.ptr -> int -> unit

    val packet_from_hdr :
      T.t structure ->
      count:int -> char Ctypes.CArray.t

    val make_from_hdr :
      T.t structure -> 'a Ctypes.typ -> 'a
  end

  module Init : sig
    module T = T.Init
  end

  module Open : sig
    module T = T.Open
  end

  module Read : sig
    module T = T.Read
  end

  module Release : sig
    module T = T.Release
  end

  module Access : sig
    module T = T.Access
  end

  module Forget : sig
    module T = T.Forget
  end

  module Flush : sig
    module T = T.Flush
  end

  module Create : sig
    module T = T.Create

    val name : unit Ctypes.ptr -> string
  end

  module Mknod : sig
    module T = T.Mknod

    val name : unit Ctypes.ptr -> string
  end

  module Mkdir : sig
    module T = T.Mkdir

    val name : unit Ctypes.ptr -> string
  end

  module Rename : sig
    module T = T.Rename

    val source_destination : unit Ctypes.ptr -> string * string
  end

  module Link : sig
    module T = T.Link

    val name : unit Ctypes.ptr -> string
  end

  module Write : sig
    module T = T.Write
  end

  module Fsync : sig
    module T = T.Fsync
  end

  module Lk : sig
    module T = T.Lk
  end

  module Interrupt : sig
    module T = T.Interrupt
  end

  module Bmap : sig
    module T = T.Bmap
  end

  module Setattr : sig
    module T = T.Setattr

    module Valid : sig
      module T = T.Valid

      type t = {
        mode : bool;
        uid : bool;
        gid : bool;
        size : bool;
        atime : bool;
        mtime : bool;
        fh : bool;
        unknown : int32;
        (*atime_now : bool;
        mtime_now : bool;
          lockowner : bool;*)
      }

      val to_string_list : t -> string list

      val of_uint32 : Unsigned.uint32 -> t

      val to_uint32 : t -> Unsigned.uint32
    end
    
    val create_from_hdr :
      valid:Unsigned.uint32 ->
      fh:Unsigned.uint64 ->
      size:Unsigned.uint64 ->
      atime:Unsigned.uint64 ->
      mtime:Unsigned.uint64 ->
      atimensec:Unsigned.uint32 ->
      mtimensec:Unsigned.uint32 ->
      mode:Unsigned.uint32 ->
      uid:Unsigned.uint32 ->
      gid:Unsigned.uint32 ->
      Hdr.T.t structure -> char Ctypes.CArray.t
  end

  module Getxattr : sig
    module T = T.Getxattr

    val create_from_hdr :
      size:Unsigned.uint32 ->
      Hdr.T.t structure -> char Ctypes.CArray.t
  end

  module Setxattr : sig
    module T = T.Setxattr

    val create_from_hdr :
      size:Unsigned.uint32 ->
      flags:Unsigned.uint32 ->
      Hdr.T.t structure -> char Ctypes.CArray.t
  end

  module Message : sig
    type t =
      | Init of Init.T.t Ctypes.structure
      | Getattr
      | Lookup of string
      | Opendir of Open.T.t Ctypes.structure
      | Readdir of Read.T.t Ctypes.structure
      | Releasedir of Release.T.t Ctypes.structure
      | Fsyncdir of Fsync.T.t Ctypes.structure
      | Rmdir of string
      | Getxattr of Getxattr.T.t Ctypes.structure * string
      | Setxattr of Setxattr.T.t Ctypes.structure * string
      | Listxattr of Getxattr.T.t Ctypes.structure
      | Removexattr of string
      | Access of Access.T.t Ctypes.structure
      | Forget of Forget.T.t Ctypes.structure
      | Readlink
      | Open of Open.T.t Ctypes.structure
      | Read of Read.T.t Ctypes.structure
      | Write of Write.T.t structure * char Ctypes.ptr
      | Statfs
      | Flush of Flush.T.t Ctypes.structure
      | Release of Release.T.t Ctypes.structure
      | Fsync of Fsync.T.t Ctypes.structure
      | Unlink of string
      | Create of Create.T.t Ctypes.structure * string
      | Mknod of Mknod.T.t Ctypes.structure * string
      | Mkdir of Mkdir.T.t Ctypes.structure * string
      | Setattr of Setattr.T.t Ctypes.structure
      | Link of Link.T.t Ctypes.structure * string
      | Symlink of string * string
      | Rename of Rename.T.t Ctypes.structure * string * string
      | Getlk of Lk.T.t Ctypes.structure
      | Setlk of Lk.T.t Ctypes.structure
      | Setlkw of Lk.T.t Ctypes.structure
      | Interrupt of Interrupt.T.t Ctypes.structure
      | Bmap of Bmap.T.t Ctypes.structure
      | Destroy
      | Other of Opcode.t
      | Unknown of int32

    val parse :
      chan -> Hdr.T.t Ctypes.structure -> int -> unit Ctypes.ptr
      -> (Hdr.T.t, t) packet

    val describe : (Hdr.T.t, t) packet -> string
  end
end

type 'a request = (In.Hdr.T.t, 'a) packet

module Out : sig
  module T = Types.Out

  module Hdr : sig
    module T = T.Hdr

    val sz : int

    val packet :
      ?nerrno:int32 ->
      count:int -> 'a request -> char Ctypes.CArray.t

    val make : 'a request -> 'b Ctypes.typ -> 'b

    val set_size : char Ctypes.CArray.t -> int -> char Ctypes.CArray.t
  end

  module Dirent : sig
    module T = Struct.T.Dirent

    val struct_size : int

    val size : string -> int

    val of_list :
      host:Host.t ->
      (int * int64 * string * Dirent.File_kind.t) list ->
      int -> int -> 'a request -> char Ctypes.CArray.t
  end

  module Readlink : sig
    val create :
      target:string -> 'a request -> char Ctypes.CArray.t
  end

  module Read : sig
    val allocate : size:int -> 'a request -> char Ctypes.CArray.t

    val finalize :
      size:int -> char Ctypes.CArray.t -> 'a request -> char Ctypes.CArray.t

    val describe : char Ctypes.CArray.t -> string
  end

  module Write : sig
    module T = T.Write

    val create : size:Unsigned.uint32 -> 'a request -> char Ctypes.CArray.t
  end

  module Statfs : sig
    module T = T.Statfs

    val create :
      blocks:Unsigned.uint64 ->
      bfree:Unsigned.uint64 ->
      bavail:Unsigned.uint64 ->
      files:Unsigned.uint64 ->
      ffree:Unsigned.uint64 ->
      bsize:Unsigned.uint32 ->
      namelen:Unsigned.uint32 ->
      frsize:Unsigned.uint32 ->
      'a request -> char Ctypes.CArray.t
  end

  module Open : sig
    module T = T.Open

    module Flags : sig
      module T = T.Flags

      type t = {
        direct_io  : bool;
        keep_cache : bool;
      }

      val zero : t

      val of_uint32 : Unsigned.uint32 -> t
      val to_uint32 : t -> Unsigned.uint32
      val to_string : t -> string
    end

    val store :
      fh:Unsigned.uint64 ->
      open_flags:Flags.t ->
      T.t structure -> 'a -> unit

    val create :
      fh:Unsigned.uint64 ->
      open_flags:Flags.t ->
      'a request -> char Ctypes.CArray.t
  end

  module Init : sig
    module T = T.Init

    val create :
      major:Unsigned.uint32 ->
      minor:Unsigned.uint32 ->
      max_readahead:Unsigned.uint32 ->
      flags:Unsigned.uint32 ->
      max_write:Unsigned.uint32 ->
      'a request -> char Ctypes.CArray.t
    val describe : T.t structure -> string
  end

  module Entry : sig
    module T = T.Entry

    val store :
      nodeid:Unsigned.uint64 ->
      generation:Unsigned.uint64 ->
      entry_valid:Unsigned.uint64 ->
      attr_valid:Unsigned.uint64 ->
      entry_valid_nsec:Unsigned.uint32 ->
      attr_valid_nsec:Unsigned.uint32 ->
      store_attr:(Types.Struct.Attr.t structure -> unit) ->
      T.t structure -> 'a -> unit

    val create :
      nodeid:Unsigned.uint64 ->
      generation:Unsigned.uint64 ->
      entry_valid:Unsigned.uint64 ->
      attr_valid:Unsigned.uint64 ->
      entry_valid_nsec:Unsigned.uint32 ->
      attr_valid_nsec:Unsigned.uint32 ->
      store_attr:(Types.Struct.Attr.t structure -> unit) ->
      'a request -> char Ctypes.CArray.t

    val describe : host:Host.t -> T.t structure -> string
  end

  module Attr : sig
    module T = T.Attr

    val create :
      attr_valid:Unsigned.uint64 ->
      attr_valid_nsec:Unsigned.uint32 ->
      store_attr:(Types.Struct.Attr.t structure -> unit) ->
      'a request -> char Ctypes.CArray.t
  end

  module Create : sig
    module T : sig
      type t
      val t : t structure Ctypes.typ

      val entry : (Types.Out.Entry.t structure, t structure) Ctypes.field
      val open_ : (Types.Out.Open.t structure, t structure) Ctypes.field
    end

    val create :
      store_entry:(Entry.T.t structure -> 'a request -> unit)
      -> store_open:(Open.T.t structure -> 'a request -> unit)
      -> 'a request -> char Ctypes.CArray.t
  end

  module Message : sig
    type t =
      | Init    of Init.T.t  structure
      | Getattr of Attr.T.t  structure
      | Lookup  of Entry.T.t structure
      | Opendir of Open.T.t  structure
      | Readdir of char Ctypes.CArray.t
      | Releasedir
      | Fsyncdir (* TODO: do *)
      | Rmdir
      | Mkdir   of Entry.T.t structure
      | Getxattr (* TODO: do *)
      | Setxattr (* TODO: do *)
      | Listxattr (* TODO: do *)
      | Removexattr (* TODO: do *)
      | Access
      | Forget (* TODO: should never happen? *)
      | Readlink of string
      | Open     of Open.T.t           structure
      | Read     of char Ctypes.CArray.t
      | Write    of Write.T.t          structure
      | Statfs   of Struct.Kstatfs.T.t structure
      | Flush
      | Release
      | Fsync
      | Unlink
      | Create   of Entry.T.t structure * Open.T.t structure
      | Mknod    of Entry.T.t structure
      | Setattr  of Attr.T.t  structure
      | Link     of Entry.T.t structure
      | Symlink  of Entry.T.t structure
      | Rename
      | Getlk (* TODO: do *)
      | Setlk (* TODO: do *)
      | Setlkw (* TODO: do *)
      | Interrupt (* TODO: do *)
      | Bmap (* TODO: do *)
      | Destroy
      | Other    of In.Opcode.t
      | Unknown  of int32 * int * unit Ctypes.ptr

    val deserialize :
      (In.Hdr.T.t, 'a) packet -> int -> char Ctypes.ptr
      -> (Hdr.T.t, t) packet

    val describe : ('a,t) packet -> string
  end
end
