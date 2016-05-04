module Flags : module type of Profuse_7_8.Flags
  with type t = Profuse_7_8.Flags.t
module Host : module type of Profuse_7_8.Host
  with type t = Profuse_7_8.Host.t

type chan = Profuse_7_8.chan = {
  id : int;
  mutable unique : Unsigned.uint64;
  mnt : string;
  version : int * int;
  max_readahead : int;
  max_write : int;
  flags : Flags.t;
  host : Host.t;
}

exception ProtocolError of chan * string
exception Destroy of int

type ('hdr, 'body) packet = ('hdr, 'body) Profuse_7_8.packet = {
  chan : chan;
  hdr : 'hdr Ctypes.structure;
  pkt : 'body;
}

module Types : module type of Profuse_7_8.Types 

type 'a structure = 'a Types.structure

module Struct : module type of Profuse_7_8.Struct
module In : module type of Profuse_7_8.In

type 'a request = (In.Hdr.T.t, 'a) packet

module Out : module type of Profuse_7_8.Out
