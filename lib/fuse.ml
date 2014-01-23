type flags = int32

type chan = {
  fd : Unix.file_descr;
  mnt : string;
  version : int * int;
  max_readahead : int;
  max_write : int;
  flags : flags;
}

type 'a request = {
  chan : chan;
  hdr  : In_common.Hdr.t Ctypes.structure;
  pkt  : 'a;
}

exception UnknownErrno of Unix.error
exception ExecError of string * string
exception ProtocolError of chan * string

type host = {
  unix_fcntl : Unix_fcntl.host;
}

module type HOST = sig
  val host : host
  module In : sig
      
  end
end
