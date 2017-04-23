include Profuse_signatures.S

module Types : sig
  type 'a structure = 'a Ctypes_static.structure

  open Profuse_signatures.Types_7_8
  module Struct : Profuse_signatures.Types_7_8_struct
  module Out : Profuse_signatures.Types_7_8_out with module Struct := Struct
  module In : Profuse_signatures.Types_7_8_in with module Struct := Struct
end

type 'a structure = 'a Types.structure

module Struct : Profuse_signatures.Signatures_7_8_struct
  with module T = Types.Struct
   and type host_t := Host.t

module In : Profuse_signatures.Signatures_7_8_in
  with module Struct := Types.Struct
   and module T = Types.In
   and type chan := chan
   and type ('h, 'b) packet := ('h, 'b) packet

type 'a request = (In.Hdr.T.t, 'a) packet

module Out : Profuse_signatures.Signatures_7_8_out 
  with module Struct := Types.Struct
   and module In := Types.In
   and module T = Types.Out
   and type 'a request := 'a request
   and type host_t := Host.t
   and type ('h, 'b) packet := ('h, 'b) packet
