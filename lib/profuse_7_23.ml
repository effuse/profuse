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

module Types = Profuse_7_8.Types
module Out =
struct
  include (Profuse_7_8.Out
           : module type of Profuse_7_8.Out
           with module Hdr := Profuse_7_8.Out.Hdr)
  module Hdr = Profuse_7_23_specific.Out.Hdr
end
module In = Profuse_7_8.In
module Struct = Profuse_7_8.Struct
exception Destroy = Profuse_7_8.Destroy
exception ProtocolError = Profuse_7_8.ProtocolError
module Host = Profuse_7_8.Host
module Flags = Profuse_7_8.Flags
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
type ('hdr, 'body) packet = ('hdr, 'body) Profuse_7_8.packet = {
  chan : chan;
  hdr : 'hdr Ctypes.structure;
  pkt : 'body;
}
type 'a request = 'a Profuse_7_8.request
type 'a structure = 'a Profuse_7_8.structure
