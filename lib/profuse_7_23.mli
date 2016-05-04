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
  with module Hdr = Profuse_7_23_specific.Out.Hdr
