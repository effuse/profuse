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

type flags = int32

type host = {
  unix_fcntl    : Unix_fcntl.host;
  unix_errno    : Unix_errno.host;
  unix_sys_stat : Unix_sys_stat.host;
  unix_dirent   : Unix_dirent.host;
  unix_unistd   : Unix_unistd.host;
}

type chan = {
  fd : Unix.file_descr;
  mutable unique : Unsigned.uint64;
  mnt : string;
  version : int * int;
  max_readahead : int;
  max_write : int;
  flags : flags;
  host : host;
}

type ('hdr, 'body) packet = {
  chan : chan;
  hdr  : 'hdr Ctypes.structure;
  pkt  : 'body;
}

exception UnknownErrno of Unix.error
exception ExecError of string * string
exception ProtocolError of chan * string
exception Destroy of int

let host = {
  unix_fcntl    = Unix_fcntl.host;
  unix_errno    = Unix_errno.host;
  unix_sys_stat = Unix_sys_stat.host;
  unix_dirent   = Unix_dirent.host;
  unix_unistd   = Unix_unistd.host;
}
