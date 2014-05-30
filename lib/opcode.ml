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

exception Unknown of int

type t =
(* ro *)
| FUSE_LOOKUP (* = 1 *)
| FUSE_FORGET (* no reply *)
| FUSE_GETATTR
(* end ro *)

| FUSE_SETATTR
| FUSE_READLINK
| FUSE_SYMLINK

| FUSE_MKNOD (* = 8 *)
| FUSE_MKDIR
| FUSE_UNLINK
| FUSE_RMDIR
| FUSE_RENAME
| FUSE_LINK

(* ro *)
| FUSE_OPEN (* = 14 *)
| FUSE_READ
(* end ro *)

| FUSE_WRITE
| FUSE_STATFS

(* ro *)
| FUSE_RELEASE (* = 18 *) (* 0 reply? *)
(* end ro *)

| FUSE_FSYNC (* = 20 *)
| FUSE_SETXATTR
| FUSE_GETXATTR
| FUSE_LISTXATTR
| FUSE_REMOVEXATTR
| FUSE_FLUSH (* 0 reply *)

(* ro *)
| FUSE_INIT (* = 26 *)
| FUSE_OPENDIR
| FUSE_READDIR
| FUSE_RELEASEDIR (* 0 reply *)
(* end ro *)

| FUSE_FSYNCDIR (* = 30 *)
| FUSE_GETLK
| FUSE_SETLK
| FUSE_SETLKW
| FUSE_ACCESS
| FUSE_CREATE
| FUSE_INTERRUPT
| FUSE_BMAP

(* ro *)
| FUSE_DESTROY (* = 38 *) (* 0 reply *)
(* end ro *)

(* > 7.8 *)
| FUSE_IOCTL
| FUSE_POLL
| FUSE_NOTIFY_REPLY
| FUSE_BATCH_FORGET
| FUSE_FALLOCATE

(* OS X *)
| FUSE_SETVOLNAME (* = 61 *)
| FUSE_GETXTIMES
| FUSE_EXCHANGE

let to_string = function
  | FUSE_LOOKUP -> "FUSE_LOOKUP"
  | FUSE_FORGET -> "FUSE_FORGET"
  | FUSE_GETATTR -> "FUSE_GETATTR"
  | FUSE_SETATTR -> "FUSE_SETATTR"
  | FUSE_READLINK -> "FUSE_READLINK"
  | FUSE_SYMLINK -> "FUSE_SYMLINK"

  | FUSE_MKNOD -> "FUSE_MKNOD"
  | FUSE_MKDIR -> "FUSE_MKDIR"
  | FUSE_UNLINK -> "FUSE_UNLINK"
  | FUSE_RMDIR -> "FUSE_RMDIR"
  | FUSE_RENAME -> "FUSE_RENAME"
  | FUSE_LINK -> "FUSE_LINK"
  | FUSE_OPEN -> "FUSE_OPEN"
  | FUSE_READ -> "FUSE_READ"
  | FUSE_WRITE -> "FUSE_WRITE"
  | FUSE_STATFS -> "FUSE_STATFS"
  | FUSE_RELEASE -> "FUSE_RELEASE"

  | FUSE_FSYNC -> "FUSE_FSYNC"
  | FUSE_SETXATTR -> "FUSE_SETXATTR"
  | FUSE_GETXATTR -> "FUSE_GETXATTR"
  | FUSE_LISTXATTR -> "FUSE_LISTXATTR"
  | FUSE_REMOVEXATTR -> "FUSE_REMOVEXATTR"
  | FUSE_FLUSH -> "FUSE_FLUSH"
  | FUSE_INIT -> "FUSE_INIT"
  | FUSE_OPENDIR -> "FUSE_OPENDIR"
  | FUSE_READDIR -> "FUSE_READDIR"
  | FUSE_RELEASEDIR -> "FUSE_RELEASEDIR"
  | FUSE_FSYNCDIR -> "FUSE_FSYNCDIR"
  | FUSE_GETLK -> "FUSE_GETLK"
  | FUSE_SETLK -> "FUSE_SETLK"
  | FUSE_SETLKW -> "FUSE_SETLKW"
  | FUSE_ACCESS -> "FUSE_ACCESS"
  | FUSE_CREATE -> "FUSE_CREATE"
  | FUSE_INTERRUPT -> "FUSE_INTERRUPT"
  | FUSE_BMAP -> "FUSE_BMAP"
  | FUSE_DESTROY -> "FUSE_DESTROY"

  | FUSE_IOCTL -> "FUSE_IOCTL"
  | FUSE_POLL -> "FUSE_POLL"
  | FUSE_NOTIFY_REPLY -> "FUSE_NOTIFY_REPLY"
  | FUSE_BATCH_FORGET -> "FUSE_BATCH_FORGET"
  | FUSE_FALLOCATE -> "FUSE_FALLOCATE"

  | FUSE_SETVOLNAME -> "FUSE_SETVOLNAME"
  | FUSE_GETXTIMES -> "FUSE_GETXTIMES"
  | FUSE_EXCHANGE -> "FUSE_EXCHANGE"

let to_int = function
  | FUSE_LOOKUP       -> 1
  | FUSE_FORGET       -> 2
  | FUSE_GETATTR      -> 3
  | FUSE_SETATTR      -> 4
  | FUSE_READLINK     -> 5
  | FUSE_SYMLINK      -> 6

  | FUSE_MKNOD        -> 8
  | FUSE_MKDIR        -> 9
  | FUSE_UNLINK       -> 10
  | FUSE_RMDIR        -> 11
  | FUSE_RENAME       -> 12
  | FUSE_LINK         -> 13
  | FUSE_OPEN         -> 14
  | FUSE_READ         -> 15
  | FUSE_WRITE        -> 16
  | FUSE_STATFS       -> 17
  | FUSE_RELEASE      -> 18

  | FUSE_FSYNC        -> 20
  | FUSE_SETXATTR     -> 21
  | FUSE_GETXATTR     -> 22
  | FUSE_LISTXATTR    -> 23
  | FUSE_REMOVEXATTR  -> 24
  | FUSE_FLUSH        -> 25
  | FUSE_INIT         -> 26
  | FUSE_OPENDIR      -> 27
  | FUSE_READDIR      -> 28
  | FUSE_RELEASEDIR   -> 29
  | FUSE_FSYNCDIR     -> 30
  | FUSE_GETLK        -> 31
  | FUSE_SETLK        -> 32
  | FUSE_SETLKW       -> 33
  | FUSE_ACCESS       -> 34
  | FUSE_CREATE       -> 35
  | FUSE_INTERRUPT    -> 36
  | FUSE_BMAP         -> 37
  | FUSE_DESTROY      -> 38

  | FUSE_IOCTL        -> 39
  | FUSE_POLL         -> 40
  | FUSE_NOTIFY_REPLY -> 41
  | FUSE_BATCH_FORGET -> 42
  | FUSE_FALLOCATE    -> 43

  | FUSE_SETVOLNAME   -> 61
  | FUSE_GETXTIMES    -> 62
  | FUSE_EXCHANGE     -> 63

let of_uint32 i = match UInt32.to_int i with
  |       1  -> FUSE_LOOKUP
  |       2  -> FUSE_FORGET
  |      3  -> FUSE_GETATTR
  |      4  -> FUSE_SETATTR
  |     5  -> FUSE_READLINK
  |      6  -> FUSE_SYMLINK

  |        8  -> FUSE_MKNOD
  |        9  -> FUSE_MKDIR
  |      10  -> FUSE_UNLINK
  |       11  -> FUSE_RMDIR
  |      12  -> FUSE_RENAME
  |        13  -> FUSE_LINK
  |        14  -> FUSE_OPEN
  |        15  -> FUSE_READ
  |       16  -> FUSE_WRITE
  |      17  -> FUSE_STATFS
  |     18  -> FUSE_RELEASE

  |       20  -> FUSE_FSYNC
  |    21  -> FUSE_SETXATTR
  |    22  -> FUSE_GETXATTR
  |   23  -> FUSE_LISTXATTR
  | 24  -> FUSE_REMOVEXATTR
  |       25  -> FUSE_FLUSH
  |        26  -> FUSE_INIT
  |     27  -> FUSE_OPENDIR
  |     28  -> FUSE_READDIR
  |  29  -> FUSE_RELEASEDIR
  |    30  -> FUSE_FSYNCDIR
  |       31  -> FUSE_GETLK
  |       32  -> FUSE_SETLK
  |      33  -> FUSE_SETLKW
  |      34  -> FUSE_ACCESS
  |      35  -> FUSE_CREATE
  |   36  -> FUSE_INTERRUPT
  |        37  -> FUSE_BMAP
  |     38  -> FUSE_DESTROY

  |       39  -> FUSE_IOCTL
  |        40  -> FUSE_POLL
  | 41 -> FUSE_NOTIFY_REPLY
  | 42 -> FUSE_BATCH_FORGET
  |   43  -> FUSE_FALLOCATE

  |  61  -> FUSE_SETVOLNAME
  |   62  -> FUSE_GETXTIMES
  |    63  -> FUSE_EXCHANGE

  |  k -> raise (Unknown k)

let view = view
  ~read:of_uint32
  ~write:(fun o -> UInt32.of_int (to_int o))
  uint32_t

let returns = function
  | FUSE_FORGET -> false
  | _ -> true
