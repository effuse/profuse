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

exception Remote_shutdown

let rec receive_bytes fd buf offset len flags =
  let recvd = Unix.recv fd buf offset len flags in
  if recvd = 0 then raise Remote_shutdown
  else begin
    Printf.eprintf "Read %d bytes of %d\n%!" recvd len;
    if recvd <> len
    then receive_bytes fd buf (offset + recvd) (len - recvd) flags
    else ()
  end

(* TODO: don't copy into string *)
let receive fd =
  let sz_buf = "\000\000\000\000" in
  receive_bytes fd sz_buf 0 4 Unix.([MSG_PEEK]);
  let sz = (* TODO: endianness? *)
    (int_of_char sz_buf.[3]) lsl 24
    + (int_of_char sz_buf.[2]) lsl 16
    + (int_of_char sz_buf.[1]) lsl 8
    + (int_of_char sz_buf.[0])
  in
  let buf = String.create sz in
  receive_bytes fd buf 0 sz [];
  buf

let parse_server argv0 server =
  match Stringext.split ~max:2 server ~on:':' with
  | [] -> assert false
  | [_] -> (Printf.eprintf "%s : malformed ipaddr:port\n%!" argv0; exit 1)
  | server::port::_ -> server, int_of_string port

module Client = struct
  (* TODO: negotiate endianness, host profile, and protocol flavor *)
  (* TODO: pass command-line mount parameters *)

  module In = In.Linux_7_8
  module Support = Profuse.Linux_7_8(In)(Out.Linux_7_8)
  module Trace_linux_7_8 = Profuse.Trace(In)(Out.Linux_7_8)
  let out   = (module Out.Linux_7_8 : Out.LINUX_7_8)
  let trace = (module Trace_linux_7_8 : Out.LINUX_7_8)

  type conn = {
    fd   : Unix.file_descr;
    addr : Unix.sockaddr;
  }

  type t = conn option

  let make () = None

  let fd = function None -> raise (Failure "no endpoint") | Some { fd } -> fd

  let string_of_nodeid id _st = Int64.to_string id

  let mount_wrapper reply ~argv ~mnt st =
    let argc = Array.length argv in
    let server =
      if argc > 1
      then argv.(argc - 1)
      else (Printf.eprintf "%s : missing endpoint argument\n%!" argv.(0); exit 1)
    in
    let server, port = parse_server argv.(0) server in
    let argv = Array.sub argv 0 (argc - 1) in
    let fd = Unix.(socket PF_INET SOCK_STREAM 0) in
    let addr = Unix.(ADDR_INET (inet_addr_of_string server, port)) in
    Unix.connect fd addr;
    Support.(mount reply ~argv ~mnt (Some { fd; addr }))

  (* TODO: don't copy into string *)
  let remote (module Out : Out.LINUX_7_8) req st =
    let length = Ctypes.getf req.Fuse.hdr In.Hdr.size in
    let ptr = Ctypes.(
      coerce (ptr void) (ptr char) (to_voidp (addr req.Fuse.hdr))
    ) in
    let buf = Ctypes.string_from_ptr ptr ~length in
    let sent = Unix.send (fd st) buf 0 length [] in
    assert (length = sent); (* TODO: Check? Buffer? *)
    let opcode = Ctypes.getf req.Fuse.hdr In.Hdr.opcode in
    if Opcode.returns opcode
    then
      let str = receive (fd st) in
      let sz = String.length str in
      let buf = Ctypes.(allocate_n char ~count:sz) in
      Ctypes.(for i=0 to sz - 1 do (buf +@ i) <-@ str.[i] done);
      Out.write_reply_raw req sz buf;
      buf
    else Ctypes.( (* TODO: ? *)
      coerce (ptr void) (ptr char) (ptr_of_raw_address Int64.zero)
    )

  (* TODO: update chan *)
  let proxy_mount out _pkt req st =
    let _buf = remote out req st in
    let chan = req.Fuse.chan in
    (*let chan = Fuse.({ chan with
      version = (major, minor); max_readahead; flags;
      }) in*)
    { req with Fuse.chan }, st

  let mount = mount_wrapper (proxy_mount out)
  let mount_trace = mount_wrapper (proxy_mount trace)

  let serve chan =
    let read = In.read chan in
    fun t -> ignore (remote out (read ()) t : char Ctypes.ptr); t

  (* TODO: trace the return messages *)
  let trace chan =
    let read = In.read chan in
    fun tag t ->
      let req = read () in
      (* can raise Opcode.Unknown? *)
      Printf.fprintf Out.Linux_7_8.trace_channel "    %s %s\n%!"
        tag (Support.string_of_request string_of_nodeid req t);
      ignore (remote trace req t : char Ctypes.ptr);
      t
end

module Server : Profuse.FS_SERVER =
  functor (In : In.LINUX_7_8) -> functor (Out : Out.LINUX_7_8) ->
    functor (Fs : Profuse.FS) ->
struct
  module In = In
  module Trace = Profuse.Trace(In)(Out)

  module S = Profuse.Server(In)(Out)(Fs)

  module Fs_trace = Fs.Linux_7_8(In)(Trace)
  module Fs = Fs.Linux_7_8(In)(Out)

  type t = Fs.t

  let serve = S.serve
  let trace = S.trace

  let mount_wrapper reply ~argv ~mnt st =
    let max_write = 1 lsl 16 in

    let interface, port = parse_server argv.(0) mnt in
    let fd = Unix.(socket PF_INET SOCK_STREAM 0) in
    let addr = Unix.(ADDR_INET (inet_addr_of_string interface, port)) in
    Unix.bind fd addr;
    Unix.listen fd 1;
    Printf.eprintf "before accept\n%!";
    let fd, remote = Unix.accept fd in
    Printf.eprintf "after accept for %s\n%!" (match remote with
    | Unix.ADDR_UNIX s -> s
    | Unix.ADDR_INET (addr, port) ->
      (Unix.string_of_inet_addr addr)^":"^(string_of_int port)
    );

    let init_fs = Fuse.({
      mnt; fd; unique=Unsigned.UInt64.zero; version=(0,0);
      max_readahead=0; max_write; flags=Profuse.no_flags; host;
    }) in
    let req = In.read init_fs () in

    In.(match req with
    | { Fuse.pkt=Init pkt } -> reply pkt req st
    | { Fuse.hdr } -> raise
      (Fuse.ProtocolError
         (init_fs, Printf.sprintf "Unexpected opcode %s <> FUSE_INIT"
           (Opcode.to_string (Ctypes.getf hdr Hdr.opcode))))
    )

  let mount = mount_wrapper Fs.negotiate_mount
  let mount_trace = mount_wrapper Fs_trace.negotiate_mount
end
