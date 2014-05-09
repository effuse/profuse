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

(* This module manages a network of asset identities as subprocesses. *)

(* WARNING: DOES NOT PROVIDE ISOLATION!
   If an agent is compromised, it can compromise the handler and the handler
   can compromise the parent.
   WARNING: DOES NOT PROVIDE ISOLATION!
 *)

type 'a result =
| Ok of 'a
| Unix_error of Unix.error * string * string
| Err of exn
type _ command =
| Mkdir  : string * int32 -> unit command
| Mknod  : string * int32 * int32 -> unit command
| Access : string * Unix.access_permission list -> unit command
| Chown  : string * int32 * int32 -> unit command
| Rmdir  : string -> unit command
type 'a directive = {
  uid : int32;
  gid : int32;
  cmd : 'a command;
}
type 'a interp = uid:int32 -> gid:int32 -> 'a command -> 'a result
type 'a perspective = uid:int32 -> gid:int32 -> 'a
type interp_box = { interp : 'a. 'a interp }
type t = {
  mkdir  : (string -> int32 -> unit) perspective;
  mknod  : (string -> int32 -> int32 -> unit) perspective;
  access : (string -> Unix.access_permission list -> unit) perspective;
  chown  : (string -> int32 -> int32 -> unit) perspective;
  rmdir  : (string -> unit) perspective;
}

let to_mode_t i = PosixTypes.(Ctypes.(Unsigned.(
  coerce uint32_t mode_t (UInt32.of_int32 i)
)))
let to_dev_t i = PosixTypes.(Ctypes.(Unsigned.(
  coerce uint64_t dev_t (UInt64.of_int64 (Int64.of_int32 i))
)))

let unwrap_result = function
  | Ok retval -> retval
  | Unix_error (err, call, arg) -> raise (Unix.Unix_error (err, call, arg))
  | Err exn -> raise exn

let make box = let { interp } = box in {
  mkdir = (fun ~uid ~gid path perms ->
    unwrap_result (interp ~uid ~gid (Mkdir (path, perms))));
  mknod = (fun ~uid ~gid path mode dev ->
    unwrap_result (interp ~uid ~gid (Mknod (path, mode, dev))));
  access = (fun ~uid ~gid path perms ->
    unwrap_result (interp ~uid ~gid (Access (path, perms))));
  chown = (fun ~uid ~gid path u g ->
    unwrap_result (interp ~uid ~gid (Chown (path, u, g))));
  rmdir = (fun ~uid ~gid path ->
    unwrap_result (interp ~uid ~gid (Rmdir path)));
}

let fork_proc listen request =
  let request_in, request_out = Unix.pipe () in
  let reply_in, reply_out = Unix.pipe () in
  let child = Unix.fork () in
  if child = 0
  then begin
    Unix.close request_out;
    Unix.close reply_in;
    ignore (Unix.umask 0o000);
    listen
      (Unix.in_channel_of_descr request_in)
      (Unix.out_channel_of_descr reply_out)
  end
  else begin
    at_exit (fun () ->
      Unix.close request_out;
      Unix.close reply_in;
    );
    Unix.close request_in;
    Unix.close reply_out;
    request
      (Unix.out_channel_of_descr request_out)
      (Unix.in_channel_of_descr reply_in)
  end

let agent_request (type a) request reply (cmd : a command) : a result =
  Marshal.to_channel request cmd [];
  flush request;
  ((Marshal.from_channel reply) : a result)

let reply (type a) : a command -> a result = fun cmd ->
  try Ok (match cmd with
  | Mkdir (path,perms) -> Unix.mkdir path (Int32.to_int perms)
  | Mknod (path,mode,dev) ->
    Unix_sys_stat.mknod path (to_mode_t mode) (to_dev_t dev)
  | Access (path,perms) -> Unix.access path perms
  | Chown (path, uid, gid) ->
    Unix.chown path (Int32.to_int uid) (Int32.to_int gid)
  | Rmdir path -> Unix.rmdir path
  ) with
  | Unix.Unix_error (err, call, arg) -> Unix_error (err, call, arg)
  | exn -> Err exn

let create_agent uid gid =
  fork_proc (fun (type a) requestc replyc ->
    Unix.setgid (Int32.to_int gid);
    Unix.setuid (Int32.to_int uid);
    begin try while true do
        Marshal.to_channel replyc
          ((reply ((Marshal.from_channel requestc) : a command)) : a result) [];
      flush replyc
      done with End_of_file -> exit 0
    end;
    exit 0
  ) agent_request

let listen (type a) request reply =
  let agents = Hashtbl.create 8 in
  begin try while true do
      let directive : a directive = Marshal.from_channel request in
      let { uid; gid; cmd } = directive in
      let agent =
        try
          Hashtbl.find agents (uid,gid)
        with Not_found ->
          let a = create_agent uid gid in
          Hashtbl.replace agents (uid,gid) a;
          a
      in
      Marshal.to_channel reply (agent cmd) [];
      flush reply
    done with End_of_file -> exit 0
  end;
  exit 0

let request request reply = {
  interp = (fun (type a) ~uid ~gid (cmd : a command) ->
    Marshal.to_channel request { uid; gid; cmd } [];
    flush request;
    ((Marshal.from_channel reply) : a result)
  )
}

let create () = make (fork_proc listen request)
