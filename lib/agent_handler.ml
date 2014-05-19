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

(* TODO: uid/gid map should be moved to primary process and handler
   should only spawn subprocesses to be connected to the primary
   process via fd passing instead of intermediating all commands. *)

(* WARNING: DOES NOT PROVIDE ISOLATION!
   If an agent is compromised, it can compromise the handler and the handler
   can compromise the parent.
   WARNING: DOES NOT PROVIDE ISOLATION!
 *)

type 'a result =
| Ok of 'a
| Unix_error of Unix.error * string * string
| Err of exn
type fd = Unix.file_descr

type effect    = < result : unit ; fd : unit >
type open_chan = < result : fd   ; fd : unit >
type _ cmd =
| Mkdir  : string * int32 -> effect cmd
| Mknod  : string * int32 * int32 -> effect cmd
| Access : string * Unix.access_permission list -> effect cmd
| Chown  : string * int32 * int32 -> effect cmd
| Rmdir  : string -> effect cmd
| Open   : string * Unix.open_flag list * int32 -> open_chan cmd
type 'a directive = {
  uid : int32;
  gid : int32;
  cmd : 'a cmd;
}

type ('a, 'b) interp =
  uid:int32 -> gid:int32 -> < result : 'a; fd : 'b > cmd -> 'a result

type 'a perspective = uid:int32 -> gid:int32 -> 'a
type interp_box = { interp : 'a 'r. ('a,'r) interp }
type t = {
  mkdir  : (string -> int32 -> unit) perspective;
  mknod  : (string -> int32 -> int32 -> unit) perspective;
  access : (string -> Unix.access_permission list -> unit) perspective;
  chown  : (string -> int32 -> int32 -> unit) perspective;
  rmdir  : (string -> unit) perspective;
  open_  : (string -> Unix.open_flag list -> int32 -> fd) perspective;
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
  open_ = (fun ~uid ~gid path flags mode ->
    unwrap_result (interp ~uid ~gid (Open (path, flags, mode))));
}

let fork_proc listen request =
  let request_in, request_out = Unix.pipe () in
  let reply_in,   reply_out   = Unix.pipe () in
  let pfd_sock,   cfd_sock    = Unix.(socketpair PF_UNIX SOCK_STREAM 0) in
  let child = Unix.fork () in
  if child = 0
  then begin
    Unix.close request_out;
    Unix.close reply_in;
    Unix.close pfd_sock;
    ignore (Unix.umask 0o000);
    listen
      (Unix.in_channel_of_descr request_in)
      (Unix.out_channel_of_descr reply_out)
      cfd_sock
  end
  else begin
    at_exit (fun () ->
      Unix.close request_out;
      Unix.close reply_in;
      Unix.close pfd_sock;
    );
    Unix.close request_in;
    Unix.close reply_out;
    Unix.close cfd_sock;
    request
      (Unix.out_channel_of_descr request_out)
      (Unix.in_channel_of_descr reply_in)
      pfd_sock
  end

let recv_fd fdsock =
  let _, _, fd = Fd_send_recv.recv_fd fdsock "\000" 0 1 [] in
  fd

let send_fd fdsock fd = (* TODO: check? *)
  ignore (Fd_send_recv.send_fd fdsock "\000" 0 1 [] fd)

let pipe_fd from_sock to_sock =
  let fd = recv_fd from_sock in
  send_fd to_sock fd;
  Unix.close fd

let cmd_send_fd (type a) fdsock : a cmd -> unit = function
  | Mkdir _  -> ()
  | Mknod _  -> ()
  | Access _ -> ()
  | Chown _  -> ()
  | Rmdir _  -> ()
  | Open _   -> ()

let cmd_recv_fd (type a) fdsock : a cmd -> a cmd = fun cmd -> match cmd with
  | Mkdir _  -> cmd
  | Mknod _  -> cmd
  | Access _ -> cmd
  | Chown _  -> cmd
  | Rmdir _  -> cmd
  | Open _   -> cmd

let cmd_pipe_fd psock csock cmd = cmd_send_fd csock (cmd_recv_fd psock cmd)

let result_patch_fd fdsock = function
  | Ok () -> Ok (recv_fd fdsock)
  | (Unix_error (_,_,_) | Err _) as r -> r

let is_ok = function Ok _ -> true | Unix_error (_,_,_) | Err _ -> false

let result_pipe_fd (type a) (type b) from_sock to_sock
    : b result -> < result : a; fd : b > cmd -> unit =
  fun result -> function
  | Mkdir _  -> ()
  | Mknod _  -> ()
  | Access _ -> ()
  | Chown _  -> ()
  | Rmdir _  -> ()
  | Open _   -> if is_ok result then pipe_fd from_sock to_sock

let result_recv_fd (type a) (type b) fdsock
    : < result : a; fd : b > cmd -> b result -> a result =
  fun cmd result -> match cmd with
  | Mkdir _  -> result
  | Mknod _  -> result
  | Access _ -> result
  | Chown _  -> result
  | Rmdir _  -> result
  | Open _   -> result_patch_fd fdsock result

let reply (type a) (type b)
    : Unix.file_descr -> < result : a; fd : b > cmd -> b result =
  fun fdsock cmd -> try Ok (match cmd with
  | Mkdir (path,perms) -> Unix.mkdir path (Int32.to_int perms)
  | Mknod (path,mode,dev) ->
    Unix_sys_stat.mknod path (to_mode_t mode) (to_dev_t dev)
  | Access (path,perms) -> Unix.access path perms
  | Chown (path, uid, gid) ->
    Unix.chown path (Int32.to_int uid) (Int32.to_int gid)
  | Rmdir path -> Unix.rmdir path
  | Open (path, flags, mode) ->
    let fd = Unix.openfile path flags (Int32.to_int mode) in
    send_fd fdsock fd;
    Unix.close fd
  ) with
  | Unix.Unix_error (err, call, arg) -> Unix_error (err, call, arg)
  | exn -> Err exn

let agent_request (type a) (type b) psock request reply csock
    : < result : a; fd : b > cmd -> b result =
  fun cmd ->
    cmd_pipe_fd psock csock cmd;
    Marshal.to_channel request cmd [];
    flush request;
    let result : b result = Marshal.from_channel reply in
    result_pipe_fd csock psock result cmd;
    result

let create_agent psock uid gid =
  fork_proc (fun (type a) (type b) requestc replyc fdsock ->
    Unix.setgid (Int32.to_int gid);
    Unix.setuid (Int32.to_int uid);
    begin try while true do
        let cmd : < result : a ; fd : b > cmd = Marshal.from_channel requestc in
        let cmd = cmd_recv_fd fdsock cmd in
        Marshal.to_channel replyc ((reply fdsock cmd) : b result) [];
        flush replyc
      done with End_of_file -> exit 0
    end;
    exit 0
  ) (agent_request psock)

let listen (type a) (type b) request reply fdsock =
  let agents = Hashtbl.create 8 in
  begin try while true do
      let directive : < result : a ; fd : b > directive =
        Marshal.from_channel request
      in
      let { uid; gid; cmd } = directive in
      let agent =
        try
          Hashtbl.find agents (uid,gid)
        with Not_found ->
          let a = create_agent fdsock uid gid in
          Hashtbl.replace agents (uid,gid) a;
          a
      in
      Marshal.to_channel reply (agent cmd) [];
      flush reply
    done with End_of_file -> exit 0
  end;
  exit 0

let request request reply fdsock = {
  interp = (fun (type a) (type b) ~uid ~gid
    (cmd : < result : a; fd : b > cmd) ->
      cmd_send_fd fdsock cmd;
      Marshal.to_channel request { uid; gid; cmd } [];
      flush request;
      let result : b result = Marshal.from_channel reply in
      let result = result_recv_fd fdsock cmd result in
      result
  )
}

let create () = make (fork_proc listen request)
