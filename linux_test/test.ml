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

open OUnit
open Lwt

module Lwt_th = Lwt_preemptive
module Server = Profuse.Server(Lofs.Linux_7_8)

let mntdir = "mnt"
let srcdir = "lofs"
let max_wait_s = 0.01

let limit k msg lwt =
  let n = k *. max_wait_s in
  catch (fun () -> lwt <?> (Lwt_unix.timeout n)) (function
  | Lwt_unix.Timeout ->
    fail (Failure (Printf.sprintf "%s did not complete in %fs" msg n))
  | exn -> fail exn
  )

let state = Lofs.({ root = srcdir })
let fuse_conn = ref None
let fs_state = ref state
let serve_one tag state = match !fuse_conn with
  | None -> return state
  | Some chan -> Lwt_th.detach (Server.trace tag chan) state

let rec serve_until_block tag th =
  catch (fun () ->
    (th >>= fun st ->
     fs_state := st; return (Some st)
    ) <?> (Lwt_unix.timeout max_wait_s)
  ) (function
  | Lwt_unix.Timeout -> return None
  | exn -> fail exn
  ) >>= function
  | Some st -> serve_until_block tag (serve_one tag st)
  | None ->
    Printf.eprintf "stopping serve_until_block(%s)\n%!" tag;
    return ()

let test_mount =
  let init_req = ref None in
  let mount () =
    Unix.(try access srcdir [F_OK] with Unix_error _ -> mkdir srcdir 0o700);
    Unix.(try access mntdir [F_OK] with Unix_error _ -> mkdir mntdir 0o700);
    let req, st = Profuse.mount [|"test"|] mntdir state in
    fuse_conn := Some req.Fuse.chan;
    fs_state := st;
    init_req  := Some req;
    Printf.eprintf "%s\n%!" (Server.string_of_request req st)
  in
  "mount", [
    "mount",`Quick,mount;
  ]

let test_ops =
  let read () =
    let string = "a short read test\n" in
    let file = "read" in
    let path = Filename.concat srcdir file in
    let len = String.length string in
    let w = Unix.(openfile path [O_CREAT; O_WRONLY; O_DSYNC] 0o600) in
    Printf.eprintf "before lofs write\n%!";
    let bytes_written = Unix.write w string 0 len in
    assert_equal ~msg:"wrote expected number of bytes" bytes_written len;
    let () = Unix.close w in
    Printf.eprintf "after lofs write\n%!";
    Lwt_main.run begin (serve_until_block "read" (return !fs_state)) <&>
        (Lwt_th.detach (fun () ->
          let fd = Unix.(
            openfile (Filename.concat mntdir file) [] 0o000
          ) in
          let s = String.create (2*len) in
          Printf.eprintf "before fuse read\n%!";
          let bytes_read = Unix.read fd s 0 (2*len) in
          Printf.eprintf "after fuse read\n%!";
          assert_equal ~msg:"read same number bytes written" bytes_read len;
          assert_equal ~msg:"read back same bytes written"
            string (String.sub s 0 len);
          Unix_unistd.close fd
         ) ())
    end;
    Unix.unlink path
  in

  let write () =
    let string = "a short write test\n" in
    let len = String.length string in
    let file = "write" in
    Lwt_main.run begin (serve_until_block "write" (return !fs_state)) <&>
        (Lwt_th.detach (fun () ->
          let fd = Unix.(
            openfile (Filename.concat mntdir file) [O_CREAT; O_WRONLY] 0o600
          ) in
          Printf.eprintf "before fuse write\n%!";
          let bytes_written = Unix.write fd string 0 len in
          Printf.eprintf "after fuse write\n%!";
          assert_equal ~msg:"wrote expected number of bytes" bytes_written len;
          Unix_unistd.close fd
         ) ())
    end;
    let path = Filename.concat srcdir file in
    let r = Unix.(openfile path [O_RDONLY] 0o000) in
    let s = String.create (2*len) in
    Printf.eprintf "before lofs read\n%!";
    let bytes_read = Unix.read r s 0 (2*len) in
    Printf.eprintf "after lofs read\n%!";
    assert_equal ~msg:"read back same number of bytes written" bytes_read len;
    assert_equal ~msg:"read back same bytes written"
      string (String.sub s 0 len);
    let () = Unix.close r in
    Unix.unlink path
  in

  "ops", [
    "read", `Quick,read;
    "write",`Quick,write;
  ]

let test_unmount =
  let unmount () =
    match !fuse_conn with
    | None -> ()
    | Some chan ->
      (*prerr_endline "before lsof";
      let lsof_pid = Unix.(
        create_process "lsof" [|mntdir|] stdin stdout stderr
      ) in
      ignore (Unix.waitpid [] lsof_pid);*)
      Lwt_main.run (serve_until_block "unmount" (return !fs_state));
      Profuse.unmount chan
  in
  "unmount", [
    "unmount",`Quick,unmount;
  ]

;;
Printexc.record_backtrace true;
Alcotest.run "profuse_linux" [
  test_mount;
  test_ops;
  test_unmount;
]
