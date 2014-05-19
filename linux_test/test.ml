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

(* FUSE client-side integration tests with a cooperative server *)

open OUnit
open Lwt

module Lwt_th = Lwt_preemptive
module Server = Profuse.Server(Lofs)
module Linux_fs = Lofs.Linux_7_8(Server.Trace_out)

type fs = {
  chan  : Fuse.chan;
  state : Lofs.t;
}

let mntdir = "mnt"
let mntpath = Filename.concat mntdir
let multidir = "multi"
let multipath = Filename.concat multidir
let srcdir = "lofs"
let srcpath = Filename.concat srcdir

let max_wait_s = 0.01

let limit k msg lwt =
  let n = k *. max_wait_s in
  catch (fun () -> lwt <?> (Lwt_unix.timeout n)) (function
  | Lwt_unix.Timeout ->
    fail (Failure (Printf.sprintf "%s did not complete in %fs" msg n))
  | exn -> fail exn
  )

let lower_priv =
  let uid = Int32.of_int (int_of_string (Unix.readlink "run_as")) in
  Printf.eprintf "using: uid=%d euid=%d duid=%ld gid=%d egid=%d dgid=%ld\n%!"
    (Unix.getuid ()) (Unix.geteuid ()) uid
    (Unix.getgid ()) (Unix.getegid ()) uid;
  fun () ->
    Printf.eprintf "now running as euid %ld egid %ld\n%!" uid uid;
    Unix_unistd.setegid uid;
    Unix_unistd.seteuid uid

let as_root fn k =
  Unix_unistd.setegid 0l;
  Unix_unistd.seteuid 0l;
  Printf.eprintf "now running as euid %d egid %d\n%!"
    (Unix.geteuid ()) (Unix.getegid ());
  let v = fn k in
  lower_priv ();
  v

let agents = Agent_handler.create ()
let mounts = Hashtbl.create 2

let serve_one tag fs =
  Lwt_th.detach (fun () ->
    try { fs with state = Server.trace fs.chan tag fs.state }
    with
    | (Fuse.Destroy 0) as exn -> raise exn
    | exn ->
      Printf.eprintf "Fatal error: exception %s\n%!" (Printexc.to_string exn);
      Printexc.print_backtrace stderr;
      raise exn
  ) ()

let rec serve_until_block tag th =
  catch (fun () ->
    (th >>= fun fs ->
     Hashtbl.replace mounts fs.chan.Fuse.mnt fs;
     return (Some fs)
    ) <?> (Lwt_unix.timeout max_wait_s)
  ) (function
  | Lwt_unix.Timeout -> return None
  | exn -> fail exn
  ) >>= function
  | Some fs -> serve_until_block tag (serve_one tag fs)
  | None ->
    Printf.eprintf "stopping serve_until_block(%s)\n%!" tag;
    return ()

let run_fuse mnts tag fn = Lwt_main.run begin
  List.fold_left (fun th mnt ->
    (serve_until_block (mnt^"."^tag) (return (Hashtbl.find mounts mnt))) <&> th
  ) (Lwt_th.detach fn ()) mnts
end

let mount mnt () =
  lower_priv ();
  Unix.(try access srcdir [F_OK] with Unix_error _ -> mkdir srcdir 0o700);
  Unix.(try access mnt    [F_OK] with Unix_error _ -> mkdir mnt    0o700);
  let state = as_root Lofs.make srcdir in
  let req, state = as_root
    (Linux_fs.mount ~argv:[|"test";"-o";"allow_other"|] ~mnt) state in
  Hashtbl.replace mounts mnt { chan=req.Fuse.chan; state };
  Printf.eprintf "%s\n%!" (Server.string_of_request req state)

let test_mount =
  "mount", [
    "mount",`Quick,mount mntdir;
  ]

let test_ops =
  let read () =
    let string = "a short read test\n" in
    let file = "read" in
    let path = srcpath file in
    let len = String.length string in
    let w = Unix.(openfile path [O_CREAT; O_WRONLY; O_DSYNC] 0o600) in
    Printf.eprintf "before lofs write\n%!";
    let bytes_written = Unix.write w string 0 len in
    assert_equal ~msg:"wrote expected number of bytes" bytes_written len;
    let () = Unix.close w in
    Printf.eprintf "after lofs write\n%!";
    run_fuse [mntdir] "read" (fun () ->
      let fd = Unix.(
        openfile (mntpath file) [] 0o000
      ) in
      let s = String.create (2*len) in
      Printf.eprintf "before fuse read\n%!";
      let bytes_read = Unix.read fd s 0 (2*len) in
      Printf.eprintf "after fuse read\n%!";
      assert_equal ~msg:"read same number bytes written" bytes_read len;
      assert_equal ~msg:"read back same bytes written"
        string (String.sub s 0 len);
      Unix_unistd.close fd
    );
    Unix.unlink path
  in

  let readlink () =
    let file = "symlink" in
    let path = srcpath file in
    let lnk = "unicorn" in
    Unix.symlink "unicorn" path;
    run_fuse [mntdir] "readlink" (fun () ->
      let lnk_v = Unix_unistd.readlink (mntpath file) in
      assert_equal ~msg:(lnk_v ^ " <> " ^ lnk) lnk_v lnk
    );
    Unix.unlink path
  in

  let to_mode_t i = PosixTypes.(Ctypes.(Unsigned.(
    coerce uint32_t mode_t (UInt32.of_int32 i)
  ))) in
  let to_dev_t i = PosixTypes.(Ctypes.(Unsigned.(
    coerce uint64_t dev_t (UInt64.of_int i)
  ))) in

  let dir_file = "dir" in
  let dir_perms = 0o700l in
  let subdir_file = Filename.concat dir_file "subdir" in
  let subdir_perms = 0o700l in
  let mkdir () =
    let file = dir_file in
    let path = srcpath file in
    Unix.(assert_raises (Unix_error (ENOENT, "access", path))
            (fun () -> access path [F_OK]));
    run_fuse [mntdir] "mkdir" (fun () ->
      let path = mntpath file in
      Unix_sys_stat.mkdir path (to_mode_t dir_perms);
      let path = mntpath subdir_file in
      let uid = 999l and gid = 999l in
      Unix.(assert_raises ~msg:"uid 999 shouldn't be able to mkdir"
              (Unix_error (EACCES, "mkdir", path))
              (fun () ->
                agents.Agent_handler.mkdir ~uid ~gid path subdir_perms));
      Unix_sys_stat.mkdir path (to_mode_t subdir_perms)
    );
    Unix.(access path [F_OK])
  in

  let nod_file  = Filename.concat dir_file "nod" in
  let nod_perms = 0o777l in
  let mknod () =
    let file = nod_file in
    let path = srcpath file in
    Unix.(assert_raises (Unix_error (ENOENT, "access", path))
            (fun () -> access path [F_OK]));
    run_fuse [mntdir] "mknod" (fun () ->
      let path = mntpath file in
      let uid = 999l and gid = 999l in
      Unix.(assert_raises ~msg:"uid 999 shouldn't be able to mknod"
              (Unix_error (EACCES, "mknod", path))
              (fun () ->
                agents.Agent_handler.mknod ~uid ~gid path nod_perms 0l));
      Unix_sys_stat.mknod path (to_mode_t nod_perms) (to_dev_t 0);
      Unix_unistd.(access path [Unix.F_OK])
    );
    Unix.(access path [F_OK])
  in

  let chmod () =
    let file = nod_file in
    let path = mntpath file in
    run_fuse [mntdir] "chmod" Unix.(LargeFile.(fun () ->
      let perms = Unix_sys_stat.(Stat.to_unix (stat path)).st_perm in
      let msg = Printf.sprintf "%s perms.0 0o%03o <> 0o%03lo"
        file perms nod_perms in
      assert_equal ~msg (Int32.to_int nod_perms) perms;
      let new_perms = 0o644l in
      Unix_sys_stat.chmod path (to_mode_t new_perms);
      let perms = Unix_sys_stat.(Stat.to_unix (stat path)).st_perm in
      let msg = Printf.sprintf "%s perms.1 0o%03o <> 0o%03lo"
        file perms new_perms in
      assert_equal ~msg (Int32.to_int new_perms) perms;
      let new_perms = 0o640l in
      let fd = openfile path [] 0o000 in
      (try
         Unix_sys_stat.fchmod fd (to_mode_t new_perms);
         let perms = Unix_sys_stat.(Stat.to_unix (stat path)).st_perm in
         let msg = Printf.sprintf "%s perms.2 0o%03o <> 0o%03lo"
           file perms new_perms in
         assert_equal ~msg (Int32.to_int new_perms) perms
       with exn -> Unix_unistd.close fd; raise exn);
      Unix_unistd.close fd
    ))
  in

  let access () =
    let file = nod_file in
    let path = mntpath file in
    run_fuse [mntdir] "access" (fun () ->
      let uid = 999l and gid = 999l in
      Unix.(assert_raises ~msg:"uid 999 shouldn't be able to write"
              (Unix_error (EACCES, "access", path))
              (fun () ->
                agents.Agent_handler.access ~uid ~gid path Unix.([W_OK])));
      let uid = Int32.of_int (Unix.geteuid ()) in
      let gid = Int32.of_int (Unix.getegid ()) in
      Unix.(assert_raises ~msg:"user shouldn't be able to exec"
              (Unix_error (EACCES, "access", path))
              (fun () ->
                agents.Agent_handler.access ~uid ~gid path Unix.([X_OK])));
      let uid = 0l and gid = 0l in
      (* root should be able to write *)
      agents.Agent_handler.access ~uid ~gid path Unix.([W_OK])
    )
  in

  let chown () =
    let file = nod_file in
    let path = mntpath file in
    let uid = Int32.of_int (Unix.geteuid ()) in
    let gid = Int32.of_int (Unix.getegid ()) in
    run_fuse [mntdir] "chown" Unix.(LargeFile.(fun () ->
      Unix_unistd.chown path (-1l) (-1l);
      let st = Unix_sys_stat.(Stat.to_unix (stat path)) in
      let msg = Printf.sprintf "chown noopuid (%ld <> %d)" uid st.st_uid in
      assert_equal ~msg uid (Int32.of_int st.st_uid);
      assert_equal ~msg:"chown noopgid" gid (Int32.of_int st.st_gid);
      as_root (Unix_unistd.chown path (1l)) (-1l);
      let st = Unix_sys_stat.(Stat.to_unix (stat path)) in
      assert_equal ~msg:("chown setuid (1 <> "^(string_of_int st.st_uid)^")")
        1 st.st_uid;
      assert_equal ~msg:"chown setuid but noopgid" gid (Int32.of_int st.st_gid);
      as_root (Unix_unistd.chown path (-1l)) (1l);
      let st = Unix_sys_stat.(Stat.to_unix (stat path)) in
      assert_equal ~msg:"chown setgid but noopuid" 1 st.st_uid;
      assert_equal ~msg:"chown setgid" 1 st.st_gid;
      let fd = as_root (Unix.openfile path []) 0o000 in
      (try
         as_root (Unix_unistd.fchown fd uid) gid;
         let st = Unix_sys_stat.(Stat.to_unix (fstat fd)) in
         assert_equal ~msg:"fchown setuid" uid (Int32.of_int st.st_uid);
         assert_equal ~msg:"fchown setgid" gid (Int32.of_int st.st_gid);
       with exn -> Unix_unistd.close fd; raise exn);
      Unix_unistd.close fd
    ))
  in

  let symlink () =
    let file = "symlink" in
    let lnk = "unicorn" in
    run_fuse [mntdir] "symlink" (fun () ->
      let path = mntpath file in
      Unix_unistd.symlink lnk path
    );
    let path = srcpath file in
    let lnk_v = Unix.readlink path in
    assert_equal ~msg:(lnk_v ^ " <> " ^ lnk) lnk_v lnk;
    Unix.unlink path
  in

  let write_string = "a short write test\n" in
  let write () =
    let len = String.length write_string in
    let file = nod_file in
    run_fuse [mntdir] "write" (fun () ->
      let fd = Unix.(
        openfile (mntpath file) [O_WRONLY] 0o600
      ) in
      Printf.eprintf "before fuse write\n%!";
      let bytes_written = Unix.write fd write_string 0 len in
      Printf.eprintf "after fuse write\n%!";
      assert_equal ~msg:"wrote expected number of bytes" bytes_written len;
      Unix_unistd.close fd
    );
    let path = srcpath file in
    let r = Unix.(openfile path [O_RDONLY] 0o000) in
    let s = String.create (2*len) in
    Printf.eprintf "before lofs read\n%!";
    let bytes_read = Unix.read r s 0 (2*len) in
    Printf.eprintf "after lofs read\n%!";
    assert_equal ~msg:"read back same number of bytes written" bytes_read len;
    assert_equal ~msg:"read back same bytes written"
      write_string (String.sub s 0 len);
    Unix.close r
  in

  let truncate () =
    let file = nod_file in
    run_fuse [mntdir] "truncate" (fun () ->
      let path = mntpath file in
      let wstr = String.copy write_string in
      let wlen = String.length wstr in
      let tlen = 17 in
      let tstr = String.sub write_string 0 tlen in
      let ftlen = 13 in
      let ftstr = String.sub write_string 0 ftlen in
      Unix_unistd.truncate path (Int64.of_int tlen);
      let s = String.create (2*tlen) in
      let fd = Unix.openfile path [] 0o000 in
      (try
         let bytes_read = Unix.read fd s 0 (2*tlen) in
         assert_equal ~msg:"read only non-truncated bytes" bytes_read tlen;
         assert_equal ~msg:"read truncated substring" tstr (String.sub s 0 tlen);
         String.iteri (fun i _ -> if i >= tlen then wstr.[i] <- '\000') wstr;
         Unix_unistd.truncate path (Int64.of_int wlen)
       with exn -> Unix_unistd.close fd; raise exn);
      Unix_unistd.close fd;
      let fd = Unix.(openfile path [O_RDWR] 0o000) in
      (try
         let bytes_read = Unix.read fd s 0 (2*tlen) in
         let msg = Printf.sprintf "truncate extends (%d = %d)" bytes_read wlen in
         assert_equal ~msg bytes_read wlen;
         assert_equal ~msg:"truncate extends with 0" wstr (String.sub s 0 wlen);
         Unix_unistd.ftruncate fd (Int64.of_int ftlen)
       with exn -> Unix_unistd.close fd; raise exn);
      Unix_unistd.close fd;
      let fd = Unix.(openfile path [O_RDWR] 0o000) in
      (try
         let bytes_read = Unix.read fd s 0 (2*ftlen) in
         assert_equal ~msg:"read only non-ftruncated bytes" bytes_read ftlen;
         assert_equal ~msg:"read ftruncated substring"
           ftstr (String.sub s 0 ftlen);
         String.iteri (fun i _ -> if i >= ftlen then wstr.[i] <- '\000') wstr;
         Unix_unistd.ftruncate fd (Int64.of_int wlen)
       with exn -> Unix_unistd.close fd; raise exn);
      Unix_unistd.close fd;
      let fd = Unix.(openfile path [O_RDWR] 0o000) in
      (try
         let bytes_read = Unix.read fd s 0 (2*ftlen) in
         assert_equal ~msg:"ftruncate extends" bytes_read wlen;
         assert_equal ~msg:"ftruncate extends with 0" wstr (String.sub s 0 wlen)
       with exn -> Unix_unistd.close fd; raise exn);
      Unix_unistd.close fd
    )
  in

  let unlink () =
    let file = nod_file in
    run_fuse [mntdir] "unlink" (fun () ->
      let path = mntpath file in
      Unix_unistd.unlink path
    )
  in

  let rmdir () =
    let file = dir_file in
    run_fuse [mntdir] "rmdir" (fun () ->
      let path = mntpath file in
      Unix.(assert_raises ~msg:"should throw ENOTEMPTY for rmdir"
              (Unix_error (ENOTEMPTY, "rmdir", path))
              (fun () -> Unix_unistd.rmdir path));
      let subdir_path = mntpath subdir_file in
      let uid = 999l and gid = 999l in
      Unix.(assert_raises ~msg:"uid 999 shouldn't be able to rmdir"
              (Unix_error (EACCES, "rmdir", subdir_path))
              (fun () -> agents.Agent_handler.rmdir ~uid ~gid subdir_path));
      Unix_unistd.rmdir subdir_path;
      Unix_unistd.rmdir path
    )
  in

  let create () =
    let string = "a short create test\n" in
    let len = String.length string in
    let file = "create" in
    run_fuse [mntdir] "create" (fun () ->
      let fd = Unix.(
        openfile (mntpath file) [O_CREAT; O_WRONLY; O_TRUNC] 0o600
      ) in
      let bytes_written = Unix.write fd string 0 len in
      assert_equal ~msg:"wrote expected number of bytes" bytes_written len;
      Unix_unistd.close fd
    );
    Unix.unlink (srcpath file)
  in

  "ops", [
    "read",     `Quick,read;
    "readlink", `Quick,readlink;
    "mkdir",    `Quick,mkdir;
    "mknod",    `Quick,mknod;
    "chmod",    `Quick,chmod;
    "access",   `Quick,access;
    "chown",    `Quick,chown;
    "symlink",  `Quick,symlink;
    "write",    `Quick,write;
    "truncate", `Quick,truncate;
    "unlink",   `Quick,unlink;
    "rmdir",    `Quick,rmdir;
    "create",   `Quick,create;
  ]

let test_errs =
  let enoent () =
    let file = "unicorn" in
    run_fuse [mntdir] "enoent" (fun () ->
      let path = mntpath file in
      Unix.(assert_raises (Unix_error (ENOENT, "open", path))
              (fun () -> openfile path [] 0o000))
    )
  in

  let symlink_eexist () =
    let file = "symlink" in
    let path = srcpath file in
    ignore (Unix.system ("touch "^path));
    run_fuse [mntdir] "symlink_eexist" (fun () ->
      let path = mntpath file in
      Unix.(assert_raises (Unix_error (EEXIST, "symlink", path))
              (fun () -> Unix_unistd.symlink "unicorn" path))
    );
    Unix.unlink path
  in

  let eloop () =
    let file = "symlink" in
    let path = srcpath file in
    Unix.symlink file path;
    run_fuse [mntdir] "eloop" (fun () ->
      let path = mntpath file in
      Unix.(assert_raises (Unix_error (ELOOP, "open", path))
              (fun () -> openfile path [] 0o000))
    );
    Unix.unlink path
  in

  let readlink_eacces () =
    let dir = "bad_dir" in
    let path = srcpath dir in
    Unix.mkdir path 0o000;
    run_fuse [mntdir] "readlink_eacces" (fun () ->
      let path = Filename.concat (mntpath dir) "symlink" in
      Unix.(assert_raises (Unix_error (EACCES, "readlink", path))
              (fun () -> Unix_unistd.readlink path));
      Unix.(assert_raises (Unix_error (EACCES, "access", path))
              (fun () -> Unix_unistd.access path [R_OK]))
    );
    Unix.rmdir path
  in

  let chown_eperm () =
    let file = "eperm" in
    let path = srcpath file in
    ignore (Unix.system ("touch "^path));
    run_fuse [mntdir] "chown_eperm" (fun () ->
      let path = mntpath file in
      Unix.(assert_raises (Unix_error (EPERM, "chown", path))
              (fun () -> Unix_unistd.chown path 1l (-1l)))
    );
    Unix.unlink path
  in

  let open_eacces () =
    let file = "private" in
    let path = srcpath file in
    ignore (Unix.system ("touch "^path));
    Unix.chmod path 0o640;
    run_fuse [mntdir] "open" (fun () ->
      let path = mntpath file in
      let uid = 999l and gid = 999l in
      Unix.(assert_raises ~msg:"uid 999 shouldn't be able to open"
              (Unix_error (EACCES, "open", path))
              (fun () ->
                let fd = agents.Agent_handler.open_ ~uid ~gid
                  path [O_RDONLY] 0o600_l
                in Unix_unistd.close fd))
    );
    Unix.unlink path
  in

  "errs", [
    "enoent",         `Quick,enoent;
    "symlink_eexist", `Quick,symlink_eexist;
    "eloop",          `Quick,eloop;
    "readlink_eacces",`Quick,readlink_eacces;
    "chown_eperm",    `Quick,chown_eperm;
    "open_eacces",    `Quick,open_eacces;
  ]

let unmount mnt () =
  let fs = Hashtbl.find mounts mnt in
  Lwt_main.run (serve_until_block (mnt^".preunmount") (return fs));
  as_root Profuse.unmount fs.chan;
  assert_raises (Fuse.Destroy 0) (fun () -> Lwt_main.run (
    serve_until_block (mnt^".postunmount") (return (Hashtbl.find mounts mnt))
  ));
  Hashtbl.remove mounts mnt

let test_multi =
  let dircheck () =
    (*let file = "child" in*)
    let dir = "conflict" in
    let path = srcpath dir in
    Unix.mkdir path 0o700;
    (*ignore (Unix.system ("touch "^(Filename.concat path file)));*)
    run_fuse [mntdir] "dircheck" (fun () ->
      let mnt = mntpath dir in
      let mntdh = Unix_dirent.opendir mnt in
      Unix_dirent.closedir mntdh
    );
    Unix.rmdir path
  in

  "multi", [
    "mount",`Quick,mount multidir;
    "unmount",`Quick,unmount multidir;
    "dircheck",`Quick,dircheck;
  ]

let test_unmount =
  let unmount () =
      (*prerr_endline "before lsof";
        let lsof_pid = Unix.(
        create_process "lsof" [|mntdir|] stdin stdout stderr
        ) in
        ignore (Unix.waitpid [] lsof_pid);*)
    Hashtbl.iter (fun mnt _ -> unmount mnt ()) mounts
  in
  "unmount", [
    "unmount",`Quick,unmount;
  ]

;;
Printexc.record_backtrace true;
Alcotest.run "profuse_linux" [
  test_mount;
  test_ops;
  test_errs;
  test_multi;
  test_unmount;
]
