let mnt =
  if Array.length Sys.argv > 1
  then Sys.argv.(Array.length Sys.argv - 1)
  else (Printf.eprintf "%s: missing mountpoint argument\n%!" Sys.argv.(0);
        exit 1)

module Server = Profuse.Server(Lofs.Linux_7_8)

;;
try
  let state = Lofs.({ root = Unix.getcwd () }) in
  let {Fuse.chan}, state = Profuse.mount
    (Array.sub Sys.argv 0 (Array.length Sys.argv - 1)) mnt state in
  let rec forever state = forever (Server.trace "" chan state) in forever state
with Fuse.ExecError (exec, cause) ->
  Printf.eprintf "Couldn't exec '%s': %s\n%!" exec cause;
  Profuse.unmount_path mnt;
  exit 1
| Fuse.ProtocolError (fs, message) ->
  Printf.eprintf "%s\n%!" message;
  Profuse.unmount fs;
  exit 1
