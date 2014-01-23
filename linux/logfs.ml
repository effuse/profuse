module Stat = Unix_sys_stat.File_kind

let s_dir = match Stat.(to_code ~host Unix.S_DIR) with
  | Some x -> Int32.of_int x
  | None -> raise (Failure "This host doesn't know about sys/stat.h:S_IFDIR")
let s_reg = match Stat.(to_code ~host Unix.S_REG) with
  | Some x -> Int32.of_int x
  | None -> raise (Failure "This host doesn't know about sys/stat.h:S_IFREG")
let s_lnk = match Stat.(to_code ~host Unix.S_LNK) with
  | Some x -> Int32.of_int x
  | None -> raise (Failure "This host doesn't know about sys/stat.h:S_IFLNK")
let s_fifo = match Stat.(to_code ~host Unix.S_FIFO) with
  | Some x -> Int32.of_int x
  | None -> raise (Failure "This host doesn't know about sys/stat.h:S_IFIFO")
;;

let mnt =
  if Array.length Sys.argv > 1
  then Sys.argv.(Array.length Sys.argv - 1)
  else (Printf.eprintf "%s: missing mountpoint argument\n%!" Sys.argv.(0);
        exit 1)
in

let generation = ref 1L in

let root = Struct.Attr.create ~ino:1L ~size:0L ~blocks:0L
  ~atime:0L ~atimensec:0l
  ~mtime:0L ~mtimensec:0l
  ~ctime:0L ~ctimensec:0l
  ~mode:Int32.(logor s_dir 0o755l) ~nlink:1l
  ~uid:1l ~gid:1l ~rdev:0l
in

let foo = Struct.Attr.create ~ino:2L ~size:0L ~blocks:0L
  ~atime:0L ~atimensec:0l
  ~mtime:0L ~mtimensec:0l
  ~ctime:0L ~ctimensec:0l
  ~mode:Int32.(logor s_reg 0o644l) ~nlink:1l
  ~uid:1l ~gid:1l ~rdev:0l
in

let bar = Struct.Attr.create ~ino:3L ~size:0L ~blocks:0L
  ~atime:0L ~atimensec:0l
  ~mtime:0L ~mtimensec:0l
  ~ctime:0L ~ctimensec:0l
  ~mode:Int32.(logor s_lnk 0o644l) ~nlink:1l
  ~uid:1l ~gid:1l ~rdev:0l
in

let baz = Struct.Attr.create ~ino:4L ~size:0L ~blocks:0L
  ~atime:0L ~atimensec:0l
  ~mtime:0L ~mtimensec:0l
  ~ctime:0L ~ctimensec:0l
  ~mode:Int32.(logor s_fifo 0o644l) ~nlink:1l
  ~uid:1l ~gid:1l ~rdev:0l
in

let nodes = ["", root; "foo", foo; "bar", bar; "baz", baz] in

let node_table = Hashtbl.create 128 in
let () = List.iter (fun (_,attr) ->
  Hashtbl.replace node_table (Ctypes.getf attr Struct.Attr.ino) attr
) nodes in
let name_table = Hashtbl.create 128 in
let () = List.iter (fun (name,attr) ->
  Hashtbl.replace name_table name attr
) nodes in

let attr_of_req req = Hashtbl.find node_table
  (Unsigned.UInt64.to_int64 In.(Ctypes.getf req.hdr Hdr.nodeid))
in

let attr_of_name name = Hashtbl.find name_table name in

let readdir_count = ref 0 in

try
  let {In.fs} = Profuse.mount (Array.sub Sys.argv 0 (Array.length Sys.argv - 1)) mnt in
  let recv = In.read fs in
  while true do
    let req = recv () in
    Printf.eprintf "%s %Lx\n%!"
      (Opcode.to_string In.(Ctypes.getf req.hdr Hdr.opcode))
      (Unsigned.UInt64.to_int64 In.(Ctypes.getf req.hdr Hdr.nodeid));
    In.(match req.pkt with
    | Init _ -> raise (Failure "INIT after mount")
    | Getattr -> Out.(
      write_reply req
        (Attr.create ~attr_valid:0L ~attr_valid_nsec:0l
           ~attr:(attr_of_req req)))
    | Opendir op -> Out.(
      write_reply req (Open.create ~fh:0L ~open_flags:0l))
    | Forget f -> generation := Int64.add 1L !generation
    | Lookup "foo" -> Printf.eprintf "foo\n%!"; Out.(
      write_reply req
        (Entry.create ~nodeid:2L ~generation:!generation
           ~entry_valid:0L ~attr_valid:0L
           ~entry_valid_nsec:0l ~attr_valid_nsec:0l
           ~attr:(attr_of_name "foo")))
    | Lookup "bar" -> Printf.eprintf "bar\n%!"; Out.(
      write_reply req
        (Entry.create ~nodeid:3L ~generation:!generation
           ~entry_valid:0L ~attr_valid:0L
           ~entry_valid_nsec:0l ~attr_valid_nsec:0l
           ~attr:(attr_of_name "bar")))
    | Lookup s -> Printf.eprintf "$$$ %s\n%!" s; Out.write_error req Unix.ENOENT
    | Readdir r ->
      incr readdir_count;
      (*if !readdir_count > 20 then exit 1;*)
      Printf.eprintf "%d READDIR %Lx %Lx %ld\n%!"
        !readdir_count
        (Unsigned.UInt64.to_int64 (Ctypes.getf r In.Read.fh))
        (Unsigned.UInt64.to_int64 (Ctypes.getf r In.Read.offset))
        (Unsigned.UInt32.to_int32 (Ctypes.getf r In.Read.size));
      let make_dirent = Out.Dirent.create [
        1L,2L,"foo",Unix_dirent.File_kind.DT_REG;
        2L,3L,"bar",Unix_dirent.File_kind.DT_LNK;
        3L,4L,"baz",Unix_dirent.File_kind.DT_FIFO;
      ] in
      let pkt = make_dirent req in
      let len = Ctypes.Array.length pkt in
      begin
        try
          let fd = Unix.(openfile ("log/"^(string_of_int !readdir_count))
                           [O_CREAT; O_WRONLY; O_TRUNC] 0o640) in
          let wrote = Unix_unistd.write
            fd Ctypes.(to_voidp (Array.start pkt)) len in
          Unix.close fd;
          assert (wrote = len);
        with Unix.Unix_error(err, _, _) ->
          raise (Failure ("UNIX ERROR: "^(Unix.error_message err)))
      end;
      Out.write_reply req make_dirent
    | Readlink -> Out.(write_reply req (Readlink.create ~target:"../../"))
    | Releasedir r -> Out.(write_reply req (Hdr.packet ~count:0))
    | Open op -> Out.(write_reply req (Open.create ~fh:1L ~open_flags:0l))
    | Read r ->
      let data = Bigarray.(Array1.create char c_layout 0) in
      Out.(write_reply req (Read.create ~data))
    | Flush f -> Out.(write_reply req (Hdr.packet ~count:0))
    | Release r -> Out.(write_reply req (Hdr.packet ~count:0))
    | Getxattr _ | Access _ | Create _ | Mknod _ | Setattr _ ->
      Out.write_error req Unix.ENOSYS
    ) done
with Fs.ExecError (exec, cause) ->
  Printf.eprintf "Couldn't exec '%s': %s\n%!" exec cause;
  Profuse.unmount_path mnt;
  exit 1
| Fs.ProtocolError (fs, message) ->
  Printf.eprintf "%s\n%!" message;
  Profuse.unmount fs;
  exit 1
