open Lwt
open Ctypes
open Unsigned
open Profuse

module type IO_LWT = Fuse.IO with type 'a t = 'a Lwt.t

module type FS_IO_LWT = Fuse.FS_IO with type 'a IO.t = 'a Lwt.t

module type FS_LWT = sig
  include Fuse.STATE

  module Calls :
    functor(IO : IO_LWT) ->
      FS_IO_LWT with type 'a IO.t = 'a IO.t and type t = t
end

type socket = {
  id    : int;
  read  : uint8 Ctypes.ptr -> int -> int Lwt.t;
  write : uint8 Ctypes.ptr -> int -> int Lwt.t;
}

let null_socket = {
  id = -1;
  read  = (fun _ _ -> Lwt.return 0);
  write = (fun _ _ -> Lwt.return 0);
}

let socket_table = ref (Array.make 0 null_socket)

(* TODO: release socket *)
let new_socket ~read ~write =
  let table = !socket_table in
  let next_id = Array.length table in
  let table = Array.init (next_id + 1) (fun i ->
    if i <> next_id
    then table.(i)
    else { id = next_id; read; write }
  )  in
  socket_table := table;
  table.(next_id)

let socket_id { id } = id

let get_socket k =
  (!socket_table).(k)

let write_reply_raw req sz ptr =
  let socket = get_socket req.chan.id in
  socket.write (coerce (Ctypes.ptr char) (Ctypes.ptr uint8_t) ptr) sz
  >>= fun len ->
  if sz <> len
  then
    let msg =
      Printf.sprintf "Tried to write %d but only wrote %d" sz len
    in
    fail (Profuse.ProtocolError (req.chan,msg))
  else return_unit

module IO : IO_LWT = struct
  type 'a t = 'a Lwt.t

  let (>>=) = Lwt.(>>=)

  let return = Lwt.return

  let fail = Lwt.fail

  module In = struct
    include In

    (* TODO: ugggh this shouldn't be needed/used! REMOVE *)
    let memcpy_b2p ~dest ~src n =
      for i = 0 to n - 1 do
        (dest +@ i) <-@ Bytes.get src i
      done

    let remaining = ref None
    let parse chan n mem =
      let hdr_ptr = coerce (ptr uint8_t) (ptr Hdr.T.t) mem in
      let hdr = !@ hdr_ptr in
      chan.unique <- getf hdr Hdr.T.unique;
      let len = UInt32.to_int (getf hdr Hdr.T.len) in
      (if n < len
       then (* TODO: accumulate? *)
         let msg =
           Printf.sprintf "Packet has %d bytes but only read %d" len n
         in
         fail (ProtocolError (chan, msg))
       else if n > len
       then (remaining := Some (n - len, mem +@ len); return_unit)
       else return_unit
      ) >>= fun () ->
      let len = len - Hdr.sz in
      let ptr = to_voidp (mem +@ Hdr.sz) in
      let message = Message.parse chan hdr len ptr in
      return message

    let read chan =
      let approx_page_size = 4096 in
      let count = chan.max_write + approx_page_size in
      let buf = allocate_n uint8_t ~count in (* TODO: pool? *)
      fun () ->
        (* TODO: stop copying! *)
        catch (fun () ->
          match !remaining with
          | None ->
            let socket = get_socket chan.Profuse.id in
            socket.read buf count
            >>= fun n ->
            (*let fd = Unix.(
              openfile ("read_packet_"^(string_of_int !pnum))
              [O_WRONLY;O_CREAT] 0o600
              ) in
              let logn = Unix.write fd buf 0 n in
              assert (logn = n);
              let () = Unix.close fd in
              incr pnum;
            *)
            parse chan n buf
          | Some (n, mem) ->
            remaining := None;
            parse chan n mem
        ) Unix.(function
          | Unix_error ((
            EINTR  (* SIGINT *)
          | ENODEV (* umount *)
          | EBADF  (* internal unmount *)
          ), "read", _) ->
            let nodeid = UInt64.zero in
            let uid = UInt32.zero in
            let gid = UInt32.zero in
            let pid = UInt32.zero in
            (* assumes sequentially increasing packet ids *)
            let unique = UInt64.succ chan.Profuse.unique in
            chan.Profuse.unique <- unique;
            let pkt = Hdr.packet ~opcode:Opcode.FUSE_DESTROY ~unique
                ~nodeid ~uid ~gid ~pid ~count:0
            in
            let hdr = !@ (coerce (ptr char) (ptr Hdr.T.t)
                            ((CArray.start pkt) -@ Hdr.sz)) in
            Lwt.return Profuse.({ chan; hdr; pkt=Message.Destroy })
          | Unix_error (err, call, s) ->
            let msg =
              Printf.sprintf "%s(%s) error: %s" call s (error_message err)
            in
            fail (ProtocolError (chan, msg))
          | exn -> fail exn
        )

  end

  module Out = struct

    let write_reply req arrfn =
      let arr = arrfn req in
      let sz  = CArray.length arr + Out.Hdr.sz in
      let ptr = CArray.start arr -@ Out.Hdr.sz in
      write_reply_raw req sz ptr

    let write_ack req = write_reply req (Out.Hdr.packet ~count:0)

    let write_error req err =
      let host = req.chan.host.Host.errno in
      let nerrno = match Errno.to_code ~host err with
        | Some errno -> Int32.of_int (-errno)
        | None -> match Errno.to_code ~host Errno.EIO with
          | Some errno -> Int32.of_int (-errno)
          | None ->
            let errstr = Errno.to_string err in
            failwith (Printf.sprintf "errno for %s and EIO unknown" errstr)
      in
      write_reply req (Out.Hdr.packet ~nerrno ~count:0)
  end
end

module Trace(F : FS_LWT) : FS_LWT with type t = F.t = struct
  type t = F.t

  let string_of_state = F.string_of_state

  let string_of_nodeid = F.string_of_nodeid

  let string_of_mode req mode =
    let sys_stat = Fuse.(req.chan.host).Profuse.Host.sys_stat in
    let host = sys_stat.Sys_stat.Host.mode in
    let open Sys_stat.Mode in
    Printf.sprintf "%s (%x)"
      (to_string ~host (of_code_exn ~host mode))
      mode

  let string_of_perms req perms =
    let sys_stat = Fuse.(req.chan.host).Profuse.Host.sys_stat in
    let host = sys_stat.Sys_stat.Host.file_perm in
    let open Sys_stat.File_perm in
    to_string ~host (full_of_code ~host perms)

  let string_of_request string_of_nodeid req t =
    let module Hdr = Profuse.In.Hdr.T in
    Printf.sprintf "%s.p%ld.u%ld.g%ld.%Ld (%s) %s"
      (Profuse.In.Opcode.to_string (getf req.hdr Hdr.opcode))
      (UInt32.to_int32 (getf req.hdr Hdr.pid))
      (UInt32.to_int32 (getf req.hdr Hdr.uid))
      (UInt32.to_int32 (getf req.hdr Hdr.gid))
      (UInt64.to_int64 (getf req.hdr Hdr.unique))
      (string_of_nodeid (getf req.hdr Hdr.nodeid) t)
      Profuse.In.(Message.(match req.pkt with
        | Init i ->
          Printf.sprintf "version=%d.%d max_readahead=%d flags=0x%lX"
            (UInt32.to_int (getf i Init.T.major))
            (UInt32.to_int (getf i Init.T.minor))
            (UInt32.to_int (getf i Init.T.max_readahead))
            (Unsigned.UInt32.to_int32 (getf i Init.T.flags))
        | Getattr | Readlink | Destroy -> ""
        | Symlink (name,target) -> name ^ " -> " ^ target
        | Forget f ->
          Int64.to_string (UInt64.to_int64 (getf f Forget.T.nlookup))
        | Lookup name -> name
        | Mkdir (m,name) ->
          Printf.sprintf "mode=%s %s"
            (string_of_perms req (UInt32.to_int (getf m Mkdir.T.mode))) name
        | Mknod (m,name) ->
          Printf.sprintf "mode=%s rdev=%ld %s"
            (string_of_mode req (UInt32.to_int (getf m Mknod.T.mode)))
            (Unsigned.UInt32.to_int32 (getf m Mknod.T.rdev))
            name
        | Create (c,name) ->
          let host = req.chan.host.Host.fcntl.Fcntl.Host.oflags in
          let flags_code = UInt32.to_int (getf c Create.T.flags) in
          let flags = Fcntl.Oflags.of_code ~host flags_code in
          let flags_s =
            String.concat " " (List.map Fcntl.Oflags.to_string flags)
          in
          Printf.sprintf "flags=[%s] mode=%s %s"
            flags_s
            (string_of_mode req (UInt32.to_int (getf c Create.T.mode))) name
        | Open o ->
          let host = req.chan.host.Host.fcntl.Fcntl.Host.oflags in
          let flags_code = UInt32.to_int (getf o Open.T.flags) in
          let flags = Fcntl.Oflags.of_code ~host flags_code in
          let flags_s =
            String.concat " " (List.map Fcntl.Oflags.to_string flags)
          in
          Printf.sprintf "flags=[%s]" flags_s
        | Setattr s ->
          let attrs = Setattr.Valid.of_uint32 (getf s Setattr.T.valid) in
          Printf.sprintf "0x%lX[%s]"
            (UInt32.to_int32 (getf s Setattr.T.valid))
            (String.concat " " (Setattr.Valid.to_string_list attrs))
        | Access a ->
          let code = getf a Access.T.mask in
          (*let phost = Fuse.(req.chan.host.unistd.Unix_unistd.access) in
            let perms = Unix_unistd.Access.(of_code ~host:phost code) in
            (List.fold_left Unix.(fun s -> function
               | R_OK -> s^"R" | W_OK -> s^"W" | X_OK -> s^"X" | F_OK -> s^"F"
             ) "" perms)
          *)
          (* TODO: fix symbolic host map *)
          let perms = string_of_int (UInt32.to_int code) in
          let uid = getf req.hdr Hdr.T.uid in
          let gid = getf req.hdr Hdr.T.gid in
          Printf.sprintf "uid:%ld gid:%ld (%s)"
            (UInt32.to_int32 uid)
            (UInt32.to_int32 gid)
            perms
        | Unlink name | Rmdir name -> name
        | Rename (_r,src,dest) -> src ^ " -> " ^ dest
        | Read r ->
          let fh = getf r Read.T.fh in
          let offset = getf r Read.T.offset in
          let size = getf r Read.T.size in
          Printf.sprintf "fh=%Ld offset=%Ld size=%ld"
            (UInt64.to_int64 fh)
            (UInt64.to_int64 offset)
            (UInt32.to_int32 size)
        | Interrupt _
        | Getxattr _
        | Setxattr _
        | Listxattr _
        | Removexattr _
        | Getlk _
        | Setlk _
        | Setlkw _
        | Link (_,_)
        | Write (_,_)
        | Flush _
        | Release _
        | Opendir _
        | Readdir _
        | Releasedir _
        | Fsyncdir _
        | Fsync _
        | Statfs
        | Bmap _ -> "FIX ME"
        | Other opcode -> "OTHER "^(Opcode.to_string opcode)
        | Unknown i -> "UNKNOWN "^(Int32.to_string i)
      ))

  module Calls(IO : IO_LWT) : FS_IO_LWT with type t = t = struct
    module Trace_IO = struct
      type 'a t = 'a Lwt.t

      let (>>=) = Lwt.(>>=)

      let return = Lwt.return

      let fail = Lwt.fail

      module Out = struct
        let write_reply req arrfn =
          let arr = arrfn req in
          let sz  = CArray.length arr + Profuse.Out.Hdr.sz in
          let ptr = CArray.start arr -@ Profuse.Out.Hdr.sz in
          Printf.eprintf "    returning %s from %Ld\n%!"
            Out.Message.(describe_reply (deserialize req sz ptr))
            (UInt64.to_int64 (getf req.hdr Profuse.In.Hdr.T.unique));
          write_reply_raw req sz ptr

        let write_ack req =
          Printf.eprintf "    returning ack from %Ld\n%!"
            (Unsigned.UInt64.to_int64 (getf req.hdr Profuse.In.Hdr.T.unique));
          IO.Out.write_ack req

        let write_error req err =
          Printf.eprintf "    returning err %s from %Ld\n%!"
            (Errno.to_string err)
            (UInt64.to_int64 (getf req.hdr In.Hdr.T.unique));
          IO.Out.write_error req err
      end

      module In = IO.In
    end

    module Fs = F.Calls(Trace_IO)
    module Rw_full :
      Fuse.RW_FULL with type 'a IO.t = 'a Lwt.t and type t = F.t = Fs
    include Rw_full

    let negotiate_mount = Fs.negotiate_mount

    let dispatch req t =
      Printf.eprintf "    %s\n%!"
        (string_of_request string_of_nodeid req t);
      Fs.dispatch req t
      >>= fun t ->
      Printf.eprintf "    %s\n%!" (F.string_of_state req t);
      return t
  end
end

(* TODO: Only depends on UNIX not LWT; we're conflating those deps for now *)
module Dispatch(F : FS_LWT) : FS_LWT with type t = F.t = struct
  type t = F.t

  let string_of_state = F.string_of_state
  let string_of_nodeid = F.string_of_nodeid

  module Calls(IO : IO_LWT) : FS_IO_LWT with type t = t = struct
    module Calls = F.Calls(IO)
    include Calls

    let dispatch req t =
      catch In.Message.(fun () -> match req.pkt with
        | Init _ -> fail (ProtocolError (req.chan, "INIT after mount"))
        | Getattr -> getattr req t
        | Opendir op -> opendir op req t
        | Forget f -> forget (Ctypes.getf f In.Forget.T.nlookup) req t
        | Lookup name -> lookup name req t
        | Readdir r -> readdir r req t
        | Readlink -> readlink req t
        | Releasedir r -> releasedir r req t
        | Open op -> open_ op req t
        | Read r -> read r req t
        | Flush f -> flush f req t
        | Release r -> release r req t
        | Symlink (name,target) -> symlink name target req t
        | Rename (r,src,dest) -> rename r src dest req t
        | Unlink name -> unlink name req t
        | Rmdir name -> rmdir name req t
        | Statfs -> statfs req t
        | Fsync f -> fsync f req t
        | Write (w, data) -> write w data req t
        | Link (l,name) -> link l name req t
        | Getxattr g -> getxattr g req t
        | Setxattr s -> setxattr s req t
        | Listxattr g -> listxattr g req t
        | Removexattr name -> removexattr name req t
        | Access a -> access a req t
        | Create (c,name) -> create c name req t
        | Mknod (m,name) -> mknod m name req t
        | Mkdir (m,name) -> mkdir m name req t
        | Fsyncdir f -> fsyncdir f req t
        | Getlk lk  -> getlk  lk req t
        | Setlk lk  -> setlk  lk req t
        | Setlkw lk -> setlkw lk req t
        | Interrupt i -> interrupt i req t
        | Bmap b -> bmap b req t
        | Destroy -> destroy req t
        | Setattr s -> setattr s req t
        | Other _ | Unknown _ ->
          IO.(Out.write_error req Errno.ENOSYS >>= fun () -> return t)
      ) (function
        | Unix.Unix_error(e, _, _) ->
          let host = req.chan.host.Host.errno in
          let errno = match Errno_unix.of_unix ~host e with
            | [] -> Errno.EIO
            | errno::_ -> errno
          in
          IO.(Out.write_error req errno
              >>= fun () ->
              return t
             )
        | (Destroy k) as exn -> IO.fail exn
        | exn -> IO.(Out.write_error req Errno.EIO >>= fun () -> fail exn)
      )
  end
end

module type SERVER_LWT = Fuse.SERVER with type 'a IO.t = 'a Lwt.t

module type MOUNT_LWT =
  functor(F : FS_LWT)(IO : IO_LWT) ->
    Fuse.MOUNT_IO with type t = F.t and type 'a IO.t = 'a IO.t

module Server(M : MOUNT_LWT)(F : FS_LWT)(IO : IO_LWT)
  : SERVER_LWT with module IO = IO and type t = F.t =
struct
  module IO = IO
  module M = M(F)(IO)
  type t = F.t
  module Calls = F.Calls(IO)

  let mount = M.mount Calls.negotiate_mount

  let rec serve_forever chan t = IO.(
    IO.In.read chan ()
    >>= fun p ->
    async (fun () -> Calls.dispatch p t >>= fun _ -> return_unit);
    serve_forever chan t
  )
end
