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

(* TODO: ugggh this shouldn't be needed/used! REMOVE *)
let memcpy_p2b ~dest ~src n =
  for i = 0 to n - 1 do
    Bytes.set dest i (!@ (src +@ i))
  done

(* TODO: stop copying! *)
let write_reply_raw req sz ptr =
  let buf = Bytes.create sz in
  memcpy_p2b ~dest:buf ~src:ptr sz;
  Lwt_unix.(write (of_unix_file_descr req.chan.fd)) buf 0 sz
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
      let sp = ref dest in
      Bytes.iter (fun c -> !sp <-@ c; sp := !sp +@ 1) src

    let read chan =
      let approx_page_size = 4096 in
      let count = chan.max_write + approx_page_size in
      let buf = Bytes.create count in
      fun () ->
        (* TODO: stop copying! *)
        (*let buf = allocate_n uint8_t ~count in (* TODO: pool? *)*)
        catch (fun () ->
          Lwt_unix.(read (of_unix_file_descr chan.fd)) buf 0 count
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
          
          let mem = allocate_n uint8_t ~count:n in
          memcpy_b2p ~dest:(coerce (ptr uint8_t) (ptr char) mem) ~src:buf n;
          let hdr_ptr = coerce (ptr uint8_t) (ptr Hdr.T.t) mem in
          let hdr = !@ hdr_ptr in
          chan.unique <- getf hdr Hdr.T.unique;
          let len = UInt32.to_int (getf hdr Hdr.T.len) in
          (if n <> len
           then
             let msg =
               Printf.sprintf "Packet has %d bytes but only read %d" len n
             in
             fail (ProtocolError (chan, msg))
           else return_unit
          ) >>= fun () ->
          let len = len - Hdr.sz in
          let ptr = to_voidp (mem +@ Hdr.sz) in
          let message = Message.parse chan hdr len ptr (*mem*) in
          Gc.full_major ();
          return message
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
            (*let mem = coerce (ptr char) (ptr uint8_t) (CArray.start pkt) in*)
            let hdr = !@ (coerce (ptr char) (ptr Hdr.T.t)
                            ((CArray.start pkt) -@ Hdr.sz)) in
            Lwt.return Profuse.({ chan; hdr; (*mem;*) pkt=Message.Destroy })
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
      let host = Profuse.(req.chan.host.Host.errno) in
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
    (*let open Sys_stat.Mode in
      let host = Fuse.(req.chan.host.unix_sys_stat.Unix_sys_stat.mode) in
      to_string ~host (of_code_exn ~host mode)
    *)
    (* TODO: fix symbolic host map *)
    string_of_int mode

  let string_of_perms req perms =
    (*let open Sys_stat.File_perm in
      let host = Fuse.(req.chan.host.unix_sys_stat.Unix_sys_stat.file_perm) in
      to_string ~host (full_of_code ~host perms)
    *)
    (* TODO: fix symbolic host map *)
    string_of_int perms

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
          Printf.sprintf "flags=%ld mode=%s %s"
            (UInt32.to_int32 (getf c Create.T.flags))
            (string_of_mode req (UInt32.to_int (getf c Create.T.mode))) name
        | Setattr s ->
          Printf.sprintf "0x%lX[%s]"
            (UInt32.to_int32 (getf s Setattr.T.valid))
            (*
(String.concat " " (Setattr.Valid.T.attrs (getf s Setattr.T.valid)))
               *)
            (* TODO: fix symbolic host map *)
            (string_of_int (UInt32.to_int (getf s Setattr.T.valid)))
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
        | _ -> "FIX ME"
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
          (*let mem =
            Ctypes.(coerce (ptr char) (ptr uint8_t) (CArray.start arr))
            in*)
          Printf.eprintf "    returning %s from %Ld\n%!"
            Out.Message.(describe_reply (deserialize req sz ptr (*mem*)))
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
