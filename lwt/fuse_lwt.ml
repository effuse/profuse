open Lwt
open Ctypes
open Unsigned
module Profuse = Profuse_7_23
open Profuse

module type IO_LWT = Fuse.IO with type 'a t = 'a Lwt.t

module type FS_IO_LWT = Fuse.FS_IO with type 'a IO.t = 'a Lwt.t

module type FS_LWT = sig
  include Fuse.STATE

  val log_error : string -> unit

  module Calls :
    functor(IO : IO_LWT) ->
      FS_IO_LWT with type 'a IO.t = 'a IO.t and type t = t
end

type socket = {
  id    : int;
  read  : int -> uint8 Ctypes.CArray.t Lwt.t;
  write : uint8 Ctypes.ptr -> int -> int Lwt.t;
}

let null_socket = {
  id = -1;
  read  = (fun _ -> Lwt.return (Ctypes.CArray.make uint8_t 0));
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

let read_socket { read } = read

let write_socket { write } = write

let set_socket k ?read ?write () =
  let table = !socket_table in
  if k >= Array.length table
  then raise (Invalid_argument "bad socket table index")
  else
    let socket = table.(k) in
    table.(k) <- {
      socket with
      read = (match read with
          | None -> socket.read
          | Some read -> read
        );
      write = (match write with
          | None -> socket.write
          | Some write -> write
        );
    }

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
      fun () ->
        catch (fun () ->
          match !remaining with
          | None ->
            let socket = get_socket chan.Profuse.id in
            socket.read count
            >>= fun carray ->
            let ptr = Ctypes.CArray.start carray in
            let len = Ctypes.CArray.length carray in
            parse chan len ptr
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
            let pkt = Hdr.packet ~opcode:`FUSE_DESTROY ~unique
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

    let write_error log_error req err =
      let host = req.chan.host.Host.errno in
      let nerrno = match Errno.to_code ~host err with
        | Some errno -> Int32.of_int (-errno)
        | None -> match Errno.to_code ~host Errno.EIO with
          | Some errno ->
            let errno_string = Errno.to_string err in
            log_error ("Couldn't find host error code for "^errno_string);
            Int32.of_int (-errno)
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

  let log_error = F.log_error

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
            Out.Message.(describe (deserialize req sz ptr))
            (UInt64.to_int64 (getf req.hdr Profuse.In.Hdr.T.unique));
          write_reply_raw req sz ptr

        let write_ack req =
          Printf.eprintf "    returning ack from %Ld\n%!"
            (Unsigned.UInt64.to_int64 (getf req.hdr Profuse.In.Hdr.T.unique));
          IO.Out.write_ack req

        let write_error log_error req err =
          Printf.eprintf "    returning err %s from %Ld\n%!"
            (Errno.to_string err)
            (UInt64.to_int64 (getf req.hdr In.Hdr.T.unique));
          IO.Out.write_error log_error req err
      end

      module In = IO.In
    end

    module Fs = F.Calls(Trace_IO)
    module Rw_full :
      Fuse.RW_FULL with type 'a IO.t = 'a Lwt.t and type t = F.t = Fs
    include Rw_full

    let negotiate_mount = Fs.negotiate_mount

    let dispatch req t =
      Printf.eprintf "    %s\n%!" (Profuse.In.Message.describe req);
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

  let log_error = F.log_error

  module Calls(IO : IO_LWT) : FS_IO_LWT with type t = t = struct
    module Calls = F.Calls(IO)
    include Calls

    let dispatch req t =
      catch In.Message.(fun () -> match req.pkt with
        | Init _ -> fail (ProtocolError (req.chan, "INIT after mount"))
        | Getattr -> getattr req t
        | Opendir op -> opendir op req t
        | Forget f -> forget (Ctypes.getf f In.Forget.T.nlookup) req t
        | Batch_forget b -> batch_forget b req t
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
          IO.(Out.write_error log_error req Errno.ENOSYS
              >>= fun () -> return t)
      ) (function
        | Unix.Unix_error(e, _, _) as exn ->
          let host = req.chan.host.Host.errno in
          let errno = match Errno_unix.of_unix ~host e with
            | [] ->
              let error_string = Printexc.to_string exn in
              log_error ("Couldn't find host errno for "^error_string);
              Errno.EIO
            | errno::_ -> errno
          in
          IO.(Out.write_error log_error req errno
              >>= fun () ->
              return t
             )
        | Errno.Error { Errno.errno = errno :: _ } ->
          IO.(Out.write_error log_error req errno
              >>= fun () ->
              return t
             )
        | (Destroy k) as exn -> IO.fail exn
        | exn ->
          log_error ("Unknown exception caught: "^(Printexc.to_string exn));
          IO.(Out.write_error log_error req Errno.EIO
              >>= fun () -> fail exn)
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
