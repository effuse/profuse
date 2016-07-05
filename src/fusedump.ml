(*
 * Copyright (c) 2016 David Sheets <david.sheets@docker.com>
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

let version = "0.5.0"

type request = Profuse.In.Message.t Profuse.request
type reply = (Profuse.Out.Hdr.T.t, Profuse.Out.Message.t) Profuse.packet

type packet =
  | Query of request
  | Reply_to of (request * reply)
  | Reply_unlinked of (int * Profuse.Out.Hdr.T.t Profuse.Types.structure)

type time_label =
  | Mtime

let failf fmt = Printf.kprintf failwith fmt

let is_fuse_packet filename =
  try
    let fin = String.rindex filename '.' + 1 in
    let ext = String.sub filename fin (String.length filename - fin) in
    match ext with
    | "fuse" -> true
    | _ -> false
  with Not_found -> false

let int64_of_le_bytes off bytes =
  let open Int64 in
  let (++) = add in
  let int64_of_byte k = of_int (int_of_char (Bytes.get bytes k)) in
  (int64_of_byte off) ++
  (shift_left (int64_of_byte (off + 1))  8) ++
  (shift_left (int64_of_byte (off + 2)) 16) ++
  (shift_left (int64_of_byte (off + 3)) 24) ++
  (shift_left (int64_of_byte (off + 4)) 32) ++
  (shift_left (int64_of_byte (off + 5)) 40) ++
  (shift_left (int64_of_byte (off + 6)) 48) ++
  (shift_left (int64_of_byte (off + 7)) 56)

let int_of_4_le_bytes off bytes =
  let int_of_byte k = int_of_char (Bytes.get bytes k) in
  (int_of_byte off) +
  ((int_of_byte (off + 1)) lsl  8) +
  ((int_of_byte (off + 2)) lsl 16) +
  ((int_of_byte (off + 3)) lsl 24)

let parse_query host size query_table fd =
  let chan = Profuse.({
      id = 0;
      unique = Unsigned.UInt64.of_int 0;
      mnt = "";
      version = (7, 8);
      max_readahead = 0;
      max_write = 0;
      flags = Flags.empty;
      host;
    })
  in
  let mem = Ctypes.allocate_n Ctypes.uint8_t ~count:size in
  let size_read =
    try Unistd_unix.read fd (Ctypes.to_voidp mem) size
    with exn -> Unix.close fd; raise exn
  in
  (if size_read <> size then failwith "couldn't read packet");
  let hdr_ptr = Ctypes.(coerce (ptr uint8_t) (ptr Profuse.In.Hdr.T.t)) mem in
  let hdr = Ctypes.(!@ hdr_ptr) in
  let unique = Ctypes.getf hdr Profuse.In.Hdr.T.unique in
  let unique = Unsigned.UInt64.to_int64 unique in
  let len = Unsigned.UInt32.to_int (Ctypes.getf hdr Profuse.In.Hdr.T.len) in
  (if size_read < len
   then (* TODO: accumulate? *)
     failf "Packet has %d bytes but only read %d" len size_read
   else if size_read > len
   then failf "Packet has %d bytes but file contained %d" len size_read
  );
  let len = len - Profuse.In.Hdr.sz in
  let ptr = Ctypes.(to_voidp (mem +@ Profuse.In.Hdr.sz)) in
  let message = Profuse.In.Message.parse chan hdr len ptr in
  Hashtbl.replace query_table unique message;
  message

let parse_reply host size query_table fd =
  let mem = Ctypes.allocate_n Ctypes.uint8_t ~count:size in
  let size_read =
    try Unistd_unix.read fd (Ctypes.to_voidp mem) size
    with exn -> Unix.close fd; raise exn
  in
  (if size_read <> size then failwith "couldn't read packet");
  let hdr_ptr = Ctypes.(coerce (ptr uint8_t) (ptr Profuse.Out.Hdr.T.t)) mem in
  let hdr = Ctypes.(!@ hdr_ptr) in
  let unique = Ctypes.getf hdr Profuse.Out.Hdr.T.unique in
  let unique = Unsigned.UInt64.to_int64 unique in
  let len = Unsigned.UInt32.to_int (Ctypes.getf hdr Profuse.Out.Hdr.T.len) in
  (if size_read < len
   then (* TODO: accumulate? *)
       failf "Packet has %d bytes but only read %d" len size_read
   else if size_read > len
   then failf "Packet has %d bytes but file contained %d" len size_read);
  try
    let query = Hashtbl.find query_table unique in
    Hashtbl.remove query_table unique;
    let p = Ctypes.(coerce (ptr uint8_t) (ptr char) mem) in
    let pkt = Profuse.Out.Message.deserialize query len p in
    Reply_to (query, pkt)
  with Not_found -> Reply_unlinked (len, hdr)

let parse_packet host query_table fd =
  let typ = Bytes.create 1 in
  match try Unix.read fd typ 0 1 with exn -> Unix.close fd; raise exn with
  | 0 -> None
  | _ ->
    let buf = Bytes.create 12 in
    ignore (try Unix.read fd buf 0 12 with exn -> Unix.close fd; raise exn);
    let offset = Unix.(lseek fd ~-4 SEEK_CUR) in
    let now = int64_of_le_bytes 0 buf in
    let size = int_of_4_le_bytes 8 buf in
    let packet = match Bytes.to_string typ with
      | "Q" -> Query (parse_query host size query_table fd)
      | "R" -> parse_reply host size query_table fd
      | x ->
        failf "unknown packet type %S at byte 0x%x" x (offset - 9)
    in
    Some (now, packet)

let parse_session filename =
  let header_len = 4 + 1 + 1 + 2 in
  let header = Bytes.create header_len in
  let fd = Unix.(openfile filename [O_RDONLY] 0) in
  let host =
    begin try
        let header_read = Unix.read fd header 0 header_len in
        (if header_read <> header_len then failwith "couldn't read header");
        (if Bytes.sub header 0 5 <> Bytes.of_string "FUSES"
         then failwith "not a FUSE session");
        let major = int_of_char (Bytes.get header 6) in
        let minor = int_of_char (Bytes.get header 7) in
        (if major <> 7
         then failf "only FUSE major version 7 supported (not %d)" major
         else if minor <> 8
         then failf "only FUSE version 7.8 supported (not 7.%d)" minor
        );
        match Bytes.get header 5 with
        | 'L' -> Profuse.Host.linux_4_0_5
        | c -> failf "unknown host type '%c'" c
      with exn ->
        Unix.close fd;
        raise exn
    end
  in
  let query_table = Hashtbl.create 128 in
  let rec read_next session = match parse_packet host query_table fd with
    | None -> List.rev session
    | Some packet -> read_next (packet::session)
  in
  let session = read_next [] in
  Unix.close fd;
  session

let pretty_string_of_query q = Profuse.In.Message.describe q

let pretty_string_of_reply pkt =
  let id =
    Unsigned.UInt64.to_int64 Profuse.(Ctypes.getf pkt.hdr Out.Hdr.T.unique)
  in
  match Ctypes.getf pkt.Profuse.hdr Profuse.Out.Hdr.T.error with
  | 0_l ->
    Printf.sprintf "returning %s from %Ld"
      (Profuse.Out.Message.describe pkt) id
  | nerrno ->
    let host = Profuse.(pkt.chan.host.Host.errno) in
    let errnos = Errno.of_code ~host (- (Int32.to_int nerrno)) in
    Printf.sprintf "returning err [ %s ] from %Ld"
      (String.concat ", " (List.map Errno.to_string errnos)) id

let pretty_string_of_reply_unlinked len hdr =
  let id =
    Unsigned.UInt64.to_int64 Profuse.(Ctypes.getf hdr Out.Hdr.T.unique)
  in
  match Ctypes.getf hdr Profuse.Out.Hdr.T.error with
  | 0_l ->
    Printf.sprintf "returning UNKNOWN SUCCESS of length %d from %Ld" len id
  | nerrno ->
    (* TODO: this should look up the stream host *)
    let host = Profuse.Host.(linux_4_0_5.errno) in
    let errnos = Errno.of_code ~host (- (Int32.to_int nerrno)) in
    Printf.sprintf "returning err [ %s ] from %Ld"
      (String.concat ", " (List.map Errno.to_string errnos)) id

let time_label = function
  | None -> (fun _ -> "")
  | Some Mtime -> Printf.sprintf "%Ld: "

let pretty_print time =
  let time_label = time_label time in
  function
  | (t, Query q) ->
    Printf.printf "%s%s\n%!" (time_label t) (pretty_string_of_query q)
  | (t, Reply_to (_q, r)) ->
    Printf.printf "%s%s\n%!" (time_label t) (pretty_string_of_reply r)
  | (t, Reply_unlinked (len, hdr)) ->
    Printf.printf "%s%s\n%!"
      (time_label t) (pretty_string_of_reply_unlinked len hdr)

let show time session_file =
  try
    let session = parse_session session_file in
    List.iter (pretty_print time) session;
    `Ok ()
  with exn ->
    `Error (false,
            Printf.sprintf "Couldn't parse FUSE session %s: %s\n%!"
              session_file (Printexc.to_string exn))

open Cmdliner

let time_label = Arg.enum [
  "mtime", Some Mtime;
  "none", None;
]

let show_cmd =
  let doc = "pretty print the packets in a session" in
  let session = Arg.(
    value (pos 0 file (Sys.getcwd ()) (info ~docv:"SESSION" []))
  ) in
  let time = Arg.(
    value (opt time_label (Some Mtime)
             (info ~docv:"TIME_LABEL" ["time"]))
  ) in
  Term.(ret (pure show $ time $ session)),
  Term.info "show" ~doc

let cmds = [show_cmd]

let help_cmd =
  let show_usage =
    "  show [SESSION]   Read the packets in SESSION and print to stdout.\n"
  in
  let usage () = Printf.printf
      "fusedump %s\n\nSubcommands:\n%s\n" version show_usage
  in
  Term.(pure usage $ pure ()),
  Term.info "fusedump" ~version

;;
match Term.eval_choice help_cmd cmds with
| `Error _ -> exit 1
| _ -> exit 0
