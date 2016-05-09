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

module Profuse = Profuse_7_23

type packet =
  | Query of Profuse.In.Message.t Profuse.request
  | Reply of (int64 * Unsigned.UInt8.t Ctypes.ptr * int)

let is_fuse_packet filename =
  try
    let fin = String.rindex filename '.' + 1 in
    let ext = String.sub filename fin (String.length filename - fin) in
    match ext with
    | "fuse" -> true
    | _ -> false
  with Not_found -> false

let int64_of_le_bytes bytes =
  let open Int64 in
  let (+) = add in
  let int64_of_byte k = of_int (int_of_char (Bytes.get bytes k)) in
  (int64_of_byte 0) +
  (shift_left (int64_of_byte 1)  8) +
  (shift_left (int64_of_byte 2) 16) +
  (shift_left (int64_of_byte 3) 24) +
  (shift_left (int64_of_byte 4) 32) +
  (shift_left (int64_of_byte 5) 40) +
  (shift_left (int64_of_byte 6) 48) +
  (shift_left (int64_of_byte 7) 56)

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
  Unix.close fd;
  (if size_read <> size then failwith "couldn't read packet");
  let hdr_ptr = Ctypes.(coerce (ptr uint8_t) (ptr Profuse.In.Hdr.T.t)) mem in
  let hdr = Ctypes.(!@ hdr_ptr) in
  let unique = Ctypes.getf hdr Profuse.In.Hdr.T.unique in
  let unique = Unsigned.UInt64.to_int64 unique in
  let len = Unsigned.UInt32.to_int (Ctypes.getf hdr Profuse.In.Hdr.T.len) in
  (if size_read < len
   then (* TODO: accumulate? *)
     let msg =
       Printf.sprintf "Packet has %d bytes but only read %d" len size_read
     in
     failwith msg
   else if size_read > len
   then
     let msg =
       Printf.sprintf "Packet has %d bytes but file contained %d" len size_read
     in
     failwith msg
  );
  let len = len - Profuse.In.Hdr.sz in
  let ptr = Ctypes.(to_voidp (mem +@ Profuse.In.Hdr.sz)) in
  let message = Profuse.In.Message.parse chan hdr len ptr in
  Hashtbl.replace query_table unique message;
  message

let read_reply size fd =
  let mem = Ctypes.allocate_n Ctypes.uint8_t ~count:size in
  let size_read =
    try Unistd_unix.read fd (Ctypes.to_voidp mem) size
    with exn -> Unix.close fd; raise exn
  in
  Unix.close fd;
  (if size_read <> size then failwith "couldn't read packet");
  let hdr_ptr = Ctypes.(coerce (ptr uint8_t) (ptr Profuse.Out.Hdr.T.t)) mem in
  let hdr = Ctypes.(!@ hdr_ptr) in
  let unique = Ctypes.getf hdr Profuse.Out.Hdr.T.unique in
  let unique = Unsigned.UInt64.to_int64 unique in
  let len = Unsigned.UInt32.to_int (Ctypes.getf hdr Profuse.Out.Hdr.T.len) in
  (if size_read < len
   then (* TODO: accumulate? *)
     let msg =
       Printf.sprintf "Packet has %d bytes but only read %d" len size_read
     in
     failwith msg
   else if size_read > len
   then
     let msg =
       Printf.sprintf "Packet has %d bytes but file contained %d" len size_read
     in
     failwith msg
  );
  (unique, mem, size)

let parse_packet query_table filename =
  let header_len = 4 + 1 + 1 + 2 + 8 in
  let header = Bytes.create header_len in
  let fd = Unix.(openfile filename [O_RDONLY] 0) in
  let (host, time, size) =
    begin try
        let header_read = Unix.read fd header 0 header_len in
        (if header_read <> header_len then failwith "couldn't read header");
        (if Bytes.sub header 0 4 <> Bytes.of_string "FUSE"
         then failwith "not a FUSE packet");
        let major = int_of_char (Bytes.get header 6) in
        let minor = int_of_char (Bytes.get header 7) in
        (if major <> 7
         then failwith
             ("only FUSE major version 7 supported (not "^
              string_of_int major^")")
        );
        (if minor <> 23
         then failwith
             ("only FUSE version 7.23 supported (not 7."^
              string_of_int minor^")")
        );
        let host = match Bytes.get header 5 with
          | 'L' -> Profuse.Host.linux_4_0_5
          | c -> failwith ("unknown host type '"^(String.make 1 c)^"'")
        in
        let time = int64_of_le_bytes (Bytes.sub header 8 8) in
        let size = Unix.((fstat fd).st_size) - header_len in
        (host, time, size)
      with exn ->
        Unix.close fd;
        raise exn
    end
  in
  (time, match Bytes.get header 4 with
   | 'Q' -> Query (parse_query host size query_table fd)
   | 'R' -> Reply (read_reply size fd)
   | _ -> failwith "unknown FUSE packet type"
  )

let pretty_string_of_query q = Profuse.In.Message.describe q

let pretty_string_of_reply q p len =
  let p = Ctypes.(coerce (ptr uint8_t) (ptr char) p) in
  let pkt = Profuse.Out.Message.deserialize q len p in
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

let pretty_string_of_reply_without_query p len =
  let hdr = Ctypes.(!@ (coerce (ptr uint8_t) (ptr Profuse.Out.Hdr.T.t) p)) in
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

let pretty_print query_table = function
  | (t, Query q) ->
    Printf.printf "%Ld: %s\n%!" t (pretty_string_of_query q)
  | (t, Reply (unique, ptr, len)) ->
    let message = try
        let query = Hashtbl.find query_table unique in
        pretty_string_of_reply query ptr len
      with Not_found -> pretty_string_of_reply_without_query ptr len
    in
    Printf.printf "%Ld: %s\n%!" t message

let show dir =
  let files = Array.to_list (Sys.readdir dir) in
  let query_table = Hashtbl.create 128 in
  let packets = List.fold_left (fun list filename ->
      if is_fuse_packet filename
      then
        try parse_packet query_table filename :: list
        with exn ->
          Printf.eprintf "Couldn't parse FUSE packet %s: %s\n%!"
            filename (Printexc.to_string exn);
          list
      else list
    ) [] files
  in
  match packets with
  | [] -> `Error (false, "No fuse packets found.")
  | packets ->
    let compare_time (t, _) (t', _) = Int64.compare t t' in
    let timeline = List.sort compare_time packets in
    List.iter (pretty_print query_table) timeline;
    `Ok ()

open Cmdliner

let show_cmd =
  let doc = "pretty print the packets in directory (or pwd)" in
  let dir = Arg.(value (pos 0 dir (Sys.getcwd ()) (info ~docv:"DIR" []))) in
  Term.(ret (pure show $ dir)),
  Term.info "show" ~doc

let cmds = [show_cmd]

let help_cmd =
  let show_usage =
    "  show [DIR]   Read the packet files in a directory (or pwd if omitted)\n"
    ^"               and pretty print the session to stdout.\n"
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
