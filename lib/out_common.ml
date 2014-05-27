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

open Ctypes
open Unsigned
open View

module Hdr = struct
  type t
  let t : t structure typ = structure "Out_Hdr"
  let ( -:* ) s x = field t s x
  let size   = "size"   -:* int_of_32
  let error  = "error"  -:* int32_t
  let unique = "unique" -:* uint64_t
  let () = seal t

  let hdrsz = sizeof t

  let packet ?(nerrno=0l) ~count req =
    let bodysz = count in
    let count = hdrsz + bodysz in
    let pkt = allocate_n char ~count in
    let hdr = !@ (coerce (ptr char) (ptr t) pkt) in
    setf hdr size   count;
    setf hdr error  nerrno;
    setf hdr unique In_common.(getf req.Fuse.hdr Hdr.unique);
    CArray.from_ptr (pkt +@ hdrsz) bodysz

  let make req st = 
    let count = sizeof st in
    let pkt = packet ~count req in
    !@ (coerce (ptr char) (ptr st) (CArray.start pkt))

  let set_size pkt sz =
    let pktsz = hdrsz + sz in
    let hdr = !@ (coerce (ptr char) (ptr t) ((CArray.start pkt) -@ hdrsz)) in
    setf hdr size pktsz;
    CArray.from_ptr (CArray.start pkt) sz
end

type 'b reply = (Hdr.t,'b) Fuse.packet

(* TODO: t doesn't define a struct with a packet header but rather
   a list element *)
module Dirent = struct
  type t
  let t : t structure typ = structure "Dirent"
  let ( -:* ) s x = field t s x
  let ino     = "ino"     -:* int64_of_64
  let off     = "off"     -:* int_of_64 (* TODO: copacetic? *)
  let namelen = "namelen" -:* uint32_t
  let typ     = "type"    -:* uint32_t
  let name    = "name"    -:* array 0 char
  let () = seal t

  let ino_ = ino
  let off_ = off
  let namelen_ = namelen
  let typ_ = typ
  let name_ = name
  let hdrsz = sizeof t
  let entsz name = hdrsz + 8 * (((String.length name) + 7) / 8)

  (* TODO: respect size *)
  let of_list ~host listing offset req =
    let phost = host.Fuse.unix_dirent.Unix_dirent.file_kind in
    let emit = ref false in
    let count = ref 0 in
    let listing = List.fold_left (fun acc ((off,_,name,_) as ent) ->
      if offset <> 0 && not !emit then (* TODO: fixme this is gross *)
        if off = offset
        then (emit := true; acc)
        else acc
      else (count := !count + (entsz name); ent::acc)
    ) [] listing in
    let count = !count in
    let pkt = Hdr.packet ~count req in
    let buf = CArray.start pkt in
    let sz = List.fold_left (fun p (off,ino,name,typ) -> (* TODO: use sz? *)
      let sz = entsz name in
      let dirent = !@ (coerce (ptr char) (ptr t) p) in
      setf dirent ino_     ino;
      setf dirent off_     off;
      setf dirent namelen_ (UInt32.of_int (String.length name));
      setf dirent typ_
        (UInt32.of_int (int_of_char
                          (Unix_dirent.File_kind.(to_code ~host:phost typ))));
      let sp = ref (p +@ (offsetof name_)) in
      String.iter (fun c -> !sp <-@ c; sp := !sp +@ 1) name;
      (* Printf.eprintf "dirent serialized %s\n%!" name; *)
      p +@ sz (* TODO: zero-write padding? *)
    ) buf (List.rev listing) in
    pkt
end

module Readlink = struct
  type t
  let t : t structure typ = structure "Out_Readlink"
  let ( -:* ) s x = field t s x
  let target = "target" -:* array 0 char
  let () = seal t

  let target_ = target
  let create ~target req =
    let count = String.length target in
    let pkt = Hdr.packet ~count req in
    let sp = ref (CArray.start pkt) in
    String.iter (fun c -> !sp <-@ c; sp := !sp +@ 1) target;
    pkt
end

module Read = struct
  type t
  let t : t structure typ = structure "Out_Read"
  let ( -:* ) s x = field t s x
  let data = "data" -:* array 0 char
  let () = seal t

  let data_ = data
  (* TODO: respect size, offset *)
  let create ~size ~data_fn req =
    let pkt = Hdr.packet ~count:size req in
    let body = CArray.start pkt in
    let buf = bigarray_of_ptr array1 size Bigarray.char body in
    let sz = data_fn buf in
    Hdr.set_size pkt sz
end

module Write = struct
  type t
  let t : t structure typ = structure "Out_Write"
  let ( -:* ) s x = field t s x
  let size    = "size"    -:* int_of_32
  let padding = "padding" -:* int32_of_32
  let () = seal t

  let size_ = size
  let create ~size req =
    let pkt = Hdr.make req t in
    setf pkt size_    size;
    setf pkt padding  0l; (* TODO: necessary? *)
    CArray.from_ptr (coerce (ptr t) (ptr char) (addr pkt)) (sizeof t)
end

module Open = struct
  type t
  let t : t structure typ = structure "Out_Open"
  let ( -:* ) s x = field t s x
  let fh         = "fh"         -:* int64_of_64
  let open_flags = "open_flags" -:* int32_of_32
  let padding    = "padding"    -:* int32_of_32
  let () = seal t

  let fh_ = fh
  let open_flags_ = open_flags
  let store ~fh ~open_flags mem req =
    setf mem fh_         fh;
    setf mem open_flags_ open_flags;
    setf mem padding     0l; (* TODO: necessary? *)
    ()

  let create ~fh ~open_flags req =
    let pkt = Hdr.make req t in
    store ~fh ~open_flags pkt req;
    CArray.from_ptr (coerce (ptr t) (ptr char) (addr pkt)) (sizeof t)
end

module Init = struct
  type t
  let t : t structure typ = structure "Out_Init"
  let ( -:* ) s x = field t s x
  let major         = "major"         -:* int_of_32
  let minor         = "minor"         -:* int_of_32
  let max_readahead = "max_readahead" -:* int_of_32
  let flags         = "flags"         -:* uint32_t
  let unused        = "unused"        -:* uint32_t
  let max_write     = "max_write"     -:* int_of_32
  let () = seal t

  let major_ = major
  let minor_ = minor
  let max_readahead_ = max_readahead
  let flags_ = flags
  let unused_ = unused
  let max_write_ = max_write
  let create ~major ~minor ~max_readahead ~flags ~max_write req =
    let pkt = Hdr.make req t in
    setf pkt major_         major;
    setf pkt minor_         minor;
    setf pkt max_readahead_ max_readahead;
    setf pkt flags_         flags;
    setf pkt unused_        (UInt32.of_int 0); (* TODO: necessary? *)
    setf pkt max_write_     max_write;
    CArray.from_ptr (coerce (ptr t) (ptr char) (addr pkt)) (sizeof t)

  let describe pkt =
    Printf.sprintf
      "version=%d.%d max_readahead=%d flags=0x%lX max_write=%d"
      (getf pkt major) (getf pkt minor)
      (getf pkt max_readahead)
      (Unsigned.UInt32.to_int32 (getf pkt flags))
      (getf pkt max_write)

end
