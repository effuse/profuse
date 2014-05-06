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

open Out_common

module Struct = Struct.Osx_7_8

module Entry = struct
  type t
  let t : t structure typ = structure "Out_Entry"
  let ( -:* ) s x = field t s x
  let nodeid           = "nodeid"           -:* int64_of_64 (* inode *)
  let generation       = "generation"       -:* int64_of_64
  (* (inode, generation) must be unique over fs life *)
  let entry_valid      = "entry_valid"      -:* int64_of_64
  (* name cache timeout *)
  let attr_valid       = "attr_valid"       -:* int64_of_64
  (* attribute cache timeout *)
  let entry_valid_nsec = "entry_valid_nsec" -:* int32_of_32
  let attr_valid_nsec  = "attr_valid_nsec"  -:* int32_of_32
  (* TODO: Is that really the right order?! *)
  let attr             = "attr"             -:* Struct.Attr.t
  let () = seal t

  let nodeid_ = nodeid
  let generation_ = generation
  let entry_valid_ = entry_valid
  let attr_valid_ = attr_valid
  let entry_valid_nsec_ = entry_valid_nsec
  let attr_valid_nsec_ = attr_valid_nsec
  let attr_ = attr
  let store ~nodeid ~generation ~entry_valid ~attr_valid
      ~entry_valid_nsec ~attr_valid_nsec ~store_attr mem req =
    setf mem nodeid_ nodeid;
    setf mem generation_ generation;
    setf mem entry_valid_ entry_valid;
    setf mem attr_valid_ attr_valid;
    setf mem entry_valid_nsec_ entry_valid_nsec;
    setf mem attr_valid_nsec_ attr_valid_nsec;
    store_attr (getf mem attr_);
    ()    

  let create ~nodeid ~generation ~entry_valid ~attr_valid
      ~entry_valid_nsec ~attr_valid_nsec ~store_attr req =
    let pkt = Hdr.make req t in
    store ~nodeid ~generation ~entry_valid ~attr_valid
      ~entry_valid_nsec ~attr_valid_nsec ~store_attr pkt req;
    CArray.from_ptr (coerce (ptr t) (ptr char) (addr pkt)) (sizeof t)

  let describe ~host pkt =
    (* TODO: times? *)
    Printf.sprintf
      "nodeid=%Ld.%Ld attr={%s}"
      (getf pkt generation) (getf pkt nodeid)
      (Struct.Attr.describe ~host (getf pkt attr))

end

module Attr = struct
  type t
  let t : t structure typ = structure "Out_Attr"
  let ( -:* ) s x = field t s x
  let attr_valid      = "attr_valid"      -:* int64_of_64
  let attr_valid_nsec = "attr_valid_nsec" -:* int32_of_32
  let dummy           = "dummy"           -:* int32_of_32
  let attr            = "attr"            -:* Struct.Attr.t
  let () = seal t

  let attr_valid_ = attr_valid
  let attr_valid_nsec_ = attr_valid_nsec
  let dummy_ = dummy
  let attr_ = attr
  let create ~attr_valid ~attr_valid_nsec ~store_attr req =
    let pkt = Hdr.make req t in
    setf pkt attr_valid_      attr_valid;
    setf pkt attr_valid_nsec_ attr_valid_nsec;
    store_attr (getf pkt attr_);
    CArray.from_ptr (coerce (ptr t) (ptr char) (addr pkt)) (sizeof t)
end

module Create = struct
  type t
  let t : t structure typ = structure "Out_Create"
  let ( -:* ) s x = field t s x
  let entry  = "entry" -:* Entry.t
  let open_  = "open"  -:* Open.t
  let () = seal t

  let create ~store_entry ~store_open req =
    let pkt = Hdr.make req t in
    store_entry (getf pkt entry) req;
    store_open  (getf pkt open_) req;
    CArray.from_ptr (coerce (ptr t) (ptr char) (addr pkt)) (sizeof t)
end
