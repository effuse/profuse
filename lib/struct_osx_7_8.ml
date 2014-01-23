open Ctypes
open View

module Attr = struct
  type t
  let t : t structure typ = structure "Attr"
  let ( -:* ) s x = field t s x
  let ino       = "ino"       -:* int64_of_64
  let size      = "size"      -:* int64_of_64
  let blocks    = "blocks"    -:* int64_of_64
  let atime     = "atime"     -:* int64_of_64
  let mtime     = "mtime"     -:* int64_of_64
  let ctime     = "ctime"     -:* int64_of_64

  let crtime    = "crtime"    -:* int64_of_64

  let atimensec = "atimensec" -:* int32_of_32
  let mtimensec = "mtimensec" -:* int32_of_32
  let ctimensec = "ctimensec" -:* int32_of_32

  let crtimensec= "crtimensec"-:* int32_of_32

  (* TODO: check order *)
  let mode      = "mode"      -:* int32_of_32
  let nlink     = "nlink"     -:* int32_of_32
  let uid       = "uid"       -:* int32_of_32
  let gid       = "gid"       -:* int32_of_32
  let rdev      = "rdev"      -:* int32_of_32

  let flags     = "flags"     -:* int32_of_32

  let () = seal t

  let ino_       = ino
  let size_      = size
  let blocks_    = blocks
  let atime_     = atime
  let mtime_     = mtime
  let ctime_     = ctime

  let crtime_    = crtime

  let atimensec_ = atimensec
  let mtimensec_ = mtimensec
  let ctimensec_ = ctimensec

  let crtimensec_= crtimensec

  let mode_      = mode
  let nlink_     = nlink
  let uid_       = uid
  let gid_       = gid
  let rdev_      = rdev

  let flags_     = flags

  let store ~ino ~size ~blocks
      ~atime ~mtime ~ctime ?(crtime=0L)
      ~atimensec ~mtimensec ~ctimensec ?(crtimensec=0l)
      ~mode ~nlink ~uid ~gid ~rdev ?(flags=0l) mem =
    setf mem ino_        ino;
    setf mem size_       size;
    setf mem blocks_     blocks;
    setf mem atime_      atime;
    setf mem mtime_      mtime;
    setf mem ctime_      ctime;
    setf mem crtime_     crtime;
    setf mem atimensec_  atimensec;
    setf mem mtimensec_  mtimensec;
    setf mem ctimensec_  ctimensec;
    setf mem crtimensec_ crtimensec;
    setf mem mode_       mode;
    setf mem nlink_      nlink;
    setf mem uid_        uid;
    setf mem gid_        gid;
    setf mem rdev_       rdev;
    setf mem flags_      flags;
    ()

  let create ~ino ~size ~blocks
      ~atime ~mtime ~ctime ?(crtime=0L)
      ~atimensec ~mtimensec ~ctimensec ?(crtimensec=0l)
      ~mode ~nlink ~uid ~gid ~rdev ?(flags=0l) () =
    let attr = make t in
    store ~ino ~size ~blocks
      ~atime ~mtime ~ctime ~crtime
      ~atimensec ~mtimensec ~ctimensec ~crtimensec
      ~mode ~nlink ~uid ~gid ~rdev ~flags attr;
    attr
end
