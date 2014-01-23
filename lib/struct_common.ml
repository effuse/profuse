open Ctypes
open View

module Kstatfs = struct
  type t
  let t : t structure typ = structure "Kstatfs"
  let ( -:* ) s x = field t s x
  let blocks  = "blocks"  -:* uint64_t
  let bfree   = "bfree"   -:* uint64_t
  let bavail  = "bavail"  -:* uint64_t
  let files   = "files"   -:* uint64_t
  let ffree   = "ffree"   -:* uint64_t
  let bsize   = "bsize"   -:* uint32_t
  let namelen = "namelen" -:* uint32_t
  let frsize  = "frsize"  -:* uint32_t
  let padding = "padding" -:* uint32_t
  let spare   = "spare"   -:* array 6 uint32_t
  let () = seal t
end

module File_lock = struct
  type t
  let t : t structure typ = structure "File_lock"
  let ( -:* ) s x = field t s x
  let start  = "start" -:* uint64_t
  let end_   = "end_"  -:* uint64_t
  let type_  = "type_" -:* uint32_t
  let pid    = "pid"   -:* uint32_t (* tgid *)
  let () = seal t
end
