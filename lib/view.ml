open Ctypes
open Unsigned

let int_of_32 = view ~read:UInt32.to_int ~write:UInt32.of_int uint32_t
let int32_of_32 = view ~read:UInt32.to_int32 ~write:UInt32.of_int32 uint32_t
let int_of_64 = view ~read:UInt64.to_int ~write:UInt64.of_int uint64_t
let int64_of_64 = view ~read:UInt64.to_int64 ~write:UInt64.of_int64 uint64_t
