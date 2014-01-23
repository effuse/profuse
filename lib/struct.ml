open Ctypes

module type LINUX_7_8 = sig
  include module type of Struct_common
  include module type of Struct_linux_7_8
    with module Attr = Struct_linux_7_8.Attr

  val store_attr_to_osx_7_8 :
    Attr.t structure -> Struct_osx_7_8.Attr.t structure -> unit
  val create_attr_to_osx_7_8 :
    Attr.t structure -> Struct_osx_7_8.Attr.t structure
end

module type OSX_7_8 = sig
  include module type of Struct_common
  include module type of Struct_osx_7_8
    with module Attr = Struct_osx_7_8.Attr

  val store_attr_to_linux_7_8 :
    Attr.t structure -> Struct_linux_7_8.Attr.t structure -> unit
  val create_attr_to_linux_7_8 :
    Attr.t structure -> Struct_linux_7_8.Attr.t structure
end

module Linux_7_8 : LINUX_7_8 = struct
  include Struct_common
  include Struct_linux_7_8

  let store_attr_to_osx_7_8 attr mem =
    let open Attr in
    Struct_osx_7_8.Attr.store
      ~ino:(getf attr ino)
      ~size:(getf attr size)
      ~blocks:(getf attr blocks)
      ~atime:(getf attr atime)
      ~mtime:(getf attr mtime)
      ~ctime:(getf attr ctime)
      ~atimensec:(getf attr atimensec)
      ~mtimensec:(getf attr mtimensec)
      ~ctimensec:(getf attr ctimensec)
      ~mode:(getf attr mode)
      ~nlink:(getf attr nlink)
      ~uid:(getf attr uid)
      ~gid:(getf attr gid)
      ~rdev:(getf attr rdev)
      mem

  let create_attr_to_osx_7_8 attr =
    let open Attr in
    Struct_osx_7_8.Attr.create
      ~ino:(getf attr ino)
      ~size:(getf attr size)
      ~blocks:(getf attr blocks)
      ~atime:(getf attr atime)
      ~mtime:(getf attr mtime)
      ~ctime:(getf attr ctime)
      ~atimensec:(getf attr atimensec)
      ~mtimensec:(getf attr mtimensec)
      ~ctimensec:(getf attr ctimensec)
      ~mode:(getf attr mode)
      ~nlink:(getf attr nlink)
      ~uid:(getf attr uid)
      ~gid:(getf attr gid)
      ~rdev:(getf attr rdev)
      ()
end

module Osx_7_8 : OSX_7_8 = struct
  include Struct_common
  include Struct_osx_7_8

  let store_attr_to_linux_7_8 attr mem =
    let open Attr in
    Struct_linux_7_8.Attr.store
      ~ino:(getf attr ino)
      ~size:(getf attr size)
      ~blocks:(getf attr blocks)
      ~atime:(getf attr atime)
      ~mtime:(getf attr mtime)
      ~ctime:(getf attr ctime)
      ~atimensec:(getf attr atimensec)
      ~mtimensec:(getf attr mtimensec)
      ~ctimensec:(getf attr ctimensec)
      ~mode:(getf attr mode)
      ~nlink:(getf attr nlink)
      ~uid:(getf attr uid)
      ~gid:(getf attr gid)
      ~rdev:(getf attr rdev)
      mem

  let create_attr_to_linux_7_8 attr =
    let open Attr in
    Struct_linux_7_8.Attr.create
      ~ino:(getf attr ino)
      ~size:(getf attr size)
      ~blocks:(getf attr blocks)
      ~atime:(getf attr atime)
      ~mtime:(getf attr mtime)
      ~ctime:(getf attr ctime)
      ~atimensec:(getf attr atimensec)
      ~mtimensec:(getf attr mtimensec)
      ~ctimensec:(getf attr ctimensec)
      ~mode:(getf attr mode)
      ~nlink:(getf attr nlink)
      ~uid:(getf attr uid)
      ~gid:(getf attr gid)
      ~rdev:(getf attr rdev)
      ()
end
