open Ctypes
open Unsigned
open View

module type LINUX_7_8 = sig
  include module type of Out_common
  include module type of Out_linux_7_8
end
module type OSX_7_8 = sig
  include module type of Out_common
  include module type of Out_osx_7_8
end

module Linux_7_8 : LINUX_7_8 = struct
  include Out_common
  include Out_linux_7_8
end

module Osx_7_8 : OSX_7_8 = struct
  include Out_common
  include Out_osx_7_8
end

module Linux_7_8_of_osx_7_8 : LINUX_7_8 = struct
  include Out_common

  module Struct = Struct.Linux_7_8

  module Create = struct
    include Linux_7_8.Create

    let create ~store_entry ~store_open req =
      let open Linux_7_8 in
      let entry = make Entry.t in
      store_entry entry req;
      let nodeid = getf entry Entry.nodeid in
      let generation = getf entry Entry.generation in
      let entry_valid = getf entry Entry.entry_valid in
      let attr_valid = getf entry Entry.attr_valid in
      let entry_valid_nsec = getf entry Entry.entry_valid_nsec in
      let attr_valid_nsec = getf entry Entry.attr_valid_nsec in
      let store_attr = Struct.store_attr_to_osx_7_8 (getf entry Entry.attr) in
      Osx_7_8.(
        Create.create
          ~store_entry:(Entry.store
                          ~nodeid
                          ~generation
                          ~entry_valid
                          ~attr_valid
                          ~entry_valid_nsec
                          ~attr_valid_nsec
                          ~store_attr
          )
          ~store_open
          req
      )
  end

  module Attr = struct
    include Linux_7_8.Attr

    let create ~attr_valid ~attr_valid_nsec ~store_attr req =
      let attr = make Struct.Attr.t in
      store_attr attr;
      let store_attr = Struct.store_attr_to_osx_7_8 attr in
      Osx_7_8.(Attr.create ~attr_valid ~attr_valid_nsec ~store_attr req)
  end

  module Entry = struct
    include Linux_7_8.Entry

    let create ~nodeid ~generation ~entry_valid ~attr_valid
        ~entry_valid_nsec ~attr_valid_nsec ~store_attr req =
      let attr = make Struct.Attr.t in
      store_attr attr;
      let store_attr = Struct.store_attr_to_osx_7_8 attr in
      Osx_7_8.Entry.create
        ~nodeid
        ~generation
        ~entry_valid
        ~attr_valid
        ~entry_valid_nsec
        ~attr_valid_nsec
        ~store_attr
        req
  end
end

module Osx_7_8_of_linux_7_8 : OSX_7_8 = struct
  include Out_common

  module Struct = Struct.Osx_7_8

  module Create = struct
    include Osx_7_8.Create

    let create ~store_entry ~store_open req =
      let open Osx_7_8 in
      let entry = make Entry.t in
      store_entry entry req;
      let nodeid = getf entry Entry.nodeid in
      let generation = getf entry Entry.generation in
      let entry_valid = getf entry Entry.entry_valid in
      let attr_valid = getf entry Entry.attr_valid in
      let entry_valid_nsec = getf entry Entry.entry_valid_nsec in
      let attr_valid_nsec = getf entry Entry.attr_valid_nsec in
      let store_attr = Struct.store_attr_to_linux_7_8 (getf entry Entry.attr) in
      Linux_7_8.(
        Create.create
          ~store_entry:(Entry.store
                          ~nodeid
                          ~generation
                          ~entry_valid
                          ~attr_valid
                          ~entry_valid_nsec
                          ~attr_valid_nsec
                          ~store_attr
          )
          ~store_open
          req
      )
  end

  module Attr = struct
    include Osx_7_8.Attr

    let create ~attr_valid ~attr_valid_nsec ~store_attr req =
      let attr = make Struct.Attr.t in
      store_attr attr;
      let store_attr = Struct.store_attr_to_linux_7_8 attr in
      Linux_7_8.(Attr.create ~attr_valid ~attr_valid_nsec ~store_attr req)
  end

  module Entry = struct
    include Osx_7_8.Entry

    let create ~nodeid ~generation ~entry_valid ~attr_valid
        ~entry_valid_nsec ~attr_valid_nsec ~store_attr req =
      let attr = make Struct.Attr.t in
      store_attr attr;
      let store_attr = Struct.store_attr_to_linux_7_8 attr in
      Linux_7_8.Entry.create
        ~nodeid
        ~generation
        ~entry_valid
        ~attr_valid
        ~entry_valid_nsec
        ~attr_valid_nsec
        ~store_attr
        req
  end
end
