(*
 * Copyright (c) 2016 Jeremy Yallop <yallop@gmail.com>
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

(* There were no changes to struct definitions between FUSE 7.9 and 7.10 *)
module C_compatible(F: Cstubs.Types.TYPE) = struct
  open F

  module Version_7_9 = Profuse_types_7_9.C_compatible(F)

  module Struct = Version_7_9.Struct

  module Out =
  struct
    module Open =
    struct
      module Flags =
      struct
        type t = [ `FOPEN_NONSEEKABLE
                 | Version_7_9.Out.Open.Flags.t ]
        include (Version_7_9.Out.Open.Flags
                 : module type of Version_7_9.Out.Open.Flags
                 with type t := Version_7_9.Out.Open.Flags.t)
        let fopen_nonseekable = constant "FOPEN_NONSEEKABLE" t
        let enum_values : (t * _) list =
          (`FOPEN_NONSEEKABLE, fopen_nonseekable) ::
          (Version_7_9.Out.Open.Flags.enum_values :> (t * _) list)
      end
      include (Version_7_9.Out.Open
               : module type of Version_7_9.Out.Open
               with module Flags := Version_7_9.Out.Open.Flags)
    end
    include (Version_7_9.Out
             : module type of Version_7_9.Out
             with module Open := Version_7_9.Out.Open)
  end

  module In =
  struct
    module Init =
    struct
      module Flags =
      struct
        include Version_7_9.In.Init.Flags
        let fuse_export_support = constant "FUSE_EXPORT_SUPPORT" t
      end
      include (Version_7_9.In.Init
               : module type of Version_7_9.In.Init
               with module Flags := Flags)
    end
    include (Version_7_9.In
             : module type of Version_7_9.In
             with module Init := Version_7_9.In.Init)
  end
end

module C_incompatible = Profuse_types_7_9.C_incompatible

module C(F: Cstubs.Types.TYPE) = struct
  type 'a structure = 'a Ctypes_static.structure
  module Compatible = C_compatible(F)
  module Incompatible = C_incompatible(F)
  module Struct = Compatible.Struct
  module Out = Compatible.Out
  module In =
  struct
    include Compatible.In
    include Incompatible.In
  end
end
