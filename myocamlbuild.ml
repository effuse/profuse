open Ocamlbuild_plugin;;
open Ocamlbuild_pack;;

let ctypes_libdir = Sys.getenv "CTYPES_LIB_DIR" in
let ocaml_libdir = Sys.getenv "OCAML_LIB_DIR" in

dispatch begin
  function
  | After_rules ->

    rule "cstubs: lib_gen/x_types_detect.c -> x_types_detect"
      ~prods:["lib_gen/%_types_detect_%(version).exe"]
      ~deps:["lib_gen/%_types_detect_%(version).c"; "lib_gen/fuse_kernel.h.%(version)"]
      (fun env build ->
         Cmd (S[A"cc";
                A("-I"); A ctypes_libdir;
                A("-I"); A ocaml_libdir;
                A"-o";
                A(env "lib_gen/%_types_detect_%(version).exe");
                A(env "lib_gen/%_types_detect_%(version).c");
               ]));

    rule "cstubs: lib_gen/x_types_detect -> lib/x_types_detected.ml"
      ~prods:["lib/%_types_detected_%(version).ml"]
      ~deps:["lib_gen/%_types_detect_%(version).exe"]
      (fun env build ->
         Cmd (S[A(env "lib_gen/%_types_detect_%(version).exe");
                Sh">";
                A(env "lib/%_types_detected_%(version).ml");
               ]));

    rule "cstubs: lib_gen/x_types.ml -> x_types_detect.c"
      ~prods:["lib_gen/%_types_detect_%(version).c"]
      ~deps: ["lib_gen/%_typegen.byte"]
      (fun env build ->
         Cmd (S[A(env "lib_gen/%_typegen.byte");
                A"--fuse-version";
                A(env "%(version)");
                A"--output-file";
                A(env "lib_gen/profuse_types_detect_%(version).c")]));

    copy_rule "cstubs: lib_gen/x_types.ml -> lib/x_types.ml"
      "lib_gen/%_types_%(version).ml" "lib/%_types_%(version).ml";

    rule "cstubs: lib/x_bindings.ml -> x_stubs.c, x_generated.ml"
      ~prods:["lib/%_stubs.c"; "lib/%_generated.ml"]
      ~deps: ["lib_gen/%_bindgen.byte"]
      (fun env build ->
        Cmd (A(env "lib_gen/%_bindgen.byte")));

    copy_rule "cstubs: lib_gen/x_bindings.ml -> lib/x_bindings.ml"
      "lib_gen/%_bindings.ml" "lib/%_bindings.ml";

    flag ["c"; "compile"] & S[A"-ccopt"; A"-I/usr/local/include"];
    flag ["c"; "ocamlmklib"] & S[A"-L/usr/local/lib"];
    flag ["ocaml"; "link"; "native"; "program"] &
      S[A"-cclib"; A"-L/usr/local/lib";
        A"-cclib"; A("-L"^Unix.getcwd()^"/_build/lib")];

    (* Linking cstubs *)
    dep ["c"; "compile"; "use_profuse_util"]
      ["lib/profuse_util.o"; "lib/profuse_util.h"];
    flag ["c"; "compile"; "use_ctypes"] & S[A"-I"; A ctypes_libdir];
    flag ["c"; "compile"; "debug"] & A"-g";

    (* Linking generated stubs *)
    dep ["ocaml"; "link"; "byte"; "library"; "use_profuse_stubs"]
      ["lib/dllprofuse_stubs"-.-(!Options.ext_dll)];
    flag ["ocaml"; "link"; "byte"; "library"; "use_profuse_stubs"] &
      S[A"-dllib"; A"-lprofuse_stubs"];

    dep ["ocaml"; "link"; "native"; "library"; "use_profuse_stubs"]
      ["lib/libprofuse_stubs"-.-(!Options.ext_lib)];
    flag ["ocaml"; "link"; "native"; "library"; "use_profuse_stubs"] &
      S[A"-cclib"; A"-lprofuse_stubs"];

    (* Linking tests *)
    flag ["ocaml"; "link"; "byte"; "program"; "use_profuse_stubs"] &
      S[A"-dllib"; A"-lprofuse_stubs"];
    dep ["ocaml"; "link"; "byte"; "program"; "use_profuse_stubs"]
      ["lib/dllprofuse_stubs"-.-(!Options.ext_dll)];

    flag ["ocaml"; "link"; "native"; "program"; "use_profuse_stubs"] &
      S[A"-cclib"; A"-lprofuse_stubs"];
    dep ["ocaml"; "link"; "native"; "program"; "use_profuse_stubs"]
      ["lib/libprofuse_stubs"-.-(!Options.ext_lib)];

  | _ -> ()
end;;
