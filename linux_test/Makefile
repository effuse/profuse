.PHONY: all

FLAGS=-package oUnit \
      -package alcotest \
      -package ctypes \
      -package fd-send-recv \
      -package unix-unistd \
      -package unix-errno \
      -package unix-dirent \
      -package unix-sys-stat \
      -package unix-fcntl \
      -package lwt.unix \
      -package lwt.preemptive \
      -g -thread

all: test.ml
	ocamlfind ocamlopt -o test $(FLAGS) -I ../_build/lib -I ../_build/linux \
		../_build/lib/opcode.cmx \
		../_build/lib/fuse.cmx \
		../_build/lib/view.cmx \
		../_build/lib/struct_common.cmx \
		../_build/lib/struct_linux_7_8.cmx \
		../_build/lib/struct_osx_7_8.cmx \
		../_build/lib/struct.cmx \
		../_build/lib/in_common.cmx \
		../_build/lib/in_linux_7_8.cmx \
		../_build/lib/in_osx_7_8.cmx \
		../_build/lib/in.cmx \
		../_build/lib/out_common.cmx \
		../_build/lib/out_linux_7_8.cmx \
		../_build/lib/out_osx_7_8.cmx \
		../_build/lib/out.cmx \
		../_build/linux/profuse.cmx \
		../_build/linux/lofs.cmx \
		-linkpkg test.ml
	./test || true
	@echo
	@rm test.cmi test.cmx test.o test
