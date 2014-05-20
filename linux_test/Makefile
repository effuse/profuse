.PHONY: all clean

FLAGS=-package lwt.unix \
      -package lwt.preemptive \
      -package oUnit \
      -package alcotest \
      -package ctypes \
      -package fd-send-recv \
      -package unix-unistd \
      -package unix-errno \
      -package unix-dirent \
      -package unix-sys-stat \
      -package unix-fcntl \
      -package stringext \
      -g -thread

all: run_test run_as
# strace -f -e trace=file,desc
	mkdir -p _tests
	sudo ./run_test || true
	$(MAKE) clean

run_as:
	ln -sf `id -u` run_as

run_test: test.ml
	ocamlfind ocamlopt -o run_test $(FLAGS) \
		-I ../_build/lib -I ../_build/linux \
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
		../_build/lib/nodes.cmx \
		../_build/lib/handles.cmx \
		../_build/lib/agent_handler.cmx \
		../_build/linux/profuse.cmx \
		../_build/linux/lofs.cmx \
		-linkpkg test.ml

clean:
	sudo fusermount -u mnt || true
	sudo fusermount -u multi || true
	rm -f test.cmi test.cmx test.o run_as
	rm -f run_test
	rm -rf lofs mnt multi
