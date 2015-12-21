.PHONY: build test install uninstall reinstall clean

FINDLIB_NAME=profuse
MOD_NAME=profuse

OCAML_LIB_DIR=$(shell ocamlc -where)

CTYPES_LIB_DIR=$(shell ocamlfind query ctypes)

OCAMLBUILD=CTYPES_LIB_DIR=$(CTYPES_LIB_DIR) OCAML_LIB_DIR=$(OCAML_LIB_DIR) \
	ocamlbuild -use-ocamlfind -classic-display

WITH_LWT=$(shell ocamlfind query lwt > /dev/null 2>&1 ; echo $$?)

TARGETS=.cma .cmxa

PRODUCTS=$(addprefix $(MOD_NAME),$(TARGETS)) \

#         $(addprefix $(MOD_NAME)_linux,$(TARGETS))
#	lib$(MOD_NAME)_stubs.a dll$(MOD_NAME)_stubs.so

ifeq ($(WITH_LWT), 0)
PRODUCTS+=$(addprefix $(MOD_NAME)_lwt,$(TARGETS))
endif

TYPES=.mli .cmi .cmti

INSTALL:=$(addprefix $(MOD_NAME), $(TYPES)) \
         $(addprefix fuse, $(TYPES)) \
         $(addprefix handles, $(TYPES)) \
         $(addprefix nodes, $(TYPES)) \
         $(addprefix $(MOD_NAME), $(TARGETS))

INSTALL:=$(addprefix _build/lib/,$(INSTALL))

ifeq ($(WITH_LWT), 0)
INSTALL_LWT:=$(addprefix fuse_lwt,$(TYPES)) \
             $(addprefix $(MOD_NAME)_lwt,$(TARGETS))

INSTALL_LWT:=$(addprefix _build/lwt/,$(INSTALL_LWT))

INSTALL+=$(INSTALL_LWT)
endif

ARCHIVES:=_build/lib/$(MOD_NAME).a

ifeq ($(WITH_LWT), 0)
ARCHIVES+=_build/lwt/$(MOD_NAME)_lwt.a
endif

build:
	$(OCAMLBUILD) $(PRODUCTS)

test: build
	$(OCAMLBUILD) lib_test/test.native
	./test.native

install:
	ocamlfind install $(FINDLIB_NAME) META \
		$(INSTALL) \
		$(ARCHIVES)

uninstall:
	ocamlfind remove $(FINDLIB_NAME)

reinstall: uninstall install

clean:
	ocamlbuild -clean
