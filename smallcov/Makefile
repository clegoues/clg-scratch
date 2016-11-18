# Makefile for Program Repair Tool (v2)

# You may need to set the CIL environment variable for this to
# work. It should point to the base of the CI source directory, mine
# is /usr/local/src/cil.
#
# If the CIL is not set then the ocamlfind utility will be used to set
# the CIL environment variable.

OS=$(shell uname)
ifeq ($(OS),Linux)
	OS=LINUX
 endif
ifeq ($(OS),Darwin)
	OS=DARWIN
endif

ifneq ($(CIL),)
	OCAML_OPTIONS = -I $(CIL)/obj/x86_$(OS)
else
  ifneq ($(shell type ocamlfind 2> /dev/null),)
	OCAML_OPTIONS = -I $(shell ocamlfind query cil)
  else
    ifeq ($(MAKECMDGOALS),)
$(error Please set the CIL environment variable)
    else
      ifneq ($(filter-out clean,$(MAKECMDGOALS)),)
$(error Please set the CIL environment variable)
      endif
    endif
  endif
endif

OCAMLC   = ocamlc -g $(OCAML_OPTIONS)
OCAMLOPT = ocamlopt -w Aelzv-7 $(OCAML_OPTIONS)
OCAMLDEP = ocamldep $(OCAML_OPTIONS)
OCAMLLEX = ocamllex
OCAMLDOC = ocamldoc $(OCAML_OPTIONS)

MBITS := $(if $(shell touch null.ml ; $(OCAMLOPT) -c null.ml ; file null.o | grep 64 ; rm null.*),-m64,-m32)

###
#
# You should not have to change anything below this line. 
#
###

# We use an internal utility to auto-generate token information,
# visitor code and pretty-printing code from ocaml type definitions. 
# If you don't change "tokens.type" or "jabs.ml" you won't need this. 

ALL = smallcov
all: $(ALL)

%.cmo: %.ml 
	@if [ -f $*.mli -a ! -f $*.cmi ] ; then $(OCAMLC) -c -g $*.mli ; fi 
	$(OCAMLC) -c -g $*.ml
	@$(OCAMLDEP) $*.ml > $*.d 

%.cmx: %.ml 
	@if [ -f $*.mli -a ! -f $*.cmi ] ; then $(OCAMLC) -c -g $*.mli ; fi 
	$(OCAMLOPT) -c $*.ml
	@$(OCAMLDEP) $*.ml > $*.d 

%.cmi: %.mli
	$(OCAMLC) -c -g $*.mli

%.ml: %.mll
	$(OCAMLLEX) $*.mll

# NOTE: Module order is important!  OCaml module dependencies cannot
# be cyclic, and the order presented must respect the dependency order.

BASE_MODULES = \
  stats2.cmo \
  global.cmo 
REPAIR_MODULES = \
  $(BASE_MODULES) \
  fileprocess.cmo \
  runtests.cmo \
  main.cmo

STANDARD_LIBS = \
	bigarray.cmxa \
	unix.cmxa \
	str.cmxa \
	nums.cmxa

# This .PHONY target stores the current repository version into an OCaml file
# so that we can report it in the debug log. It is .PHONY to ensure that make
# always rebuilds the file; otherwise, a stale file might keep reporting the
# wrong version even after many updates.


smallcov: $(REPAIR_MODULES:.cmo=.cmx) 
	$(OCAMLOPT) -o $@ $(STANDARD_LIBS) cil.cmxa $^


-include $(ALL_MODULES:.cmo=.d)

clean:
	rm -f smallcov *.mli *.cmo *.cmi *.d *.cmx *.dx *.o $(ALL)
