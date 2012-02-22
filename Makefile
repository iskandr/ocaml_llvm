# see: http://www.ocaml.info/home/ocaml_sources.html#toc16

# put here the names of your source files (in the right order)
SOURCES = dsl.mli dsl.ml compiler.mli compiler.ml main.ml

CLIBS = llvm LLVMCore LLVMSupport
LIBS = llvm llvm_analysis llvm_executionengine llvm_target llvm_scalar_opts


# the name of the resulting executable
RESULT  = main

# generate type information (.annot files)
ANNOTATE = yes

# LLVM depends on lots of gunk from C++ 
OCAMLOPT = ocamlopt -cc g++ 

# make target (see manual) : byte-code, debug-code, native-code, ...
all: native-code

include OCamlMakefile