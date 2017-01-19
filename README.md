# LLVMlite Compiler #
This project was the third homework for the compilers class at Penn. I worked with a partner to implement a compiler for a subset of LLVM (LLVMlite) to x86 in Ocaml. Details about the LLVMlite available [here](http://www.cis.upenn.edu/~cis341/15sp/hw/hw3/llvmlite.shtml).

## Setup ##

Setup instructions for a relatively recent version of Ubuntu (>= 14.04):

### Install GCC ###

    sudo apt-get install gcc

### Install Clang ###

    sudo apt-get install clang

### Install OCaml >= 4.01.0 ###

    sudo apt-get install m4 ocaml-native-compilers camlp4-extra opam

### Install Menhir ###

    opam install menhir

## Build ##

Navigate to the folder and type

    make

## Test cases ##

`main.native` is the executable that is built. It is setup to run the test harness for the compiler, which are defined by the `gradedtests.mll` and `providedtests.ml` files. The test harness compiles llvm programs in the `llprograms` folder and compares the output of the compiled program with the expected output.

On Linux, run:

    main.native -linux --test

On OSX, run:
    
    main.native --test

More details about how to use `main.native` for testing can be found in `README_TESTING`.

## Use ##

All of the code that for the compiler is located in the `backend.ml` file.

To run the compiler on your own LLVM program and then run the generated executable:

    let ll_ast = Driver.parse_file path in
    let output_path = !Platform.output_path in
    let dot_s_file = Platform.gen_name output_path "test" ".s" in
    let exec_file = Platform.gen_name output_path "exec" "" in
    let asm_ast = Backend.compile_prog ll_ast in
    let asm_str = X86.string_of_prog asm_ast in
    let _ = Driver.write_file dot_s_file asm_str in
    let _ = Platform.link (dot_s_file::c_files) exec_file in
    let args = String.concat " " args in
    let result = Driver.run_executable args exec_file in
    let _ = Platform.sh (Printf.sprintf "rm -f %s %s" dot_s_file exec_file) Platform.ignore_error in
    Int64.of_int result

`path` is a string representing the location of your LLVM file. `c_files` is an array of strings containing the names of `C` files with functions that you are calling from your LLVM program. The result of the compilation can be seen in the `test.s` file.
