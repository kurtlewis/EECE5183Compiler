# EECE5183 Compiler, "kjlc"
This is a compiler I'm building for my EECE5183 course at the University of
Cincinnati. Our task is to build a simple one-pass compiler that can compile
the language defined at [`docs/language-spec.pdf'](docs/language-spec.pdf).

I have named my compiler `kjlc`.

## Dependencies
This project depends on the following for building it via make:
 * g++
 * a c++11 std library
 * llvm-8 installed, with `llvm-config` in the executable path 
   * I accomplish this through the [apt repositories.](apt.llvm.org)

## Instructions for use
Compile the compiler using `make`. Run the executable `out/kjlc`.
