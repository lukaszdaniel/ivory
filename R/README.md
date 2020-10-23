# CXXR

The goal of the CXXR project is to refactor the interpreter of the R language into a fully-compatible, efficient, VM for R using modern software engineering techniques.  CXXR is being carried out independently of the main R development and maintenance effort.

## Build Requirements

Compiling rho requires a GCC or Clang compiler with C++ 11 support and fortran support.  In addition the following libraries must be installed:
   * boost >= 1.48.0
   * libcurl >= 7.28.0
   * zlib >= 1.2.5
   * libbzip2 >= 1.0.6
   * liblzma >= 5.0.3
   * pcre >= 8.10
   * libedit


## Configuration and Compilation

To build CXXR from scratch run:
    aclocal -I m4 --install && autoconf  
    ./configure CC=clang CXX=clang++ --with-included-gettext --enable-R-profiling --enable-memory-profiling --enable-werror  
    or  
    ./configure CC=gcc CXX=g++ --with-included-gettext --enable-R-profiling --enable-memory-profiling --enable-werror  
    make  
    make check-devel
