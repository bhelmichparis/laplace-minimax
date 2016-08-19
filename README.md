# laplace-minimax
Routine that computes the numerical Laplace weights and exponents 
of orbital energy denominators using the minimax approximation

          written by Benjamin Helmich-Paris
              VU University Amsterdam

To compile the code, do  the following:

  (1) export FC=<fortran_compiler>

       - supported compilers are 
  
           gfortran ...
           ifort    ...
           pgf95    ...

  (2) make 

       - generates the library liblaplace_minimax.a that can be
         linked externally and is provided in lib/ 

  (3) make test

       - compiles test_laplace and links the library. Three test
         examples are run tested by a Dalton-like script
