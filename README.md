# laplace-minimax
Routine that computes the numerical Laplace weights and exponents 
of orbital energy denominators using the minimax approximation



          written by Benjamin Helmich-Paris

              VU University Amsterdam



Please cite the following work for referencing:

 [1] A. Takatsuka et al.,  J. Chem. Phys. 129, 044112 (2008); 
      doi: 10.1063/1.2958921

 [2] B. Helmich-Paris and L. Visscher,  J. Comput. Phys. 321 (2016) 927 - 931; 
      doi: 10.1016/j.jcp.2016.06.011


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
