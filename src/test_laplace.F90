!==============================================================================!
program test_laplace
!------------------------------------------------------------------------------!
!
!  test the static library that does the numerical Laplace transformation
!  following the minimax approximation algorithm by Tatsuka et al.
!
! BHP
!------------------------------------------------------------------------------!

 implicit none

#include "consts.h"
#include "laplace_minimax.h"

! parameter:
 integer, parameter :: neig = 4

! local scalars:
 integer :: nlap, istro, iendo, istrv, iendv

! local arrays:
 real(8) :: xpnts(mxlap), wghts(mxlap), eig(neig)

 istro = 1; iendo = 2; istrv = 3; iendv = 4

! ------
! test 1
! ------
 nlap = 3
 eig(1) = -2.d0; eig(2) = -1.d0; eig(3) = +1.d0; eig(4) = +2.d0

 call laplace_minimax(xpnts,wghts,nlap,eig,neig,istro,iendo,istrv,iendv,iprint=1)

! ------
! test 2
! ------
 nlap = 8
 eig(1) = -23.d0; eig(2) = -1.7d0; eig(3) = +1.7d0; eig(4) = +23.d0

 call laplace_minimax(xpnts,wghts,nlap,eig,neig,istro,iendo,istrv,iendv,iprint=1,do_rmsd=.true.)

! ------
! test 3
! ------
 nlap = 25
 eig(1) = -2.3d3; eig(2) = -0.8d-1; eig(3) = +0.8d-1; eig(4) = +2.3d3

 call laplace_minimax(xpnts,wghts,nlap,eig,neig,istro,iendo,istrv,iendv,iprint=1)

end program test_laplace
!==============================================================================!
