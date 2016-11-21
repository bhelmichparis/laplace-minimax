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

! local scalars:
 integer :: nlap
 real(8) :: errmax

! local arrays:
 real(8) :: xpnts(mxlap), wghts(mxlap), eig(4), ymin, ymax

! ------
! test 1
! ------
 nlap = 3
 eig(1) = -2.d0; eig(2) = -1.d0; eig(3) = +1.d0; eig(4) = +2.d0

 ymin = 2.d0*(eig(3)-eig(2))
 ymax = 2.d0*(eig(4)-eig(1))

 call laplace_minimax(errmax,xpnts,wghts,nlap,ymin,ymax,&
                      iprint=1,do_rmsd=.true.)

! ------
! test 2
! ------
 nlap = 8
 eig(1) = -23.d0; eig(2) = -1.7d0; eig(3) = +1.7d0; eig(4) = +23.d0

 ymin = 2.d0*(eig(3)-eig(2))
 ymax = 2.d0*(eig(4)-eig(1))

 call laplace_minimax(errmax,xpnts,wghts,nlap,ymin,ymax,&
                      iprint=1,do_rmsd=.true.)

! ------
! test 3
! ------
 nlap = 25
 eig(1) = -2.3d3; eig(2) = -0.8d-1; eig(3) = +0.8d-1; eig(4) = +2.3d3

 ymin = 2.d0*(eig(3)-eig(2))
 ymax = 2.d0*(eig(4)-eig(1))

 call laplace_minimax(errmax,xpnts,wghts,nlap,ymin,ymax,&
                      iprint=1,do_rmsd=.true.)

! ------
! test 4
! ------
 nlap = 3

 eig(1) = -20.557772190846030D0
 eig(2) = -20.557772190846030D0
 eig(3) = +1.1900668172888584D0
 eig(4) = +2.6379585887262555D0

 xpnts(1) = 0.009260954330D0; wghts(1) = 0.024005004419D0
 xpnts(2) = 0.051107515326D0; wghts(2) = 0.061536647229D0
 xpnts(3) = 0.140142919490D0; wghts(3) = 0.124833915648D0

 ymin = 2.d0*(eig(3)-eig(2))
 ymax = 2.d0*(eig(4)-eig(1))

 call laplace_minimax(errmax,xpnts,wghts,nlap,ymin,ymax,&
                      iprint=1,mxiter=100,do_rmsd=.true.,do_init=.false.)

end program test_laplace
!==============================================================================!
