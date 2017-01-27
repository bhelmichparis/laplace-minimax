!==============================================================================!
subroutine lap_numlap(nlap,tolerr,rnge)
!------------------------------------------------------------------------------!
!
! get the number of quadrature points from the tabulated errors of
! initial parameters
!
! BHP, winter 2016
!
!------------------------------------------------------------------------------!

 implicit none

#include "consts.h"
#include "error.h"

! constants:
 logical, parameter :: locdbg = .false.
 character(len=*), parameter :: chrdbg = 'lap_numlap>'

! input:
 real(8), intent(in) :: rnge(2), tolerr

! output:
 integer, intent(out) :: nlap

! local:
 integer :: ilap
 real(8) :: rlen

! functions:
 integer :: lap_getnlap

 if (locdbg) write(istdout,"(a)") chrdbg//"enter lap_numlap ..."

 rlen = rnge(2)/rnge(1)

 if (rlen.le.d1) then
  stop chrdbg//"Upper bound shall be greater than lower bound!"
 end if

 if      (rlen.gt.1.D00 .and. rlen.le.2.D00) then
  nlap = lap_getnlap(tolerr,error_init_2E00,ilap_init_2E00,nlap_init_2E00)
 else if () then
 else
 end if

 if (locdbg) write(istdout,"(a)") "... left lap_numlap"

end subroutine lap_numlap
!==============================================================================!
!==============================================================================!
pure integer function lap_getnlap(tolerr,error_init,ilap_init,nlap_init)
!------------------------------------------------------------------------------!
! get number of quadrature points by scanning the arrays in error.h
!------------------------------------------------------------------------------!

! dimensions:
 integer, intent(in) :: nlap_init

! input:
 integer, intent(in) :: ilap_init(nlap_init)
 real(8), intent(in) :: error_init(nlap_init), tolerr

! local:
 integer :: nlap, ilap

 nlap = -1
 do ilap = 1,nlap_init
  if (error_init(ilap).le.tolerr) then
   nlap = ilap_init(ilap)
  end if
 end do

 lap_getnlap = nlap

end function lap_getnlap
!==============================================================================!
