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
 else if (rlen.gt.2.D00 .and. rlen.le.3.D00) then
  nlap = lap_getnlap(tolerr,error_init_3E00,ilap_init_3E00,nlap_init_3E00)
 else if (rlen.gt.3.D00 .and. rlen.le.4.D00) then
  nlap = lap_getnlap(tolerr,error_init_4E00,ilap_init_4E00,nlap_init_4E00)
 else if (rlen.gt.4.D00 .and. rlen.le.5.D00) then
  nlap = lap_getnlap(tolerr,error_init_5E00,ilap_init_5E00,nlap_init_5E00)
 else if (rlen.gt.5.D00 .and. rlen.le.6.D00) then
  nlap = lap_getnlap(tolerr,error_init_6E00,ilap_init_6E00,nlap_init_6E00)
 else if (rlen.gt.6.D00 .and. rlen.le.7.D00) then
  nlap = lap_getnlap(tolerr,error_init_7E00,ilap_init_7E00,nlap_init_7E00)
 else if (rlen.gt.7.D00 .and. rlen.le.8.D00) then
  nlap = lap_getnlap(tolerr,error_init_8E00,ilap_init_8E00,nlap_init_8E00)
 else if (rlen.gt.8.D00 .and. rlen.le.9.D00) then
  nlap = lap_getnlap(tolerr,error_init_9E00,ilap_init_9E00,nlap_init_9E00)
 else if (rlen.gt.9.D00 .and. rlen.le.1.D01) then
  nlap = lap_getnlap(tolerr,error_init_1E01,ilap_init_1E01,nlap_init_1E01)
 else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then
  nlap = lap_getnlap(tolerr,error_init_2E01,ilap_init_2E01,nlap_init_2E01)
 else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then
  nlap = lap_getnlap(tolerr,error_init_3E01,ilap_init_3E01,nlap_init_3E01)
 else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then
  nlap = lap_getnlap(tolerr,error_init_4E01,ilap_init_4E01,nlap_init_4E01)
 else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then
  nlap = lap_getnlap(tolerr,error_init_5E01,ilap_init_5E01,nlap_init_5E01)
 else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then
  nlap = lap_getnlap(tolerr,error_init_6E01,ilap_init_6E01,nlap_init_6E01)
 else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then
  nlap = lap_getnlap(tolerr,error_init_7E01,ilap_init_7E01,nlap_init_7E01)
 else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then
  nlap = lap_getnlap(tolerr,error_init_8E01,ilap_init_8E01,nlap_init_8E01)
 else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then
  nlap = lap_getnlap(tolerr,error_init_9E01,ilap_init_9E01,nlap_init_9E01)
 else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then
  nlap = lap_getnlap(tolerr,error_init_1E02,ilap_init_1E02,nlap_init_1E02)
 else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then
  nlap = lap_getnlap(tolerr,error_init_2E02,ilap_init_2E02,nlap_init_2E02)
 else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
  nlap = lap_getnlap(tolerr,error_init_3E02,ilap_init_3E02,nlap_init_3E02)
 else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
  nlap = lap_getnlap(tolerr,error_init_4E02,ilap_init_4E02,nlap_init_4E02)
 else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
  nlap = lap_getnlap(tolerr,error_init_5E02,ilap_init_5E02,nlap_init_5E02)
 else if (rlen.gt.5.D02 .and. rlen.le.6.D02) then
  nlap = lap_getnlap(tolerr,error_init_6E02,ilap_init_6E02,nlap_init_6E02)
 else if (rlen.gt.6.D02 .and. rlen.le.7.D02) then
  nlap = lap_getnlap(tolerr,error_init_7E02,ilap_init_7E02,nlap_init_7E02)
 else if (rlen.gt.7.D02 .and. rlen.le.8.D02) then
  nlap = lap_getnlap(tolerr,error_init_8E02,ilap_init_8E02,nlap_init_8E02)
 else if (rlen.gt.8.D02 .and. rlen.le.9.D02) then
  nlap = lap_getnlap(tolerr,error_init_9E02,ilap_init_9E02,nlap_init_9E02)
 else if (rlen.gt.9.D02 .and. rlen.le.1.D03) then
  nlap = lap_getnlap(tolerr,error_init_1E03,ilap_init_1E03,nlap_init_1E03)
 else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
  nlap = lap_getnlap(tolerr,error_init_2E03,ilap_init_2E03,nlap_init_2E03)
 else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then
  nlap = lap_getnlap(tolerr,error_init_3E03,ilap_init_3E03,nlap_init_3E03)
 else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then
  nlap = lap_getnlap(tolerr,error_init_4E03,ilap_init_4E03,nlap_init_4E03)
 else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then
  nlap = lap_getnlap(tolerr,error_init_5E03,ilap_init_5E03,nlap_init_5E03)
 else if (rlen.gt.5.D03 .and. rlen.le.6.D03) then
  nlap = lap_getnlap(tolerr,error_init_6E03,ilap_init_6E03,nlap_init_6E03)
 else if (rlen.gt.6.D03 .and. rlen.le.7.D03) then
  nlap = lap_getnlap(tolerr,error_init_7E03,ilap_init_7E03,nlap_init_7E03)
 else if (rlen.gt.7.D03 .and. rlen.le.8.D03) then
  nlap = lap_getnlap(tolerr,error_init_8E03,ilap_init_8E03,nlap_init_8E03)
 else if (rlen.gt.8.D03 .and. rlen.le.9.D03) then
  nlap = lap_getnlap(tolerr,error_init_9E03,ilap_init_9E03,nlap_init_9E03)
 else if (rlen.gt.9.D03 .and. rlen.le.1.D04) then
  nlap = lap_getnlap(tolerr,error_init_1E04,ilap_init_1E04,nlap_init_1E04)
 else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then
  nlap = lap_getnlap(tolerr,error_init_2E04,ilap_init_2E04,nlap_init_2E04)
 else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then
  nlap = lap_getnlap(tolerr,error_init_3E04,ilap_init_3E04,nlap_init_3E04)
 else if (rlen.gt.3.D04 .and. rlen.le.4.D04) then
  nlap = lap_getnlap(tolerr,error_init_4E04,ilap_init_4E04,nlap_init_4E04)
 else if (rlen.gt.4.D04 .and. rlen.le.5.D04) then
  nlap = lap_getnlap(tolerr,error_init_5E04,ilap_init_5E04,nlap_init_5E04)
 else if (rlen.gt.5.D04 .and. rlen.le.6.D04) then
  nlap = lap_getnlap(tolerr,error_init_6E04,ilap_init_6E04,nlap_init_6E04)
 else if (rlen.gt.6.D04 .and. rlen.le.7.D04) then
  nlap = lap_getnlap(tolerr,error_init_7E04,ilap_init_7E04,nlap_init_7E04)
 else if (rlen.gt.7.D04 .and. rlen.le.8.D04) then
  nlap = lap_getnlap(tolerr,error_init_8E04,ilap_init_8E04,nlap_init_8E04)
 else if (rlen.gt.8.D04 .and. rlen.le.9.D04) then
  nlap = lap_getnlap(tolerr,error_init_9E04,ilap_init_9E04,nlap_init_9E04)
 else if (rlen.gt.9.D04 .and. rlen.le.1.D05) then
  nlap = lap_getnlap(tolerr,error_init_1E05,ilap_init_1E05,nlap_init_1E05)
 else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then
  nlap = lap_getnlap(tolerr,error_init_2E05,ilap_init_2E05,nlap_init_2E05)
 else if (rlen.gt.2.D05 .and. rlen.le.3.D05) then
  nlap = lap_getnlap(tolerr,error_init_3E05,ilap_init_3E05,nlap_init_3E05)
 else if (rlen.gt.3.D05 .and. rlen.le.4.D05) then
  nlap = lap_getnlap(tolerr,error_init_4E05,ilap_init_4E05,nlap_init_4E05)
 else if (rlen.gt.4.D05 .and. rlen.le.5.D05) then
  nlap = lap_getnlap(tolerr,error_init_5E05,ilap_init_5E05,nlap_init_5E05)
 else if (rlen.gt.5.D05 .and. rlen.le.6.D05) then
  nlap = lap_getnlap(tolerr,error_init_6E05,ilap_init_6E05,nlap_init_6E05)
 else if (rlen.gt.6.D05 .and. rlen.le.7.D05) then
  nlap = lap_getnlap(tolerr,error_init_7E05,ilap_init_7E05,nlap_init_7E05)
 else if (rlen.gt.7.D05 .and. rlen.le.8.D05) then
  nlap = lap_getnlap(tolerr,error_init_8E05,ilap_init_8E05,nlap_init_8E05)
 else if (rlen.gt.8.D05 .and. rlen.le.9.D05) then
  nlap = lap_getnlap(tolerr,error_init_9E05,ilap_init_9E05,nlap_init_9E05)
 else if (rlen.gt.9.D05 .and. rlen.le.1.D06) then
  nlap = lap_getnlap(tolerr,error_init_1E06,ilap_init_1E06,nlap_init_1E06)
 else if (rlen.gt.1.D06 .and. rlen.le.2.D06) then
  nlap = lap_getnlap(tolerr,error_init_2E06,ilap_init_2E06,nlap_init_2E06)
 else if (rlen.gt.2.D06 .and. rlen.le.3.D06) then
  nlap = lap_getnlap(tolerr,error_init_3E06,ilap_init_3E06,nlap_init_3E06)
 else if (rlen.gt.3.D06 .and. rlen.le.4.D06) then
  nlap = lap_getnlap(tolerr,error_init_4E06,ilap_init_4E06,nlap_init_4E06)
 else if (rlen.gt.4.D06 .and. rlen.le.5.D06) then
  nlap = lap_getnlap(tolerr,error_init_5E06,ilap_init_5E06,nlap_init_5E06)
 else if (rlen.gt.5.D06 .and. rlen.le.6.D06) then
  nlap = lap_getnlap(tolerr,error_init_6E06,ilap_init_6E06,nlap_init_6E06)
 else if (rlen.gt.6.D06 .and. rlen.le.7.D06) then
  nlap = lap_getnlap(tolerr,error_init_7E06,ilap_init_7E06,nlap_init_7E06)
 else if (rlen.gt.7.D06 .and. rlen.le.8.D06) then
  nlap = lap_getnlap(tolerr,error_init_8E06,ilap_init_8E06,nlap_init_8E06)
 else if (rlen.gt.8.D06 .and. rlen.le.9.D06) then
  nlap = lap_getnlap(tolerr,error_init_9E06,ilap_init_9E06,nlap_init_9E06)
 else if (rlen.gt.9.D06 .and. rlen.le.1.D07) then
  nlap = lap_getnlap(tolerr,error_init_1E07,ilap_init_1E07,nlap_init_1E07)
 else if (rlen.gt.1.D07 .and. rlen.le.2.D07) then
  nlap = lap_getnlap(tolerr,error_init_2E07,ilap_init_2E07,nlap_init_2E07)
 else if (rlen.gt.2.D07 .and. rlen.le.3.D07) then
  nlap = lap_getnlap(tolerr,error_init_3E07,ilap_init_3E07,nlap_init_3E07)
 else if (rlen.gt.3.D07 .and. rlen.le.4.D07) then
  nlap = lap_getnlap(tolerr,error_init_4E07,ilap_init_4E07,nlap_init_4E07)
 else if (rlen.gt.4.D07 .and. rlen.le.5.D07) then
  nlap = lap_getnlap(tolerr,error_init_5E07,ilap_init_5E07,nlap_init_5E07)
 else if (rlen.gt.5.D07 .and. rlen.le.7.D07) then
  nlap = lap_getnlap(tolerr,error_init_7E07,ilap_init_7E07,nlap_init_7E07)
 else if (rlen.gt.7.D07 .and. rlen.le.1.D08) then
  nlap = lap_getnlap(tolerr,error_init_1E08,ilap_init_1E08,nlap_init_1E08)
 else if (rlen.gt.1.D08 .and. rlen.le.2.D08) then
  nlap = lap_getnlap(tolerr,error_init_2E08,ilap_init_2E08,nlap_init_2E08)
 else if (rlen.gt.2.D08 .and. rlen.le.3.D08) then
  nlap = lap_getnlap(tolerr,error_init_3E08,ilap_init_3E08,nlap_init_3E08)
 else if (rlen.gt.3.D08 .and. rlen.le.4.D08) then
  nlap = lap_getnlap(tolerr,error_init_4E08,ilap_init_4E08,nlap_init_4E08)
 else if (rlen.gt.4.D08 .and. rlen.le.5.D08) then
  nlap = lap_getnlap(tolerr,error_init_5E08,ilap_init_5E08,nlap_init_5E08)
 else if (rlen.gt.5.D08 .and. rlen.le.7.D08) then
  nlap = lap_getnlap(tolerr,error_init_7E08,ilap_init_7E08,nlap_init_7E08)
 else if (rlen.gt.7.D08 .and. rlen.le.1.D09) then
  nlap = lap_getnlap(tolerr,error_init_1E09,ilap_init_1E09,nlap_init_1E09)
 else if (rlen.gt.1.D09 .and. rlen.le.2.D09) then
  nlap = lap_getnlap(tolerr,error_init_2E09,ilap_init_2E09,nlap_init_2E09)
 else if (rlen.gt.2.D09 .and. rlen.le.3.D09) then
  nlap = lap_getnlap(tolerr,error_init_3E09,ilap_init_3E09,nlap_init_3E09)
 else if (rlen.gt.3.D09 .and. rlen.le.4.D09) then
  nlap = lap_getnlap(tolerr,error_init_4E09,ilap_init_4E09,nlap_init_4E09)
 else if (rlen.gt.4.D09 .and. rlen.le.5.D09) then
  nlap = lap_getnlap(tolerr,error_init_5E09,ilap_init_5E09,nlap_init_5E09)
 else if (rlen.gt.5.D09 .and. rlen.le.6.D09) then
  nlap = lap_getnlap(tolerr,error_init_6E09,ilap_init_6E09,nlap_init_6E09)
 else if (rlen.gt.6.D09 .and. rlen.le.7.D09) then
  nlap = lap_getnlap(tolerr,error_init_7E09,ilap_init_7E09,nlap_init_7E09)
 else if (rlen.gt.7.D09 .and. rlen.le.1.D10) then
  nlap = lap_getnlap(tolerr,error_init_1E10,ilap_init_1E10,nlap_init_1E10)
 else if (rlen.gt.1.D10 .and. rlen.le.2.D10) then
  nlap = lap_getnlap(tolerr,error_init_2E10,ilap_init_2E10,nlap_init_2E10)
 else if (rlen.gt.2.D10 .and. rlen.le.3.D10) then
  nlap = lap_getnlap(tolerr,error_init_3E10,ilap_init_3E10,nlap_init_3E10)
 else if (rlen.gt.3.D10 .and. rlen.le.4.D10) then
  nlap = lap_getnlap(tolerr,error_init_4E10,ilap_init_4E10,nlap_init_3E10)
 else if (rlen.gt.4.D10 .and. rlen.le.5.D10) then
  nlap = lap_getnlap(tolerr,error_init_5E10,ilap_init_5E10,nlap_init_5E10)
 else if (rlen.gt.5.D10 .and. rlen.le.7.D10) then
  nlap = lap_getnlap(tolerr,error_init_7E10,ilap_init_7E10,nlap_init_7E10)
 else if (rlen.gt.7.D10 .and. rlen.le.1.D11) then
  nlap = lap_getnlap(tolerr,error_init_1E11,ilap_init_1E11,nlap_init_1E11)
 else if (rlen.gt.1.D11 .and. rlen.le.2.D11) then
  nlap = lap_getnlap(tolerr,error_init_2E11,ilap_init_2E11,nlap_init_2E11)
 else if (rlen.gt.2.D11 .and. rlen.le.3.D11) then
  nlap = lap_getnlap(tolerr,error_init_3E11,ilap_init_3E11,nlap_init_3E11)
 else if (rlen.gt.3.D11 .and. rlen.le.4.D11) then
  nlap = lap_getnlap(tolerr,error_init_4E11,ilap_init_4E11,nlap_init_4E11)
 else if (rlen.gt.4.D11 .and. rlen.le.5.D11) then
  nlap = lap_getnlap(tolerr,error_init_5E11,ilap_init_5E11,nlap_init_5E11)
 else if (rlen.gt.5.D11 .and. rlen.le.7.D11) then
  nlap = lap_getnlap(tolerr,error_init_7E11,ilap_init_7E11,nlap_init_7E11)
 else if (rlen.gt.7.D11 .and. rlen.le.1.D12) then
  nlap = lap_getnlap(tolerr,error_init_1E12,ilap_init_1E12,nlap_init_1E12)
 else if (rlen.gt.1.D12 .and. rlen.le.2.D12) then
  nlap = lap_getnlap(tolerr,error_init_2E12,ilap_init_2E12,nlap_init_2E12)
 else if (rlen.gt.2.D12 .and. rlen.le.3.D12) then
  nlap = lap_getnlap(tolerr,error_init_3E12,ilap_init_3E12,nlap_init_3E12)
 else
  nlap = lap_getnlap(tolerr,error_init_inft,ilap_init_inft,nlap_init_inft)
 end if

 ! catch errors
 if (nlap.eq.-1) then
  write(istdout,"(/a/)") "Could not find a suitable number of quadrature points for"  
  write(istdout,"(a,f10.4)") "        tolerr = ",tolerr
  write(istdout,"(/a/)") "Either increase tolerr or tabulate new start values yourself!"  
  stop "Bye bye ..."
 end if

 if (locdbg) write(istdout,"(a)") "... left lap_numlap"

end subroutine lap_numlap
!==============================================================================!
!==============================================================================!
pure integer function lap_getnlap(tolerr,error_init,ilap_init,nlap_init)
!------------------------------------------------------------------------------!
! get number of quadrature points by scanning the arrays in error.h
!------------------------------------------------------------------------------!

 implicit none

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
