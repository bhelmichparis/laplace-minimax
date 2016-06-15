!==================================================================================================================================!
subroutine lap_init(xpnts,wghts,rnge,nlap)
!----------------------------------------------------------------------------------------------------------------------------------!
!
! get initial Laplace data from pre-tabulated
!  Laplace exponents and weights of Takatsuka, Ten-no, and Hackbusch.
! Those initial values depend on the interval [1,R].
!
!----------------------------------------------------------------------------------------------------------------------------------!


 implicit none

#include "consts.h"
#include "init.h"

! constants:
 logical, parameter :: locdbg = .false.
 character(len=*), parameter :: chrdbg = 'lap_init>'

! dimensions:
 integer, intent(in) :: nlap

! input:
 real(8), intent(in) :: rnge(2)

! in/output:
 real(8), intent(inout)  :: xpnts(2,nlap), wghts(2,nlap)

! local:
 integer :: ilap
 real(8) :: rlen

 if (locdbg) write(istdout,"(a)") chrdbg//"enter lap_init ..."

 rlen = rnge(2)/rnge(1)

 if (rlen .le. d1) then
  stop chrdbg//"Upper bound shall be greater than lower bound!"
 end if

!-----------------------------------------------------------------------------!
! find start address for initilization
!-----------------------------------------------------------------------------!

 select case (nlap)

!-----------------------------------------------------------------------------!
 case (1)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.2.D00) then
   call lap_assign_para(wghts,wghts_init_n01_2E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n01_2E00,nlap)
  else if (rlen.gt.2.D00 .and. rlen.le.3.D00) then
   call lap_assign_para(wghts,wghts_init_n01_2E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n01_2E00,nlap)
  else if (rlen.gt.3.D00 .and. rlen.le.4.D00) then
   call lap_assign_para(wghts,wghts_init_n01_3E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n01_3E00,nlap)
  else if (rlen.gt.4.D00 .and. rlen.le.5.D00) then
   call lap_assign_para(wghts,wghts_init_n01_4E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n01_4E00,nlap)
  else if (rlen.gt.5.D00 .and. rlen.le.6.D00) then
   call lap_assign_para(wghts,wghts_init_n01_5E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n01_5E00,nlap)
  else if (rlen.gt.6.D00 .and. rlen.le.7.D00) then
   call lap_assign_para(wghts,wghts_init_n01_6E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n01_6E00,nlap)
  else if (rlen.gt.7.D00 .and. rlen.le.8.D00) then
   call lap_assign_para(wghts,wghts_init_n01_7E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n01_7E00,nlap)
  else if (rlen.gt.8.D00 .and. rlen.le.9.D00) then
   call lap_assign_para(wghts,wghts_init_n01_8E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n01_8E00,nlap)
  else if (rlen.gt.9.D00 .and. rlen.le.1.D01) then
   call lap_assign_para(wghts,wghts_init_n01_9E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n01_9E00,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n01_1E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n01_1E01,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (2)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.2.D00) then
   call lap_assign_para(wghts,wghts_init_n02_2E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_2E00,nlap)
  else if (rlen.gt.2.D00 .and. rlen.le.3.D00) then
   call lap_assign_para(wghts,wghts_init_n02_2E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_2E00,nlap)
  else if (rlen.gt.3.D00 .and. rlen.le.4.D00) then
   call lap_assign_para(wghts,wghts_init_n02_3E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_3E00,nlap)
  else if (rlen.gt.4.D00 .and. rlen.le.5.D00) then
   call lap_assign_para(wghts,wghts_init_n02_4E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_4E00,nlap)
  else if (rlen.gt.5.D00 .and. rlen.le.6.D00) then
   call lap_assign_para(wghts,wghts_init_n02_5E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_5E00,nlap)
  else if (rlen.gt.6.D00 .and. rlen.le.7.D00) then
   call lap_assign_para(wghts,wghts_init_n02_6E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_6E00,nlap)
  else if (rlen.gt.7.D00 .and. rlen.le.8.D00) then
   call lap_assign_para(wghts,wghts_init_n02_7E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_7E00,nlap)
  else if (rlen.gt.8.D00 .and. rlen.le.9.D00) then
   call lap_assign_para(wghts,wghts_init_n02_8E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_8E00,nlap)
  else if (rlen.gt.9.D00 .and. rlen.le.1.D01) then
   call lap_assign_para(wghts,wghts_init_n02_9E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_9E00,nlap)
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then
   call lap_assign_para(wghts,wghts_init_n02_1E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1E01,nlap)
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then
   call lap_assign_para(wghts,wghts_init_n02_2E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_2E01,nlap)
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then
   call lap_assign_para(wghts,wghts_init_n02_3E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_3E01,nlap)
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then
   call lap_assign_para(wghts,wghts_init_n02_4E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_4E01,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n02_5E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_5E01,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (3)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.2.D00) then
   call lap_assign_para(wghts,wghts_init_n03_2E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_2E00,nlap)
  else if (rlen.gt.2.D00 .and. rlen.le.3.D00) then
   call lap_assign_para(wghts,wghts_init_n03_2E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_2E00,nlap)
  else if (rlen.gt.3.D00 .and. rlen.le.4.D00) then
   call lap_assign_para(wghts,wghts_init_n03_3E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_3E00,nlap)
  else if (rlen.gt.4.D00 .and. rlen.le.5.D00) then
   call lap_assign_para(wghts,wghts_init_n03_4E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_4E00,nlap)
  else if (rlen.gt.5.D00 .and. rlen.le.6.D00) then
   call lap_assign_para(wghts,wghts_init_n03_5E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_5E00,nlap)
  else if (rlen.gt.6.D00 .and. rlen.le.7.D00) then
   call lap_assign_para(wghts,wghts_init_n03_6E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_6E00,nlap)
  else if (rlen.gt.7.D00 .and. rlen.le.8.D00) then
   call lap_assign_para(wghts,wghts_init_n03_7E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_7E00,nlap)
  else if (rlen.gt.8.D00 .and. rlen.le.9.D00) then
   call lap_assign_para(wghts,wghts_init_n03_8E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_8E00,nlap)
  else if (rlen.gt.9.D00 .and. rlen.le.1.D01) then
   call lap_assign_para(wghts,wghts_init_n03_9E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_9E00,nlap)
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then
   call lap_assign_para(wghts,wghts_init_n03_1E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1E01,nlap)
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then
   call lap_assign_para(wghts,wghts_init_n03_2E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_2E01,nlap)
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then
   call lap_assign_para(wghts,wghts_init_n03_3E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_3E01,nlap)
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then
   call lap_assign_para(wghts,wghts_init_n03_4E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_4E01,nlap)
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then
   call lap_assign_para(wghts,wghts_init_n03_5E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_5E01,nlap)
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then
   call lap_assign_para(wghts,wghts_init_n03_6E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_6E01,nlap)
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then
   call lap_assign_para(wghts,wghts_init_n03_7E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_7E01,nlap)
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then
   call lap_assign_para(wghts,wghts_init_n03_8E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_8E01,nlap)
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then
   call lap_assign_para(wghts,wghts_init_n03_9E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_9E01,nlap)
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then
   call lap_assign_para(wghts,wghts_init_n03_1E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1E02,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n03_2E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_2E02,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (4)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.2.D00) then
   call lap_assign_para(wghts,wghts_init_n04_2E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_2E00,nlap)
  else if (rlen.gt.2.D00 .and. rlen.le.3.D00) then
   call lap_assign_para(wghts,wghts_init_n04_2E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_2E00,nlap)
  else if (rlen.gt.3.D00 .and. rlen.le.4.D00) then
   call lap_assign_para(wghts,wghts_init_n04_3E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_3E00,nlap)
  else if (rlen.gt.4.D00 .and. rlen.le.5.D00) then
   call lap_assign_para(wghts,wghts_init_n04_4E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_4E00,nlap)
  else if (rlen.gt.5.D00 .and. rlen.le.6.D00) then
   call lap_assign_para(wghts,wghts_init_n04_5E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_5E00,nlap)
  else if (rlen.gt.6.D00 .and. rlen.le.7.D00) then
   call lap_assign_para(wghts,wghts_init_n04_6E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_6E00,nlap)
  else if (rlen.gt.7.D00 .and. rlen.le.8.D00) then
   call lap_assign_para(wghts,wghts_init_n04_7E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_7E00,nlap)
  else if (rlen.gt.8.D00 .and. rlen.le.9.D00) then
   call lap_assign_para(wghts,wghts_init_n04_8E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_8E00,nlap)
  else if (rlen.gt.9.D00 .and. rlen.le.1.D01) then
   call lap_assign_para(wghts,wghts_init_n04_9E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_9E00,nlap)
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then
   call lap_assign_para(wghts,wghts_init_n04_1E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1E01,nlap)
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then
   call lap_assign_para(wghts,wghts_init_n04_2E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_2E01,nlap)
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then
   call lap_assign_para(wghts,wghts_init_n04_3E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_3E01,nlap)
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then
   call lap_assign_para(wghts,wghts_init_n04_4E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_4E01,nlap)
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then
   call lap_assign_para(wghts,wghts_init_n04_5E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_5E01,nlap)
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then
   call lap_assign_para(wghts,wghts_init_n04_6E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_6E01,nlap)
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then
   call lap_assign_para(wghts,wghts_init_n04_7E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_7E01,nlap)
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then
   call lap_assign_para(wghts,wghts_init_n04_8E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_8E01,nlap)
  else if (rlen.gt.9.D01 .and. rlen.le.1.D01) then
   call lap_assign_para(wghts,wghts_init_n04_9E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_9E01,nlap)
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then
   call lap_assign_para(wghts,wghts_init_n04_1E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1E02,nlap)
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
   call lap_assign_para(wghts,wghts_init_n04_2E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_2E02,nlap)
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
   call lap_assign_para(wghts,wghts_init_n04_3E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_3E02,nlap)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   call lap_assign_para(wghts,wghts_init_n04_4E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_4E02,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n04_5E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_5E02,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (5)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.2.D00) then
   call lap_assign_para(wghts,wghts_init_n05_2E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_2E00,nlap)
  else if (rlen.gt.2.D00 .and. rlen.le.3.D00) then
   call lap_assign_para(wghts,wghts_init_n05_2E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_2E00,nlap)
  else if (rlen.gt.3.D00 .and. rlen.le.4.D00) then
   call lap_assign_para(wghts,wghts_init_n05_3E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_3E00,nlap)
  else if (rlen.gt.4.D00 .and. rlen.le.5.D00) then
   call lap_assign_para(wghts,wghts_init_n05_4E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_4E00,nlap)
  else if (rlen.gt.5.D00 .and. rlen.le.6.D00) then
   call lap_assign_para(wghts,wghts_init_n05_5E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_5E00,nlap)
  else if (rlen.gt.6.D00 .and. rlen.le.7.D00) then
   call lap_assign_para(wghts,wghts_init_n05_6E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_6E00,nlap)
  else if (rlen.gt.7.D00 .and. rlen.le.8.D00) then
   call lap_assign_para(wghts,wghts_init_n05_7E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_7E00,nlap)
  else if (rlen.gt.8.D00 .and. rlen.le.9.D00) then
   call lap_assign_para(wghts,wghts_init_n05_8E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_8E00,nlap)
  else if (rlen.gt.9.D00 .and. rlen.le.1.D01) then
   call lap_assign_para(wghts,wghts_init_n05_9E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_9E00,nlap)
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then
   call lap_assign_para(wghts,wghts_init_n05_1E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1E01,nlap)
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then
   call lap_assign_para(wghts,wghts_init_n05_2E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_2E01,nlap)
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then
   call lap_assign_para(wghts,wghts_init_n05_3E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_3E01,nlap)
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then
   call lap_assign_para(wghts,wghts_init_n05_4E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_4E01,nlap)
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then
   call lap_assign_para(wghts,wghts_init_n05_5E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_5E01,nlap)
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then
   call lap_assign_para(wghts,wghts_init_n05_6E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_6E01,nlap)
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then
   call lap_assign_para(wghts,wghts_init_n05_7E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_7E01,nlap)
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then
   call lap_assign_para(wghts,wghts_init_n05_8E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_8E01,nlap)
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then
   call lap_assign_para(wghts,wghts_init_n05_9E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_9E01,nlap)
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then
   call lap_assign_para(wghts,wghts_init_n05_1E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1E02,nlap)
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
   call lap_assign_para(wghts,wghts_init_n05_2E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_2E02,nlap)
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
   call lap_assign_para(wghts,wghts_init_n05_3E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_3E02,nlap)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   call lap_assign_para(wghts,wghts_init_n05_4E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_4E02,nlap)
  else if (rlen.gt.5.D02 .and. rlen.le.6.D02) then
   call lap_assign_para(wghts,wghts_init_n05_5E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_5E02,nlap)
  else if (rlen.gt.6.D02 .and. rlen.le.7.D02) then
   call lap_assign_para(wghts,wghts_init_n05_6E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_6E02,nlap)
  else if (rlen.gt.7.D02 .and. rlen.le.8.D02) then
   call lap_assign_para(wghts,wghts_init_n05_7E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_7E02,nlap)
  else if (rlen.gt.8.D02 .and. rlen.le.9.D02) then
   call lap_assign_para(wghts,wghts_init_n05_8E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_8E02,nlap)
  else if (rlen.gt.9.D02 .and. rlen.le.1.D03) then
   call lap_assign_para(wghts,wghts_init_n05_9E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_9E02,nlap)
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
   call lap_assign_para(wghts,wghts_init_n05_1E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1E03,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n05_2E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_2E03,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (6)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.2.D00) then
   call lap_assign_para(wghts,wghts_init_n06_2E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_2E00,nlap)
  else if (rlen.gt.2.D00 .and. rlen.le.3.D00) then
   call lap_assign_para(wghts,wghts_init_n06_2E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_2E00,nlap)
  else if (rlen.gt.3.D00 .and. rlen.le.4.D00) then
   call lap_assign_para(wghts,wghts_init_n06_3E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_3E00,nlap)
  else if (rlen.gt.4.D00 .and. rlen.le.5.D00) then
   call lap_assign_para(wghts,wghts_init_n06_4E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_4E00,nlap)
  else if (rlen.gt.5.D00 .and. rlen.le.6.D00) then
   call lap_assign_para(wghts,wghts_init_n06_5E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_5E00,nlap)
  else if (rlen.gt.6.D00 .and. rlen.le.7.D00) then
   call lap_assign_para(wghts,wghts_init_n06_6E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_6E00,nlap)
  else if (rlen.gt.7.D00 .and. rlen.le.8.D00) then
   call lap_assign_para(wghts,wghts_init_n06_7E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_7E00,nlap)
  else if (rlen.gt.8.D00 .and. rlen.le.9.D00) then
   call lap_assign_para(wghts,wghts_init_n06_8E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_8E00,nlap)
  else if (rlen.gt.9.D00 .and. rlen.le.1.D01) then
   call lap_assign_para(wghts,wghts_init_n06_9E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_9E00,nlap)
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then
   call lap_assign_para(wghts,wghts_init_n06_1E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1E01,nlap)
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then
   call lap_assign_para(wghts,wghts_init_n06_2E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_2E01,nlap)
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then
   call lap_assign_para(wghts,wghts_init_n06_3E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_3E01,nlap)
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then
   call lap_assign_para(wghts,wghts_init_n06_4E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_4E01,nlap)
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then
   call lap_assign_para(wghts,wghts_init_n06_5E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_5E01,nlap)
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then
   call lap_assign_para(wghts,wghts_init_n06_6E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_6E01,nlap)
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then
   call lap_assign_para(wghts,wghts_init_n06_7E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_7E01,nlap)
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then
   call lap_assign_para(wghts,wghts_init_n06_8E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_8E01,nlap)
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then
   call lap_assign_para(wghts,wghts_init_n06_9E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_9E01,nlap)
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then
   call lap_assign_para(wghts,wghts_init_n06_1E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1E02,nlap)
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
   call lap_assign_para(wghts,wghts_init_n06_2E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_2E02,nlap)
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
   call lap_assign_para(wghts,wghts_init_n06_3E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_3E02,nlap)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   call lap_assign_para(wghts,wghts_init_n06_4E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_4E02,nlap)
  else if (rlen.gt.5.D02 .and. rlen.le.6.D02) then
   call lap_assign_para(wghts,wghts_init_n06_5E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_5E02,nlap)
  else if (rlen.gt.6.D02 .and. rlen.le.7.D02) then
   call lap_assign_para(wghts,wghts_init_n06_6E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_6E02,nlap)
  else if (rlen.gt.7.D02 .and. rlen.le.8.D02) then
   call lap_assign_para(wghts,wghts_init_n06_7E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_7E02,nlap)
  else if (rlen.gt.8.D02 .and. rlen.le.9.D02) then
   call lap_assign_para(wghts,wghts_init_n06_8E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_8E02,nlap)
  else if (rlen.gt.9.D02 .and. rlen.le.1.D03) then
   call lap_assign_para(wghts,wghts_init_n06_9E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_9E02,nlap)
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
   call lap_assign_para(wghts,wghts_init_n06_1E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1E03,nlap)
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then
   call lap_assign_para(wghts,wghts_init_n06_2E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_2E03,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n06_3E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_3E03,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (7)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.2.D00) then
   call lap_assign_para(wghts,wghts_init_n07_2E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n07_2E00,nlap)
  else if (rlen.gt.2.D00 .and. rlen.le.3.D00) then
   call lap_assign_para(wghts,wghts_init_n07_2E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n07_2E00,nlap)
  else if (rlen.gt.3.D00 .and. rlen.le.4.D00) then
   call lap_assign_para(wghts,wghts_init_n07_3E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n07_3E00,nlap)
  else if (rlen.gt.4.D00 .and. rlen.le.5.D00) then
   call lap_assign_para(wghts,wghts_init_n07_4E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n07_4E00,nlap)
  else if (rlen.gt.5.D00 .and. rlen.le.6.D00) then
   call lap_assign_para(wghts,wghts_init_n07_5E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n07_5E00,nlap)
  else if (rlen.gt.6.D00 .and. rlen.le.7.D00) then
   call lap_assign_para(wghts,wghts_init_n07_6E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n07_6E00,nlap)
  else if (rlen.gt.7.D00 .and. rlen.le.8.D00) then
   call lap_assign_para(wghts,wghts_init_n07_7E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n07_7E00,nlap)
  else if (rlen.gt.8.D00 .and. rlen.le.9.D00) then
   call lap_assign_para(wghts,wghts_init_n07_8E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n07_8E00,nlap)
  else if (rlen.gt.9.D00 .and. rlen.le.1.D01) then
   call lap_assign_para(wghts,wghts_init_n07_9E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n07_9E00,nlap)
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then
   call lap_assign_para(wghts,wghts_init_n07_1E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n07_1E01,nlap)
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then
   call lap_assign_para(wghts,wghts_init_n07_2E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n07_2E01,nlap)
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then
   call lap_assign_para(wghts,wghts_init_n07_3E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n07_3E01,nlap)
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then
   call lap_assign_para(wghts,wghts_init_n07_4E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n07_4E01,nlap)
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then
   call lap_assign_para(wghts,wghts_init_n07_5E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n07_5E01,nlap)
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then
   call lap_assign_para(wghts,wghts_init_n07_6E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n07_6E01,nlap)
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then
   call lap_assign_para(wghts,wghts_init_n07_7E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n07_7E01,nlap)
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then
   call lap_assign_para(wghts,wghts_init_n07_8E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n07_8E01,nlap)
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then
   call lap_assign_para(wghts,wghts_init_n07_9E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n07_9E01,nlap)
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then
   call lap_assign_para(wghts,wghts_init_n07_1E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n07_1E02,nlap)
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
   call lap_assign_para(wghts,wghts_init_n07_2E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n07_2E02,nlap)
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
   call lap_assign_para(wghts,wghts_init_n07_3E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n07_3E02,nlap)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   call lap_assign_para(wghts,wghts_init_n07_4E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n07_4E02,nlap)
  else if (rlen.gt.5.D02 .and. rlen.le.6.D02) then
   call lap_assign_para(wghts,wghts_init_n07_5E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n07_5E02,nlap)
  else if (rlen.gt.6.D02 .and. rlen.le.7.D02) then
   call lap_assign_para(wghts,wghts_init_n07_6E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n07_6E02,nlap)
  else if (rlen.gt.7.D02 .and. rlen.le.8.D02) then
   call lap_assign_para(wghts,wghts_init_n07_7E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n07_7E02,nlap)
  else if (rlen.gt.8.D02 .and. rlen.le.9.D02) then
   call lap_assign_para(wghts,wghts_init_n07_8E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n07_8E02,nlap)
  else if (rlen.gt.9.D02 .and. rlen.le.1.D03) then
   call lap_assign_para(wghts,wghts_init_n07_9E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n07_9E02,nlap)
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
   call lap_assign_para(wghts,wghts_init_n07_1E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n07_1E03,nlap)
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then
   call lap_assign_para(wghts,wghts_init_n07_2E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n07_2E03,nlap)
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then
   call lap_assign_para(wghts,wghts_init_n07_3E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n07_3E03,nlap)
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then
   call lap_assign_para(wghts,wghts_init_n07_4E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n07_4E03,nlap)
  else if (rlen.gt.5.D03 .and. rlen.le.6.D03) then
   call lap_assign_para(wghts,wghts_init_n07_5E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n07_5E03,nlap)
  else if (rlen.gt.6.D03 .and. rlen.le.7.D03) then
   call lap_assign_para(wghts,wghts_init_n07_6E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n07_6E03,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n07_7E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n07_7E03,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (8)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.1.D01) then
   call lap_assign_para(wghts,wghts_init_n08_1E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n08_1E01,nlap)
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then
   call lap_assign_para(wghts,wghts_init_n08_1E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n08_1E01,nlap)
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then
   call lap_assign_para(wghts,wghts_init_n08_2E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n08_2E01,nlap)
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then
   call lap_assign_para(wghts,wghts_init_n08_3E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n08_3E01,nlap)
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then
   call lap_assign_para(wghts,wghts_init_n08_4E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n08_4E01,nlap)
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then
   call lap_assign_para(wghts,wghts_init_n08_5E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n08_5E01,nlap)
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then
   call lap_assign_para(wghts,wghts_init_n08_6E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n08_6E01,nlap)
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then
   call lap_assign_para(wghts,wghts_init_n08_7E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n08_7E01,nlap)
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then
   call lap_assign_para(wghts,wghts_init_n08_8E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n08_8E01,nlap)
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then
   call lap_assign_para(wghts,wghts_init_n08_9E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n08_9E01,nlap)
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then
   call lap_assign_para(wghts,wghts_init_n08_1E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n08_1E02,nlap)
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
   call lap_assign_para(wghts,wghts_init_n08_2E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n08_2E02,nlap)
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
   call lap_assign_para(wghts,wghts_init_n08_3E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n08_3E02,nlap)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   call lap_assign_para(wghts,wghts_init_n08_4E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n08_4E02,nlap)
  else if (rlen.gt.5.D02 .and. rlen.le.7.D02) then
   call lap_assign_para(wghts,wghts_init_n08_5E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n08_5E02,nlap)
  else if (rlen.gt.7.D02 .and. rlen.le.1.D03) then
   call lap_assign_para(wghts,wghts_init_n08_7E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n08_7E02,nlap)
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
   call lap_assign_para(wghts,wghts_init_n08_1E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n08_1E03,nlap)
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then
   call lap_assign_para(wghts,wghts_init_n08_2E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n08_2E03,nlap)
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then
   call lap_assign_para(wghts,wghts_init_n08_3E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n08_3E03,nlap)
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then
   call lap_assign_para(wghts,wghts_init_n08_4E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n08_4E03,nlap)
  else if (rlen.gt.5.D03 .and. rlen.le.7.D03) then
   call lap_assign_para(wghts,wghts_init_n08_5E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n08_5E03,nlap)
  else if (rlen.gt.7.D03 .and. rlen.le.1.D04) then
   call lap_assign_para(wghts,wghts_init_n08_7E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n08_7E03,nlap)
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then
   call lap_assign_para(wghts,wghts_init_n08_1E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n08_1E04,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n08_2E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n08_2E04,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (9)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.1.D01) then
   call lap_assign_para(wghts,wghts_init_n09_1E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n09_1E01,nlap)
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then
   call lap_assign_para(wghts,wghts_init_n09_1E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n09_1E01,nlap)
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then
   call lap_assign_para(wghts,wghts_init_n09_2E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n09_2E01,nlap)
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then
   call lap_assign_para(wghts,wghts_init_n09_3E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n09_3E01,nlap)
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then
   call lap_assign_para(wghts,wghts_init_n09_4E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n09_4E01,nlap)
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then
   call lap_assign_para(wghts,wghts_init_n09_5E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n09_5E01,nlap)
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then
   call lap_assign_para(wghts,wghts_init_n09_6E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n09_6E01,nlap)
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then
   call lap_assign_para(wghts,wghts_init_n09_7E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n09_7E01,nlap)
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then
   call lap_assign_para(wghts,wghts_init_n09_8E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n09_8E01,nlap)
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then
   call lap_assign_para(wghts,wghts_init_n09_9E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n09_9E01,nlap)
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then
   call lap_assign_para(wghts,wghts_init_n09_1E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n09_1E02,nlap)
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
   call lap_assign_para(wghts,wghts_init_n09_2E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n09_2E02,nlap)
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
   call lap_assign_para(wghts,wghts_init_n09_3E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n09_3E02,nlap)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   call lap_assign_para(wghts,wghts_init_n09_4E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n09_4E02,nlap)
  else if (rlen.gt.5.D02 .and. rlen.le.7.D02) then
   call lap_assign_para(wghts,wghts_init_n09_5E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n09_5E02,nlap)
  else if (rlen.gt.7.D02 .and. rlen.le.1.D03) then
   call lap_assign_para(wghts,wghts_init_n09_7E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n09_7E02,nlap)
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
   call lap_assign_para(wghts,wghts_init_n09_1E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n09_1E03,nlap)
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then
   call lap_assign_para(wghts,wghts_init_n09_2E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n09_2E03,nlap)
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then
   call lap_assign_para(wghts,wghts_init_n09_3E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n09_3E03,nlap)
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then
   call lap_assign_para(wghts,wghts_init_n09_4E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n09_4E03,nlap)
  else if (rlen.gt.5.D03 .and. rlen.le.7.D03) then
   call lap_assign_para(wghts,wghts_init_n09_5E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n09_5E03,nlap)
  else if (rlen.gt.7.D03 .and. rlen.le.1.D04) then
   call lap_assign_para(wghts,wghts_init_n09_7E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n09_7E03,nlap)
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then
   call lap_assign_para(wghts,wghts_init_n09_1E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n09_1E04,nlap)
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then
   call lap_assign_para(wghts,wghts_init_n09_2E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n09_2E04,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n09_3E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n09_3E04,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (10)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.1.D01) then
   call lap_assign_para(wghts,wghts_init_n10_1E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n10_1E01,nlap)
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then
   call lap_assign_para(wghts,wghts_init_n10_1E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n10_1E01,nlap)
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then
   call lap_assign_para(wghts,wghts_init_n10_2E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n10_2E01,nlap)
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then
   call lap_assign_para(wghts,wghts_init_n10_3E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n10_3E01,nlap)
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then
   call lap_assign_para(wghts,wghts_init_n10_4E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n10_4E01,nlap)
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then
   call lap_assign_para(wghts,wghts_init_n10_5E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n10_5E01,nlap)
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then
   call lap_assign_para(wghts,wghts_init_n10_6E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n10_6E01,nlap)
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then
   call lap_assign_para(wghts,wghts_init_n10_7E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n10_7E01,nlap)
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then
   call lap_assign_para(wghts,wghts_init_n10_8E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n10_8E01,nlap)
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then
   call lap_assign_para(wghts,wghts_init_n10_9E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n10_9E01,nlap)
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then
   call lap_assign_para(wghts,wghts_init_n10_1E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n10_1E02,nlap)
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
   call lap_assign_para(wghts,wghts_init_n10_2E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n10_2E02,nlap)
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
   call lap_assign_para(wghts,wghts_init_n10_3E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n10_3E02,nlap)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   call lap_assign_para(wghts,wghts_init_n10_4E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n10_4E02,nlap)
  else if (rlen.gt.5.D02 .and. rlen.le.7.D02) then
   call lap_assign_para(wghts,wghts_init_n10_5E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n10_5E02,nlap)
  else if (rlen.gt.7.D02 .and. rlen.le.1.D03) then
   call lap_assign_para(wghts,wghts_init_n10_7E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n10_7E02,nlap)
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
   call lap_assign_para(wghts,wghts_init_n10_1E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n10_1E03,nlap)
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then
   call lap_assign_para(wghts,wghts_init_n10_2E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n10_2E03,nlap)
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then
   call lap_assign_para(wghts,wghts_init_n10_3E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n10_3E03,nlap)
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then
   call lap_assign_para(wghts,wghts_init_n10_4E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n10_4E03,nlap)
  else if (rlen.gt.5.D03 .and. rlen.le.7.D03) then
   call lap_assign_para(wghts,wghts_init_n10_5E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n10_5E03,nlap)
  else if (rlen.gt.7.D03 .and. rlen.le.1.D04) then
   call lap_assign_para(wghts,wghts_init_n10_7E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n10_7E03,nlap)
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then
   call lap_assign_para(wghts,wghts_init_n10_1E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n10_1E04,nlap)
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then
   call lap_assign_para(wghts,wghts_init_n10_2E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n10_2E04,nlap)
  else if (rlen.gt.3.D04 .and. rlen.le.5.D04) then
   call lap_assign_para(wghts,wghts_init_n10_3E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n10_3E04,nlap)
  else if (rlen.gt.5.D04 .and. rlen.le.1.D05) then
   call lap_assign_para(wghts,wghts_init_n10_5E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n10_5E04,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n10_1E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n10_1E05,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (11)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.1.D01) then
   call lap_assign_para(wghts,wghts_init_n11_1E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n11_1E01,nlap)
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then
   call lap_assign_para(wghts,wghts_init_n11_1E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n11_1E01,nlap)
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then
   call lap_assign_para(wghts,wghts_init_n11_2E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n11_2E01,nlap)
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then
   call lap_assign_para(wghts,wghts_init_n11_3E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n11_3E01,nlap)
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then
   call lap_assign_para(wghts,wghts_init_n11_4E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n11_4E01,nlap)
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then
   call lap_assign_para(wghts,wghts_init_n11_5E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n11_5E01,nlap)
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then
   call lap_assign_para(wghts,wghts_init_n11_6E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n11_6E01,nlap)
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then
   call lap_assign_para(wghts,wghts_init_n11_7E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n11_7E01,nlap)
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then
   call lap_assign_para(wghts,wghts_init_n11_8E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n11_8E01,nlap)
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then
   call lap_assign_para(wghts,wghts_init_n11_9E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n11_9E01,nlap)
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then
   call lap_assign_para(wghts,wghts_init_n11_1E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n11_1E02,nlap)
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
   call lap_assign_para(wghts,wghts_init_n11_2E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n11_2E02,nlap)
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
   call lap_assign_para(wghts,wghts_init_n11_3E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n11_3E02,nlap)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   call lap_assign_para(wghts,wghts_init_n11_4E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n11_4E02,nlap)
  else if (rlen.gt.5.D02 .and. rlen.le.7.D02) then
   call lap_assign_para(wghts,wghts_init_n11_5E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n11_5E02,nlap)
  else if (rlen.gt.7.D02 .and. rlen.le.1.D03) then
   call lap_assign_para(wghts,wghts_init_n11_7E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n11_7E02,nlap)
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
   call lap_assign_para(wghts,wghts_init_n11_1E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n11_1E03,nlap)
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then
   call lap_assign_para(wghts,wghts_init_n11_2E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n11_2E03,nlap)
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then
   call lap_assign_para(wghts,wghts_init_n11_3E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n11_3E03,nlap)
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then
   call lap_assign_para(wghts,wghts_init_n11_4E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n11_4E03,nlap)
  else if (rlen.gt.5.D03 .and. rlen.le.7.D03) then
   call lap_assign_para(wghts,wghts_init_n11_5E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n11_5E03,nlap)
  else if (rlen.gt.7.D03 .and. rlen.le.1.D04) then
   call lap_assign_para(wghts,wghts_init_n11_7E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n11_7E03,nlap)
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then
   call lap_assign_para(wghts,wghts_init_n11_1E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n11_1E04,nlap)
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then
   call lap_assign_para(wghts,wghts_init_n11_2E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n11_2E04,nlap)
  else if (rlen.gt.3.D04 .and. rlen.le.5.D04) then
   call lap_assign_para(wghts,wghts_init_n11_3E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n11_3E04,nlap)
  else if (rlen.gt.5.D04 .and. rlen.le.1.D05) then
   call lap_assign_para(wghts,wghts_init_n11_5E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n11_5E04,nlap)
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then
   call lap_assign_para(wghts,wghts_init_n11_1E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n11_1E05,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n11_2E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n11_2E05,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (12)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.1.D01) then
   call lap_assign_para(wghts,wghts_init_n12_1E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n12_1E01,nlap)
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then
   call lap_assign_para(wghts,wghts_init_n12_1E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n12_1E01,nlap)
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then
   call lap_assign_para(wghts,wghts_init_n12_2E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n12_2E01,nlap)
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then
   call lap_assign_para(wghts,wghts_init_n12_3E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n12_3E01,nlap)
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then
   call lap_assign_para(wghts,wghts_init_n12_4E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n12_4E01,nlap)
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then
   call lap_assign_para(wghts,wghts_init_n12_5E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n12_5E01,nlap)
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then
   call lap_assign_para(wghts,wghts_init_n12_6E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n12_6E01,nlap)
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then
   call lap_assign_para(wghts,wghts_init_n12_7E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n12_7E01,nlap)
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then
   call lap_assign_para(wghts,wghts_init_n12_8E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n12_8E01,nlap)
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then
   call lap_assign_para(wghts,wghts_init_n12_9E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n12_9E01,nlap)
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then
   call lap_assign_para(wghts,wghts_init_n12_1E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n12_1E02,nlap)
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
   call lap_assign_para(wghts,wghts_init_n12_2E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n12_2E02,nlap)
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
   call lap_assign_para(wghts,wghts_init_n12_3E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n12_3E02,nlap)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   call lap_assign_para(wghts,wghts_init_n12_4E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n12_4E02,nlap)
  else if (rlen.gt.5.D02 .and. rlen.le.7.D02) then
   call lap_assign_para(wghts,wghts_init_n12_5E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n12_5E02,nlap)
  else if (rlen.gt.7.D02 .and. rlen.le.1.D03) then
   call lap_assign_para(wghts,wghts_init_n12_7E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n12_7E02,nlap)
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
   call lap_assign_para(wghts,wghts_init_n12_1E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n12_1E03,nlap)
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then
   call lap_assign_para(wghts,wghts_init_n12_2E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n12_2E03,nlap)
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then
   call lap_assign_para(wghts,wghts_init_n12_3E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n12_3E03,nlap)
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then
   call lap_assign_para(wghts,wghts_init_n12_4E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n12_4E03,nlap)
  else if (rlen.gt.5.D03 .and. rlen.le.7.D03) then
   call lap_assign_para(wghts,wghts_init_n12_5E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n12_5E03,nlap)
  else if (rlen.gt.7.D03 .and. rlen.le.1.D04) then
   call lap_assign_para(wghts,wghts_init_n12_7E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n12_7E03,nlap)
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then
   call lap_assign_para(wghts,wghts_init_n12_1E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n12_1E04,nlap)
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then
   call lap_assign_para(wghts,wghts_init_n12_2E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n12_2E04,nlap)
  else if (rlen.gt.3.D04 .and. rlen.le.5.D04) then
   call lap_assign_para(wghts,wghts_init_n12_3E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n12_3E04,nlap)
  else if (rlen.gt.5.D04 .and. rlen.le.1.D05) then
   call lap_assign_para(wghts,wghts_init_n12_5E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n12_5E04,nlap)
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then
   call lap_assign_para(wghts,wghts_init_n12_1E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n12_1E05,nlap)
  else if (rlen.gt.2.D05 .and. rlen.le.3.D05) then
   call lap_assign_para(wghts,wghts_init_n12_2E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n12_2E05,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n12_3E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n12_3E05,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (13)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.1.D01) then
   call lap_assign_para(wghts,wghts_init_n13_1E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n13_1E01,nlap)
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then
   call lap_assign_para(wghts,wghts_init_n13_1E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n13_1E01,nlap)
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then
   call lap_assign_para(wghts,wghts_init_n13_2E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n13_2E01,nlap)
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then
   call lap_assign_para(wghts,wghts_init_n13_3E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n13_3E01,nlap)
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then
   call lap_assign_para(wghts,wghts_init_n13_4E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n13_4E01,nlap)
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then
   call lap_assign_para(wghts,wghts_init_n13_5E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n13_5E01,nlap)
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then
   call lap_assign_para(wghts,wghts_init_n13_6E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n13_6E01,nlap)
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then
   call lap_assign_para(wghts,wghts_init_n13_7E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n13_7E01,nlap)
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then
   call lap_assign_para(wghts,wghts_init_n13_8E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n13_8E01,nlap)
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then
   call lap_assign_para(wghts,wghts_init_n13_9E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n13_9E01,nlap)
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then
   call lap_assign_para(wghts,wghts_init_n13_1E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n13_1E02,nlap)
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
   call lap_assign_para(wghts,wghts_init_n13_2E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n13_2E02,nlap)
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
   call lap_assign_para(wghts,wghts_init_n13_3E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n13_3E02,nlap)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   call lap_assign_para(wghts,wghts_init_n13_4E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n13_4E02,nlap)
  else if (rlen.gt.5.D02 .and. rlen.le.7.D02) then
   call lap_assign_para(wghts,wghts_init_n13_5E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n13_5E02,nlap)
  else if (rlen.gt.7.D02 .and. rlen.le.1.D03) then
   call lap_assign_para(wghts,wghts_init_n13_7E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n13_7E02,nlap)
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
   call lap_assign_para(wghts,wghts_init_n13_1E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n13_1E03,nlap)
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then
   call lap_assign_para(wghts,wghts_init_n13_2E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n13_2E03,nlap)
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then
   call lap_assign_para(wghts,wghts_init_n13_3E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n13_3E03,nlap)
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then
   call lap_assign_para(wghts,wghts_init_n13_4E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n13_4E03,nlap)
  else if (rlen.gt.5.D03 .and. rlen.le.7.D03) then
   call lap_assign_para(wghts,wghts_init_n13_5E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n13_5E03,nlap)
  else if (rlen.gt.7.D03 .and. rlen.le.1.D04) then
   call lap_assign_para(wghts,wghts_init_n13_7E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n13_7E03,nlap)
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then
   call lap_assign_para(wghts,wghts_init_n13_1E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n13_1E04,nlap)
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then
   call lap_assign_para(wghts,wghts_init_n13_2E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n13_2E04,nlap)
  else if (rlen.gt.3.D04 .and. rlen.le.5.D04) then
   call lap_assign_para(wghts,wghts_init_n13_3E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n13_3E04,nlap)
  else if (rlen.gt.5.D04 .and. rlen.le.1.D05) then
   call lap_assign_para(wghts,wghts_init_n13_5E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n13_5E04,nlap)
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then
   call lap_assign_para(wghts,wghts_init_n13_1E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n13_1E05,nlap)
  else if (rlen.gt.2.D05 .and. rlen.le.3.D05) then
   call lap_assign_para(wghts,wghts_init_n13_2E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n13_2E05,nlap)
  else if (rlen.gt.3.D05 .and. rlen.le.4.D05) then
   call lap_assign_para(wghts,wghts_init_n13_3E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n13_3E05,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n13_4E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n13_4E05,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (14)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.1.D01) then
   call lap_assign_para(wghts,wghts_init_n14_1E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n14_1E01,nlap)
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then
   call lap_assign_para(wghts,wghts_init_n14_1E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n14_1E01,nlap)
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then
   call lap_assign_para(wghts,wghts_init_n14_2E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n14_2E01,nlap)
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then
   call lap_assign_para(wghts,wghts_init_n14_3E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n14_3E01,nlap)
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then
   call lap_assign_para(wghts,wghts_init_n14_4E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n14_4E01,nlap)
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then
   call lap_assign_para(wghts,wghts_init_n14_5E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n14_5E01,nlap)
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then
   call lap_assign_para(wghts,wghts_init_n14_6E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n14_6E01,nlap)
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then
   call lap_assign_para(wghts,wghts_init_n14_7E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n14_7E01,nlap)
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then
   call lap_assign_para(wghts,wghts_init_n14_8E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n14_8E01,nlap)
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then
   call lap_assign_para(wghts,wghts_init_n14_9E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n14_9E01,nlap)
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then
   call lap_assign_para(wghts,wghts_init_n14_1E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n14_1E02,nlap)
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
   call lap_assign_para(wghts,wghts_init_n14_2E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n14_2E02,nlap)
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
   call lap_assign_para(wghts,wghts_init_n14_3E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n14_3E02,nlap)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   call lap_assign_para(wghts,wghts_init_n14_4E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n14_4E02,nlap)
  else if (rlen.gt.5.D02 .and. rlen.le.7.D02) then
   call lap_assign_para(wghts,wghts_init_n14_5E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n14_5E02,nlap)
  else if (rlen.gt.7.D02 .and. rlen.le.1.D03) then
   call lap_assign_para(wghts,wghts_init_n14_7E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n14_7E02,nlap)
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
   call lap_assign_para(wghts,wghts_init_n14_1E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n14_1E03,nlap)
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then
   call lap_assign_para(wghts,wghts_init_n14_2E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n14_2E03,nlap)
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then
   call lap_assign_para(wghts,wghts_init_n14_3E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n14_3E03,nlap)
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then
   call lap_assign_para(wghts,wghts_init_n14_4E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n14_4E03,nlap)
  else if (rlen.gt.5.D03 .and. rlen.le.7.D03) then
   call lap_assign_para(wghts,wghts_init_n14_5E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n14_5E03,nlap)
  else if (rlen.gt.7.D03 .and. rlen.le.1.D04) then
   call lap_assign_para(wghts,wghts_init_n14_7E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n14_7E03,nlap)
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then
   call lap_assign_para(wghts,wghts_init_n14_1E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n14_1E04,nlap)
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then
   call lap_assign_para(wghts,wghts_init_n14_2E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n14_2E04,nlap)
  else if (rlen.gt.3.D04 .and. rlen.le.5.D04) then
   call lap_assign_para(wghts,wghts_init_n14_3E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n14_3E04,nlap)
  else if (rlen.gt.5.D04 .and. rlen.le.1.D05) then
   call lap_assign_para(wghts,wghts_init_n14_5E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n14_5E04,nlap)
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then
   call lap_assign_para(wghts,wghts_init_n14_1E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n14_1E05,nlap)
  else if (rlen.gt.2.D05 .and. rlen.le.3.D05) then
   call lap_assign_para(wghts,wghts_init_n14_2E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n14_2E05,nlap)
  else if (rlen.gt.3.D05 .and. rlen.le.4.D05) then
   call lap_assign_para(wghts,wghts_init_n14_3E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n14_3E05,nlap)
  else if (rlen.gt.4.D05 .and. rlen.le.5.D05) then
   call lap_assign_para(wghts,wghts_init_n14_4E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n14_4E05,nlap)
  else if (rlen.gt.5.D05 .and. rlen.le.7.D05) then
   call lap_assign_para(wghts,wghts_init_n14_5E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n14_5E05,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n14_7E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n14_7E05,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (15)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.1.D01) then
   call lap_assign_para(wghts,wghts_init_n15_1E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n15_1E01,nlap)
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then
   call lap_assign_para(wghts,wghts_init_n15_1E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n15_1E01,nlap)
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then
   call lap_assign_para(wghts,wghts_init_n15_2E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n15_2E01,nlap)
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then
   call lap_assign_para(wghts,wghts_init_n15_3E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n15_3E01,nlap)
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then
   call lap_assign_para(wghts,wghts_init_n15_4E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n15_4E01,nlap)
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then
   call lap_assign_para(wghts,wghts_init_n15_5E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n15_5E01,nlap)
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then
   call lap_assign_para(wghts,wghts_init_n15_6E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n15_6E01,nlap)
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then
   call lap_assign_para(wghts,wghts_init_n15_7E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n15_7E01,nlap)
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then
   call lap_assign_para(wghts,wghts_init_n15_8E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n15_8E01,nlap)
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then
   call lap_assign_para(wghts,wghts_init_n15_9E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n15_9E01,nlap)
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then
   call lap_assign_para(wghts,wghts_init_n15_1E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n15_1E02,nlap)
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
   call lap_assign_para(wghts,wghts_init_n15_2E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n15_2E02,nlap)
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
   call lap_assign_para(wghts,wghts_init_n15_3E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n15_3E02,nlap)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   call lap_assign_para(wghts,wghts_init_n15_4E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n15_4E02,nlap)
  else if (rlen.gt.5.D02 .and. rlen.le.6.D02) then
   call lap_assign_para(wghts,wghts_init_n15_5E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n15_5E02,nlap)
  else if (rlen.gt.6.D02 .and. rlen.le.7.D02) then
   call lap_assign_para(wghts,wghts_init_n15_6E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n15_6E02,nlap)
  else if (rlen.gt.7.D02 .and. rlen.le.8.D02) then
   call lap_assign_para(wghts,wghts_init_n15_7E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n15_7E02,nlap)
  else if (rlen.gt.8.D02 .and. rlen.le.9.D02) then
   call lap_assign_para(wghts,wghts_init_n15_8E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n15_8E02,nlap)
  else if (rlen.gt.9.D02 .and. rlen.le.1.D03) then
   call lap_assign_para(wghts,wghts_init_n15_9E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n15_9E02,nlap)
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
   call lap_assign_para(wghts,wghts_init_n15_1E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n15_1E03,nlap)
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then
   call lap_assign_para(wghts,wghts_init_n15_2E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n15_2E03,nlap)
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then
   call lap_assign_para(wghts,wghts_init_n15_3E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n15_3E03,nlap)
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then
   call lap_assign_para(wghts,wghts_init_n15_4E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n15_4E03,nlap)
  else if (rlen.gt.5.D03 .and. rlen.le.6.D03) then
   call lap_assign_para(wghts,wghts_init_n15_5E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n15_5E03,nlap)
  else if (rlen.gt.6.D03 .and. rlen.le.7.D03) then
   call lap_assign_para(wghts,wghts_init_n15_6E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n15_6E03,nlap)
  else if (rlen.gt.7.D03 .and. rlen.le.8.D03) then
   call lap_assign_para(wghts,wghts_init_n15_7E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n15_7E03,nlap)
  else if (rlen.gt.8.D03 .and. rlen.le.9.D03) then
   call lap_assign_para(wghts,wghts_init_n15_8E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n15_8E03,nlap)
  else if (rlen.gt.9.D03 .and. rlen.le.1.D04) then
   call lap_assign_para(wghts,wghts_init_n15_9E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n15_9E03,nlap)
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then
   call lap_assign_para(wghts,wghts_init_n15_1E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n15_1E04,nlap)
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then
   call lap_assign_para(wghts,wghts_init_n15_2E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n15_2E04,nlap)
  else if (rlen.gt.3.D04 .and. rlen.le.4.D04) then
   call lap_assign_para(wghts,wghts_init_n15_3E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n15_3E04,nlap)
  else if (rlen.gt.4.D04 .and. rlen.le.5.D04) then
   call lap_assign_para(wghts,wghts_init_n15_4E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n15_4E04,nlap)
  else if (rlen.gt.5.D04 .and. rlen.le.7.D04) then
   call lap_assign_para(wghts,wghts_init_n15_5E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n15_5E04,nlap)
  else if (rlen.gt.7.D04 .and. rlen.le.1.D05) then
   call lap_assign_para(wghts,wghts_init_n15_7E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n15_7E04,nlap)
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then
   call lap_assign_para(wghts,wghts_init_n15_1E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n15_1E05,nlap)
  else if (rlen.gt.2.D05 .and. rlen.le.5.D05) then
   call lap_assign_para(wghts,wghts_init_n15_2E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n15_2E05,nlap)
  else if (rlen.gt.5.D05 .and. rlen.le.1.D06) then
   call lap_assign_para(wghts,wghts_init_n15_5E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n15_5E05,nlap)
  else if (rlen.gt.1.D06 .and. rlen.le.2.D06) then
   call lap_assign_para(wghts,wghts_init_n15_1E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n15_1E06,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n15_2E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n15_2E06,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (16)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.1.D01) then
   call lap_assign_para(wghts,wghts_init_n16_1E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n16_1E01,nlap)
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then
   call lap_assign_para(wghts,wghts_init_n16_1E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n16_1E01,nlap)
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then
   call lap_assign_para(wghts,wghts_init_n16_2E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n16_2E01,nlap)
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then
   call lap_assign_para(wghts,wghts_init_n16_3E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n16_3E01,nlap)
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then
   call lap_assign_para(wghts,wghts_init_n16_4E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n16_4E01,nlap)
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then
   call lap_assign_para(wghts,wghts_init_n16_5E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n16_5E01,nlap)
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then
   call lap_assign_para(wghts,wghts_init_n16_6E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n16_6E01,nlap)
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then
   call lap_assign_para(wghts,wghts_init_n16_7E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n16_7E01,nlap)
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then
   call lap_assign_para(wghts,wghts_init_n16_8E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n16_8E01,nlap)
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then
   call lap_assign_para(wghts,wghts_init_n16_9E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n16_9E01,nlap)
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then
   call lap_assign_para(wghts,wghts_init_n16_1E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n16_1E02,nlap)
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
   call lap_assign_para(wghts,wghts_init_n16_2E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n16_2E02,nlap)
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
   call lap_assign_para(wghts,wghts_init_n16_3E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n16_3E02,nlap)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   call lap_assign_para(wghts,wghts_init_n16_4E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n16_4E02,nlap)
  else if (rlen.gt.5.D02 .and. rlen.le.6.D02) then
   call lap_assign_para(wghts,wghts_init_n16_5E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n16_5E02,nlap)
  else if (rlen.gt.6.D02 .and. rlen.le.7.D02) then
   call lap_assign_para(wghts,wghts_init_n16_6E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n16_6E02,nlap)
  else if (rlen.gt.7.D02 .and. rlen.le.8.D02) then
   call lap_assign_para(wghts,wghts_init_n16_7E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n16_7E02,nlap)
  else if (rlen.gt.8.D02 .and. rlen.le.9.D02) then
   call lap_assign_para(wghts,wghts_init_n16_8E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n16_8E02,nlap)
  else if (rlen.gt.9.D02 .and. rlen.le.1.D03) then
   call lap_assign_para(wghts,wghts_init_n16_9E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n16_9E02,nlap)
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
   call lap_assign_para(wghts,wghts_init_n16_1E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n16_1E03,nlap)
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then
   call lap_assign_para(wghts,wghts_init_n16_2E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n16_2E03,nlap)
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then
   call lap_assign_para(wghts,wghts_init_n16_3E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n16_3E03,nlap)
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then
   call lap_assign_para(wghts,wghts_init_n16_4E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n16_4E03,nlap)
  else if (rlen.gt.5.D03 .and. rlen.le.6.D03) then
   call lap_assign_para(wghts,wghts_init_n16_5E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n16_5E03,nlap)
  else if (rlen.gt.6.D03 .and. rlen.le.7.D03) then
   call lap_assign_para(wghts,wghts_init_n16_6E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n16_6E03,nlap)
  else if (rlen.gt.7.D03 .and. rlen.le.8.D03) then
   call lap_assign_para(wghts,wghts_init_n16_7E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n16_7E03,nlap)
  else if (rlen.gt.8.D03 .and. rlen.le.9.D03) then
   call lap_assign_para(wghts,wghts_init_n16_8E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n16_8E03,nlap)
  else if (rlen.gt.9.D03 .and. rlen.le.1.D04) then
   call lap_assign_para(wghts,wghts_init_n16_9E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n16_9E03,nlap)
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then
   call lap_assign_para(wghts,wghts_init_n16_1E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n16_1E04,nlap)
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then
   call lap_assign_para(wghts,wghts_init_n16_2E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n16_2E04,nlap)
  else if (rlen.gt.3.D04 .and. rlen.le.4.D04) then
   call lap_assign_para(wghts,wghts_init_n16_3E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n16_3E04,nlap)
  else if (rlen.gt.4.D04 .and. rlen.le.5.D04) then
   call lap_assign_para(wghts,wghts_init_n16_4E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n16_4E04,nlap)
  else if (rlen.gt.5.D04 .and. rlen.le.7.D04) then
   call lap_assign_para(wghts,wghts_init_n16_5E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n16_5E04,nlap)
  else if (rlen.gt.7.D04 .and. rlen.le.1.D05) then
   call lap_assign_para(wghts,wghts_init_n16_7E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n16_7E04,nlap)
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then
   call lap_assign_para(wghts,wghts_init_n16_1E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n16_1E05,nlap)
  else if (rlen.gt.2.D05 .and. rlen.le.5.D05) then
   call lap_assign_para(wghts,wghts_init_n16_2E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n16_2E05,nlap)
  else if (rlen.gt.5.D05 .and. rlen.le.1.D06) then
   call lap_assign_para(wghts,wghts_init_n16_5E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n16_5E05,nlap)
  else if (rlen.gt.1.D06 .and. rlen.le.2.D06) then
   call lap_assign_para(wghts,wghts_init_n16_1E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n16_1E06,nlap)
  else if (rlen.gt.2.D06 .and. rlen.le.3.D06) then
   call lap_assign_para(wghts,wghts_init_n16_2E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n16_2E06,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n16_3E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n16_3E06,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (17)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.2.D01) then
   call lap_assign_para(wghts,wghts_init_n17_2E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n17_2E01,nlap)
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then
   call lap_assign_para(wghts,wghts_init_n17_2E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n17_2E01,nlap)
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then
   call lap_assign_para(wghts,wghts_init_n17_3E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n17_3E01,nlap)
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then
   call lap_assign_para(wghts,wghts_init_n17_4E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n17_4E01,nlap)
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then
   call lap_assign_para(wghts,wghts_init_n17_5E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n17_5E01,nlap)
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then
   call lap_assign_para(wghts,wghts_init_n17_6E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n17_6E01,nlap)
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then
   call lap_assign_para(wghts,wghts_init_n17_7E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n17_7E01,nlap)
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then
   call lap_assign_para(wghts,wghts_init_n17_8E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n17_8E01,nlap)
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then
   call lap_assign_para(wghts,wghts_init_n17_9E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n17_9E01,nlap)
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then
   call lap_assign_para(wghts,wghts_init_n17_1E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n17_1E02,nlap)
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
   call lap_assign_para(wghts,wghts_init_n17_2E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n17_2E02,nlap)
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
   call lap_assign_para(wghts,wghts_init_n17_3E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n17_3E02,nlap)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   call lap_assign_para(wghts,wghts_init_n17_4E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n17_4E02,nlap)
  else if (rlen.gt.5.D02 .and. rlen.le.6.D02) then
   call lap_assign_para(wghts,wghts_init_n17_5E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n17_5E02,nlap)
  else if (rlen.gt.6.D02 .and. rlen.le.7.D02) then
   call lap_assign_para(wghts,wghts_init_n17_6E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n17_6E02,nlap)
  else if (rlen.gt.7.D02 .and. rlen.le.8.D02) then
   call lap_assign_para(wghts,wghts_init_n17_7E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n17_7E02,nlap)
  else if (rlen.gt.8.D02 .and. rlen.le.9.D02) then
   call lap_assign_para(wghts,wghts_init_n17_8E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n17_8E02,nlap)
  else if (rlen.gt.9.D02 .and. rlen.le.1.D03) then
   call lap_assign_para(wghts,wghts_init_n17_9E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n17_9E02,nlap)
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
   call lap_assign_para(wghts,wghts_init_n17_1E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n17_1E03,nlap)
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then
   call lap_assign_para(wghts,wghts_init_n17_2E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n17_2E03,nlap)
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then
   call lap_assign_para(wghts,wghts_init_n17_3E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n17_3E03,nlap)
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then
   call lap_assign_para(wghts,wghts_init_n17_4E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n17_4E03,nlap)
  else if (rlen.gt.5.D03 .and. rlen.le.6.D03) then
   call lap_assign_para(wghts,wghts_init_n17_5E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n17_5E03,nlap)
  else if (rlen.gt.6.D03 .and. rlen.le.7.D03) then
   call lap_assign_para(wghts,wghts_init_n17_6E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n17_6E03,nlap)
  else if (rlen.gt.7.D03 .and. rlen.le.8.D03) then
   call lap_assign_para(wghts,wghts_init_n17_7E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n17_7E03,nlap)
  else if (rlen.gt.8.D03 .and. rlen.le.9.D03) then
   call lap_assign_para(wghts,wghts_init_n17_8E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n17_8E03,nlap)
  else if (rlen.gt.9.D03 .and. rlen.le.1.D04) then
   call lap_assign_para(wghts,wghts_init_n17_9E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n17_9E03,nlap)
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then
   call lap_assign_para(wghts,wghts_init_n17_1E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n17_1E04,nlap)
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then
   call lap_assign_para(wghts,wghts_init_n17_2E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n17_2E04,nlap)
  else if (rlen.gt.3.D04 .and. rlen.le.4.D04) then
   call lap_assign_para(wghts,wghts_init_n17_3E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n17_3E04,nlap)
  else if (rlen.gt.4.D04 .and. rlen.le.5.D04) then
   call lap_assign_para(wghts,wghts_init_n17_4E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n17_4E04,nlap)
  else if (rlen.gt.5.D04 .and. rlen.le.7.D04) then
   call lap_assign_para(wghts,wghts_init_n17_5E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n17_5E04,nlap)
  else if (rlen.gt.7.D04 .and. rlen.le.1.D05) then
   call lap_assign_para(wghts,wghts_init_n17_7E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n17_7E04,nlap)
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then
   call lap_assign_para(wghts,wghts_init_n17_1E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n17_1E05,nlap)
  else if (rlen.gt.2.D05 .and. rlen.le.5.D05) then
   call lap_assign_para(wghts,wghts_init_n17_2E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n17_2E05,nlap)
  else if (rlen.gt.5.D05 .and. rlen.le.1.D06) then
   call lap_assign_para(wghts,wghts_init_n17_5E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n17_5E05,nlap)
  else if (rlen.gt.1.D06 .and. rlen.le.2.D06) then
   call lap_assign_para(wghts,wghts_init_n17_1E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n17_1E06,nlap)
  else if (rlen.gt.2.D06 .and. rlen.le.3.D06) then
   call lap_assign_para(wghts,wghts_init_n17_2E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n17_2E06,nlap)
  else if (rlen.gt.3.D06 .and. rlen.le.4.D06) then
   call lap_assign_para(wghts,wghts_init_n17_3E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n17_3E06,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n17_4E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n17_4E06,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (18)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.4.D01) then
   call lap_assign_para(wghts,wghts_init_n18_4E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n18_4E01,nlap)
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then
   call lap_assign_para(wghts,wghts_init_n18_4E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n18_4E01,nlap)
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then
   call lap_assign_para(wghts,wghts_init_n18_5E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n18_5E01,nlap)
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then
   call lap_assign_para(wghts,wghts_init_n18_6E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n18_6E01,nlap)
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then
   call lap_assign_para(wghts,wghts_init_n18_7E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n18_7E01,nlap)
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then
   call lap_assign_para(wghts,wghts_init_n18_8E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n18_8E01,nlap)
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then
   call lap_assign_para(wghts,wghts_init_n18_9E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n18_9E01,nlap)
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then
   call lap_assign_para(wghts,wghts_init_n18_1E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n18_1E02,nlap)
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
   call lap_assign_para(wghts,wghts_init_n18_2E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n18_2E02,nlap)
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
   call lap_assign_para(wghts,wghts_init_n18_3E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n18_3E02,nlap)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   call lap_assign_para(wghts,wghts_init_n18_4E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n18_4E02,nlap)
  else if (rlen.gt.5.D02 .and. rlen.le.6.D02) then
   call lap_assign_para(wghts,wghts_init_n18_5E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n18_5E02,nlap)
  else if (rlen.gt.6.D02 .and. rlen.le.7.D02) then
   call lap_assign_para(wghts,wghts_init_n18_6E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n18_6E02,nlap)
  else if (rlen.gt.7.D02 .and. rlen.le.8.D02) then
   call lap_assign_para(wghts,wghts_init_n18_7E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n18_7E02,nlap)
  else if (rlen.gt.8.D02 .and. rlen.le.9.D02) then
   call lap_assign_para(wghts,wghts_init_n18_8E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n18_8E02,nlap)
  else if (rlen.gt.9.D02 .and. rlen.le.1.D03) then
   call lap_assign_para(wghts,wghts_init_n18_9E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n18_9E02,nlap)
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
   call lap_assign_para(wghts,wghts_init_n18_1E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n18_1E03,nlap)
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then
   call lap_assign_para(wghts,wghts_init_n18_2E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n18_2E03,nlap)
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then
   call lap_assign_para(wghts,wghts_init_n18_3E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n18_3E03,nlap)
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then
   call lap_assign_para(wghts,wghts_init_n18_4E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n18_4E03,nlap)
  else if (rlen.gt.5.D03 .and. rlen.le.6.D03) then
   call lap_assign_para(wghts,wghts_init_n18_5E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n18_5E03,nlap)
  else if (rlen.gt.6.D03 .and. rlen.le.7.D03) then
   call lap_assign_para(wghts,wghts_init_n18_6E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n18_6E03,nlap)
  else if (rlen.gt.7.D03 .and. rlen.le.8.D03) then
   call lap_assign_para(wghts,wghts_init_n18_7E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n18_7E03,nlap)
  else if (rlen.gt.8.D03 .and. rlen.le.9.D03) then
   call lap_assign_para(wghts,wghts_init_n18_8E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n18_8E03,nlap)
  else if (rlen.gt.9.D03 .and. rlen.le.1.D04) then
   call lap_assign_para(wghts,wghts_init_n18_9E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n18_9E03,nlap)
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then
   call lap_assign_para(wghts,wghts_init_n18_1E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n18_1E04,nlap)
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then
   call lap_assign_para(wghts,wghts_init_n18_2E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n18_2E04,nlap)
  else if (rlen.gt.3.D04 .and. rlen.le.4.D04) then
   call lap_assign_para(wghts,wghts_init_n18_3E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n18_3E04,nlap)
  else if (rlen.gt.4.D04 .and. rlen.le.5.D04) then
   call lap_assign_para(wghts,wghts_init_n18_4E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n18_4E04,nlap)
  else if (rlen.gt.5.D04 .and. rlen.le.7.D04) then
   call lap_assign_para(wghts,wghts_init_n18_5E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n18_5E04,nlap)
  else if (rlen.gt.7.D04 .and. rlen.le.1.D05) then
   call lap_assign_para(wghts,wghts_init_n18_7E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n18_7E04,nlap)
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then
   call lap_assign_para(wghts,wghts_init_n18_1E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n18_1E05,nlap)
  else if (rlen.gt.2.D05 .and. rlen.le.5.D05) then
   call lap_assign_para(wghts,wghts_init_n18_2E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n18_2E05,nlap)
  else if (rlen.gt.5.D05 .and. rlen.le.1.D06) then
   call lap_assign_para(wghts,wghts_init_n18_5E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n18_5E05,nlap)
  else if (rlen.gt.1.D06 .and. rlen.le.2.D06) then
   call lap_assign_para(wghts,wghts_init_n18_1E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n18_1E06,nlap)
  else if (rlen.gt.2.D06 .and. rlen.le.3.D06) then
   call lap_assign_para(wghts,wghts_init_n18_2E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n18_2E06,nlap)
  else if (rlen.gt.3.D06 .and. rlen.le.4.D06) then
   call lap_assign_para(wghts,wghts_init_n18_3E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n18_3E06,nlap)
  else if (rlen.gt.4.D06 .and. rlen.le.5.D06) then
   call lap_assign_para(wghts,wghts_init_n18_4E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n18_4E06,nlap)
  else if (rlen.gt.5.D06 .and. rlen.le.7.D06) then
   call lap_assign_para(wghts,wghts_init_n18_5E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n18_5E06,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n18_7E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n18_7E06,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (19)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.5.D01) then
   call lap_assign_para(wghts,wghts_init_n19_5E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n19_5E01,nlap)
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then
   call lap_assign_para(wghts,wghts_init_n19_5E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n19_5E01,nlap)
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then
   call lap_assign_para(wghts,wghts_init_n19_6E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n19_6E01,nlap)
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then
   call lap_assign_para(wghts,wghts_init_n19_7E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n19_7E01,nlap)
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then
   call lap_assign_para(wghts,wghts_init_n19_8E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n19_8E01,nlap)
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then
   call lap_assign_para(wghts,wghts_init_n19_9E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n19_9E01,nlap)
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then
   call lap_assign_para(wghts,wghts_init_n19_1E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n19_1E02,nlap)
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
   call lap_assign_para(wghts,wghts_init_n19_2E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n19_2E02,nlap)
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
   call lap_assign_para(wghts,wghts_init_n19_3E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n19_3E02,nlap)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   call lap_assign_para(wghts,wghts_init_n19_4E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n19_4E02,nlap)
  else if (rlen.gt.5.D02 .and. rlen.le.6.D02) then
   call lap_assign_para(wghts,wghts_init_n19_5E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n19_5E02,nlap)
  else if (rlen.gt.6.D02 .and. rlen.le.7.D02) then
   call lap_assign_para(wghts,wghts_init_n19_6E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n19_6E02,nlap)
  else if (rlen.gt.7.D02 .and. rlen.le.8.D02) then
   call lap_assign_para(wghts,wghts_init_n19_7E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n19_7E02,nlap)
  else if (rlen.gt.8.D02 .and. rlen.le.9.D02) then
   call lap_assign_para(wghts,wghts_init_n19_8E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n19_8E02,nlap)
  else if (rlen.gt.9.D02 .and. rlen.le.1.D03) then
   call lap_assign_para(wghts,wghts_init_n19_9E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n19_9E02,nlap)
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
   call lap_assign_para(wghts,wghts_init_n19_1E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n19_1E03,nlap)
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then
   call lap_assign_para(wghts,wghts_init_n19_2E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n19_2E03,nlap)
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then
   call lap_assign_para(wghts,wghts_init_n19_3E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n19_3E03,nlap)
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then
   call lap_assign_para(wghts,wghts_init_n19_4E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n19_4E03,nlap)
  else if (rlen.gt.5.D03 .and. rlen.le.6.D03) then
   call lap_assign_para(wghts,wghts_init_n19_5E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n19_5E03,nlap)
  else if (rlen.gt.6.D03 .and. rlen.le.7.D03) then
   call lap_assign_para(wghts,wghts_init_n19_6E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n19_6E03,nlap)
  else if (rlen.gt.7.D03 .and. rlen.le.8.D03) then
   call lap_assign_para(wghts,wghts_init_n19_7E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n19_7E03,nlap)
  else if (rlen.gt.8.D03 .and. rlen.le.9.D03) then
   call lap_assign_para(wghts,wghts_init_n19_8E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n19_8E03,nlap)
  else if (rlen.gt.9.D03 .and. rlen.le.1.D04) then
   call lap_assign_para(wghts,wghts_init_n19_9E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n19_9E03,nlap)
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then
   call lap_assign_para(wghts,wghts_init_n19_1E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n19_1E04,nlap)
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then
   call lap_assign_para(wghts,wghts_init_n19_2E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n19_2E04,nlap)
  else if (rlen.gt.3.D04 .and. rlen.le.4.D04) then
   call lap_assign_para(wghts,wghts_init_n19_3E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n19_3E04,nlap)
  else if (rlen.gt.4.D04 .and. rlen.le.5.D04) then
   call lap_assign_para(wghts,wghts_init_n19_4E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n19_4E04,nlap)
  else if (rlen.gt.5.D04 .and. rlen.le.7.D04) then
   call lap_assign_para(wghts,wghts_init_n19_5E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n19_5E04,nlap)
  else if (rlen.gt.7.D04 .and. rlen.le.1.D05) then
   call lap_assign_para(wghts,wghts_init_n19_7E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n19_7E04,nlap)
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then
   call lap_assign_para(wghts,wghts_init_n19_1E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n19_1E05,nlap)
  else if (rlen.gt.2.D05 .and. rlen.le.5.D05) then
   call lap_assign_para(wghts,wghts_init_n19_2E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n19_2E05,nlap)
  else if (rlen.gt.5.D05 .and. rlen.le.1.D06) then
   call lap_assign_para(wghts,wghts_init_n19_5E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n19_5E05,nlap)
  else if (rlen.gt.1.D06 .and. rlen.le.2.D06) then
   call lap_assign_para(wghts,wghts_init_n19_1E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n19_1E06,nlap)
  else if (rlen.gt.2.D06 .and. rlen.le.3.D06) then
   call lap_assign_para(wghts,wghts_init_n19_2E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n19_2E06,nlap)
  else if (rlen.gt.3.D06 .and. rlen.le.4.D06) then
   call lap_assign_para(wghts,wghts_init_n19_3E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n19_3E06,nlap)
  else if (rlen.gt.4.D06 .and. rlen.le.5.D06) then
   call lap_assign_para(wghts,wghts_init_n19_4E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n19_4E06,nlap)
  else if (rlen.gt.5.D06 .and. rlen.le.7.D06) then
   call lap_assign_para(wghts,wghts_init_n19_5E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n19_5E06,nlap)
  else if (rlen.gt.7.D06 .and. rlen.le.1.D07) then
   call lap_assign_para(wghts,wghts_init_n19_7E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n19_7E06,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n19_1E07,nlap)
   call lap_assign_para(xpnts,xpnts_init_n19_1E07,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (20)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.6.D01) then
   call lap_assign_para(wghts,wghts_init_n20_6E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n20_6E01,nlap)
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then
   call lap_assign_para(wghts,wghts_init_n20_6E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n20_6E01,nlap)
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then
   call lap_assign_para(wghts,wghts_init_n20_7E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n20_7E01,nlap)
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then
   call lap_assign_para(wghts,wghts_init_n20_8E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n20_8E01,nlap)
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then
   call lap_assign_para(wghts,wghts_init_n20_9E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n20_9E01,nlap)
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then
   call lap_assign_para(wghts,wghts_init_n20_1E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n20_1E02,nlap)
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
   call lap_assign_para(wghts,wghts_init_n20_2E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n20_2E02,nlap)
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
   call lap_assign_para(wghts,wghts_init_n20_3E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n20_3E02,nlap)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   call lap_assign_para(wghts,wghts_init_n20_4E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n20_4E02,nlap)
  else if (rlen.gt.5.D02 .and. rlen.le.6.D02) then
   call lap_assign_para(wghts,wghts_init_n20_5E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n20_5E02,nlap)
  else if (rlen.gt.6.D02 .and. rlen.le.7.D02) then
   call lap_assign_para(wghts,wghts_init_n20_6E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n20_6E02,nlap)
  else if (rlen.gt.7.D02 .and. rlen.le.8.D02) then
   call lap_assign_para(wghts,wghts_init_n20_7E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n20_7E02,nlap)
  else if (rlen.gt.8.D02 .and. rlen.le.9.D02) then
   call lap_assign_para(wghts,wghts_init_n20_8E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n20_8E02,nlap)
  else if (rlen.gt.9.D02 .and. rlen.le.1.D03) then
   call lap_assign_para(wghts,wghts_init_n20_9E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n20_9E02,nlap)
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
   call lap_assign_para(wghts,wghts_init_n20_1E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n20_1E03,nlap)
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then
   call lap_assign_para(wghts,wghts_init_n20_2E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n20_2E03,nlap)
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then
   call lap_assign_para(wghts,wghts_init_n20_3E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n20_3E03,nlap)
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then
   call lap_assign_para(wghts,wghts_init_n20_4E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n20_4E03,nlap)
  else if (rlen.gt.5.D03 .and. rlen.le.6.D03) then
   call lap_assign_para(wghts,wghts_init_n20_5E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n20_5E03,nlap)
  else if (rlen.gt.6.D03 .and. rlen.le.7.D03) then
   call lap_assign_para(wghts,wghts_init_n20_6E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n20_6E03,nlap)
  else if (rlen.gt.7.D03 .and. rlen.le.8.D03) then
   call lap_assign_para(wghts,wghts_init_n20_7E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n20_7E03,nlap)
  else if (rlen.gt.8.D03 .and. rlen.le.9.D03) then
   call lap_assign_para(wghts,wghts_init_n20_8E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n20_8E03,nlap)
  else if (rlen.gt.9.D03 .and. rlen.le.1.D04) then
   call lap_assign_para(wghts,wghts_init_n20_9E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n20_9E03,nlap)
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then
   call lap_assign_para(wghts,wghts_init_n20_1E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n20_1E04,nlap)
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then
   call lap_assign_para(wghts,wghts_init_n20_2E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n20_2E04,nlap)
  else if (rlen.gt.3.D04 .and. rlen.le.4.D04) then
   call lap_assign_para(wghts,wghts_init_n20_3E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n20_3E04,nlap)
  else if (rlen.gt.4.D04 .and. rlen.le.5.D04) then
   call lap_assign_para(wghts,wghts_init_n20_4E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n20_4E04,nlap)
  else if (rlen.gt.5.D04 .and. rlen.le.7.D04) then
   call lap_assign_para(wghts,wghts_init_n20_5E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n20_5E04,nlap)
  else if (rlen.gt.7.D04 .and. rlen.le.1.D05) then
   call lap_assign_para(wghts,wghts_init_n20_7E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n20_7E04,nlap)
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then
   call lap_assign_para(wghts,wghts_init_n20_1E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n20_1E05,nlap)
  else if (rlen.gt.2.D05 .and. rlen.le.5.D05) then
   call lap_assign_para(wghts,wghts_init_n20_2E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n20_2E05,nlap)
  else if (rlen.gt.5.D05 .and. rlen.le.1.D06) then
   call lap_assign_para(wghts,wghts_init_n20_5E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n20_5E05,nlap)
  else if (rlen.gt.1.D06 .and. rlen.le.2.D06) then
   call lap_assign_para(wghts,wghts_init_n20_1E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n20_1E06,nlap)
  else if (rlen.gt.2.D06 .and. rlen.le.3.D06) then
   call lap_assign_para(wghts,wghts_init_n20_2E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n20_2E06,nlap)
  else if (rlen.gt.3.D06 .and. rlen.le.4.D06) then
   call lap_assign_para(wghts,wghts_init_n20_3E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n20_3E06,nlap)
  else if (rlen.gt.4.D06 .and. rlen.le.5.D06) then
   call lap_assign_para(wghts,wghts_init_n20_4E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n20_4E06,nlap)
  else if (rlen.gt.5.D06 .and. rlen.le.7.D06) then
   call lap_assign_para(wghts,wghts_init_n20_5E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n20_5E06,nlap)
  else if (rlen.gt.7.D06 .and. rlen.le.1.D07) then
   call lap_assign_para(wghts,wghts_init_n20_7E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n20_7E06,nlap)
  else if (rlen.gt.1.D07 .and. rlen.le.2.D07) then
   call lap_assign_para(wghts,wghts_init_n20_1E07,nlap)
   call lap_assign_para(xpnts,xpnts_init_n20_1E07,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n20_2E07,nlap)
   call lap_assign_para(xpnts,xpnts_init_n20_2E07,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (21)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.9.D01) then
   call lap_assign_para(wghts,wghts_init_n21_9E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n21_9E01,nlap)
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then
   call lap_assign_para(wghts,wghts_init_n21_9E01,nlap)
   call lap_assign_para(xpnts,xpnts_init_n21_9E01,nlap)
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then
   call lap_assign_para(wghts,wghts_init_n21_1E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n21_1E02,nlap)
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
   call lap_assign_para(wghts,wghts_init_n21_2E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n21_2E02,nlap)
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
   call lap_assign_para(wghts,wghts_init_n21_3E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n21_3E02,nlap)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   call lap_assign_para(wghts,wghts_init_n21_4E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n21_4E02,nlap)
  else if (rlen.gt.5.D02 .and. rlen.le.6.D02) then
   call lap_assign_para(wghts,wghts_init_n21_5E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n21_5E02,nlap)
  else if (rlen.gt.6.D02 .and. rlen.le.7.D02) then
   call lap_assign_para(wghts,wghts_init_n21_6E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n21_6E02,nlap)
  else if (rlen.gt.7.D02 .and. rlen.le.8.D02) then
   call lap_assign_para(wghts,wghts_init_n21_7E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n21_7E02,nlap)
  else if (rlen.gt.8.D02 .and. rlen.le.9.D02) then
   call lap_assign_para(wghts,wghts_init_n21_8E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n21_8E02,nlap)
  else if (rlen.gt.9.D02 .and. rlen.le.1.D03) then
   call lap_assign_para(wghts,wghts_init_n21_9E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n21_9E02,nlap)
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
   call lap_assign_para(wghts,wghts_init_n21_1E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n21_1E03,nlap)
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then
   call lap_assign_para(wghts,wghts_init_n21_2E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n21_2E03,nlap)
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then
   call lap_assign_para(wghts,wghts_init_n21_3E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n21_3E03,nlap)
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then
   call lap_assign_para(wghts,wghts_init_n21_4E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n21_4E03,nlap)
  else if (rlen.gt.5.D03 .and. rlen.le.6.D03) then
   call lap_assign_para(wghts,wghts_init_n21_5E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n21_5E03,nlap)
  else if (rlen.gt.6.D03 .and. rlen.le.7.D03) then
   call lap_assign_para(wghts,wghts_init_n21_6E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n21_6E03,nlap)
  else if (rlen.gt.7.D03 .and. rlen.le.8.D03) then
   call lap_assign_para(wghts,wghts_init_n21_7E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n21_7E03,nlap)
  else if (rlen.gt.8.D03 .and. rlen.le.9.D03) then
   call lap_assign_para(wghts,wghts_init_n21_8E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n21_8E03,nlap)
  else if (rlen.gt.9.D03 .and. rlen.le.1.D04) then
   call lap_assign_para(wghts,wghts_init_n21_9E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n21_9E03,nlap)
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then
   call lap_assign_para(wghts,wghts_init_n21_1E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n21_1E04,nlap)
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then
   call lap_assign_para(wghts,wghts_init_n21_2E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n21_2E04,nlap)
  else if (rlen.gt.3.D04 .and. rlen.le.4.D04) then
   call lap_assign_para(wghts,wghts_init_n21_3E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n21_3E04,nlap)
  else if (rlen.gt.4.D04 .and. rlen.le.5.D04) then
   call lap_assign_para(wghts,wghts_init_n21_4E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n21_4E04,nlap)
  else if (rlen.gt.5.D04 .and. rlen.le.7.D04) then
   call lap_assign_para(wghts,wghts_init_n21_5E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n21_5E04,nlap)
  else if (rlen.gt.7.D04 .and. rlen.le.1.D05) then
   call lap_assign_para(wghts,wghts_init_n21_7E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n21_7E04,nlap)
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then
   call lap_assign_para(wghts,wghts_init_n21_1E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n21_1E05,nlap)
  else if (rlen.gt.2.D05 .and. rlen.le.5.D05) then
   call lap_assign_para(wghts,wghts_init_n21_2E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n21_2E05,nlap)
  else if (rlen.gt.5.D05 .and. rlen.le.1.D06) then
   call lap_assign_para(wghts,wghts_init_n21_5E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n21_5E05,nlap)
  else if (rlen.gt.1.D06 .and. rlen.le.2.D06) then
   call lap_assign_para(wghts,wghts_init_n21_1E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n21_1E06,nlap)
  else if (rlen.gt.2.D06 .and. rlen.le.3.D06) then
   call lap_assign_para(wghts,wghts_init_n21_2E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n21_2E06,nlap)
  else if (rlen.gt.3.D06 .and. rlen.le.4.D06) then
   call lap_assign_para(wghts,wghts_init_n21_3E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n21_3E06,nlap)
  else if (rlen.gt.4.D06 .and. rlen.le.5.D06) then
   call lap_assign_para(wghts,wghts_init_n21_4E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n21_4E06,nlap)
  else if (rlen.gt.5.D06 .and. rlen.le.7.D06) then
   call lap_assign_para(wghts,wghts_init_n21_5E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n21_5E06,nlap)
  else if (rlen.gt.7.D06 .and. rlen.le.1.D07) then
   call lap_assign_para(wghts,wghts_init_n21_7E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n21_7E06,nlap)
  else if (rlen.gt.1.D07 .and. rlen.le.2.D07) then
   call lap_assign_para(wghts,wghts_init_n21_1E07,nlap)
   call lap_assign_para(xpnts,xpnts_init_n21_1E07,nlap)
  else if (rlen.gt.2.D07 .and. rlen.le.3.D07) then
   call lap_assign_para(wghts,wghts_init_n21_2E07,nlap)
   call lap_assign_para(xpnts,xpnts_init_n21_2E07,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n21_3E07,nlap)
   call lap_assign_para(xpnts,xpnts_init_n21_3E07,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (22)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.1.D02) then
   call lap_assign_para(wghts,wghts_init_n22_1E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n22_1E02,nlap)
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then
   call lap_assign_para(wghts,wghts_init_n22_1E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n22_1E02,nlap)
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
   call lap_assign_para(wghts,wghts_init_n22_2E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n22_2E02,nlap)
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
   call lap_assign_para(wghts,wghts_init_n22_3E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n22_3E02,nlap)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   call lap_assign_para(wghts,wghts_init_n22_4E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n22_4E02,nlap)
  else if (rlen.gt.5.D02 .and. rlen.le.7.D02) then
   call lap_assign_para(wghts,wghts_init_n22_5E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n22_5E02,nlap)
  else if (rlen.gt.7.D02 .and. rlen.le.1.D03) then
   call lap_assign_para(wghts,wghts_init_n22_7E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n22_7E02,nlap)
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
   call lap_assign_para(wghts,wghts_init_n22_1E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n22_1E03,nlap)
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then
   call lap_assign_para(wghts,wghts_init_n22_2E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n22_2E03,nlap)
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then
   call lap_assign_para(wghts,wghts_init_n22_3E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n22_3E03,nlap)
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then
   call lap_assign_para(wghts,wghts_init_n22_4E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n22_4E03,nlap)
  else if (rlen.gt.5.D03 .and. rlen.le.7.D03) then
   call lap_assign_para(wghts,wghts_init_n22_5E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n22_5E03,nlap)
  else if (rlen.gt.7.D03 .and. rlen.le.1.D04) then
   call lap_assign_para(wghts,wghts_init_n22_7E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n22_7E03,nlap)
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then
   call lap_assign_para(wghts,wghts_init_n22_1E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n22_1E04,nlap)
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then
   call lap_assign_para(wghts,wghts_init_n22_2E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n22_2E04,nlap)
  else if (rlen.gt.3.D04 .and. rlen.le.4.D04) then
   call lap_assign_para(wghts,wghts_init_n22_3E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n22_3E04,nlap)
  else if (rlen.gt.4.D04 .and. rlen.le.5.D04) then
   call lap_assign_para(wghts,wghts_init_n22_4E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n22_4E04,nlap)
  else if (rlen.gt.5.D04 .and. rlen.le.7.D04) then
   call lap_assign_para(wghts,wghts_init_n22_5E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n22_5E04,nlap)
  else if (rlen.gt.7.D04 .and. rlen.le.1.D05) then
   call lap_assign_para(wghts,wghts_init_n22_7E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n22_7E04,nlap)
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then
   call lap_assign_para(wghts,wghts_init_n22_1E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n22_1E05,nlap)
  else if (rlen.gt.2.D05 .and. rlen.le.3.D05) then
   call lap_assign_para(wghts,wghts_init_n22_2E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n22_2E05,nlap)
  else if (rlen.gt.3.D05 .and. rlen.le.4.D05) then
   call lap_assign_para(wghts,wghts_init_n22_3E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n22_3E05,nlap)
  else if (rlen.gt.4.D05 .and. rlen.le.5.D05) then
   call lap_assign_para(wghts,wghts_init_n22_4E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n22_4E05,nlap)
  else if (rlen.gt.5.D05 .and. rlen.le.7.D05) then
   call lap_assign_para(wghts,wghts_init_n22_5E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n22_5E05,nlap)
  else if (rlen.gt.7.D05 .and. rlen.le.1.D06) then
   call lap_assign_para(wghts,wghts_init_n22_7E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n22_7E05,nlap)
  else if (rlen.gt.1.D06 .and. rlen.le.2.D06) then
   call lap_assign_para(wghts,wghts_init_n22_1E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n22_1E06,nlap)
  else if (rlen.gt.2.D06 .and. rlen.le.3.D06) then
   call lap_assign_para(wghts,wghts_init_n22_2E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n22_2E06,nlap)
  else if (rlen.gt.3.D06 .and. rlen.le.4.D06) then
   call lap_assign_para(wghts,wghts_init_n22_3E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n22_3E06,nlap)
  else if (rlen.gt.4.D06 .and. rlen.le.5.D06) then
   call lap_assign_para(wghts,wghts_init_n22_4E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n22_4E06,nlap)
  else if (rlen.gt.5.D06 .and. rlen.le.7.D06) then
   call lap_assign_para(wghts,wghts_init_n22_5E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n22_5E06,nlap)
  else if (rlen.gt.7.D06 .and. rlen.le.1.D07) then
   call lap_assign_para(wghts,wghts_init_n22_7E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n22_7E06,nlap)
  else if (rlen.gt.1.D07 .and. rlen.le.2.D07) then
   call lap_assign_para(wghts,wghts_init_n22_1E07,nlap)
   call lap_assign_para(xpnts,xpnts_init_n22_1E07,nlap)
  else if (rlen.gt.2.D07 .and. rlen.le.3.D07) then
   call lap_assign_para(wghts,wghts_init_n22_2E07,nlap)
   call lap_assign_para(xpnts,xpnts_init_n22_2E07,nlap)
  else if (rlen.gt.3.D07 .and. rlen.le.4.D07) then
   call lap_assign_para(wghts,wghts_init_n22_3E07,nlap)
   call lap_assign_para(xpnts,xpnts_init_n22_3E07,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n22_4E07,nlap)
   call lap_assign_para(xpnts,xpnts_init_n22_4E07,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (23)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.2.D02) then
   call lap_assign_para(wghts,wghts_init_n23_2E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n23_2E02,nlap)
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
   call lap_assign_para(wghts,wghts_init_n23_2E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n23_2E02,nlap)
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
   call lap_assign_para(wghts,wghts_init_n23_3E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n23_3E02,nlap)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   call lap_assign_para(wghts,wghts_init_n23_4E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n23_4E02,nlap)
  else if (rlen.gt.5.D02 .and. rlen.le.7.D02) then
   call lap_assign_para(wghts,wghts_init_n23_5E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n23_5E02,nlap)
  else if (rlen.gt.7.D02 .and. rlen.le.1.D03) then
   call lap_assign_para(wghts,wghts_init_n23_7E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n23_7E02,nlap)
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
   call lap_assign_para(wghts,wghts_init_n23_1E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n23_1E03,nlap)
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then
   call lap_assign_para(wghts,wghts_init_n23_2E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n23_2E03,nlap)
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then
   call lap_assign_para(wghts,wghts_init_n23_3E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n23_3E03,nlap)
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then
   call lap_assign_para(wghts,wghts_init_n23_4E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n23_4E03,nlap)
  else if (rlen.gt.5.D03 .and. rlen.le.7.D03) then
   call lap_assign_para(wghts,wghts_init_n23_5E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n23_5E03,nlap)
  else if (rlen.gt.7.D03 .and. rlen.le.1.D04) then
   call lap_assign_para(wghts,wghts_init_n23_7E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n23_7E03,nlap)
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then
   call lap_assign_para(wghts,wghts_init_n23_1E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n23_1E04,nlap)
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then
   call lap_assign_para(wghts,wghts_init_n23_2E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n23_2E04,nlap)
  else if (rlen.gt.3.D04 .and. rlen.le.4.D04) then
   call lap_assign_para(wghts,wghts_init_n23_3E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n23_3E04,nlap)
  else if (rlen.gt.4.D04 .and. rlen.le.5.D04) then
   call lap_assign_para(wghts,wghts_init_n23_4E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n23_4E04,nlap)
  else if (rlen.gt.5.D04 .and. rlen.le.7.D04) then
   call lap_assign_para(wghts,wghts_init_n23_5E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n23_5E04,nlap)
  else if (rlen.gt.7.D04 .and. rlen.le.1.D05) then
   call lap_assign_para(wghts,wghts_init_n23_7E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n23_7E04,nlap)
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then
   call lap_assign_para(wghts,wghts_init_n23_1E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n23_1E05,nlap)
  else if (rlen.gt.2.D05 .and. rlen.le.3.D05) then
   call lap_assign_para(wghts,wghts_init_n23_2E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n23_2E05,nlap)
  else if (rlen.gt.3.D05 .and. rlen.le.4.D05) then
   call lap_assign_para(wghts,wghts_init_n23_3E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n23_3E05,nlap)
  else if (rlen.gt.4.D05 .and. rlen.le.5.D05) then
   call lap_assign_para(wghts,wghts_init_n23_4E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n23_4E05,nlap)
  else if (rlen.gt.5.D05 .and. rlen.le.7.D05) then
   call lap_assign_para(wghts,wghts_init_n23_5E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n23_5E05,nlap)
  else if (rlen.gt.7.D05 .and. rlen.le.1.D06) then
   call lap_assign_para(wghts,wghts_init_n23_7E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n23_7E05,nlap)
  else if (rlen.gt.1.D06 .and. rlen.le.2.D06) then
   call lap_assign_para(wghts,wghts_init_n23_1E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n23_1E06,nlap)
  else if (rlen.gt.2.D06 .and. rlen.le.3.D06) then
   call lap_assign_para(wghts,wghts_init_n23_2E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n23_2E06,nlap)
  else if (rlen.gt.3.D06 .and. rlen.le.4.D06) then
   call lap_assign_para(wghts,wghts_init_n23_3E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n23_3E06,nlap)
  else if (rlen.gt.4.D06 .and. rlen.le.5.D06) then
   call lap_assign_para(wghts,wghts_init_n23_4E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n23_4E06,nlap)
  else if (rlen.gt.5.D06 .and. rlen.le.7.D06) then
   call lap_assign_para(wghts,wghts_init_n23_5E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n23_5E06,nlap)
  else if (rlen.gt.7.D06 .and. rlen.le.1.D07) then
   call lap_assign_para(wghts,wghts_init_n23_7E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n23_7E06,nlap)
  else if (rlen.gt.1.D07 .and. rlen.le.2.D07) then
   call lap_assign_para(wghts,wghts_init_n23_1E07,nlap)
   call lap_assign_para(xpnts,xpnts_init_n23_1E07,nlap)
  else if (rlen.gt.2.D07 .and. rlen.le.3.D07) then
   call lap_assign_para(wghts,wghts_init_n23_2E07,nlap)
   call lap_assign_para(xpnts,xpnts_init_n23_2E07,nlap)
  else if (rlen.gt.3.D07 .and. rlen.le.4.D07) then
   call lap_assign_para(wghts,wghts_init_n23_3E07,nlap)
   call lap_assign_para(xpnts,xpnts_init_n23_3E07,nlap)
  else if (rlen.gt.4.D07 .and. rlen.le.5.D07) then
   call lap_assign_para(wghts,wghts_init_n23_4E07,nlap)
   call lap_assign_para(xpnts,xpnts_init_n23_4E07,nlap)
  else if (rlen.gt.5.D07 .and. rlen.le.7.D07) then
   call lap_assign_para(wghts,wghts_init_n23_5E07,nlap)
   call lap_assign_para(xpnts,xpnts_init_n23_5E07,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n23_7E07,nlap)
   call lap_assign_para(xpnts,xpnts_init_n23_7E07,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (24)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.2.D02) then
   call lap_assign_para(wghts,wghts_init_n24_2E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n24_2E02,nlap)
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
   call lap_assign_para(wghts,wghts_init_n24_2E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n24_2E02,nlap)
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
   call lap_assign_para(wghts,wghts_init_n24_3E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n24_3E02,nlap)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   call lap_assign_para(wghts,wghts_init_n24_4E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n24_4E02,nlap)
  else if (rlen.gt.5.D02 .and. rlen.le.7.D02) then
   call lap_assign_para(wghts,wghts_init_n24_5E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n24_5E02,nlap)
  else if (rlen.gt.7.D02 .and. rlen.le.1.D03) then
   call lap_assign_para(wghts,wghts_init_n24_7E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n24_7E02,nlap)
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
   call lap_assign_para(wghts,wghts_init_n24_1E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n24_1E03,nlap)
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then
   call lap_assign_para(wghts,wghts_init_n24_2E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n24_2E03,nlap)
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then
   call lap_assign_para(wghts,wghts_init_n24_3E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n24_3E03,nlap)
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then
   call lap_assign_para(wghts,wghts_init_n24_4E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n24_4E03,nlap)
  else if (rlen.gt.5.D03 .and. rlen.le.7.D03) then
   call lap_assign_para(wghts,wghts_init_n24_5E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n24_5E03,nlap)
  else if (rlen.gt.7.D03 .and. rlen.le.1.D04) then
   call lap_assign_para(wghts,wghts_init_n24_7E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n24_7E03,nlap)
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then
   call lap_assign_para(wghts,wghts_init_n24_1E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n24_1E04,nlap)
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then
   call lap_assign_para(wghts,wghts_init_n24_2E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n24_2E04,nlap)
  else if (rlen.gt.3.D04 .and. rlen.le.4.D04) then
   call lap_assign_para(wghts,wghts_init_n24_3E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n24_3E04,nlap)
  else if (rlen.gt.4.D04 .and. rlen.le.5.D04) then
   call lap_assign_para(wghts,wghts_init_n24_4E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n24_4E04,nlap)
  else if (rlen.gt.5.D04 .and. rlen.le.7.D04) then
   call lap_assign_para(wghts,wghts_init_n24_5E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n24_5E04,nlap)
  else if (rlen.gt.7.D04 .and. rlen.le.1.D05) then
   call lap_assign_para(wghts,wghts_init_n24_7E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n24_7E04,nlap)
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then
   call lap_assign_para(wghts,wghts_init_n24_1E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n24_1E05,nlap)
  else if (rlen.gt.2.D05 .and. rlen.le.3.D05) then
   call lap_assign_para(wghts,wghts_init_n24_2E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n24_2E05,nlap)
  else if (rlen.gt.3.D05 .and. rlen.le.4.D05) then
   call lap_assign_para(wghts,wghts_init_n24_3E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n24_3E05,nlap)
  else if (rlen.gt.4.D05 .and. rlen.le.5.D05) then
   call lap_assign_para(wghts,wghts_init_n24_4E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n24_4E05,nlap)
  else if (rlen.gt.5.D05 .and. rlen.le.7.D05) then
   call lap_assign_para(wghts,wghts_init_n24_5E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n24_5E05,nlap)
  else if (rlen.gt.7.D05 .and. rlen.le.1.D06) then
   call lap_assign_para(wghts,wghts_init_n24_7E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n24_7E05,nlap)
  else if (rlen.gt.1.D06 .and. rlen.le.2.D06) then
   call lap_assign_para(wghts,wghts_init_n24_1E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n24_1E06,nlap)
  else if (rlen.gt.2.D06 .and. rlen.le.3.D06) then
   call lap_assign_para(wghts,wghts_init_n24_2E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n24_2E06,nlap)
  else if (rlen.gt.3.D06 .and. rlen.le.4.D06) then
   call lap_assign_para(wghts,wghts_init_n24_3E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n24_3E06,nlap)
  else if (rlen.gt.4.D06 .and. rlen.le.5.D06) then
   call lap_assign_para(wghts,wghts_init_n24_4E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n24_4E06,nlap)
  else if (rlen.gt.5.D06 .and. rlen.le.7.D06) then
   call lap_assign_para(wghts,wghts_init_n24_5E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n24_5E06,nlap)
  else if (rlen.gt.7.D06 .and. rlen.le.1.D07) then
   call lap_assign_para(wghts,wghts_init_n24_7E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n24_7E06,nlap)
  else if (rlen.gt.1.D07 .and. rlen.le.2.D07) then
   call lap_assign_para(wghts,wghts_init_n24_1E07,nlap)
   call lap_assign_para(xpnts,xpnts_init_n24_1E07,nlap)
  else if (rlen.gt.2.D07 .and. rlen.le.3.D07) then
   call lap_assign_para(wghts,wghts_init_n24_2E07,nlap)
   call lap_assign_para(xpnts,xpnts_init_n24_2E07,nlap)
  else if (rlen.gt.3.D07 .and. rlen.le.4.D07) then
   call lap_assign_para(wghts,wghts_init_n24_3E07,nlap)
   call lap_assign_para(xpnts,xpnts_init_n24_3E07,nlap)
  else if (rlen.gt.4.D07 .and. rlen.le.5.D07) then
   call lap_assign_para(wghts,wghts_init_n24_4E07,nlap)
   call lap_assign_para(xpnts,xpnts_init_n24_4E07,nlap)
  else if (rlen.gt.5.D07 .and. rlen.le.7.D07) then
   call lap_assign_para(wghts,wghts_init_n24_5E07,nlap)
   call lap_assign_para(xpnts,xpnts_init_n24_5E07,nlap)
  else if (rlen.gt.7.D07 .and. rlen.le.1.D08) then
   call lap_assign_para(wghts,wghts_init_n24_7E07,nlap)
   call lap_assign_para(xpnts,xpnts_init_n24_7E07,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n24_1E08,nlap)
   call lap_assign_para(xpnts,xpnts_init_n24_1E08,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (25)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.2.D02) then
   call lap_assign_para(wghts,wghts_init_n25_2E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n25_2E02,nlap)
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
   call lap_assign_para(wghts,wghts_init_n25_2E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n25_2E02,nlap)
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
   call lap_assign_para(wghts,wghts_init_n25_3E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n25_3E02,nlap)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   call lap_assign_para(wghts,wghts_init_n25_4E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n25_4E02,nlap)
  else if (rlen.gt.5.D02 .and. rlen.le.7.D02) then
   call lap_assign_para(wghts,wghts_init_n25_5E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n25_5E02,nlap)
  else if (rlen.gt.7.D02 .and. rlen.le.1.D03) then
   call lap_assign_para(wghts,wghts_init_n25_7E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n25_7E02,nlap)
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
   call lap_assign_para(wghts,wghts_init_n25_1E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n25_1E03,nlap)
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then
   call lap_assign_para(wghts,wghts_init_n25_2E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n25_2E03,nlap)
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then
   call lap_assign_para(wghts,wghts_init_n25_3E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n25_3E03,nlap)
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then
   call lap_assign_para(wghts,wghts_init_n25_4E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n25_4E03,nlap)
  else if (rlen.gt.5.D03 .and. rlen.le.6.D03) then
   call lap_assign_para(wghts,wghts_init_n25_5E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n25_5E03,nlap)
  else if (rlen.gt.6.D03 .and. rlen.le.7.D03) then
   call lap_assign_para(wghts,wghts_init_n25_6E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n25_6E03,nlap)
  else if (rlen.gt.7.D03 .and. rlen.le.8.D03) then
   call lap_assign_para(wghts,wghts_init_n25_7E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n25_7E03,nlap)
  else if (rlen.gt.8.D03 .and. rlen.le.9.D03) then
   call lap_assign_para(wghts,wghts_init_n25_8E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n25_8E03,nlap)
  else if (rlen.gt.9.D03 .and. rlen.le.1.D04) then
   call lap_assign_para(wghts,wghts_init_n25_9E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n25_9E03,nlap)
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then
   call lap_assign_para(wghts,wghts_init_n25_1E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n25_1E04,nlap)
  else if (rlen.gt.2.D04 .and. rlen.le.5.D04) then
   call lap_assign_para(wghts,wghts_init_n25_2E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n25_2E04,nlap)
  else if (rlen.gt.5.D04 .and. rlen.le.1.D05) then
   call lap_assign_para(wghts,wghts_init_n25_5E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n25_5E04,nlap)
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then
   call lap_assign_para(wghts,wghts_init_n25_1E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n25_1E05,nlap)
  else if (rlen.gt.2.D05 .and. rlen.le.5.D05) then
   call lap_assign_para(wghts,wghts_init_n25_2E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n25_2E05,nlap)
  else if (rlen.gt.5.D05 .and. rlen.le.1.D06) then
   call lap_assign_para(wghts,wghts_init_n25_5E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n25_5E05,nlap)
  else if (rlen.gt.1.D06 .and. rlen.le.2.D06) then
   call lap_assign_para(wghts,wghts_init_n25_1E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n25_1E06,nlap)
  else if (rlen.gt.2.D06 .and. rlen.le.5.D06) then
   call lap_assign_para(wghts,wghts_init_n25_2E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n25_2E06,nlap)
  else if (rlen.gt.5.D06 .and. rlen.le.1.D07) then
   call lap_assign_para(wghts,wghts_init_n25_5E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n25_5E06,nlap)
  else if (rlen.gt.1.D07 .and. rlen.le.5.D07) then
   call lap_assign_para(wghts,wghts_init_n25_1E07,nlap)
   call lap_assign_para(xpnts,xpnts_init_n25_1E07,nlap)
  else if (rlen.gt.5.D07 .and. rlen.le.1.D08) then
   call lap_assign_para(wghts,wghts_init_n25_5E07,nlap)
   call lap_assign_para(xpnts,xpnts_init_n25_5E07,nlap)
  else if (rlen.gt.1.D08 .and. rlen.le.2.D08) then
   call lap_assign_para(wghts,wghts_init_n25_1E08,nlap)
   call lap_assign_para(xpnts,xpnts_init_n25_1E08,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n25_2E08,nlap)
   call lap_assign_para(xpnts,xpnts_init_n25_2E08,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (26)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.4.D02) then
   call lap_assign_para(wghts,wghts_init_n26_4E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_4E02,nlap)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   call lap_assign_para(wghts,wghts_init_n26_4E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_4E02,nlap)
  else if (rlen.gt.5.D02 .and. rlen.le.6.D02) then
   call lap_assign_para(wghts,wghts_init_n26_5E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_5E02,nlap)
  else if (rlen.gt.6.D02 .and. rlen.le.7.D02) then
   call lap_assign_para(wghts,wghts_init_n26_6E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_6E02,nlap)
  else if (rlen.gt.7.D02 .and. rlen.le.8.D02) then
   call lap_assign_para(wghts,wghts_init_n26_7E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_7E02,nlap)
  else if (rlen.gt.8.D02 .and. rlen.le.9.D02) then
   call lap_assign_para(wghts,wghts_init_n26_8E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_8E02,nlap)
  else if (rlen.gt.9.D02 .and. rlen.le.1.D03) then
   call lap_assign_para(wghts,wghts_init_n26_9E02,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_9E02,nlap)
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
   call lap_assign_para(wghts,wghts_init_n26_1E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_1E03,nlap)
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then
   call lap_assign_para(wghts,wghts_init_n26_2E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_2E03,nlap)
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then
   call lap_assign_para(wghts,wghts_init_n26_3E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_3E03,nlap)
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then
   call lap_assign_para(wghts,wghts_init_n26_4E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_4E03,nlap)
  else if (rlen.gt.5.D03 .and. rlen.le.6.D03) then
   call lap_assign_para(wghts,wghts_init_n26_5E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_5E03,nlap)
  else if (rlen.gt.6.D03 .and. rlen.le.7.D03) then
   call lap_assign_para(wghts,wghts_init_n26_6E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_6E03,nlap)
  else if (rlen.gt.7.D03 .and. rlen.le.8.D03) then
   call lap_assign_para(wghts,wghts_init_n26_7E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_7E03,nlap)
  else if (rlen.gt.8.D03 .and. rlen.le.9.D03) then
   call lap_assign_para(wghts,wghts_init_n26_8E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_8E03,nlap)
  else if (rlen.gt.9.D03 .and. rlen.le.1.D04) then
   call lap_assign_para(wghts,wghts_init_n26_9E03,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_9E03,nlap)
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then
   call lap_assign_para(wghts,wghts_init_n26_1E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_1E04,nlap)
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then
   call lap_assign_para(wghts,wghts_init_n26_2E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_2E04,nlap)
  else if (rlen.gt.3.D04 .and. rlen.le.4.D04) then
   call lap_assign_para(wghts,wghts_init_n26_3E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_3E04,nlap)
  else if (rlen.gt.4.D04 .and. rlen.le.5.D04) then
   call lap_assign_para(wghts,wghts_init_n26_4E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_4E04,nlap)
  else if (rlen.gt.5.D04 .and. rlen.le.6.D04) then
   call lap_assign_para(wghts,wghts_init_n26_5E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_5E04,nlap)
  else if (rlen.gt.6.D04 .and. rlen.le.7.D04) then
   call lap_assign_para(wghts,wghts_init_n26_6E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_6E04,nlap)
  else if (rlen.gt.7.D04 .and. rlen.le.8.D04) then
   call lap_assign_para(wghts,wghts_init_n26_7E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_7E04,nlap)
  else if (rlen.gt.8.D04 .and. rlen.le.9.D04) then
   call lap_assign_para(wghts,wghts_init_n26_8E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_8E04,nlap)
  else if (rlen.gt.9.D04 .and. rlen.le.1.D05) then
   call lap_assign_para(wghts,wghts_init_n26_9E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_9E04,nlap)
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then
   call lap_assign_para(wghts,wghts_init_n26_1E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_1E05,nlap)
  else if (rlen.gt.2.D05 .and. rlen.le.3.D05) then
   call lap_assign_para(wghts,wghts_init_n26_2E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_2E05,nlap)
  else if (rlen.gt.3.D05 .and. rlen.le.4.D05) then
   call lap_assign_para(wghts,wghts_init_n26_3E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_3E05,nlap)
  else if (rlen.gt.4.D05 .and. rlen.le.5.D05) then
   call lap_assign_para(wghts,wghts_init_n26_4E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_4E05,nlap)
  else if (rlen.gt.5.D05 .and. rlen.le.7.D05) then
   call lap_assign_para(wghts,wghts_init_n26_5E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_5E05,nlap)
  else if (rlen.gt.7.D05 .and. rlen.le.1.D06) then
   call lap_assign_para(wghts,wghts_init_n26_7E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_7E05,nlap)
  else if (rlen.gt.1.D06 .and. rlen.le.2.D06) then
   call lap_assign_para(wghts,wghts_init_n26_1E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_1E06,nlap)
  else if (rlen.gt.2.D06 .and. rlen.le.3.D06) then
   call lap_assign_para(wghts,wghts_init_n26_2E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_2E06,nlap)
  else if (rlen.gt.3.D06 .and. rlen.le.4.D06) then
   call lap_assign_para(wghts,wghts_init_n26_3E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_3E06,nlap)
  else if (rlen.gt.4.D06 .and. rlen.le.5.D06) then
   call lap_assign_para(wghts,wghts_init_n26_4E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_4E06,nlap)
  else if (rlen.gt.5.D06 .and. rlen.le.7.D06) then
   call lap_assign_para(wghts,wghts_init_n26_5E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_5E06,nlap)
  else if (rlen.gt.7.D06 .and. rlen.le.1.D07) then
   call lap_assign_para(wghts,wghts_init_n26_7E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_7E06,nlap)
  else if (rlen.gt.1.D07 .and. rlen.le.2.D07) then
   call lap_assign_para(wghts,wghts_init_n26_1E07,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_1E07,nlap)
  else if (rlen.gt.2.D07 .and. rlen.le.3.D07) then
   call lap_assign_para(wghts,wghts_init_n26_2E07,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_2E07,nlap)
  else if (rlen.gt.3.D07 .and. rlen.le.4.D07) then
   call lap_assign_para(wghts,wghts_init_n26_3E07,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_3E07,nlap)
  else if (rlen.gt.4.D07 .and. rlen.le.5.D07) then
   call lap_assign_para(wghts,wghts_init_n26_4E07,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_4E07,nlap)
  else if (rlen.gt.5.D07 .and. rlen.le.7.D07) then
   call lap_assign_para(wghts,wghts_init_n26_5E07,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_5E07,nlap)
  else if (rlen.gt.7.D07 .and. rlen.le.1.D08) then
   call lap_assign_para(wghts,wghts_init_n26_7E07,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_7E07,nlap)
  else if (rlen.gt.1.D08 .and. rlen.le.2.D08) then
   call lap_assign_para(wghts,wghts_init_n26_1E08,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_1E08,nlap)
  else if (rlen.gt.2.D08 .and. rlen.le.3.D08) then
   call lap_assign_para(wghts,wghts_init_n26_2E08,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_2E08,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n26_3E08,nlap)
   call lap_assign_para(xpnts,xpnts_init_n26_3E08,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (27)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.1.D04) then
   call lap_assign_para(wghts,wghts_init_n27_1E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n27_1E04,nlap)
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then
   call lap_assign_para(wghts,wghts_init_n27_1E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n27_1E04,nlap)
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then
   call lap_assign_para(wghts,wghts_init_n27_2E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n27_2E04,nlap)
  else if (rlen.gt.3.D04 .and. rlen.le.4.D04) then
   call lap_assign_para(wghts,wghts_init_n27_3E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n27_3E04,nlap)
  else if (rlen.gt.4.D04 .and. rlen.le.5.D04) then
   call lap_assign_para(wghts,wghts_init_n27_4E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n27_4E04,nlap)
  else if (rlen.gt.5.D04 .and. rlen.le.6.D04) then
   call lap_assign_para(wghts,wghts_init_n27_5E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n27_5E04,nlap)
  else if (rlen.gt.6.D04 .and. rlen.le.7.D04) then
   call lap_assign_para(wghts,wghts_init_n27_6E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n27_6E04,nlap)
  else if (rlen.gt.7.D04 .and. rlen.le.8.D04) then
   call lap_assign_para(wghts,wghts_init_n27_7E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n27_7E04,nlap)
  else if (rlen.gt.8.D04 .and. rlen.le.9.D04) then
   call lap_assign_para(wghts,wghts_init_n27_8E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n27_8E04,nlap)
  else if (rlen.gt.9.D04 .and. rlen.le.1.D05) then
   call lap_assign_para(wghts,wghts_init_n27_9E04,nlap)
   call lap_assign_para(xpnts,xpnts_init_n27_9E04,nlap)
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then
   call lap_assign_para(wghts,wghts_init_n27_1E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n27_1E05,nlap)
  else if (rlen.gt.2.D05 .and. rlen.le.3.D05) then
   call lap_assign_para(wghts,wghts_init_n27_2E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n27_2E05,nlap)
  else if (rlen.gt.3.D05 .and. rlen.le.4.D05) then
   call lap_assign_para(wghts,wghts_init_n27_3E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n27_3E05,nlap)
  else if (rlen.gt.4.D05 .and. rlen.le.5.D05) then
   call lap_assign_para(wghts,wghts_init_n27_4E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n27_4E05,nlap)
  else if (rlen.gt.5.D05 .and. rlen.le.6.D05) then
   call lap_assign_para(wghts,wghts_init_n27_5E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n27_5E05,nlap)
  else if (rlen.gt.6.D05 .and. rlen.le.7.D05) then
   call lap_assign_para(wghts,wghts_init_n27_6E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n27_6E05,nlap)
  else if (rlen.gt.7.D05 .and. rlen.le.8.D05) then
   call lap_assign_para(wghts,wghts_init_n27_7E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n27_7E05,nlap)
  else if (rlen.gt.8.D05 .and. rlen.le.9.D05) then
   call lap_assign_para(wghts,wghts_init_n27_8E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n27_8E05,nlap)
  else if (rlen.gt.9.D05 .and. rlen.le.1.D06) then
   call lap_assign_para(wghts,wghts_init_n27_9E05,nlap)
   call lap_assign_para(xpnts,xpnts_init_n27_9E05,nlap)
  else if (rlen.gt.1.D06 .and. rlen.le.2.D06) then
   call lap_assign_para(wghts,wghts_init_n27_1E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n27_1E06,nlap)
  else if (rlen.gt.2.D06 .and. rlen.le.3.D06) then
   call lap_assign_para(wghts,wghts_init_n27_2E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n27_2E06,nlap)
  else if (rlen.gt.3.D06 .and. rlen.le.4.D06) then
   call lap_assign_para(wghts,wghts_init_n27_3E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n27_3E06,nlap)
  else if (rlen.gt.4.D06 .and. rlen.le.5.D06) then
   call lap_assign_para(wghts,wghts_init_n27_4E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n27_4E06,nlap)
  else if (rlen.gt.5.D06 .and. rlen.le.6.D06) then
   call lap_assign_para(wghts,wghts_init_n27_5E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n27_5E06,nlap)
  else if (rlen.gt.6.D06 .and. rlen.le.7.D06) then
   call lap_assign_para(wghts,wghts_init_n27_6E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n27_6E06,nlap)
  else if (rlen.gt.7.D06 .and. rlen.le.8.D06) then
   call lap_assign_para(wghts,wghts_init_n27_7E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n27_7E06,nlap)
  else if (rlen.gt.8.D06 .and. rlen.le.9.D06) then
   call lap_assign_para(wghts,wghts_init_n27_8E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n27_8E06,nlap)
  else if (rlen.gt.9.D06 .and. rlen.le.1.D07) then
   call lap_assign_para(wghts,wghts_init_n27_9E06,nlap)
   call lap_assign_para(xpnts,xpnts_init_n27_9E06,nlap)
  else if (rlen.gt.1.D07 .and. rlen.le.2.D07) then
   call lap_assign_para(wghts,wghts_init_n27_1E07,nlap)
   call lap_assign_para(xpnts,xpnts_init_n27_1E07,nlap)
  else if (rlen.gt.2.D07 .and. rlen.le.3.D07) then
   call lap_assign_para(wghts,wghts_init_n27_2E07,nlap)
   call lap_assign_para(xpnts,xpnts_init_n27_2E07,nlap)
  else if (rlen.gt.3.D07 .and. rlen.le.4.D07) then
   call lap_assign_para(wghts,wghts_init_n27_3E07,nlap)
   call lap_assign_para(xpnts,xpnts_init_n27_3E07,nlap)
  else if (rlen.gt.4.D07 .and. rlen.le.5.D07) then
   call lap_assign_para(wghts,wghts_init_n27_4E07,nlap)
   call lap_assign_para(xpnts,xpnts_init_n27_4E07,nlap)
  else if (rlen.gt.5.D07 .and. rlen.le.7.D07) then
   call lap_assign_para(wghts,wghts_init_n27_5E07,nlap)
   call lap_assign_para(xpnts,xpnts_init_n27_5E07,nlap)
  else if (rlen.gt.7.D07 .and. rlen.le.1.D08) then
   call lap_assign_para(wghts,wghts_init_n27_7E07,nlap)
   call lap_assign_para(xpnts,xpnts_init_n27_7E07,nlap)
  else if (rlen.gt.1.D08 .and. rlen.le.2.D08) then
   call lap_assign_para(wghts,wghts_init_n27_1E08,nlap)
   call lap_assign_para(xpnts,xpnts_init_n27_1E08,nlap)
  else if (rlen.gt.2.D08 .and. rlen.le.3.D08) then
   call lap_assign_para(wghts,wghts_init_n27_2E08,nlap)
   call lap_assign_para(xpnts,xpnts_init_n27_2E08,nlap)
  else if (rlen.gt.3.D08 .and. rlen.le.4.D08) then
   call lap_assign_para(wghts,wghts_init_n27_3E08,nlap)
   call lap_assign_para(xpnts,xpnts_init_n27_3E08,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n27_4E08,nlap)
   call lap_assign_para(xpnts,xpnts_init_n27_4E08,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (28)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.1.D08) then
   call lap_assign_para(wghts,wghts_init_n28_1E08,nlap)
   call lap_assign_para(xpnts,xpnts_init_n28_1E08,nlap)
  else if (rlen.gt.1.D08 .and. rlen.le.2.D08) then
   call lap_assign_para(wghts,wghts_init_n28_1E08,nlap)
   call lap_assign_para(xpnts,xpnts_init_n28_1E08,nlap)
  else if (rlen.gt.2.D08 .and. rlen.le.3.D08) then
   call lap_assign_para(wghts,wghts_init_n28_2E08,nlap)
   call lap_assign_para(xpnts,xpnts_init_n28_2E08,nlap)
  else if (rlen.gt.3.D08 .and. rlen.le.4.D08) then
   call lap_assign_para(wghts,wghts_init_n28_3E08,nlap)
   call lap_assign_para(xpnts,xpnts_init_n28_3E08,nlap)
  else if (rlen.gt.4.D08 .and. rlen.le.7.D08) then
   call lap_assign_para(wghts,wghts_init_n28_4E08,nlap)
   call lap_assign_para(xpnts,xpnts_init_n28_4E08,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n28_7E08,nlap)
   call lap_assign_para(xpnts,xpnts_init_n28_7E08,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (29)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.7.D08) then
   call lap_assign_para(wghts,wghts_init_n29_7E08,nlap)
   call lap_assign_para(xpnts,xpnts_init_n29_7E08,nlap)
  else if (rlen.gt.7.D08 .and. rlen.le.1.D09) then
   call lap_assign_para(wghts,wghts_init_n29_7E08,nlap)
   call lap_assign_para(xpnts,xpnts_init_n29_7E08,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n29_1E09,nlap)
   call lap_assign_para(xpnts,xpnts_init_n29_1E09,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (30)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.7.D08) then
   call lap_assign_para(wghts,wghts_init_n30_7E08,nlap)
   call lap_assign_para(xpnts,xpnts_init_n30_7E08,nlap)
  else if (rlen.gt.7.D08 .and. rlen.le.1.D09) then
   call lap_assign_para(wghts,wghts_init_n30_7E08,nlap)
   call lap_assign_para(xpnts,xpnts_init_n30_7E08,nlap)
  else if (rlen.gt.1.D09 .and. rlen.le.2.D09) then
   call lap_assign_para(wghts,wghts_init_n30_1E09,nlap)
   call lap_assign_para(xpnts,xpnts_init_n30_1E09,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n30_2E09,nlap)
   call lap_assign_para(xpnts,xpnts_init_n30_2E09,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (31)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.7.D08) then
   call lap_assign_para(wghts,wghts_init_n31_7E08,nlap)
   call lap_assign_para(xpnts,xpnts_init_n31_7E08,nlap)
  else if (rlen.gt.7.D08 .and. rlen.le.1.D09) then
   call lap_assign_para(wghts,wghts_init_n31_7E08,nlap)
   call lap_assign_para(xpnts,xpnts_init_n31_7E08,nlap)
  else if (rlen.gt.1.D09 .and. rlen.le.2.D09) then
   call lap_assign_para(wghts,wghts_init_n31_1E09,nlap)
   call lap_assign_para(xpnts,xpnts_init_n31_1E09,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n31_2E09,nlap)
   call lap_assign_para(xpnts,xpnts_init_n31_2E09,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (32)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.7.D08) then
   call lap_assign_para(wghts,wghts_init_n32_7E08,nlap)
   call lap_assign_para(xpnts,xpnts_init_n32_7E08,nlap)
  else if (rlen.gt.7.D08 .and. rlen.le.1.D09) then
   call lap_assign_para(wghts,wghts_init_n32_7E08,nlap)
   call lap_assign_para(xpnts,xpnts_init_n32_7E08,nlap)
  else if (rlen.gt.1.D09 .and. rlen.le.2.D09) then
   call lap_assign_para(wghts,wghts_init_n32_1E09,nlap)
   call lap_assign_para(xpnts,xpnts_init_n32_1E09,nlap)
  else if (rlen.gt.2.D09 .and. rlen.le.3.D09) then
   call lap_assign_para(wghts,wghts_init_n32_2E09,nlap)
   call lap_assign_para(xpnts,xpnts_init_n32_2E09,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n32_3E09,nlap)
   call lap_assign_para(xpnts,xpnts_init_n32_3E09,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (33)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.7.D08) then
   call lap_assign_para(wghts,wghts_init_n33_7E08,nlap)
   call lap_assign_para(xpnts,xpnts_init_n33_7E08,nlap)
  else if (rlen.gt.7.D08 .and. rlen.le.1.D09) then
   call lap_assign_para(wghts,wghts_init_n33_7E08,nlap)
   call lap_assign_para(xpnts,xpnts_init_n33_7E08,nlap)
  else if (rlen.gt.1.D09 .and. rlen.le.2.D09) then
   call lap_assign_para(wghts,wghts_init_n33_1E09,nlap)
   call lap_assign_para(xpnts,xpnts_init_n33_1E09,nlap)
  else if (rlen.gt.2.D09 .and. rlen.le.3.D09) then
   call lap_assign_para(wghts,wghts_init_n33_2E09,nlap)
   call lap_assign_para(xpnts,xpnts_init_n33_2E09,nlap)
  else if (rlen.gt.3.D09 .and. rlen.le.4.D09) then
   call lap_assign_para(wghts,wghts_init_n33_3E09,nlap)
   call lap_assign_para(xpnts,xpnts_init_n33_3E09,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n33_4E09,nlap)
   call lap_assign_para(xpnts,xpnts_init_n33_4E09,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (34)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.7.D08) then
   call lap_assign_para(wghts,wghts_init_n34_7E08,nlap)
   call lap_assign_para(xpnts,xpnts_init_n34_7E08,nlap)
  else if (rlen.gt.7.D08 .and. rlen.le.1.D09) then
   call lap_assign_para(wghts,wghts_init_n34_7E08,nlap)
   call lap_assign_para(xpnts,xpnts_init_n34_7E08,nlap)
  else if (rlen.gt.1.D09 .and. rlen.le.2.D09) then
   call lap_assign_para(wghts,wghts_init_n34_1E09,nlap)
   call lap_assign_para(xpnts,xpnts_init_n34_1E09,nlap)
  else if (rlen.gt.2.D09 .and. rlen.le.3.D09) then
   call lap_assign_para(wghts,wghts_init_n34_2E09,nlap)
   call lap_assign_para(xpnts,xpnts_init_n34_2E09,nlap)
  else if (rlen.gt.3.D09 .and. rlen.le.4.D09) then
   call lap_assign_para(wghts,wghts_init_n34_3E09,nlap)
   call lap_assign_para(xpnts,xpnts_init_n34_3E09,nlap)
  else if (rlen.gt.4.D09 .and. rlen.le.5.D09) then
   call lap_assign_para(wghts,wghts_init_n34_4E09,nlap)
   call lap_assign_para(xpnts,xpnts_init_n34_4E09,nlap)
  else if (rlen.gt.5.D09 .and. rlen.le.7.D09) then
   call lap_assign_para(wghts,wghts_init_n34_5E09,nlap)
   call lap_assign_para(xpnts,xpnts_init_n34_5E09,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n34_7E09,nlap)
   call lap_assign_para(xpnts,xpnts_init_n34_7E09,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (35)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.7.D08) then
   call lap_assign_para(wghts,wghts_init_n35_7E08,nlap)
   call lap_assign_para(xpnts,xpnts_init_n35_7E08,nlap)
  else if (rlen.gt.7.D08 .and. rlen.le.1.D09) then
   call lap_assign_para(wghts,wghts_init_n35_7E08,nlap)
   call lap_assign_para(xpnts,xpnts_init_n35_7E08,nlap)
  else if (rlen.gt.1.D09 .and. rlen.le.2.D09) then
   call lap_assign_para(wghts,wghts_init_n35_1E09,nlap)
   call lap_assign_para(xpnts,xpnts_init_n35_1E09,nlap)
  else if (rlen.gt.2.D09 .and. rlen.le.3.D09) then
   call lap_assign_para(wghts,wghts_init_n35_2E09,nlap)
   call lap_assign_para(xpnts,xpnts_init_n35_2E09,nlap)
  else if (rlen.gt.3.D09 .and. rlen.le.5.D09) then
   call lap_assign_para(wghts,wghts_init_n35_3E09,nlap)
   call lap_assign_para(xpnts,xpnts_init_n35_3E09,nlap)
  else if (rlen.gt.5.D09 .and. rlen.le.1.D10) then
   call lap_assign_para(wghts,wghts_init_n35_5E09,nlap)
   call lap_assign_para(xpnts,xpnts_init_n35_5E09,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n35_1E10,nlap)
   call lap_assign_para(xpnts,xpnts_init_n35_1E10,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (36)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.1.D10) then
   call lap_assign_para(wghts,wghts_init_n36_1E10,nlap)
   call lap_assign_para(xpnts,xpnts_init_n36_1E10,nlap)
  else if (rlen.gt.1.D10 .and. rlen.le.2.D10) then
   call lap_assign_para(wghts,wghts_init_n36_1E10,nlap)
   call lap_assign_para(xpnts,xpnts_init_n36_1E10,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n36_2E10,nlap)
   call lap_assign_para(xpnts,xpnts_init_n36_2E10,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (37)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.1.D10) then
   call lap_assign_para(wghts,wghts_init_n37_1E10,nlap)
   call lap_assign_para(xpnts,xpnts_init_n37_1E10,nlap)
  else if (rlen.gt.1.D10 .and. rlen.le.2.D10) then
   call lap_assign_para(wghts,wghts_init_n37_1E10,nlap)
   call lap_assign_para(xpnts,xpnts_init_n37_1E10,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n37_2E10,nlap)
   call lap_assign_para(xpnts,xpnts_init_n37_2E10,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (38)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.1.D10) then
   call lap_assign_para(wghts,wghts_init_n38_1E10,nlap)
   call lap_assign_para(xpnts,xpnts_init_n38_1E10,nlap)
  else if (rlen.gt.1.D10 .and. rlen.le.3.D10) then
   call lap_assign_para(wghts,wghts_init_n38_1E10,nlap)
   call lap_assign_para(xpnts,xpnts_init_n38_1E10,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n38_3E10,nlap)
   call lap_assign_para(xpnts,xpnts_init_n38_3E10,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (39)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.1.D10) then
   call lap_assign_para(wghts,wghts_init_n39_1E10,nlap)
   call lap_assign_para(xpnts,xpnts_init_n39_1E10,nlap)
  else if (rlen.gt.1.D10 .and. rlen.le.2.D10) then
   call lap_assign_para(wghts,wghts_init_n39_1E10,nlap)
   call lap_assign_para(xpnts,xpnts_init_n39_1E10,nlap)
  else if (rlen.gt.2.D10 .and. rlen.le.4.D10) then
   call lap_assign_para(wghts,wghts_init_n39_2E10,nlap)
   call lap_assign_para(xpnts,xpnts_init_n39_2E10,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n39_4E10,nlap)
   call lap_assign_para(xpnts,xpnts_init_n39_4E10,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (40)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.1.D10) then
   call lap_assign_para(wghts,wghts_init_n40_1E10,nlap)
   call lap_assign_para(xpnts,xpnts_init_n40_1E10,nlap)
  else if (rlen.gt.1.D10 .and. rlen.le.2.D10) then
   call lap_assign_para(wghts,wghts_init_n40_1E10,nlap)
   call lap_assign_para(xpnts,xpnts_init_n40_1E10,nlap)
  else if (rlen.gt.2.D10 .and. rlen.le.5.D10) then
   call lap_assign_para(wghts,wghts_init_n40_2E10,nlap)
   call lap_assign_para(xpnts,xpnts_init_n40_2E10,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n40_5E10,nlap)
   call lap_assign_para(xpnts,xpnts_init_n40_5E10,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (41)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.1.D10) then
   call lap_assign_para(wghts,wghts_init_n41_1E10,nlap)
   call lap_assign_para(xpnts,xpnts_init_n41_1E10,nlap)
  else if (rlen.gt.1.D10 .and. rlen.le.2.D10) then
   call lap_assign_para(wghts,wghts_init_n41_1E10,nlap)
   call lap_assign_para(xpnts,xpnts_init_n41_1E10,nlap)
  else if (rlen.gt.2.D10 .and. rlen.le.3.D10) then
   call lap_assign_para(wghts,wghts_init_n41_2E10,nlap)
   call lap_assign_para(xpnts,xpnts_init_n41_2E10,nlap)
  else if (rlen.gt.3.D10 .and. rlen.le.7.D10) then
   call lap_assign_para(wghts,wghts_init_n41_3E10,nlap)
   call lap_assign_para(xpnts,xpnts_init_n41_3E10,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n41_7E10,nlap)
   call lap_assign_para(xpnts,xpnts_init_n41_7E10,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (42)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.1.D10) then
   call lap_assign_para(wghts,wghts_init_n42_1E10,nlap)
   call lap_assign_para(xpnts,xpnts_init_n42_1E10,nlap)
  else if (rlen.gt.1.D10 .and. rlen.le.2.D10) then
   call lap_assign_para(wghts,wghts_init_n42_1E10,nlap)
   call lap_assign_para(xpnts,xpnts_init_n42_1E10,nlap)
  else if (rlen.gt.2.D10 .and. rlen.le.5.D10) then
   call lap_assign_para(wghts,wghts_init_n42_2E10,nlap)
   call lap_assign_para(xpnts,xpnts_init_n42_2E10,nlap)
  else if (rlen.gt.5.D10 .and. rlen.le.1.D11) then
   call lap_assign_para(wghts,wghts_init_n42_5E10,nlap)
   call lap_assign_para(xpnts,xpnts_init_n42_5E10,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n42_1E11,nlap)
   call lap_assign_para(xpnts,xpnts_init_n42_1E11,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (43)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.1.D11) then
   call lap_assign_para(wghts,wghts_init_n43_1E11,nlap)
   call lap_assign_para(xpnts,xpnts_init_n43_1E11,nlap)
  else if (rlen.gt.1.D11 .and. rlen.le.2.D11) then
   call lap_assign_para(wghts,wghts_init_n43_1E11,nlap)
   call lap_assign_para(xpnts,xpnts_init_n43_1E11,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n43_2E11,nlap)
   call lap_assign_para(xpnts,xpnts_init_n43_2E11,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (44)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.1.D11) then
   call lap_assign_para(wghts,wghts_init_n44_1E11,nlap)
   call lap_assign_para(xpnts,xpnts_init_n44_1E11,nlap)
  else if (rlen.gt.1.D11 .and. rlen.le.2.D11) then
   call lap_assign_para(wghts,wghts_init_n44_1E11,nlap)
   call lap_assign_para(xpnts,xpnts_init_n44_1E11,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n44_2E11,nlap)
   call lap_assign_para(xpnts,xpnts_init_n44_2E11,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (45)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.1.D11) then
   call lap_assign_para(wghts,wghts_init_n45_1E11,nlap)
   call lap_assign_para(xpnts,xpnts_init_n45_1E11,nlap)
  else if (rlen.gt.1.D11 .and. rlen.le.3.D11) then
   call lap_assign_para(wghts,wghts_init_n45_1E11,nlap)
   call lap_assign_para(xpnts,xpnts_init_n45_1E11,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n45_3E11,nlap)
   call lap_assign_para(xpnts,xpnts_init_n45_3E11,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (46)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.1.D11) then
   call lap_assign_para(wghts,wghts_init_n46_1E11,nlap)
   call lap_assign_para(xpnts,xpnts_init_n46_1E11,nlap)
  else if (rlen.gt.1.D11 .and. rlen.le.2.D11) then
   call lap_assign_para(wghts,wghts_init_n46_1E11,nlap)
   call lap_assign_para(xpnts,xpnts_init_n46_1E11,nlap)
  else if (rlen.gt.2.D11 .and. rlen.le.4.D11) then
   call lap_assign_para(wghts,wghts_init_n46_2E11,nlap)
   call lap_assign_para(xpnts,xpnts_init_n46_2E11,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n46_4E11,nlap)
   call lap_assign_para(xpnts,xpnts_init_n46_4E11,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (47)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.1.D11) then
   call lap_assign_para(wghts,wghts_init_n47_1E11,nlap)
   call lap_assign_para(xpnts,xpnts_init_n47_1E11,nlap)
  else if (rlen.gt.1.D11 .and. rlen.le.5.D11) then
   call lap_assign_para(wghts,wghts_init_n47_1E11,nlap)
   call lap_assign_para(xpnts,xpnts_init_n47_1E11,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n47_5E11,nlap)
   call lap_assign_para(xpnts,xpnts_init_n47_5E11,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (48)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.1.D11) then
   call lap_assign_para(wghts,wghts_init_n48_1E11,nlap)
   call lap_assign_para(xpnts,xpnts_init_n48_1E11,nlap)
  else if (rlen.gt.1.D11 .and. rlen.le.2.D11) then
   call lap_assign_para(wghts,wghts_init_n48_1E11,nlap)
   call lap_assign_para(xpnts,xpnts_init_n48_1E11,nlap)
  else if (rlen.gt.2.D11 .and. rlen.le.3.D11) then
   call lap_assign_para(wghts,wghts_init_n48_2E11,nlap)
   call lap_assign_para(xpnts,xpnts_init_n48_2E11,nlap)
  else if (rlen.gt.3.D11 .and. rlen.le.7.D11) then
   call lap_assign_para(wghts,wghts_init_n48_3E11,nlap)
   call lap_assign_para(xpnts,xpnts_init_n48_3E11,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n48_7E11,nlap)
   call lap_assign_para(xpnts,xpnts_init_n48_7E11,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (49)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.1.D11) then
   call lap_assign_para(wghts,wghts_init_n49_1E11,nlap)
   call lap_assign_para(xpnts,xpnts_init_n49_1E11,nlap)
  else if (rlen.gt.1.D11 .and. rlen.le.2.D11) then
   call lap_assign_para(wghts,wghts_init_n49_1E11,nlap)
   call lap_assign_para(xpnts,xpnts_init_n49_1E11,nlap)
  else if (rlen.gt.2.D11 .and. rlen.le.3.D11) then
   call lap_assign_para(wghts,wghts_init_n49_2E11,nlap)
   call lap_assign_para(xpnts,xpnts_init_n49_2E11,nlap)
  else if (rlen.gt.3.D11 .and. rlen.le.5.D11) then
   call lap_assign_para(wghts,wghts_init_n49_3E11,nlap)
   call lap_assign_para(xpnts,xpnts_init_n49_3E11,nlap)
  else if (rlen.gt.5.D11 .and. rlen.le.1.D12) then
   call lap_assign_para(wghts,wghts_init_n49_5E11,nlap)
   call lap_assign_para(xpnts,xpnts_init_n49_5E11,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n49_1E12,nlap)
   call lap_assign_para(xpnts,xpnts_init_n49_1E12,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (50)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.2.D08) then
   call lap_assign_para(wghts,wghts_init_n50_2E08,nlap)
   call lap_assign_para(xpnts,xpnts_init_n50_2E08,nlap)
  else if (rlen.gt.2.D08 .and. rlen.le.3.D08) then
   call lap_assign_para(wghts,wghts_init_n50_2E08,nlap)
   call lap_assign_para(xpnts,xpnts_init_n50_2E08,nlap)
  else if (rlen.gt.3.D08 .and. rlen.le.4.D08) then
   call lap_assign_para(wghts,wghts_init_n50_3E08,nlap)
   call lap_assign_para(xpnts,xpnts_init_n50_3E08,nlap)
  else if (rlen.gt.4.D08 .and. rlen.le.5.D08) then
   call lap_assign_para(wghts,wghts_init_n50_4E08,nlap)
   call lap_assign_para(xpnts,xpnts_init_n50_4E08,nlap)
  else if (rlen.gt.5.D08 .and. rlen.le.7.D08) then
   call lap_assign_para(wghts,wghts_init_n50_5E08,nlap)
   call lap_assign_para(xpnts,xpnts_init_n50_5E08,nlap)
  else if (rlen.gt.7.D08 .and. rlen.le.1.D09) then
   call lap_assign_para(wghts,wghts_init_n50_7E08,nlap)
   call lap_assign_para(xpnts,xpnts_init_n50_7E08,nlap)
  else if (rlen.gt.1.D09 .and. rlen.le.2.D09) then
   call lap_assign_para(wghts,wghts_init_n50_1E09,nlap)
   call lap_assign_para(xpnts,xpnts_init_n50_1E09,nlap)
  else if (rlen.gt.2.D09 .and. rlen.le.3.D09) then
   call lap_assign_para(wghts,wghts_init_n50_2E09,nlap)
   call lap_assign_para(xpnts,xpnts_init_n50_2E09,nlap)
  else if (rlen.gt.3.D09 .and. rlen.le.4.D09) then
   call lap_assign_para(wghts,wghts_init_n50_3E09,nlap)
   call lap_assign_para(xpnts,xpnts_init_n50_3E09,nlap)
  else if (rlen.gt.4.D09 .and. rlen.le.5.D09) then
   call lap_assign_para(wghts,wghts_init_n50_4E09,nlap)
   call lap_assign_para(xpnts,xpnts_init_n50_4E09,nlap)
  else if (rlen.gt.5.D09 .and. rlen.le.6.D09) then
   call lap_assign_para(wghts,wghts_init_n50_5E09,nlap)
   call lap_assign_para(xpnts,xpnts_init_n50_5E09,nlap)
  else if (rlen.gt.6.D09 .and. rlen.le.7.D09) then
   call lap_assign_para(wghts,wghts_init_n50_6E09,nlap)
   call lap_assign_para(xpnts,xpnts_init_n50_6E09,nlap)
  else if (rlen.gt.7.D09 .and. rlen.le.1.D10) then
   call lap_assign_para(wghts,wghts_init_n50_7E09,nlap)
   call lap_assign_para(xpnts,xpnts_init_n50_7E09,nlap)
  else if (rlen.gt.1.D10 .and. rlen.le.2.D10) then
   call lap_assign_para(wghts,wghts_init_n50_1E10,nlap)
   call lap_assign_para(xpnts,xpnts_init_n50_1E10,nlap)
  else if (rlen.gt.2.D10 .and. rlen.le.3.D10) then
   call lap_assign_para(wghts,wghts_init_n50_2E10,nlap)
   call lap_assign_para(xpnts,xpnts_init_n50_2E10,nlap)
  else if (rlen.gt.3.D10 .and. rlen.le.4.D10) then
   call lap_assign_para(wghts,wghts_init_n50_3E10,nlap)
   call lap_assign_para(xpnts,xpnts_init_n50_3E10,nlap)
  else if (rlen.gt.4.D10 .and. rlen.le.5.D10) then
   call lap_assign_para(wghts,wghts_init_n50_4E10,nlap)
   call lap_assign_para(xpnts,xpnts_init_n50_4E10,nlap)
  else if (rlen.gt.5.D10 .and. rlen.le.7.D10) then
   call lap_assign_para(wghts,wghts_init_n50_5E10,nlap)
   call lap_assign_para(xpnts,xpnts_init_n50_5E10,nlap)
  else if (rlen.gt.7.D10 .and. rlen.le.1.D11) then
   call lap_assign_para(wghts,wghts_init_n50_7E10,nlap)
   call lap_assign_para(xpnts,xpnts_init_n50_7E10,nlap)
  else if (rlen.gt.1.D11 .and. rlen.le.2.D11) then
   call lap_assign_para(wghts,wghts_init_n50_1E11,nlap)
   call lap_assign_para(xpnts,xpnts_init_n50_1E11,nlap)
  else if (rlen.gt.2.D11 .and. rlen.le.3.D11) then
   call lap_assign_para(wghts,wghts_init_n50_2E11,nlap)
   call lap_assign_para(xpnts,xpnts_init_n50_2E11,nlap)
  else if (rlen.gt.3.D11 .and. rlen.le.4.D11) then
   call lap_assign_para(wghts,wghts_init_n50_3E11,nlap)
   call lap_assign_para(xpnts,xpnts_init_n50_3E11,nlap)
  else if (rlen.gt.4.D11 .and. rlen.le.5.D11) then
   call lap_assign_para(wghts,wghts_init_n50_4E11,nlap)
   call lap_assign_para(xpnts,xpnts_init_n50_4E11,nlap)
  else if (rlen.gt.5.D11 .and. rlen.le.7.D11) then
   call lap_assign_para(wghts,wghts_init_n50_5E11,nlap)
   call lap_assign_para(xpnts,xpnts_init_n50_5E11,nlap)
  else if (rlen.gt.7.D11 .and. rlen.le.1.D12) then
   call lap_assign_para(wghts,wghts_init_n50_7E11,nlap)
   call lap_assign_para(xpnts,xpnts_init_n50_7E11,nlap)
  else if (rlen.gt.1.D12 .and. rlen.le.2.D12) then
   call lap_assign_para(wghts,wghts_init_n50_1E12,nlap)
   call lap_assign_para(xpnts,xpnts_init_n50_1E12,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n50_2E12,nlap)
   call lap_assign_para(xpnts,xpnts_init_n50_2E12,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (51)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.1.D12) then
   call lap_assign_para(wghts,wghts_init_n51_1E12,nlap)
   call lap_assign_para(xpnts,xpnts_init_n51_1E12,nlap)
  else if (rlen.gt.1.D12 .and. rlen.le.2.D12) then
   call lap_assign_para(wghts,wghts_init_n51_1E12,nlap)
   call lap_assign_para(xpnts,xpnts_init_n51_1E12,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n51_2E12,nlap)
   call lap_assign_para(xpnts,xpnts_init_n51_2E12,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (52)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.1.D12) then
   call lap_assign_para(wghts,wghts_init_n52_1E12,nlap)
   call lap_assign_para(xpnts,xpnts_init_n52_1E12,nlap)
  else if (rlen.gt.1.D12 .and. rlen.le.3.D12) then
   call lap_assign_para(wghts,wghts_init_n52_1E12,nlap)
   call lap_assign_para(xpnts,xpnts_init_n52_1E12,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n52_3E12,nlap)
   call lap_assign_para(xpnts,xpnts_init_n52_3E12,nlap)
  end if

!-----------------------------------------------------------------------------!
 case (53)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.4.D11) then
   call lap_assign_para(wghts,wghts_init_n53_4E11,nlap)
   call lap_assign_para(xpnts,xpnts_init_n53_4E11,nlap)
  else if (rlen.gt.4.D11 .and. rlen.le.1.D12) then
   call lap_assign_para(wghts,wghts_init_n53_4E11,nlap)
   call lap_assign_para(xpnts,xpnts_init_n53_4E11,nlap)
  else if (rlen.gt.1.D12 .and. rlen.le.2.D12) then
   call lap_assign_para(wghts,wghts_init_n53_1E12,nlap)
   call lap_assign_para(xpnts,xpnts_init_n53_1E12,nlap)
  else if (rlen.gt.2.D12 .and. rlen.le.3.D12) then
   call lap_assign_para(wghts,wghts_init_n53_2E12,nlap)
   call lap_assign_para(xpnts,xpnts_init_n53_2E12,nlap)
  else if (rlen.gt.3.D12 .and. rlen.le.4.D12) then
   call lap_assign_para(wghts,wghts_init_n53_3E12,nlap)
   call lap_assign_para(xpnts,xpnts_init_n53_3E12,nlap)
  else
   call lap_assign_para(wghts,wghts_init_n53_4E12,nlap)
   call lap_assign_para(xpnts,xpnts_init_n53_4E12,nlap)
  end if

!-----------------------------------------------------------------------------!
 case default
!-----------------------------------------------------------------------------!
  write(istdout,"(a)") chrdbg//"Maximum number of Laplace quadrature points is 53!"
  stop "Error"
 end select


 if (locdbg) then
  write(istdout,*)  chrdbg//" quadrature points>",nlap
  write(istdout,*)  chrdbg//" weights>"
  do ilap = 1,nlap
   write(istdout,*) ilap,wghts(1,ilap)
  end do
  write(istdout,*)  chrdbg//" exponents>"
  do ilap = 1,nlap
   write(istdout,*) ilap,xpnts(1,ilap)
  end do
 end if

 if (locdbg) write(istdout,"(a)") "... left lap_init"

end subroutine lap_init
!==================================================================================================================================!
!==================================================================================================================================!
subroutine lap_assign_para(vec2,vec1,ndim)

 implicit none

#include "consts.h"

 integer, intent(in)  :: ndim
 real(8), intent(in)  :: vec1(ndim)
 real(8), intent(out) :: vec2(2,ndim)

 integer :: idx

 do idx = 1,ndim
  vec2(1,idx) = vec1(idx)
  vec2(2,idx) = d0
 end do
end subroutine lap_assign_para
!==================================================================================================================================!
