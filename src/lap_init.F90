!==============================================================================!
subroutine lap_init(errbnd,xpnts,wghts,rnge,nlap)
!------------------------------------------------------------------------------!
!
! get initial Laplace data from pre-tabulated
!  Laplace exponents and weights of Takatsuka, Ten-no, and Hackbusch.
! Those initial values depend on the interval [1,R].
!
!------------------------------------------------------------------------------!

 implicit none

#include "consts.h"
#include "init.h"
#include "init_tiny.h"
#include "error.h"

! constants:
 logical, parameter :: locdbg = .false.
 character(len=*), parameter :: chrdbg = 'lap_init>'

! dimensions:
 integer, intent(in) :: nlap

! input:
 real(8), intent(in) :: rnge(2)

! in/output:
 real(8), intent(inout)  :: xpnts(2,nlap), wghts(2,nlap), errbnd(2)

! local:
 integer :: ilap
 real(8) :: rlen

 if (locdbg) write(istdout,"(a)") chrdbg//"enter lap_init ..."

 rlen = rnge(2)/rnge(1)

 if (rlen .le. d1) then
  stop chrdbg//"Upper bound shall be greater than lower bound!"
 end if

!-----------------------------------------------------------------------------!
!
! A) assign weights and exponents
!
!-----------------------------------------------------------------------------!

 select case (nlap)

!-----------------------------------------------------------------------------!
 case (1)
!-----------------------------------------------------------------------------!
  if      (rlen.lt.1.100D00) then            
   call lap_assign_para(wghts,wghts_init_n01_1050,nlap)
   call lap_assign_para(xpnts,xpnts_init_n01_1050,nlap)
  else if (rlen.lt.1.200D00) then            
   call lap_assign_para(wghts,wghts_init_n01_1100,nlap)
   call lap_assign_para(xpnts,xpnts_init_n01_1100,nlap)
  else if (rlen.lt.1.300D00) then            
   call lap_assign_para(wghts,wghts_init_n01_1200,nlap)
   call lap_assign_para(xpnts,xpnts_init_n01_1200,nlap)
  else if (rlen.lt.1.400D00) then            
   call lap_assign_para(wghts,wghts_init_n01_1300,nlap)
   call lap_assign_para(xpnts,xpnts_init_n01_1300,nlap)
  else if (rlen.lt.1.500D00) then            
   call lap_assign_para(wghts,wghts_init_n01_1400,nlap)
   call lap_assign_para(xpnts,xpnts_init_n01_1400,nlap)
  else if (rlen.lt.1.600D00) then            
   call lap_assign_para(wghts,wghts_init_n01_1500,nlap)
   call lap_assign_para(xpnts,xpnts_init_n01_1500,nlap)
  else if (rlen.lt.1.700D00) then            
   call lap_assign_para(wghts,wghts_init_n01_1600,nlap)
   call lap_assign_para(xpnts,xpnts_init_n01_1600,nlap)
  else if (rlen.lt.1.800D00) then            
   call lap_assign_para(wghts,wghts_init_n01_1700,nlap)
   call lap_assign_para(xpnts,xpnts_init_n01_1700,nlap)
  else if (rlen.lt.1.900D00) then            
   call lap_assign_para(wghts,wghts_init_n01_1800,nlap)
   call lap_assign_para(xpnts,xpnts_init_n01_1800,nlap)
  else if (rlen.lt.2.D00) then
   call lap_assign_para(wghts,wghts_init_n01_1900,nlap)
   call lap_assign_para(xpnts,xpnts_init_n01_1900,nlap)
  else if (rlen.lt.3.D00) then
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
  if      (rlen.lt.1.055D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1050,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1050,nlap)
  else if (rlen.lt.1.060D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1055,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1055,nlap)
  else if (rlen.lt.1.065D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1060,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1060,nlap)
  else if (rlen.lt.1.070D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1065,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1065,nlap)
  else if (rlen.lt.1.075D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1070,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1070,nlap)
  else if (rlen.lt.1.080D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1075,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1075,nlap)
  else if (rlen.lt.1.085D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1080,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1080,nlap)
  else if (rlen.lt.1.090D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1085,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1085,nlap)
  else if (rlen.lt.1.095D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1090,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1090,nlap)
  else if (rlen.lt.1.100D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1095,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1095,nlap)
  else if (rlen.lt.1.105D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1100,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1100,nlap)
  else if (rlen.lt.1.110D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1105,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1105,nlap)
  else if (rlen.lt.1.115D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1110,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1110,nlap)
  else if (rlen.lt.1.120D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1115,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1115,nlap)
  else if (rlen.lt.1.125D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1120,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1120,nlap)
  else if (rlen.lt.1.130D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1125,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1125,nlap)
  else if (rlen.lt.1.135D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1130,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1130,nlap)
  else if (rlen.lt.1.140D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1135,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1135,nlap)
  else if (rlen.lt.1.145D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1140,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1140,nlap)
  else if (rlen.lt.1.150D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1145,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1145,nlap)
  else if (rlen.lt.1.160D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1150,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1150,nlap)
  else if (rlen.lt.1.170D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1160,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1160,nlap)
  else if (rlen.lt.1.180D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1170,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1170,nlap)
  else if (rlen.lt.1.190D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1180,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1180,nlap)
  else if (rlen.lt.1.200D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1190,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1190,nlap)
  else if (rlen.lt.1.210D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1200,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1200,nlap)
  else if (rlen.lt.1.220D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1210,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1210,nlap)
  else if (rlen.lt.1.230D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1220,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1220,nlap)
  else if (rlen.lt.1.240D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1230,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1230,nlap)
  else if (rlen.lt.1.250D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1240,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1240,nlap)
  else if (rlen.lt.1.260D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1250,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1250,nlap)
  else if (rlen.lt.1.270D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1260,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1260,nlap)
  else if (rlen.lt.1.280D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1270,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1270,nlap)
  else if (rlen.lt.1.290D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1280,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1280,nlap)
  else if (rlen.lt.1.300D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1290,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1290,nlap)
  else if (rlen.lt.1.320D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1300,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1300,nlap)
  else if (rlen.lt.1.330D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1320,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1320,nlap)
  else if (rlen.lt.1.350D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1330,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1330,nlap)
  else if (rlen.lt.1.370D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1350,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1350,nlap)
  else if (rlen.lt.1.400D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1370,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1370,nlap)
  else if (rlen.lt.1.420D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1400,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1400,nlap)
  else if (rlen.lt.1.430D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1420,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1420,nlap)
  else if (rlen.lt.1.450D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1430,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1430,nlap)
  else if (rlen.lt.1.470D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1450,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1450,nlap)
  else if (rlen.lt.1.500D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1470,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1470,nlap)
  else if (rlen.lt.1.520D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1500,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1500,nlap)
  else if (rlen.lt.1.550D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1520,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1520,nlap)
  else if (rlen.lt.1.570D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1550,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1550,nlap)
  else if (rlen.lt.1.600D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1570,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1570,nlap)
  else if (rlen.lt.1.620D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1600,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1600,nlap)
  else if (rlen.lt.1.650D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1620,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1620,nlap)
  else if (rlen.lt.1.670D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1650,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1650,nlap)
  else if (rlen.lt.1.700D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1670,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1670,nlap)
  else if (rlen.lt.1.750D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1700,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1700,nlap)
  else if (rlen.lt.1.800D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1750,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1750,nlap)
  else if (rlen.lt.1.850D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1800,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1800,nlap)
  else if (rlen.lt.1.900D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1850,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1850,nlap)
  else if (rlen.lt.1.950D00) then            
   call lap_assign_para(wghts,wghts_init_n02_1900,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1900,nlap)
  else if (rlen.lt.2.D00) then
   call lap_assign_para(wghts,wghts_init_n02_1950,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_1950,nlap)
  else if (rlen.lt.3.D00) then
   call lap_assign_para(wghts,wghts_init_n02_2E00,nlap)
   call lap_assign_para(xpnts,xpnts_init_n02_2E00,nlap)
  else if (rlen.ge.3.D00 .and. rlen.le.4.D00) then
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
  if      (rlen.lt.1.075D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1070,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1070,nlap)
  else if (rlen.lt.1.080D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1075,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1075,nlap)
  else if (rlen.lt.1.085D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1080,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1080,nlap)
  else if (rlen.lt.1.090D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1085,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1085,nlap)
  else if (rlen.lt.1.095D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1090,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1090,nlap)
  else if (rlen.lt.1.100D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1095,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1095,nlap)
  else if (rlen.lt.1.105D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1100,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1100,nlap)
  else if (rlen.lt.1.110D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1105,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1105,nlap)
  else if (rlen.lt.1.115D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1110,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1110,nlap)
  else if (rlen.lt.1.120D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1115,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1115,nlap)
  else if (rlen.lt.1.125D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1120,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1120,nlap)
  else if (rlen.lt.1.130D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1125,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1125,nlap)
  else if (rlen.lt.1.135D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1130,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1130,nlap)
  else if (rlen.lt.1.140D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1135,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1135,nlap)
  else if (rlen.lt.1.145D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1140,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1140,nlap)
  else if (rlen.lt.1.150D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1145,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1145,nlap)
  else if (rlen.lt.1.160D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1150,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1150,nlap)
  else if (rlen.lt.1.170D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1160,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1160,nlap)
  else if (rlen.lt.1.180D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1170,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1170,nlap)
  else if (rlen.lt.1.190D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1180,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1180,nlap)
  else if (rlen.lt.1.200D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1190,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1190,nlap)
  else if (rlen.lt.1.210D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1200,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1200,nlap)
  else if (rlen.lt.1.220D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1210,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1210,nlap)
  else if (rlen.lt.1.230D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1220,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1220,nlap)
  else if (rlen.lt.1.240D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1230,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1230,nlap)
  else if (rlen.lt.1.250D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1240,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1240,nlap)
  else if (rlen.lt.1.260D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1250,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1250,nlap)
  else if (rlen.lt.1.270D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1260,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1260,nlap)
  else if (rlen.lt.1.280D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1270,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1270,nlap)
  else if (rlen.lt.1.290D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1280,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1280,nlap)
  else if (rlen.lt.1.300D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1290,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1290,nlap)
  else if (rlen.lt.1.320D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1300,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1300,nlap)
  else if (rlen.lt.1.330D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1320,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1320,nlap)
  else if (rlen.lt.1.350D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1330,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1330,nlap)
  else if (rlen.lt.1.370D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1350,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1350,nlap)
  else if (rlen.lt.1.400D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1370,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1370,nlap)
  else if (rlen.lt.1.420D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1400,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1400,nlap)
  else if (rlen.lt.1.430D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1420,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1420,nlap)
  else if (rlen.lt.1.450D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1430,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1430,nlap)
  else if (rlen.lt.1.470D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1450,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1450,nlap)
  else if (rlen.lt.1.500D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1470,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1470,nlap)
  else if (rlen.lt.1.520D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1500,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1500,nlap)
  else if (rlen.lt.1.550D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1520,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1520,nlap)
  else if (rlen.lt.1.570D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1550,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1550,nlap)
  else if (rlen.lt.1.600D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1570,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1570,nlap)
  else if (rlen.lt.1.620D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1600,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1600,nlap)
  else if (rlen.lt.1.650D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1620,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1620,nlap)
  else if (rlen.lt.1.670D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1650,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1650,nlap)
  else if (rlen.lt.1.700D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1670,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1670,nlap)
  else if (rlen.lt.1.750D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1700,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1700,nlap)
  else if (rlen.lt.1.800D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1750,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1750,nlap)
  else if (rlen.lt.1.850D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1800,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1800,nlap)
  else if (rlen.lt.1.900D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1850,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1850,nlap)
  else if (rlen.lt.1.950D00) then            
   call lap_assign_para(wghts,wghts_init_n03_1900,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1900,nlap)
  else if (rlen.lt.2.D00) then
   call lap_assign_para(wghts,wghts_init_n03_1950,nlap)
   call lap_assign_para(xpnts,xpnts_init_n03_1950,nlap)
  else if (rlen.le.3.D00) then
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
  if      (rlen.lt.1.125D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1120,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1120,nlap)
  else if (rlen.lt.1.130D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1125,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1125,nlap)
  else if (rlen.lt.1.135D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1130,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1130,nlap)
  else if (rlen.lt.1.140D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1135,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1135,nlap)
  else if (rlen.lt.1.145D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1140,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1140,nlap)
  else if (rlen.lt.1.150D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1145,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1145,nlap)
  else if (rlen.lt.1.155D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1150,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1150,nlap)
  else if (rlen.lt.1.160D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1155,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1155,nlap)
  else if (rlen.lt.1.165D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1160,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1160,nlap)
  else if (rlen.lt.1.170D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1165,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1165,nlap)
  else if (rlen.lt.1.175D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1170,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1170,nlap)
  else if (rlen.lt.1.180D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1175,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1175,nlap)
  else if (rlen.lt.1.185D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1180,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1180,nlap)
  else if (rlen.lt.1.190D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1185,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1185,nlap)
  else if (rlen.lt.1.195D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1190,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1190,nlap)
  else if (rlen.lt.1.200D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1195,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1195,nlap)
  else if (rlen.lt.1.205D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1200,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1200,nlap)
  else if (rlen.lt.1.210D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1205,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1205,nlap)
  else if (rlen.lt.1.215D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1210,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1210,nlap)
  else if (rlen.lt.1.220D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1215,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1215,nlap)
  else if (rlen.lt.1.225D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1220,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1220,nlap)
  else if (rlen.lt.1.230D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1225,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1225,nlap)
  else if (rlen.lt.1.235D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1230,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1230,nlap)
  else if (rlen.lt.1.240D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1235,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1235,nlap)
  else if (rlen.lt.1.245D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1240,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1240,nlap)
  else if (rlen.lt.1.250D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1245,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1245,nlap)
  else if (rlen.lt.1.260D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1250,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1250,nlap)
  else if (rlen.lt.1.270D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1260,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1260,nlap)
  else if (rlen.lt.1.280D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1270,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1270,nlap)
  else if (rlen.lt.1.290D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1280,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1280,nlap)
  else if (rlen.lt.1.300D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1290,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1290,nlap)
  else if (rlen.lt.1.310D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1300,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1300,nlap)
  else if (rlen.lt.1.320D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1310,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1310,nlap)
  else if (rlen.lt.1.330D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1320,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1320,nlap)
  else if (rlen.lt.1.340D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1330,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1330,nlap)
  else if (rlen.lt.1.350D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1340,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1340,nlap)
  else if (rlen.lt.1.360D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1350,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1350,nlap)
  else if (rlen.lt.1.370D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1360,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1360,nlap)
  else if (rlen.lt.1.380D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1370,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1370,nlap)
  else if (rlen.lt.1.390D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1380,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1380,nlap)
  else if (rlen.lt.1.400D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1390,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1390,nlap)
  else if (rlen.lt.1.410D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1400,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1400,nlap)
  else if (rlen.lt.1.420D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1410,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1410,nlap)
  else if (rlen.lt.1.430D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1420,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1420,nlap)
  else if (rlen.lt.1.440D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1430,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1430,nlap)
  else if (rlen.lt.1.450D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1440,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1440,nlap)
  else if (rlen.lt.1.460D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1450,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1450,nlap)
  else if (rlen.lt.1.470D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1460,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1460,nlap)
  else if (rlen.lt.1.480D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1470,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1470,nlap)
  else if (rlen.lt.1.490D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1480,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1480,nlap)
  else if (rlen.lt.1.500D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1490,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1490,nlap)
  else if (rlen.lt.1.510D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1500,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1500,nlap)
  else if (rlen.lt.1.520D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1510,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1510,nlap)
  else if (rlen.lt.1.530D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1520,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1520,nlap)
  else if (rlen.lt.1.540D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1530,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1530,nlap)
  else if (rlen.lt.1.550D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1540,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1540,nlap)
  else if (rlen.lt.1.560D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1550,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1550,nlap)
  else if (rlen.lt.1.570D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1560,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1560,nlap)
  else if (rlen.lt.1.580D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1570,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1570,nlap)
  else if (rlen.lt.1.590D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1580,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1580,nlap)
  else if (rlen.lt.1.600D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1590,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1590,nlap)
  else if (rlen.lt.1.610D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1600,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1600,nlap)
  else if (rlen.lt.1.620D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1610,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1610,nlap)
  else if (rlen.lt.1.630D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1620,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1620,nlap)
  else if (rlen.lt.1.640D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1630,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1630,nlap)
  else if (rlen.lt.1.650D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1640,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1640,nlap)
  else if (rlen.lt.1.660D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1650,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1650,nlap)
  else if (rlen.lt.1.670D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1660,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1660,nlap)
  else if (rlen.lt.1.680D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1670,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1670,nlap)
  else if (rlen.lt.1.690D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1680,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1680,nlap)
  else if (rlen.lt.1.700D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1690,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1690,nlap)
  else if (rlen.lt.1.710D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1700,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1700,nlap)
  else if (rlen.lt.1.720D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1710,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1710,nlap)
  else if (rlen.lt.1.730D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1720,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1720,nlap)
  else if (rlen.lt.1.740D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1730,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1730,nlap)
  else if (rlen.lt.1.750D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1740,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1740,nlap)
  else if (rlen.lt.1.760D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1750,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1750,nlap)
  else if (rlen.lt.1.770D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1760,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1760,nlap)
  else if (rlen.lt.1.780D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1770,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1770,nlap)
  else if (rlen.lt.1.790D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1780,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1780,nlap)
  else if (rlen.lt.1.800D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1790,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1790,nlap)
  else if (rlen.lt.1.810D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1800,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1800,nlap)
  else if (rlen.lt.1.820D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1810,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1810,nlap)
  else if (rlen.lt.1.830D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1820,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1820,nlap)
  else if (rlen.lt.1.840D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1830,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1830,nlap)
  else if (rlen.lt.1.850D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1840,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1840,nlap)
  else if (rlen.lt.1.860D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1850,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1850,nlap)
  else if (rlen.lt.1.870D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1860,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1860,nlap)
  else if (rlen.lt.1.880D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1870,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1870,nlap)
  else if (rlen.lt.1.890D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1880,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1880,nlap)
  else if (rlen.lt.1.900D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1890,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1890,nlap)
  else if (rlen.lt.1.910D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1900,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1900,nlap)
  else if (rlen.lt.1.920D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1910,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1910,nlap)
  else if (rlen.lt.1.930D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1920,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1920,nlap)
  else if (rlen.lt.1.940D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1930,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1930,nlap)
  else if (rlen.lt.1.950D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1940,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1940,nlap)
  else if (rlen.lt.1.960D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1950,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1950,nlap)
  else if (rlen.lt.1.970D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1960,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1960,nlap)
  else if (rlen.lt.1.980D00) then            
   call lap_assign_para(wghts,wghts_init_n04_1970,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1970,nlap)
  else if (rlen.lt.2.D00) then               
   call lap_assign_para(wghts,wghts_init_n04_1980,nlap)
   call lap_assign_para(xpnts,xpnts_init_n04_1980,nlap)
  else if (rlen.ge.2.D00 .and. rlen.le.3.D00) then
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

  if      (rlen.lt.1.350D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1340,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1340,nlap)
  else if (rlen.lt.1.360D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1350,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1350,nlap)
  else if (rlen.lt.1.370D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1360,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1360,nlap)
  else if (rlen.lt.1.380D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1370,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1370,nlap)
  else if (rlen.lt.1.390D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1380,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1380,nlap)
  else if (rlen.lt.1.400D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1390,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1390,nlap)
  else if (rlen.lt.1.410D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1400,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1400,nlap)
  else if (rlen.lt.1.420D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1410,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1410,nlap)
  else if (rlen.lt.1.430D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1420,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1420,nlap)
  else if (rlen.lt.1.440D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1430,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1430,nlap)
  else if (rlen.lt.1.450D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1440,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1440,nlap)
  else if (rlen.lt.1.460D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1450,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1450,nlap)
  else if (rlen.lt.1.470D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1460,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1460,nlap)
  else if (rlen.lt.1.480D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1470,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1470,nlap)
  else if (rlen.lt.1.490D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1480,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1480,nlap)
  else if (rlen.lt.1.500D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1490,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1490,nlap)
  else if (rlen.lt.1.510D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1500,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1500,nlap)
  else if (rlen.lt.1.520D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1510,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1510,nlap)
  else if (rlen.lt.1.530D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1520,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1520,nlap)
  else if (rlen.lt.1.540D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1530,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1530,nlap)
  else if (rlen.lt.1.550D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1540,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1540,nlap)
  else if (rlen.lt.1.560D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1550,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1550,nlap)
  else if (rlen.lt.1.570D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1560,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1560,nlap)
  else if (rlen.lt.1.580D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1570,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1570,nlap)
  else if (rlen.lt.1.590D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1580,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1580,nlap)
  else if (rlen.lt.1.600D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1590,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1590,nlap)
  else if (rlen.lt.1.610D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1600,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1600,nlap)
  else if (rlen.lt.1.620D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1610,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1610,nlap)
  else if (rlen.lt.1.630D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1620,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1620,nlap)
  else if (rlen.lt.1.640D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1630,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1630,nlap)
  else if (rlen.lt.1.650D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1640,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1640,nlap)
  else if (rlen.lt.1.660D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1650,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1650,nlap)
  else if (rlen.lt.1.670D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1660,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1660,nlap)
  else if (rlen.lt.1.680D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1670,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1670,nlap)
  else if (rlen.lt.1.690D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1680,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1680,nlap)
  else if (rlen.lt.1.700D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1690,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1690,nlap)
  else if (rlen.lt.1.710D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1700,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1700,nlap)
  else if (rlen.lt.1.720D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1710,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1710,nlap)
  else if (rlen.lt.1.730D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1720,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1720,nlap)
  else if (rlen.lt.1.740D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1730,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1730,nlap)
  else if (rlen.lt.1.750D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1740,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1740,nlap)
  else if (rlen.lt.1.760D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1750,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1750,nlap)
  else if (rlen.lt.1.770D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1760,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1760,nlap)
  else if (rlen.lt.1.780D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1770,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1770,nlap)
  else if (rlen.lt.1.790D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1780,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1780,nlap)
  else if (rlen.lt.1.800D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1790,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1790,nlap)
  else if (rlen.lt.1.810D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1800,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1800,nlap)
  else if (rlen.lt.1.820D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1810,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1810,nlap)
  else if (rlen.lt.1.830D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1820,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1820,nlap)
  else if (rlen.lt.1.850D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1830,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1830,nlap)
  else if (rlen.lt.1.860D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1850,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1850,nlap)
  else if (rlen.lt.1.870D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1860,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1860,nlap)
  else if (rlen.lt.1.880D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1870,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1870,nlap)
  else if (rlen.lt.1.890D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1880,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1880,nlap)
  else if (rlen.lt.1.900D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1890,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1890,nlap)
  else if (rlen.lt.1.910D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1900,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1900,nlap)
  else if (rlen.lt.1.920D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1910,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1910,nlap)
  else if (rlen.lt.1.930D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1920,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1920,nlap)
  else if (rlen.lt.1.950D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1930,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1930,nlap)
  else if (rlen.lt.1.970D00) then            
   call lap_assign_para(wghts,wghts_init_n05_1950,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1950,nlap)
  else if (rlen.lt.2.D00) then               
   call lap_assign_para(wghts,wghts_init_n05_1970,nlap)
   call lap_assign_para(xpnts,xpnts_init_n05_1970,nlap)
  else if (rlen.ge.2.D00 .and. rlen.le.3.D00) then
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
  if      (rlen.lt.1.490D00) then                
   call lap_assign_para(wghts,wghts_init_n06_1480,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1480,nlap)
  else if (rlen.lt.1.500D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1490,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1490,nlap)
  else if (rlen.lt.1.510D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1500,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1500,nlap)
  else if (rlen.lt.1.520D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1510,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1510,nlap)
  else if (rlen.lt.1.530D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1520,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1520,nlap)
  else if (rlen.lt.1.540D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1530,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1530,nlap)
  else if (rlen.lt.1.550D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1540,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1540,nlap)
  else if (rlen.lt.1.560D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1550,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1550,nlap)
  else if (rlen.lt.1.570D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1560,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1560,nlap)
  else if (rlen.lt.1.580D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1570,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1570,nlap)
  else if (rlen.lt.1.590D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1580,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1580,nlap)
  else if (rlen.lt.1.600D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1590,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1590,nlap)
  else if (rlen.lt.1.610D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1600,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1600,nlap)
  else if (rlen.lt.1.620D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1610,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1610,nlap)
  else if (rlen.lt.1.630D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1620,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1620,nlap)
  else if (rlen.lt.1.640D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1630,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1630,nlap)
  else if (rlen.lt.1.650D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1640,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1640,nlap)
  else if (rlen.lt.1.660D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1650,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1650,nlap)
  else if (rlen.lt.1.670D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1660,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1660,nlap)
  else if (rlen.lt.1.680D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1670,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1670,nlap)
  else if (rlen.lt.1.690D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1680,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1680,nlap)
  else if (rlen.lt.1.700D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1690,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1690,nlap)
  else if (rlen.lt.1.710D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1700,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1700,nlap)
  else if (rlen.lt.1.720D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1710,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1710,nlap)
  else if (rlen.lt.1.730D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1720,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1720,nlap)
  else if (rlen.lt.1.740D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1730,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1730,nlap)
  else if (rlen.lt.1.750D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1740,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1740,nlap)
  else if (rlen.lt.1.760D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1750,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1750,nlap)
  else if (rlen.lt.1.770D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1760,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1760,nlap)
  else if (rlen.lt.1.780D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1770,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1770,nlap)
  else if (rlen.lt.1.790D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1780,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1780,nlap)
  else if (rlen.lt.1.800D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1790,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1790,nlap)
  else if (rlen.lt.1.810D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1800,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1800,nlap)
  else if (rlen.lt.1.820D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1810,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1810,nlap)
  else if (rlen.lt.1.830D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1820,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1820,nlap)
  else if (rlen.lt.1.850D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1830,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1830,nlap)
  else if (rlen.lt.1.860D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1850,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1850,nlap)
  else if (rlen.lt.1.870D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1860,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1860,nlap)
  else if (rlen.lt.1.880D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1870,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1870,nlap)
  else if (rlen.lt.1.890D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1880,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1880,nlap)
  else if (rlen.lt.1.900D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1890,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1890,nlap)
  else if (rlen.lt.1.910D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1900,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1900,nlap)
  else if (rlen.lt.1.920D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1910,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1910,nlap)
  else if (rlen.lt.1.930D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1920,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1920,nlap)
  else if (rlen.lt.1.940D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1930,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1930,nlap)
  else if (rlen.lt.1.950D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1940,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1940,nlap)
  else if (rlen.lt.1.960D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1950,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1950,nlap)
  else if (rlen.lt.1.970D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1960,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1960,nlap)
  else if (rlen.lt.1.980D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1970,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1970,nlap)
  else if (rlen.lt.2.000D00) then            
   call lap_assign_para(wghts,wghts_init_n06_1980,nlap)
   call lap_assign_para(xpnts,xpnts_init_n06_1980,nlap)
  else if (rlen.ge.2.D00 .and. rlen.le.3.D00) then
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

!-----------------------------------------------------------------------------!
!
! B) assign error
!
!-----------------------------------------------------------------------------!

 select case (nlap)

!-----------------------------------------------------------------------------!
 case (1)
!-----------------------------------------------------------------------------!
  if      (rlen.gt.1.D00 .and. rlen.le.2.D00) then
   call lap_assign_error(errbnd,nlap,error_init_2E00,ilap_init_2E00,nlap_init_2E00)
  else if (rlen.gt.2.D00 .and. rlen.le.3.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E00,ilap_init_2E00,nlap_init_2E00)
  else if (rlen.gt.3.D00 .and. rlen.le.4.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E00,ilap_init_3E00,nlap_init_3E00)
  else if (rlen.gt.4.D00 .and. rlen.le.5.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E00,ilap_init_4E00,nlap_init_4E00)
  else if (rlen.gt.5.D00 .and. rlen.le.6.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E00,ilap_init_5E00,nlap_init_5E00)
  else if (rlen.gt.6.D00 .and. rlen.le.7.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E00,ilap_init_6E00,nlap_init_6E00)
  else if (rlen.gt.7.D00 .and. rlen.le.8.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E00,ilap_init_7E00,nlap_init_7E00)
  else if (rlen.gt.8.D00 .and. rlen.le.9.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E00,ilap_init_8E00,nlap_init_8E00)
  else if (rlen.gt.9.D00 .and. rlen.le.1.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E00,ilap_init_9E00,nlap_init_9E00)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_1E01,ilap_init_1E01,nlap_init_1E01)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (2)                                                                                                
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.2.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E00,ilap_init_2E00,nlap_init_2E00)
  else if (rlen.gt.2.D00 .and. rlen.le.3.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E00,ilap_init_2E00,nlap_init_2E00)
  else if (rlen.gt.3.D00 .and. rlen.le.4.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E00,ilap_init_3E00,nlap_init_3E00)
  else if (rlen.gt.4.D00 .and. rlen.le.5.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E00,ilap_init_4E00,nlap_init_4E00)
  else if (rlen.gt.5.D00 .and. rlen.le.6.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E00,ilap_init_5E00,nlap_init_5E00)
  else if (rlen.gt.6.D00 .and. rlen.le.7.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E00,ilap_init_6E00,nlap_init_6E00)
  else if (rlen.gt.7.D00 .and. rlen.le.8.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E00,ilap_init_7E00,nlap_init_7E00)
  else if (rlen.gt.8.D00 .and. rlen.le.9.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E00,ilap_init_8E00,nlap_init_8E00)
  else if (rlen.gt.9.D00 .and. rlen.le.1.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E00,ilap_init_9E00,nlap_init_9E00)
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E01,ilap_init_1E01,nlap_init_1E01)
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E01,ilap_init_2E01,nlap_init_2E01)
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E01,ilap_init_3E01,nlap_init_3E01)
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E01,ilap_init_4E01,nlap_init_4E01)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_5E01,ilap_init_5E01,nlap_init_5E01)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (3)                                                                                                
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.2.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E00,ilap_init_2E00,nlap_init_2E00)
  else if (rlen.gt.2.D00 .and. rlen.le.3.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E00,ilap_init_2E00,nlap_init_2E00)
  else if (rlen.gt.3.D00 .and. rlen.le.4.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E00,ilap_init_3E00,nlap_init_3E00)
  else if (rlen.gt.4.D00 .and. rlen.le.5.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E00,ilap_init_4E00,nlap_init_4E00)
  else if (rlen.gt.5.D00 .and. rlen.le.6.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E00,ilap_init_5E00,nlap_init_5E00)
  else if (rlen.gt.6.D00 .and. rlen.le.7.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E00,ilap_init_6E00,nlap_init_6E00)
  else if (rlen.gt.7.D00 .and. rlen.le.8.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E00,ilap_init_7E00,nlap_init_7E00)
  else if (rlen.gt.8.D00 .and. rlen.le.9.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E00,ilap_init_8E00,nlap_init_8E00)
  else if (rlen.gt.9.D00 .and. rlen.le.1.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E00,ilap_init_9E00,nlap_init_9E00)
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E01,ilap_init_1E01,nlap_init_1E01)
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E01,ilap_init_2E01,nlap_init_2E01)
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E01,ilap_init_3E01,nlap_init_3E01)
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E01,ilap_init_4E01,nlap_init_4E01)
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E01,ilap_init_5E01,nlap_init_5E01)
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E01,ilap_init_6E01,nlap_init_6E01)
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E01,ilap_init_7E01,nlap_init_7E01)
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E01,ilap_init_8E01,nlap_init_8E01)
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E01,ilap_init_9E01,nlap_init_9E01)
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E02,ilap_init_1E02,nlap_init_1E02)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_2E02,ilap_init_2E02,nlap_init_2E02)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (4)                                                                                                
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.2.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E00,ilap_init_2E00,nlap_init_2E00)
  else if (rlen.gt.2.D00 .and. rlen.le.3.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E00,ilap_init_2E00,nlap_init_2E00)
  else if (rlen.gt.3.D00 .and. rlen.le.4.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E00,ilap_init_3E00,nlap_init_3E00)
  else if (rlen.gt.4.D00 .and. rlen.le.5.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E00,ilap_init_4E00,nlap_init_4E00)
  else if (rlen.gt.5.D00 .and. rlen.le.6.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E00,ilap_init_5E00,nlap_init_5E00)
  else if (rlen.gt.6.D00 .and. rlen.le.7.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E00,ilap_init_6E00,nlap_init_6E00)
  else if (rlen.gt.7.D00 .and. rlen.le.8.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E00,ilap_init_7E00,nlap_init_7E00)
  else if (rlen.gt.8.D00 .and. rlen.le.9.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E00,ilap_init_8E00,nlap_init_8E00)
  else if (rlen.gt.9.D00 .and. rlen.le.1.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E00,ilap_init_9E00,nlap_init_9E00)
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E01,ilap_init_1E01,nlap_init_1E01)
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E01,ilap_init_2E01,nlap_init_2E01)
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E01,ilap_init_3E01,nlap_init_3E01)
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E01,ilap_init_4E01,nlap_init_4E01)
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E01,ilap_init_5E01,nlap_init_5E01)
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E01,ilap_init_6E01,nlap_init_6E01)
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E01,ilap_init_7E01,nlap_init_7E01)
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E01,ilap_init_8E01,nlap_init_8E01)
  else if (rlen.gt.9.D01 .and. rlen.le.1.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E01,ilap_init_9E01,nlap_init_9E01)
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E02,ilap_init_1E02,nlap_init_1E02)
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E02,ilap_init_2E02,nlap_init_2E02)
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E02,ilap_init_3E02,nlap_init_3E02)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E02,ilap_init_4E02,nlap_init_4E02)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_5E02,ilap_init_5E02,nlap_init_5E02)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (5)                                                                                                
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.2.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E00,ilap_init_2E00,nlap_init_2E00)
  else if (rlen.gt.2.D00 .and. rlen.le.3.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E00,ilap_init_2E00,nlap_init_2E00)
  else if (rlen.gt.3.D00 .and. rlen.le.4.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E00,ilap_init_3E00,nlap_init_3E00)
  else if (rlen.gt.4.D00 .and. rlen.le.5.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E00,ilap_init_4E00,nlap_init_4E00)
  else if (rlen.gt.5.D00 .and. rlen.le.6.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E00,ilap_init_5E00,nlap_init_5E00)
  else if (rlen.gt.6.D00 .and. rlen.le.7.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E00,ilap_init_6E00,nlap_init_6E00)
  else if (rlen.gt.7.D00 .and. rlen.le.8.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E00,ilap_init_7E00,nlap_init_7E00)
  else if (rlen.gt.8.D00 .and. rlen.le.9.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E00,ilap_init_8E00,nlap_init_8E00)
  else if (rlen.gt.9.D00 .and. rlen.le.1.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E00,ilap_init_9E00,nlap_init_9E00)
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E01,ilap_init_1E01,nlap_init_1E01)
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E01,ilap_init_2E01,nlap_init_2E01)
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E01,ilap_init_3E01,nlap_init_3E01)
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E01,ilap_init_4E01,nlap_init_4E01)
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E01,ilap_init_5E01,nlap_init_5E01)
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E01,ilap_init_6E01,nlap_init_6E01)
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E01,ilap_init_7E01,nlap_init_7E01)
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E01,ilap_init_8E01,nlap_init_8E01)
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E01,ilap_init_9E01,nlap_init_9E01)
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E02,ilap_init_1E02,nlap_init_1E02)
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E02,ilap_init_2E02,nlap_init_2E02)
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E02,ilap_init_3E02,nlap_init_3E02)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E02,ilap_init_4E02,nlap_init_4E02)
  else if (rlen.gt.5.D02 .and. rlen.le.6.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E02,ilap_init_5E02,nlap_init_5E02)
  else if (rlen.gt.6.D02 .and. rlen.le.7.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E02,ilap_init_6E02,nlap_init_6E02)
  else if (rlen.gt.7.D02 .and. rlen.le.8.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E02,ilap_init_7E02,nlap_init_7E02)
  else if (rlen.gt.8.D02 .and. rlen.le.9.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E02,ilap_init_8E02,nlap_init_8E02)
  else if (rlen.gt.9.D02 .and. rlen.le.1.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E02,ilap_init_9E02,nlap_init_9E02)
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E03,ilap_init_1E03,nlap_init_1E03)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_2E03,ilap_init_2E03,nlap_init_2E03)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (6)                                                                                                
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.2.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E00,ilap_init_2E00,nlap_init_2E00)
  else if (rlen.gt.2.D00 .and. rlen.le.3.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E00,ilap_init_2E00,nlap_init_2E00)
  else if (rlen.gt.3.D00 .and. rlen.le.4.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E00,ilap_init_3E00,nlap_init_3E00)
  else if (rlen.gt.4.D00 .and. rlen.le.5.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E00,ilap_init_4E00,nlap_init_4E00)
  else if (rlen.gt.5.D00 .and. rlen.le.6.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E00,ilap_init_5E00,nlap_init_5E00)
  else if (rlen.gt.6.D00 .and. rlen.le.7.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E00,ilap_init_6E00,nlap_init_6E00)
  else if (rlen.gt.7.D00 .and. rlen.le.8.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E00,ilap_init_7E00,nlap_init_7E00)
  else if (rlen.gt.8.D00 .and. rlen.le.9.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E00,ilap_init_8E00,nlap_init_8E00)
  else if (rlen.gt.9.D00 .and. rlen.le.1.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E00,ilap_init_9E00,nlap_init_9E00)
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E01,ilap_init_1E01,nlap_init_1E01)
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E01,ilap_init_2E01,nlap_init_2E01)
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E01,ilap_init_3E01,nlap_init_3E01)
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E01,ilap_init_4E01,nlap_init_4E01)
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E01,ilap_init_5E01,nlap_init_5E01)
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E01,ilap_init_6E01,nlap_init_6E01)
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E01,ilap_init_7E01,nlap_init_7E01)
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E01,ilap_init_8E01,nlap_init_8E01)
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E01,ilap_init_9E01,nlap_init_9E01)
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E02,ilap_init_1E02,nlap_init_1E02)
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E02,ilap_init_2E02,nlap_init_2E02)
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E02,ilap_init_3E02,nlap_init_3E02)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E02,ilap_init_4E02,nlap_init_4E02)
  else if (rlen.gt.5.D02 .and. rlen.le.6.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E02,ilap_init_5E02,nlap_init_5E02)
  else if (rlen.gt.6.D02 .and. rlen.le.7.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E02,ilap_init_6E02,nlap_init_6E02)
  else if (rlen.gt.7.D02 .and. rlen.le.8.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E02,ilap_init_7E02,nlap_init_7E02)
  else if (rlen.gt.8.D02 .and. rlen.le.9.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E02,ilap_init_8E02,nlap_init_8E02)
  else if (rlen.gt.9.D02 .and. rlen.le.1.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E02,ilap_init_9E02,nlap_init_9E02)
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E03,ilap_init_1E03,nlap_init_1E03)
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E03,ilap_init_2E03,nlap_init_2E03)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_3E03,ilap_init_3E03,nlap_init_3E03)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (7)                                                                                                
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.2.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E00,ilap_init_2E00,nlap_init_2E00)
  else if (rlen.gt.2.D00 .and. rlen.le.3.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E00,ilap_init_2E00,nlap_init_2E00)
  else if (rlen.gt.3.D00 .and. rlen.le.4.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E00,ilap_init_3E00,nlap_init_3E00)
  else if (rlen.gt.4.D00 .and. rlen.le.5.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E00,ilap_init_4E00,nlap_init_4E00)
  else if (rlen.gt.5.D00 .and. rlen.le.6.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E00,ilap_init_5E00,nlap_init_5E00)
  else if (rlen.gt.6.D00 .and. rlen.le.7.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E00,ilap_init_6E00,nlap_init_6E00)
  else if (rlen.gt.7.D00 .and. rlen.le.8.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E00,ilap_init_7E00,nlap_init_7E00)
  else if (rlen.gt.8.D00 .and. rlen.le.9.D00) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E00,ilap_init_8E00,nlap_init_8E00)
  else if (rlen.gt.9.D00 .and. rlen.le.1.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E00,ilap_init_9E00,nlap_init_9E00)
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E01,ilap_init_1E01,nlap_init_1E01)
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E01,ilap_init_2E01,nlap_init_2E01)
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E01,ilap_init_3E01,nlap_init_3E01)
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E01,ilap_init_4E01,nlap_init_4E01)
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E01,ilap_init_5E01,nlap_init_5E01)
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E01,ilap_init_6E01,nlap_init_6E01)
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E01,ilap_init_7E01,nlap_init_7E01)
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E01,ilap_init_8E01,nlap_init_8E01)
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E01,ilap_init_9E01,nlap_init_9E01)
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E02,ilap_init_1E02,nlap_init_1E02)
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E02,ilap_init_2E02,nlap_init_2E02)
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E02,ilap_init_3E02,nlap_init_3E02)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E02,ilap_init_4E02,nlap_init_4E02)
  else if (rlen.gt.5.D02 .and. rlen.le.6.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E02,ilap_init_5E02,nlap_init_5E02)
  else if (rlen.gt.6.D02 .and. rlen.le.7.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E02,ilap_init_6E02,nlap_init_6E02)
  else if (rlen.gt.7.D02 .and. rlen.le.8.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E02,ilap_init_7E02,nlap_init_7E02)
  else if (rlen.gt.8.D02 .and. rlen.le.9.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E02,ilap_init_8E02,nlap_init_8E02)
  else if (rlen.gt.9.D02 .and. rlen.le.1.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E02,ilap_init_9E02,nlap_init_9E02)
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E03,ilap_init_1E03,nlap_init_1E03)
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E03,ilap_init_2E03,nlap_init_2E03)
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E03,ilap_init_3E03,nlap_init_3E03)
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E03,ilap_init_4E03,nlap_init_4E03)
  else if (rlen.gt.5.D03 .and. rlen.le.6.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E03,ilap_init_5E03,nlap_init_5E03)
  else if (rlen.gt.6.D03 .and. rlen.le.7.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E03,ilap_init_6E03,nlap_init_6E03)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_7E03,ilap_init_7E03,nlap_init_7E03)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (8)                                                                                                
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.1.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E01,ilap_init_1E01,nlap_init_1E01)
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E01,ilap_init_1E01,nlap_init_1E01)
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E01,ilap_init_2E01,nlap_init_2E01)
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E01,ilap_init_3E01,nlap_init_3E01)
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E01,ilap_init_4E01,nlap_init_4E01)
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E01,ilap_init_5E01,nlap_init_5E01)
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E01,ilap_init_6E01,nlap_init_6E01)
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E01,ilap_init_7E01,nlap_init_7E01)
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E01,ilap_init_8E01,nlap_init_8E01)
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E01,ilap_init_9E01,nlap_init_9E01)
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E02,ilap_init_1E02,nlap_init_1E02)
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E02,ilap_init_2E02,nlap_init_2E02)
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E02,ilap_init_3E02,nlap_init_3E02)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E02,ilap_init_4E02,nlap_init_4E02)
  else if (rlen.gt.5.D02 .and. rlen.le.7.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E02,ilap_init_5E02,nlap_init_5E02)
  else if (rlen.gt.7.D02 .and. rlen.le.1.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E02,ilap_init_7E02,nlap_init_7E02)
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E03,ilap_init_1E03,nlap_init_1E03)
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E03,ilap_init_2E03,nlap_init_2E03)
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E03,ilap_init_3E03,nlap_init_3E03)
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E03,ilap_init_4E03,nlap_init_4E03)
  else if (rlen.gt.5.D03 .and. rlen.le.7.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E03,ilap_init_5E03,nlap_init_5E03)
  else if (rlen.gt.7.D03 .and. rlen.le.1.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E03,ilap_init_7E03,nlap_init_7E03)
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E04,ilap_init_1E04,nlap_init_1E04)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_2E04,ilap_init_2E04,nlap_init_2E04)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (9)                                                                                                
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.1.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E01,ilap_init_1E01,nlap_init_1E01)
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E01,ilap_init_1E01,nlap_init_1E01)
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E01,ilap_init_2E01,nlap_init_2E01)
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E01,ilap_init_3E01,nlap_init_3E01)
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E01,ilap_init_4E01,nlap_init_4E01)
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E01,ilap_init_5E01,nlap_init_5E01)
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E01,ilap_init_6E01,nlap_init_6E01)
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E01,ilap_init_7E01,nlap_init_7E01)
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E01,ilap_init_8E01,nlap_init_8E01)
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E01,ilap_init_9E01,nlap_init_9E01)
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E02,ilap_init_1E02,nlap_init_1E02)
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E02,ilap_init_2E02,nlap_init_2E02)
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E02,ilap_init_3E02,nlap_init_3E02)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E02,ilap_init_4E02,nlap_init_4E02)
  else if (rlen.gt.5.D02 .and. rlen.le.7.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E02,ilap_init_5E02,nlap_init_5E02)
  else if (rlen.gt.7.D02 .and. rlen.le.1.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E02,ilap_init_7E02,nlap_init_7E02)
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E03,ilap_init_1E03,nlap_init_1E03)
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E03,ilap_init_2E03,nlap_init_2E03)
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E03,ilap_init_3E03,nlap_init_3E03)
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E03,ilap_init_4E03,nlap_init_4E03)
  else if (rlen.gt.5.D03 .and. rlen.le.7.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E03,ilap_init_5E03,nlap_init_5E03)
  else if (rlen.gt.7.D03 .and. rlen.le.1.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E03,ilap_init_7E03,nlap_init_7E03)
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E04,ilap_init_1E04,nlap_init_1E04)
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E04,ilap_init_2E04,nlap_init_2E04)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_3E04,ilap_init_3E04,nlap_init_3E04)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (10)                                                                                               
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.1.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E01,ilap_init_1E01,nlap_init_1E01)
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E01,ilap_init_1E01,nlap_init_1E01)
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E01,ilap_init_2E01,nlap_init_2E01)
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E01,ilap_init_3E01,nlap_init_3E01)
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E01,ilap_init_4E01,nlap_init_4E01)
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E01,ilap_init_5E01,nlap_init_5E01)
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E01,ilap_init_6E01,nlap_init_6E01)
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E01,ilap_init_7E01,nlap_init_7E01)
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E01,ilap_init_8E01,nlap_init_8E01)
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E01,ilap_init_9E01,nlap_init_9E01)
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E02,ilap_init_1E02,nlap_init_1E02)
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E02,ilap_init_2E02,nlap_init_2E02)
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E02,ilap_init_3E02,nlap_init_3E02)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E02,ilap_init_4E02,nlap_init_4E02)
  else if (rlen.gt.5.D02 .and. rlen.le.7.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E02,ilap_init_5E02,nlap_init_5E02)
  else if (rlen.gt.7.D02 .and. rlen.le.1.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E02,ilap_init_7E02,nlap_init_7E02)
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E03,ilap_init_1E03,nlap_init_1E03)
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E03,ilap_init_2E03,nlap_init_2E03)
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E03,ilap_init_3E03,nlap_init_3E03)
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E03,ilap_init_4E03,nlap_init_4E03)
  else if (rlen.gt.5.D03 .and. rlen.le.7.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E03,ilap_init_5E03,nlap_init_5E03)
  else if (rlen.gt.7.D03 .and. rlen.le.1.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E03,ilap_init_7E03,nlap_init_7E03)
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E04,ilap_init_1E04,nlap_init_1E04)
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E04,ilap_init_2E04,nlap_init_2E04)
  else if (rlen.gt.3.D04 .and. rlen.le.5.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E04,ilap_init_3E04,nlap_init_3E04)
  else if (rlen.gt.5.D04 .and. rlen.le.1.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E04,ilap_init_5E04,nlap_init_5E04)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_1E05,ilap_init_1E05,nlap_init_1E05)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (11)                                                                                               
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.1.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E01,ilap_init_1E01,nlap_init_1E01)
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E01,ilap_init_1E01,nlap_init_1E01)
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E01,ilap_init_2E01,nlap_init_2E01)
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E01,ilap_init_3E01,nlap_init_3E01)
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E01,ilap_init_4E01,nlap_init_4E01)
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E01,ilap_init_5E01,nlap_init_5E01)
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E01,ilap_init_6E01,nlap_init_6E01)
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E01,ilap_init_7E01,nlap_init_7E01)
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E01,ilap_init_8E01,nlap_init_8E01)
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E01,ilap_init_9E01,nlap_init_9E01)
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E02,ilap_init_1E02,nlap_init_1E02)
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E02,ilap_init_2E02,nlap_init_2E02)
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E02,ilap_init_3E02,nlap_init_3E02)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E02,ilap_init_4E02,nlap_init_4E02)
  else if (rlen.gt.5.D02 .and. rlen.le.7.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E02,ilap_init_5E02,nlap_init_5E02)
  else if (rlen.gt.7.D02 .and. rlen.le.1.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E02,ilap_init_7E02,nlap_init_7E02)
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E03,ilap_init_1E03,nlap_init_1E03)
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E03,ilap_init_2E03,nlap_init_2E03)
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E03,ilap_init_3E03,nlap_init_3E03)
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E03,ilap_init_4E03,nlap_init_4E03)
  else if (rlen.gt.5.D03 .and. rlen.le.7.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E03,ilap_init_5E03,nlap_init_5E03)
  else if (rlen.gt.7.D03 .and. rlen.le.1.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E03,ilap_init_7E03,nlap_init_7E03)
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E04,ilap_init_1E04,nlap_init_1E04)
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E04,ilap_init_2E04,nlap_init_2E04)
  else if (rlen.gt.3.D04 .and. rlen.le.5.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E04,ilap_init_3E04,nlap_init_3E04)
  else if (rlen.gt.5.D04 .and. rlen.le.1.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E04,ilap_init_5E04,nlap_init_5E04)
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E05,ilap_init_1E05,nlap_init_1E05)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_2E05,ilap_init_2E05,nlap_init_2E05)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (12)                                                                                               
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.1.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E01,ilap_init_1E01,nlap_init_1E01)
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E01,ilap_init_1E01,nlap_init_1E01)
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E01,ilap_init_2E01,nlap_init_2E01)
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E01,ilap_init_3E01,nlap_init_3E01)
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E01,ilap_init_4E01,nlap_init_4E01)
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E01,ilap_init_5E01,nlap_init_5E01)
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E01,ilap_init_6E01,nlap_init_6E01)
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E01,ilap_init_7E01,nlap_init_7E01)
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E01,ilap_init_8E01,nlap_init_8E01)
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E01,ilap_init_9E01,nlap_init_9E01)
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E02,ilap_init_1E02,nlap_init_1E02)
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E02,ilap_init_2E02,nlap_init_2E02)
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E02,ilap_init_3E02,nlap_init_3E02)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E02,ilap_init_4E02,nlap_init_4E02)
  else if (rlen.gt.5.D02 .and. rlen.le.7.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E02,ilap_init_5E02,nlap_init_5E02)
  else if (rlen.gt.7.D02 .and. rlen.le.1.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E02,ilap_init_7E02,nlap_init_7E02)
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E03,ilap_init_1E03,nlap_init_1E03)
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E03,ilap_init_2E03,nlap_init_2E03)
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E03,ilap_init_3E03,nlap_init_3E03)
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E03,ilap_init_4E03,nlap_init_4E03)
  else if (rlen.gt.5.D03 .and. rlen.le.7.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E03,ilap_init_5E03,nlap_init_5E03)
  else if (rlen.gt.7.D03 .and. rlen.le.1.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E03,ilap_init_7E03,nlap_init_7E03)
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E04,ilap_init_1E04,nlap_init_1E04)
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E04,ilap_init_2E04,nlap_init_2E04)
  else if (rlen.gt.3.D04 .and. rlen.le.5.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E04,ilap_init_3E04,nlap_init_3E04)
  else if (rlen.gt.5.D04 .and. rlen.le.1.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E04,ilap_init_5E04,nlap_init_5E04)
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E05,ilap_init_1E05,nlap_init_1E05)
  else if (rlen.gt.2.D05 .and. rlen.le.3.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E05,ilap_init_2E05,nlap_init_2E05)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_3E05,ilap_init_3E05,nlap_init_3E05)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (13)                                                                                               
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.1.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E01,ilap_init_1E01,nlap_init_1E01)
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E01,ilap_init_1E01,nlap_init_1E01)
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E01,ilap_init_2E01,nlap_init_2E01)
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E01,ilap_init_3E01,nlap_init_3E01)
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E01,ilap_init_4E01,nlap_init_4E01)
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E01,ilap_init_5E01,nlap_init_5E01)
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E01,ilap_init_6E01,nlap_init_6E01)
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E01,ilap_init_7E01,nlap_init_7E01)
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E01,ilap_init_8E01,nlap_init_8E01)
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E01,ilap_init_9E01,nlap_init_9E01)
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E02,ilap_init_1E02,nlap_init_1E02)
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E02,ilap_init_2E02,nlap_init_2E02)
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E02,ilap_init_3E02,nlap_init_3E02)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E02,ilap_init_4E02,nlap_init_4E02)
  else if (rlen.gt.5.D02 .and. rlen.le.7.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E02,ilap_init_5E02,nlap_init_5E02)
  else if (rlen.gt.7.D02 .and. rlen.le.1.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E02,ilap_init_7E02,nlap_init_7E02)
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E03,ilap_init_1E03,nlap_init_1E03)
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E03,ilap_init_2E03,nlap_init_2E03)
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E03,ilap_init_3E03,nlap_init_3E03)
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E03,ilap_init_4E03,nlap_init_4E03)
  else if (rlen.gt.5.D03 .and. rlen.le.7.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E03,ilap_init_5E03,nlap_init_5E03)
  else if (rlen.gt.7.D03 .and. rlen.le.1.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E03,ilap_init_7E03,nlap_init_7E03)
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E04,ilap_init_1E04,nlap_init_1E04)
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E04,ilap_init_2E04,nlap_init_2E04)
  else if (rlen.gt.3.D04 .and. rlen.le.5.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E04,ilap_init_3E04,nlap_init_3E04)
  else if (rlen.gt.5.D04 .and. rlen.le.1.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E04,ilap_init_5E04,nlap_init_5E04)
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E05,ilap_init_1E05,nlap_init_1E05)
  else if (rlen.gt.2.D05 .and. rlen.le.3.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E05,ilap_init_2E05,nlap_init_2E05)
  else if (rlen.gt.3.D05 .and. rlen.le.4.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E05,ilap_init_3E05,nlap_init_3E05)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_4E05,ilap_init_4E05,nlap_init_4E05)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (14)                                                                                               
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.1.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E01,ilap_init_1E01,nlap_init_1E01)
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E01,ilap_init_1E01,nlap_init_1E01)
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E01,ilap_init_2E01,nlap_init_2E01)
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E01,ilap_init_3E01,nlap_init_3E01)
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E01,ilap_init_4E01,nlap_init_4E01)
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E01,ilap_init_5E01,nlap_init_5E01)
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E01,ilap_init_6E01,nlap_init_6E01)
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E01,ilap_init_7E01,nlap_init_7E01)
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E01,ilap_init_8E01,nlap_init_8E01)
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E01,ilap_init_9E01,nlap_init_9E01)
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E02,ilap_init_1E02,nlap_init_1E02)
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E02,ilap_init_2E02,nlap_init_2E02)
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E02,ilap_init_3E02,nlap_init_3E02)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E02,ilap_init_4E02,nlap_init_4E02)
  else if (rlen.gt.5.D02 .and. rlen.le.7.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E02,ilap_init_5E02,nlap_init_5E02)
  else if (rlen.gt.7.D02 .and. rlen.le.1.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E02,ilap_init_7E02,nlap_init_7E02)
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E03,ilap_init_1E03,nlap_init_1E03)
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E03,ilap_init_2E03,nlap_init_2E03)
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E03,ilap_init_3E03,nlap_init_3E03)
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E03,ilap_init_4E03,nlap_init_4E03)
  else if (rlen.gt.5.D03 .and. rlen.le.7.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E03,ilap_init_5E03,nlap_init_5E03)
  else if (rlen.gt.7.D03 .and. rlen.le.1.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E03,ilap_init_7E03,nlap_init_7E03)
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E04,ilap_init_1E04,nlap_init_1E04)
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E04,ilap_init_2E04,nlap_init_2E04)
  else if (rlen.gt.3.D04 .and. rlen.le.5.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E04,ilap_init_3E04,nlap_init_3E04)
  else if (rlen.gt.5.D04 .and. rlen.le.1.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E04,ilap_init_5E04,nlap_init_5E04)
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E05,ilap_init_1E05,nlap_init_1E05)
  else if (rlen.gt.2.D05 .and. rlen.le.3.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E05,ilap_init_2E05,nlap_init_2E05)
  else if (rlen.gt.3.D05 .and. rlen.le.4.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E05,ilap_init_3E05,nlap_init_3E05)
  else if (rlen.gt.4.D05 .and. rlen.le.5.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E05,ilap_init_4E05,nlap_init_4E05)
  else if (rlen.gt.5.D05 .and. rlen.le.7.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E05,ilap_init_5E05,nlap_init_5E05)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_7E05,ilap_init_7E05,nlap_init_7E05)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (15)                                                                                               
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.1.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E01,ilap_init_1E01,nlap_init_1E01)
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E01,ilap_init_1E01,nlap_init_1E01)
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E01,ilap_init_2E01,nlap_init_2E01)
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E01,ilap_init_3E01,nlap_init_3E01)
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E01,ilap_init_4E01,nlap_init_4E01)
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E01,ilap_init_5E01,nlap_init_5E01)
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E01,ilap_init_6E01,nlap_init_6E01)
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E01,ilap_init_7E01,nlap_init_7E01)
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E01,ilap_init_8E01,nlap_init_8E01)
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E01,ilap_init_9E01,nlap_init_9E01)
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E02,ilap_init_1E02,nlap_init_1E02)
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E02,ilap_init_2E02,nlap_init_2E02)
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E02,ilap_init_3E02,nlap_init_3E02)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E02,ilap_init_4E02,nlap_init_4E02)
  else if (rlen.gt.5.D02 .and. rlen.le.6.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E02,ilap_init_5E02,nlap_init_5E02)
  else if (rlen.gt.6.D02 .and. rlen.le.7.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E02,ilap_init_6E02,nlap_init_6E02)
  else if (rlen.gt.7.D02 .and. rlen.le.8.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E02,ilap_init_7E02,nlap_init_7E02)
  else if (rlen.gt.8.D02 .and. rlen.le.9.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E02,ilap_init_8E02,nlap_init_8E02)
  else if (rlen.gt.9.D02 .and. rlen.le.1.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E02,ilap_init_9E02,nlap_init_9E02)
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E03,ilap_init_1E03,nlap_init_1E03)
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E03,ilap_init_2E03,nlap_init_2E03)
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E03,ilap_init_3E03,nlap_init_3E03)
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E03,ilap_init_4E03,nlap_init_4E03)
  else if (rlen.gt.5.D03 .and. rlen.le.6.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E03,ilap_init_5E03,nlap_init_5E03)
  else if (rlen.gt.6.D03 .and. rlen.le.7.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E03,ilap_init_6E03,nlap_init_6E03)
  else if (rlen.gt.7.D03 .and. rlen.le.8.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E03,ilap_init_7E03,nlap_init_7E03)
  else if (rlen.gt.8.D03 .and. rlen.le.9.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E03,ilap_init_8E03,nlap_init_8E03)
  else if (rlen.gt.9.D03 .and. rlen.le.1.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E03,ilap_init_9E03,nlap_init_9E03)
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E04,ilap_init_1E04,nlap_init_1E04)
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E04,ilap_init_2E04,nlap_init_2E04)
  else if (rlen.gt.3.D04 .and. rlen.le.4.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E04,ilap_init_3E04,nlap_init_3E04)
  else if (rlen.gt.4.D04 .and. rlen.le.5.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E04,ilap_init_4E04,nlap_init_4E04)
  else if (rlen.gt.5.D04 .and. rlen.le.7.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E04,ilap_init_5E04,nlap_init_5E04)
  else if (rlen.gt.7.D04 .and. rlen.le.1.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E04,ilap_init_7E04,nlap_init_7E04)
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E05,ilap_init_1E05,nlap_init_1E05)
  else if (rlen.gt.2.D05 .and. rlen.le.5.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E05,ilap_init_2E05,nlap_init_2E05)
  else if (rlen.gt.5.D05 .and. rlen.le.1.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E05,ilap_init_5E05,nlap_init_5E05)
  else if (rlen.gt.1.D06 .and. rlen.le.2.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E06,ilap_init_1E06,nlap_init_1E06)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_2E06,ilap_init_2E06,nlap_init_2E06)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (16)                                                                                               
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.1.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E01,ilap_init_1E01,nlap_init_1E01)
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E01,ilap_init_1E01,nlap_init_1E01)
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E01,ilap_init_2E01,nlap_init_2E01)
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E01,ilap_init_3E01,nlap_init_3E01)
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E01,ilap_init_4E01,nlap_init_4E01)
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E01,ilap_init_5E01,nlap_init_5E01)
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E01,ilap_init_6E01,nlap_init_6E01)
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E01,ilap_init_7E01,nlap_init_7E01)
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E01,ilap_init_8E01,nlap_init_8E01)
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E01,ilap_init_9E01,nlap_init_9E01)
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E02,ilap_init_1E02,nlap_init_1E02)
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E02,ilap_init_2E02,nlap_init_2E02)
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E02,ilap_init_3E02,nlap_init_3E02)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E02,ilap_init_4E02,nlap_init_4E02)
  else if (rlen.gt.5.D02 .and. rlen.le.6.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E02,ilap_init_5E02,nlap_init_5E02)
  else if (rlen.gt.6.D02 .and. rlen.le.7.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E02,ilap_init_6E02,nlap_init_6E02)
  else if (rlen.gt.7.D02 .and. rlen.le.8.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E02,ilap_init_7E02,nlap_init_7E02)
  else if (rlen.gt.8.D02 .and. rlen.le.9.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E02,ilap_init_8E02,nlap_init_8E02)
  else if (rlen.gt.9.D02 .and. rlen.le.1.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E02,ilap_init_9E02,nlap_init_9E02)
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E03,ilap_init_1E03,nlap_init_1E03)
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E03,ilap_init_2E03,nlap_init_2E03)
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E03,ilap_init_3E03,nlap_init_3E03)
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E03,ilap_init_4E03,nlap_init_4E03)
  else if (rlen.gt.5.D03 .and. rlen.le.6.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E03,ilap_init_5E03,nlap_init_5E03)
  else if (rlen.gt.6.D03 .and. rlen.le.7.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E03,ilap_init_6E03,nlap_init_6E03)
  else if (rlen.gt.7.D03 .and. rlen.le.8.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E03,ilap_init_7E03,nlap_init_7E03)
  else if (rlen.gt.8.D03 .and. rlen.le.9.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E03,ilap_init_8E03,nlap_init_8E03)
  else if (rlen.gt.9.D03 .and. rlen.le.1.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E03,ilap_init_9E03,nlap_init_9E03)
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E04,ilap_init_1E04,nlap_init_1E04)
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E04,ilap_init_2E04,nlap_init_2E04)
  else if (rlen.gt.3.D04 .and. rlen.le.4.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E04,ilap_init_3E04,nlap_init_3E04)
  else if (rlen.gt.4.D04 .and. rlen.le.5.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E04,ilap_init_4E04,nlap_init_4E04)
  else if (rlen.gt.5.D04 .and. rlen.le.7.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E04,ilap_init_5E04,nlap_init_5E04)
  else if (rlen.gt.7.D04 .and. rlen.le.1.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E04,ilap_init_7E04,nlap_init_7E04)
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E05,ilap_init_1E05,nlap_init_1E05)
  else if (rlen.gt.2.D05 .and. rlen.le.5.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E05,ilap_init_2E05,nlap_init_2E05)
  else if (rlen.gt.5.D05 .and. rlen.le.1.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E05,ilap_init_5E05,nlap_init_5E05)
  else if (rlen.gt.1.D06 .and. rlen.le.2.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E06,ilap_init_1E06,nlap_init_1E06)
  else if (rlen.gt.2.D06 .and. rlen.le.3.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E06,ilap_init_2E06,nlap_init_2E06)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_3E06,ilap_init_3E06,nlap_init_3E06)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (17)                                                                                               
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.2.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E01,ilap_init_2E01,nlap_init_2E01)
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E01,ilap_init_2E01,nlap_init_2E01)
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E01,ilap_init_3E01,nlap_init_3E01)
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E01,ilap_init_4E01,nlap_init_4E01)
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E01,ilap_init_5E01,nlap_init_5E01)
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E01,ilap_init_6E01,nlap_init_6E01)
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E01,ilap_init_7E01,nlap_init_7E01)
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E01,ilap_init_8E01,nlap_init_8E01)
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E01,ilap_init_9E01,nlap_init_9E01)
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E02,ilap_init_1E02,nlap_init_1E02)
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E02,ilap_init_2E02,nlap_init_2E02)
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E02,ilap_init_3E02,nlap_init_3E02)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E02,ilap_init_4E02,nlap_init_4E02)
  else if (rlen.gt.5.D02 .and. rlen.le.6.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E02,ilap_init_5E02,nlap_init_5E02)
  else if (rlen.gt.6.D02 .and. rlen.le.7.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E02,ilap_init_6E02,nlap_init_6E02)
  else if (rlen.gt.7.D02 .and. rlen.le.8.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E02,ilap_init_7E02,nlap_init_7E02)
  else if (rlen.gt.8.D02 .and. rlen.le.9.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E02,ilap_init_8E02,nlap_init_8E02)
  else if (rlen.gt.9.D02 .and. rlen.le.1.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E02,ilap_init_9E02,nlap_init_9E02)
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E03,ilap_init_1E03,nlap_init_1E03)
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E03,ilap_init_2E03,nlap_init_2E03)
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E03,ilap_init_3E03,nlap_init_3E03)
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E03,ilap_init_4E03,nlap_init_4E03)
  else if (rlen.gt.5.D03 .and. rlen.le.6.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E03,ilap_init_5E03,nlap_init_5E03)
  else if (rlen.gt.6.D03 .and. rlen.le.7.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E03,ilap_init_6E03,nlap_init_6E03)
  else if (rlen.gt.7.D03 .and. rlen.le.8.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E03,ilap_init_7E03,nlap_init_7E03)
  else if (rlen.gt.8.D03 .and. rlen.le.9.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E03,ilap_init_8E03,nlap_init_8E03)
  else if (rlen.gt.9.D03 .and. rlen.le.1.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E03,ilap_init_9E03,nlap_init_9E03)
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E04,ilap_init_1E04,nlap_init_1E04)
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E04,ilap_init_2E04,nlap_init_2E04)
  else if (rlen.gt.3.D04 .and. rlen.le.4.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E04,ilap_init_3E04,nlap_init_3E04)
  else if (rlen.gt.4.D04 .and. rlen.le.5.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E04,ilap_init_4E04,nlap_init_4E04)
  else if (rlen.gt.5.D04 .and. rlen.le.7.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E04,ilap_init_5E04,nlap_init_5E04)
  else if (rlen.gt.7.D04 .and. rlen.le.1.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E04,ilap_init_7E04,nlap_init_7E04)
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E05,ilap_init_1E05,nlap_init_1E05)
  else if (rlen.gt.2.D05 .and. rlen.le.5.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E05,ilap_init_2E05,nlap_init_2E05)
  else if (rlen.gt.5.D05 .and. rlen.le.1.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E05,ilap_init_5E05,nlap_init_5E05)
  else if (rlen.gt.1.D06 .and. rlen.le.2.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E06,ilap_init_1E06,nlap_init_1E06)
  else if (rlen.gt.2.D06 .and. rlen.le.3.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E06,ilap_init_2E06,nlap_init_2E06)
  else if (rlen.gt.3.D06 .and. rlen.le.4.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E06,ilap_init_3E06,nlap_init_3E06)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_4E06,ilap_init_4E06,nlap_init_4E06)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (18)                                                                                               
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.4.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E01,ilap_init_4E01,nlap_init_4E01)
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E01,ilap_init_4E01,nlap_init_4E01)
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E01,ilap_init_5E01,nlap_init_5E01)
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E01,ilap_init_6E01,nlap_init_6E01)
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E01,ilap_init_7E01,nlap_init_7E01)
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E01,ilap_init_8E01,nlap_init_8E01)
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E01,ilap_init_9E01,nlap_init_9E01)
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E02,ilap_init_1E02,nlap_init_1E02)
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E02,ilap_init_2E02,nlap_init_2E02)
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E02,ilap_init_3E02,nlap_init_3E02)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E02,ilap_init_4E02,nlap_init_4E02)
  else if (rlen.gt.5.D02 .and. rlen.le.6.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E02,ilap_init_5E02,nlap_init_5E02)
  else if (rlen.gt.6.D02 .and. rlen.le.7.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E02,ilap_init_6E02,nlap_init_6E02)
  else if (rlen.gt.7.D02 .and. rlen.le.8.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E02,ilap_init_7E02,nlap_init_7E02)
  else if (rlen.gt.8.D02 .and. rlen.le.9.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E02,ilap_init_8E02,nlap_init_8E02)
  else if (rlen.gt.9.D02 .and. rlen.le.1.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E02,ilap_init_9E02,nlap_init_9E02)
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E03,ilap_init_1E03,nlap_init_1E03)
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E03,ilap_init_2E03,nlap_init_2E03)
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E03,ilap_init_3E03,nlap_init_3E03)
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E03,ilap_init_4E03,nlap_init_4E03)
  else if (rlen.gt.5.D03 .and. rlen.le.6.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E03,ilap_init_5E03,nlap_init_5E03)
  else if (rlen.gt.6.D03 .and. rlen.le.7.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E03,ilap_init_6E03,nlap_init_6E03)
  else if (rlen.gt.7.D03 .and. rlen.le.8.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E03,ilap_init_7E03,nlap_init_7E03)
  else if (rlen.gt.8.D03 .and. rlen.le.9.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E03,ilap_init_8E03,nlap_init_8E03)
  else if (rlen.gt.9.D03 .and. rlen.le.1.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E03,ilap_init_9E03,nlap_init_9E03)
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E04,ilap_init_1E04,nlap_init_1E04)
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E04,ilap_init_2E04,nlap_init_2E04)
  else if (rlen.gt.3.D04 .and. rlen.le.4.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E04,ilap_init_3E04,nlap_init_3E04)
  else if (rlen.gt.4.D04 .and. rlen.le.5.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E04,ilap_init_4E04,nlap_init_4E04)
  else if (rlen.gt.5.D04 .and. rlen.le.7.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E04,ilap_init_5E04,nlap_init_5E04)
  else if (rlen.gt.7.D04 .and. rlen.le.1.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E04,ilap_init_7E04,nlap_init_7E04)
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E05,ilap_init_1E05,nlap_init_1E05)
  else if (rlen.gt.2.D05 .and. rlen.le.5.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E05,ilap_init_2E05,nlap_init_2E05)
  else if (rlen.gt.5.D05 .and. rlen.le.1.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E05,ilap_init_5E05,nlap_init_5E05)
  else if (rlen.gt.1.D06 .and. rlen.le.2.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E06,ilap_init_1E06,nlap_init_1E06)
  else if (rlen.gt.2.D06 .and. rlen.le.3.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E06,ilap_init_2E06,nlap_init_2E06)
  else if (rlen.gt.3.D06 .and. rlen.le.4.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E06,ilap_init_3E06,nlap_init_3E06)
  else if (rlen.gt.4.D06 .and. rlen.le.5.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E06,ilap_init_4E06,nlap_init_4E06)
  else if (rlen.gt.5.D06 .and. rlen.le.7.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E06,ilap_init_5E06,nlap_init_5E06)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_7E06,ilap_init_7E06,nlap_init_7E06)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (19)                                                                                               
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.5.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E01,ilap_init_5E01,nlap_init_5E01)
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E01,ilap_init_5E01,nlap_init_5E01)
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E01,ilap_init_6E01,nlap_init_6E01)
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E01,ilap_init_7E01,nlap_init_7E01)
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E01,ilap_init_8E01,nlap_init_8E01)
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E01,ilap_init_9E01,nlap_init_9E01)
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E02,ilap_init_1E02,nlap_init_1E02)
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E02,ilap_init_2E02,nlap_init_2E02)
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E02,ilap_init_3E02,nlap_init_3E02)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E02,ilap_init_4E02,nlap_init_4E02)
  else if (rlen.gt.5.D02 .and. rlen.le.6.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E02,ilap_init_5E02,nlap_init_5E02)
  else if (rlen.gt.6.D02 .and. rlen.le.7.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E02,ilap_init_6E02,nlap_init_6E02)
  else if (rlen.gt.7.D02 .and. rlen.le.8.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E02,ilap_init_7E02,nlap_init_7E02)
  else if (rlen.gt.8.D02 .and. rlen.le.9.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E02,ilap_init_8E02,nlap_init_8E02)
  else if (rlen.gt.9.D02 .and. rlen.le.1.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E02,ilap_init_9E02,nlap_init_9E02)
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E03,ilap_init_1E03,nlap_init_1E03)
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E03,ilap_init_2E03,nlap_init_2E03)
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E03,ilap_init_3E03,nlap_init_3E03)
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E03,ilap_init_4E03,nlap_init_4E03)
  else if (rlen.gt.5.D03 .and. rlen.le.6.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E03,ilap_init_5E03,nlap_init_5E03)
  else if (rlen.gt.6.D03 .and. rlen.le.7.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E03,ilap_init_6E03,nlap_init_6E03)
  else if (rlen.gt.7.D03 .and. rlen.le.8.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E03,ilap_init_7E03,nlap_init_7E03)
  else if (rlen.gt.8.D03 .and. rlen.le.9.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E03,ilap_init_8E03,nlap_init_8E03)
  else if (rlen.gt.9.D03 .and. rlen.le.1.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E03,ilap_init_9E03,nlap_init_9E03)
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E04,ilap_init_1E04,nlap_init_1E04)
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E04,ilap_init_2E04,nlap_init_2E04)
  else if (rlen.gt.3.D04 .and. rlen.le.4.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E04,ilap_init_3E04,nlap_init_3E04)
  else if (rlen.gt.4.D04 .and. rlen.le.5.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E04,ilap_init_4E04,nlap_init_4E04)
  else if (rlen.gt.5.D04 .and. rlen.le.7.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E04,ilap_init_5E04,nlap_init_5E04)
  else if (rlen.gt.7.D04 .and. rlen.le.1.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E04,ilap_init_7E04,nlap_init_7E04)
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E05,ilap_init_1E05,nlap_init_1E05)
  else if (rlen.gt.2.D05 .and. rlen.le.5.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E05,ilap_init_2E05,nlap_init_2E05)
  else if (rlen.gt.5.D05 .and. rlen.le.1.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E05,ilap_init_5E05,nlap_init_5E05)
  else if (rlen.gt.1.D06 .and. rlen.le.2.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E06,ilap_init_1E06,nlap_init_1E06)
  else if (rlen.gt.2.D06 .and. rlen.le.3.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E06,ilap_init_2E06,nlap_init_2E06)
  else if (rlen.gt.3.D06 .and. rlen.le.4.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E06,ilap_init_3E06,nlap_init_3E06)
  else if (rlen.gt.4.D06 .and. rlen.le.5.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E06,ilap_init_4E06,nlap_init_4E06)
  else if (rlen.gt.5.D06 .and. rlen.le.7.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E06,ilap_init_5E06,nlap_init_5E06)
  else if (rlen.gt.7.D06 .and. rlen.le.1.D07) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E06,ilap_init_7E06,nlap_init_7E06)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_1E07,ilap_init_1E07,nlap_init_1E07)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (20)                                                                                               
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.6.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E01,ilap_init_6E01,nlap_init_6E01)
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E01,ilap_init_6E01,nlap_init_6E01)
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E01,ilap_init_7E01,nlap_init_7E01)
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E01,ilap_init_8E01,nlap_init_8E01)
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E01,ilap_init_9E01,nlap_init_9E01)
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E02,ilap_init_1E02,nlap_init_1E02)
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E02,ilap_init_2E02,nlap_init_2E02)
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E02,ilap_init_3E02,nlap_init_3E02)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E02,ilap_init_4E02,nlap_init_4E02)
  else if (rlen.gt.5.D02 .and. rlen.le.6.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E02,ilap_init_5E02,nlap_init_5E02)
  else if (rlen.gt.6.D02 .and. rlen.le.7.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E02,ilap_init_6E02,nlap_init_6E02)
  else if (rlen.gt.7.D02 .and. rlen.le.8.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E02,ilap_init_7E02,nlap_init_7E02)
  else if (rlen.gt.8.D02 .and. rlen.le.9.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E02,ilap_init_8E02,nlap_init_8E02)
  else if (rlen.gt.9.D02 .and. rlen.le.1.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E02,ilap_init_9E02,nlap_init_9E02)
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E03,ilap_init_1E03,nlap_init_1E03)
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E03,ilap_init_2E03,nlap_init_2E03)
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E03,ilap_init_3E03,nlap_init_3E03)
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E03,ilap_init_4E03,nlap_init_4E03)
  else if (rlen.gt.5.D03 .and. rlen.le.6.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E03,ilap_init_5E03,nlap_init_5E03)
  else if (rlen.gt.6.D03 .and. rlen.le.7.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E03,ilap_init_6E03,nlap_init_6E03)
  else if (rlen.gt.7.D03 .and. rlen.le.8.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E03,ilap_init_7E03,nlap_init_7E03)
  else if (rlen.gt.8.D03 .and. rlen.le.9.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E03,ilap_init_8E03,nlap_init_8E03)
  else if (rlen.gt.9.D03 .and. rlen.le.1.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E03,ilap_init_9E03,nlap_init_9E03)
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E04,ilap_init_1E04,nlap_init_1E04)
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E04,ilap_init_2E04,nlap_init_2E04)
  else if (rlen.gt.3.D04 .and. rlen.le.4.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E04,ilap_init_3E04,nlap_init_3E04)
  else if (rlen.gt.4.D04 .and. rlen.le.5.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E04,ilap_init_4E04,nlap_init_4E04)
  else if (rlen.gt.5.D04 .and. rlen.le.7.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E04,ilap_init_5E04,nlap_init_5E04)
  else if (rlen.gt.7.D04 .and. rlen.le.1.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E04,ilap_init_7E04,nlap_init_7E04)
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E05,ilap_init_1E05,nlap_init_1E05)
  else if (rlen.gt.2.D05 .and. rlen.le.5.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E05,ilap_init_2E05,nlap_init_2E05)
  else if (rlen.gt.5.D05 .and. rlen.le.1.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E05,ilap_init_5E05,nlap_init_5E05)
  else if (rlen.gt.1.D06 .and. rlen.le.2.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E06,ilap_init_1E06,nlap_init_1E06)
  else if (rlen.gt.2.D06 .and. rlen.le.3.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E06,ilap_init_2E06,nlap_init_2E06)
  else if (rlen.gt.3.D06 .and. rlen.le.4.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E06,ilap_init_3E06,nlap_init_3E06)
  else if (rlen.gt.4.D06 .and. rlen.le.5.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E06,ilap_init_4E06,nlap_init_4E06)
  else if (rlen.gt.5.D06 .and. rlen.le.7.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E06,ilap_init_5E06,nlap_init_5E06)
  else if (rlen.gt.7.D06 .and. rlen.le.1.D07) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E06,ilap_init_7E06,nlap_init_7E06)
  else if (rlen.gt.1.D07 .and. rlen.le.2.D07) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E07,ilap_init_1E07,nlap_init_1E07)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_2E07,ilap_init_2E07,nlap_init_2E07)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (21)                                                                                               
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.9.D01) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E01,ilap_init_9E01,nlap_init_9E01)
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E01,ilap_init_9E01,nlap_init_9E01)
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E02,ilap_init_1E02,nlap_init_1E02)
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E02,ilap_init_2E02,nlap_init_2E02)
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E02,ilap_init_3E02,nlap_init_3E02)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E02,ilap_init_4E02,nlap_init_4E02)
  else if (rlen.gt.5.D02 .and. rlen.le.6.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E02,ilap_init_5E02,nlap_init_5E02)
  else if (rlen.gt.6.D02 .and. rlen.le.7.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E02,ilap_init_6E02,nlap_init_6E02)
  else if (rlen.gt.7.D02 .and. rlen.le.8.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E02,ilap_init_7E02,nlap_init_7E02)
  else if (rlen.gt.8.D02 .and. rlen.le.9.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E02,ilap_init_8E02,nlap_init_8E02)
  else if (rlen.gt.9.D02 .and. rlen.le.1.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E02,ilap_init_9E02,nlap_init_9E02)
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E03,ilap_init_1E03,nlap_init_1E03)
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E03,ilap_init_2E03,nlap_init_2E03)
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E03,ilap_init_3E03,nlap_init_3E03)
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E03,ilap_init_4E03,nlap_init_4E03)
  else if (rlen.gt.5.D03 .and. rlen.le.6.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E03,ilap_init_5E03,nlap_init_5E03)
  else if (rlen.gt.6.D03 .and. rlen.le.7.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E03,ilap_init_6E03,nlap_init_6E03)
  else if (rlen.gt.7.D03 .and. rlen.le.8.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E03,ilap_init_7E03,nlap_init_7E03)
  else if (rlen.gt.8.D03 .and. rlen.le.9.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E03,ilap_init_8E03,nlap_init_8E03)
  else if (rlen.gt.9.D03 .and. rlen.le.1.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E03,ilap_init_9E03,nlap_init_9E03)
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E04,ilap_init_1E04,nlap_init_1E04)
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E04,ilap_init_2E04,nlap_init_2E04)
  else if (rlen.gt.3.D04 .and. rlen.le.4.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E04,ilap_init_3E04,nlap_init_3E04)
  else if (rlen.gt.4.D04 .and. rlen.le.5.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E04,ilap_init_4E04,nlap_init_4E04)
  else if (rlen.gt.5.D04 .and. rlen.le.7.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E04,ilap_init_5E04,nlap_init_5E04)
  else if (rlen.gt.7.D04 .and. rlen.le.1.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E04,ilap_init_7E04,nlap_init_7E04)
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E05,ilap_init_1E05,nlap_init_1E05)
  else if (rlen.gt.2.D05 .and. rlen.le.5.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E05,ilap_init_2E05,nlap_init_2E05)
  else if (rlen.gt.5.D05 .and. rlen.le.1.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E05,ilap_init_5E05,nlap_init_5E05)
  else if (rlen.gt.1.D06 .and. rlen.le.2.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E06,ilap_init_1E06,nlap_init_1E06)
  else if (rlen.gt.2.D06 .and. rlen.le.3.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E06,ilap_init_2E06,nlap_init_2E06)
  else if (rlen.gt.3.D06 .and. rlen.le.4.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E06,ilap_init_3E06,nlap_init_3E06)
  else if (rlen.gt.4.D06 .and. rlen.le.5.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E06,ilap_init_4E06,nlap_init_4E06)
  else if (rlen.gt.5.D06 .and. rlen.le.7.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E06,ilap_init_5E06,nlap_init_5E06)
  else if (rlen.gt.7.D06 .and. rlen.le.1.D07) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E06,ilap_init_7E06,nlap_init_7E06)
  else if (rlen.gt.1.D07 .and. rlen.le.2.D07) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E07,ilap_init_1E07,nlap_init_1E07)
  else if (rlen.gt.2.D07 .and. rlen.le.3.D07) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E07,ilap_init_2E07,nlap_init_2E07)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_3E07,ilap_init_3E07,nlap_init_3E07)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (22)                                                                                               
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.1.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E02,ilap_init_1E02,nlap_init_1E02)
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E02,ilap_init_1E02,nlap_init_1E02)
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E02,ilap_init_2E02,nlap_init_2E02)
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E02,ilap_init_3E02,nlap_init_3E02)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E02,ilap_init_4E02,nlap_init_4E02)
  else if (rlen.gt.5.D02 .and. rlen.le.7.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E02,ilap_init_5E02,nlap_init_5E02)
  else if (rlen.gt.7.D02 .and. rlen.le.1.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E02,ilap_init_7E02,nlap_init_7E02)
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E03,ilap_init_1E03,nlap_init_1E03)
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E03,ilap_init_2E03,nlap_init_2E03)
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E03,ilap_init_3E03,nlap_init_3E03)
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E03,ilap_init_4E03,nlap_init_4E03)
  else if (rlen.gt.5.D03 .and. rlen.le.7.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E03,ilap_init_5E03,nlap_init_5E03)
  else if (rlen.gt.7.D03 .and. rlen.le.1.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E03,ilap_init_7E03,nlap_init_7E03)
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E04,ilap_init_1E04,nlap_init_1E04)
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E04,ilap_init_2E04,nlap_init_2E04)
  else if (rlen.gt.3.D04 .and. rlen.le.4.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E04,ilap_init_3E04,nlap_init_3E04)
  else if (rlen.gt.4.D04 .and. rlen.le.5.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E04,ilap_init_4E04,nlap_init_4E04)
  else if (rlen.gt.5.D04 .and. rlen.le.7.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E04,ilap_init_5E04,nlap_init_5E04)
  else if (rlen.gt.7.D04 .and. rlen.le.1.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E04,ilap_init_7E04,nlap_init_7E04)
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E05,ilap_init_1E05,nlap_init_1E05)
  else if (rlen.gt.2.D05 .and. rlen.le.3.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E05,ilap_init_2E05,nlap_init_2E05)
  else if (rlen.gt.3.D05 .and. rlen.le.4.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E05,ilap_init_3E05,nlap_init_3E05)
  else if (rlen.gt.4.D05 .and. rlen.le.5.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E05,ilap_init_4E05,nlap_init_4E05)
  else if (rlen.gt.5.D05 .and. rlen.le.7.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E05,ilap_init_5E05,nlap_init_5E05)
  else if (rlen.gt.7.D05 .and. rlen.le.1.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E05,ilap_init_7E05,nlap_init_7E05)
  else if (rlen.gt.1.D06 .and. rlen.le.2.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E06,ilap_init_1E06,nlap_init_1E06)
  else if (rlen.gt.2.D06 .and. rlen.le.3.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E06,ilap_init_2E06,nlap_init_2E06)
  else if (rlen.gt.3.D06 .and. rlen.le.4.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E06,ilap_init_3E06,nlap_init_3E06)
  else if (rlen.gt.4.D06 .and. rlen.le.5.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E06,ilap_init_4E06,nlap_init_4E06)
  else if (rlen.gt.5.D06 .and. rlen.le.7.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E06,ilap_init_5E06,nlap_init_5E06)
  else if (rlen.gt.7.D06 .and. rlen.le.1.D07) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E06,ilap_init_7E06,nlap_init_7E06)
  else if (rlen.gt.1.D07 .and. rlen.le.2.D07) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E07,ilap_init_1E07,nlap_init_1E07)
  else if (rlen.gt.2.D07 .and. rlen.le.3.D07) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E07,ilap_init_2E07,nlap_init_2E07)
  else if (rlen.gt.3.D07 .and. rlen.le.4.D07) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E07,ilap_init_3E07,nlap_init_3E07)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_4E07,ilap_init_4E07,nlap_init_4E07)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (23)                                                                                               
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.2.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E02,ilap_init_2E02,nlap_init_2E02)
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E02,ilap_init_2E02,nlap_init_2E02)
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E02,ilap_init_3E02,nlap_init_3E02)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E02,ilap_init_4E02,nlap_init_4E02)
  else if (rlen.gt.5.D02 .and. rlen.le.7.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E02,ilap_init_5E02,nlap_init_5E02)
  else if (rlen.gt.7.D02 .and. rlen.le.1.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E02,ilap_init_7E02,nlap_init_7E02)
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E03,ilap_init_1E03,nlap_init_1E03)
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E03,ilap_init_2E03,nlap_init_2E03)
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E03,ilap_init_3E03,nlap_init_3E03)
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E03,ilap_init_4E03,nlap_init_4E03)
  else if (rlen.gt.5.D03 .and. rlen.le.7.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E03,ilap_init_5E03,nlap_init_5E03)
  else if (rlen.gt.7.D03 .and. rlen.le.1.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E03,ilap_init_7E03,nlap_init_7E03)
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E04,ilap_init_1E04,nlap_init_1E04)
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E04,ilap_init_2E04,nlap_init_2E04)
  else if (rlen.gt.3.D04 .and. rlen.le.4.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E04,ilap_init_3E04,nlap_init_3E04)
  else if (rlen.gt.4.D04 .and. rlen.le.5.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E04,ilap_init_4E04,nlap_init_4E04)
  else if (rlen.gt.5.D04 .and. rlen.le.7.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E04,ilap_init_5E04,nlap_init_5E04)
  else if (rlen.gt.7.D04 .and. rlen.le.1.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E04,ilap_init_7E04,nlap_init_7E04)
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E05,ilap_init_1E05,nlap_init_1E05)
  else if (rlen.gt.2.D05 .and. rlen.le.3.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E05,ilap_init_2E05,nlap_init_2E05)
  else if (rlen.gt.3.D05 .and. rlen.le.4.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E05,ilap_init_3E05,nlap_init_3E05)
  else if (rlen.gt.4.D05 .and. rlen.le.5.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E05,ilap_init_4E05,nlap_init_4E05)
  else if (rlen.gt.5.D05 .and. rlen.le.7.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E05,ilap_init_5E05,nlap_init_5E05)
  else if (rlen.gt.7.D05 .and. rlen.le.1.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E05,ilap_init_7E05,nlap_init_7E05)
  else if (rlen.gt.1.D06 .and. rlen.le.2.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E06,ilap_init_1E06,nlap_init_1E06)
  else if (rlen.gt.2.D06 .and. rlen.le.3.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E06,ilap_init_2E06,nlap_init_2E06)
  else if (rlen.gt.3.D06 .and. rlen.le.4.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E06,ilap_init_3E06,nlap_init_3E06)
  else if (rlen.gt.4.D06 .and. rlen.le.5.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E06,ilap_init_4E06,nlap_init_4E06)
  else if (rlen.gt.5.D06 .and. rlen.le.7.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E06,ilap_init_5E06,nlap_init_5E06)
  else if (rlen.gt.7.D06 .and. rlen.le.1.D07) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E06,ilap_init_7E06,nlap_init_7E06)
  else if (rlen.gt.1.D07 .and. rlen.le.2.D07) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E07,ilap_init_1E07,nlap_init_1E07)
  else if (rlen.gt.2.D07 .and. rlen.le.3.D07) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E07,ilap_init_2E07,nlap_init_2E07)
  else if (rlen.gt.3.D07 .and. rlen.le.4.D07) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E07,ilap_init_3E07,nlap_init_3E07)
  else if (rlen.gt.4.D07 .and. rlen.le.5.D07) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E07,ilap_init_4E07,nlap_init_4E07)
  else if (rlen.gt.5.D07 .and. rlen.le.7.D07) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E07,ilap_init_5E07,nlap_init_5E07)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_7E07,ilap_init_7E07,nlap_init_7E07)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (24)                                                                                               
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.2.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E02,ilap_init_2E02,nlap_init_2E02)
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E02,ilap_init_2E02,nlap_init_2E02)
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E02,ilap_init_3E02,nlap_init_3E02)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E02,ilap_init_4E02,nlap_init_4E02)
  else if (rlen.gt.5.D02 .and. rlen.le.7.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E02,ilap_init_5E02,nlap_init_5E02)
  else if (rlen.gt.7.D02 .and. rlen.le.1.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E02,ilap_init_7E02,nlap_init_7E02)
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E03,ilap_init_1E03,nlap_init_1E03)
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E03,ilap_init_2E03,nlap_init_2E03)
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E03,ilap_init_3E03,nlap_init_3E03)
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E03,ilap_init_4E03,nlap_init_4E03)
  else if (rlen.gt.5.D03 .and. rlen.le.7.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E03,ilap_init_5E03,nlap_init_5E03)
  else if (rlen.gt.7.D03 .and. rlen.le.1.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E03,ilap_init_7E03,nlap_init_7E03)
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E04,ilap_init_1E04,nlap_init_1E04)
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E04,ilap_init_2E04,nlap_init_2E04)
  else if (rlen.gt.3.D04 .and. rlen.le.4.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E04,ilap_init_3E04,nlap_init_3E04)
  else if (rlen.gt.4.D04 .and. rlen.le.5.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E04,ilap_init_4E04,nlap_init_4E04)
  else if (rlen.gt.5.D04 .and. rlen.le.7.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E04,ilap_init_5E04,nlap_init_5E04)
  else if (rlen.gt.7.D04 .and. rlen.le.1.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E04,ilap_init_7E04,nlap_init_7E04)
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E05,ilap_init_1E05,nlap_init_1E05)
  else if (rlen.gt.2.D05 .and. rlen.le.3.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E05,ilap_init_2E05,nlap_init_2E05)
  else if (rlen.gt.3.D05 .and. rlen.le.4.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E05,ilap_init_3E05,nlap_init_3E05)
  else if (rlen.gt.4.D05 .and. rlen.le.5.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E05,ilap_init_4E05,nlap_init_4E05)
  else if (rlen.gt.5.D05 .and. rlen.le.7.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E05,ilap_init_5E05,nlap_init_5E05)
  else if (rlen.gt.7.D05 .and. rlen.le.1.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E05,ilap_init_7E05,nlap_init_7E05)
  else if (rlen.gt.1.D06 .and. rlen.le.2.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E06,ilap_init_1E06,nlap_init_1E06)
  else if (rlen.gt.2.D06 .and. rlen.le.3.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E06,ilap_init_2E06,nlap_init_2E06)
  else if (rlen.gt.3.D06 .and. rlen.le.4.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E06,ilap_init_3E06,nlap_init_3E06)
  else if (rlen.gt.4.D06 .and. rlen.le.5.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E06,ilap_init_4E06,nlap_init_4E06)
  else if (rlen.gt.5.D06 .and. rlen.le.7.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E06,ilap_init_5E06,nlap_init_5E06)
  else if (rlen.gt.7.D06 .and. rlen.le.1.D07) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E06,ilap_init_7E06,nlap_init_7E06)
  else if (rlen.gt.1.D07 .and. rlen.le.2.D07) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E07,ilap_init_1E07,nlap_init_1E07)
  else if (rlen.gt.2.D07 .and. rlen.le.3.D07) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E07,ilap_init_2E07,nlap_init_2E07)
  else if (rlen.gt.3.D07 .and. rlen.le.4.D07) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E07,ilap_init_3E07,nlap_init_3E07)
  else if (rlen.gt.4.D07 .and. rlen.le.5.D07) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E07,ilap_init_4E07,nlap_init_4E07)
  else if (rlen.gt.5.D07 .and. rlen.le.7.D07) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E07,ilap_init_5E07,nlap_init_5E07)
  else if (rlen.gt.7.D07 .and. rlen.le.1.D08) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E07,ilap_init_7E07,nlap_init_7E07)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_1E08,ilap_init_1E08,nlap_init_1E08)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (25)                                                                                               
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.2.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E02,ilap_init_2E02,nlap_init_2E02)
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E02,ilap_init_2E02,nlap_init_2E02)
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E02,ilap_init_3E02,nlap_init_3E02)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E02,ilap_init_4E02,nlap_init_4E02)
  else if (rlen.gt.5.D02 .and. rlen.le.7.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E02,ilap_init_5E02,nlap_init_5E02)
  else if (rlen.gt.7.D02 .and. rlen.le.1.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E02,ilap_init_7E02,nlap_init_7E02)
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E03,ilap_init_1E03,nlap_init_1E03)
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E03,ilap_init_2E03,nlap_init_2E03)
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E03,ilap_init_3E03,nlap_init_3E03)
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E03,ilap_init_4E03,nlap_init_4E03)
  else if (rlen.gt.5.D03 .and. rlen.le.6.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E03,ilap_init_5E03,nlap_init_5E03)
  else if (rlen.gt.6.D03 .and. rlen.le.7.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E03,ilap_init_6E03,nlap_init_6E03)
  else if (rlen.gt.7.D03 .and. rlen.le.8.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E03,ilap_init_7E03,nlap_init_7E03)
  else if (rlen.gt.8.D03 .and. rlen.le.9.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E03,ilap_init_8E03,nlap_init_8E03)
  else if (rlen.gt.9.D03 .and. rlen.le.1.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E03,ilap_init_9E03,nlap_init_9E03)
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E04,ilap_init_1E04,nlap_init_1E04)
  else if (rlen.gt.2.D04 .and. rlen.le.5.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E04,ilap_init_2E04,nlap_init_2E04)
  else if (rlen.gt.5.D04 .and. rlen.le.1.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E04,ilap_init_5E04,nlap_init_5E04)
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E05,ilap_init_1E05,nlap_init_1E05)
  else if (rlen.gt.2.D05 .and. rlen.le.5.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E05,ilap_init_2E05,nlap_init_2E05)
  else if (rlen.gt.5.D05 .and. rlen.le.1.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E05,ilap_init_5E05,nlap_init_5E05)
  else if (rlen.gt.1.D06 .and. rlen.le.2.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E06,ilap_init_1E06,nlap_init_1E06)
  else if (rlen.gt.2.D06 .and. rlen.le.5.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E06,ilap_init_2E06,nlap_init_2E06)
  else if (rlen.gt.5.D06 .and. rlen.le.1.D07) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E06,ilap_init_5E06,nlap_init_5E06)
  else if (rlen.gt.1.D07 .and. rlen.le.5.D07) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E07,ilap_init_1E07,nlap_init_1E07)
  else if (rlen.gt.5.D07 .and. rlen.le.1.D08) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E07,ilap_init_5E07,nlap_init_5E07)
  else if (rlen.gt.1.D08 .and. rlen.le.2.D08) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E08,ilap_init_1E08,nlap_init_1E08)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_2E08,ilap_init_2E08,nlap_init_2E08)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (26)                                                                                               
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.4.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E02,ilap_init_4E02,nlap_init_4E02)
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E02,ilap_init_4E02,nlap_init_4E02)
  else if (rlen.gt.5.D02 .and. rlen.le.6.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E02,ilap_init_5E02,nlap_init_5E02)
  else if (rlen.gt.6.D02 .and. rlen.le.7.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E02,ilap_init_6E02,nlap_init_6E02)
  else if (rlen.gt.7.D02 .and. rlen.le.8.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E02,ilap_init_7E02,nlap_init_7E02)
  else if (rlen.gt.8.D02 .and. rlen.le.9.D02) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E02,ilap_init_8E02,nlap_init_8E02)
  else if (rlen.gt.9.D02 .and. rlen.le.1.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E02,ilap_init_9E02,nlap_init_9E02)
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E03,ilap_init_1E03,nlap_init_1E03)
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E03,ilap_init_2E03,nlap_init_2E03)
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E03,ilap_init_3E03,nlap_init_3E03)
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E03,ilap_init_4E03,nlap_init_4E03)
  else if (rlen.gt.5.D03 .and. rlen.le.6.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E03,ilap_init_5E03,nlap_init_5E03)
  else if (rlen.gt.6.D03 .and. rlen.le.7.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E03,ilap_init_6E03,nlap_init_6E03)
  else if (rlen.gt.7.D03 .and. rlen.le.8.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E03,ilap_init_7E03,nlap_init_7E03)
  else if (rlen.gt.8.D03 .and. rlen.le.9.D03) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E03,ilap_init_8E03,nlap_init_8E03)
  else if (rlen.gt.9.D03 .and. rlen.le.1.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E03,ilap_init_9E03,nlap_init_9E03)
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E04,ilap_init_1E04,nlap_init_1E04)
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E04,ilap_init_2E04,nlap_init_2E04)
  else if (rlen.gt.3.D04 .and. rlen.le.4.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E04,ilap_init_3E04,nlap_init_3E04)
  else if (rlen.gt.4.D04 .and. rlen.le.5.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E04,ilap_init_4E04,nlap_init_4E04)
  else if (rlen.gt.5.D04 .and. rlen.le.6.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E04,ilap_init_5E04,nlap_init_5E04)
  else if (rlen.gt.6.D04 .and. rlen.le.7.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E04,ilap_init_6E04,nlap_init_6E04)
  else if (rlen.gt.7.D04 .and. rlen.le.8.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E04,ilap_init_7E04,nlap_init_7E04)
  else if (rlen.gt.8.D04 .and. rlen.le.9.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E04,ilap_init_8E04,nlap_init_8E04)
  else if (rlen.gt.9.D04 .and. rlen.le.1.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E04,ilap_init_9E04,nlap_init_9E04)
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E05,ilap_init_1E05,nlap_init_1E05)
  else if (rlen.gt.2.D05 .and. rlen.le.3.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E05,ilap_init_2E05,nlap_init_2E05)
  else if (rlen.gt.3.D05 .and. rlen.le.4.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E05,ilap_init_3E05,nlap_init_3E05)
  else if (rlen.gt.4.D05 .and. rlen.le.5.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E05,ilap_init_4E05,nlap_init_4E05)
  else if (rlen.gt.5.D05 .and. rlen.le.7.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E05,ilap_init_5E05,nlap_init_5E05)
  else if (rlen.gt.7.D05 .and. rlen.le.1.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E05,ilap_init_7E05,nlap_init_7E05)
  else if (rlen.gt.1.D06 .and. rlen.le.2.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E06,ilap_init_1E06,nlap_init_1E06)
  else if (rlen.gt.2.D06 .and. rlen.le.3.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E06,ilap_init_2E06,nlap_init_2E06)
  else if (rlen.gt.3.D06 .and. rlen.le.4.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E06,ilap_init_3E06,nlap_init_3E06)
  else if (rlen.gt.4.D06 .and. rlen.le.5.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E06,ilap_init_4E06,nlap_init_4E06)
  else if (rlen.gt.5.D06 .and. rlen.le.7.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E06,ilap_init_5E06,nlap_init_5E06)
  else if (rlen.gt.7.D06 .and. rlen.le.1.D07) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E06,ilap_init_7E06,nlap_init_7E06)
  else if (rlen.gt.1.D07 .and. rlen.le.2.D07) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E07,ilap_init_1E07,nlap_init_1E07)
  else if (rlen.gt.2.D07 .and. rlen.le.3.D07) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E07,ilap_init_2E07,nlap_init_2E07)
  else if (rlen.gt.3.D07 .and. rlen.le.4.D07) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E07,ilap_init_3E07,nlap_init_3E07)
  else if (rlen.gt.4.D07 .and. rlen.le.5.D07) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E07,ilap_init_4E07,nlap_init_4E07)
  else if (rlen.gt.5.D07 .and. rlen.le.7.D07) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E07,ilap_init_5E07,nlap_init_5E07)
  else if (rlen.gt.7.D07 .and. rlen.le.1.D08) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E07,ilap_init_7E07,nlap_init_7E07)
  else if (rlen.gt.1.D08 .and. rlen.le.2.D08) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E08,ilap_init_1E08,nlap_init_1E08)
  else if (rlen.gt.2.D08 .and. rlen.le.3.D08) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E08,ilap_init_2E08,nlap_init_2E08)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_3E08,ilap_init_3E08,nlap_init_3E08)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (27)                                                                                               
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.1.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E04,ilap_init_1E04,nlap_init_1E04)
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E04,ilap_init_1E04,nlap_init_1E04)
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E04,ilap_init_2E04,nlap_init_2E04)
  else if (rlen.gt.3.D04 .and. rlen.le.4.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E04,ilap_init_3E04,nlap_init_3E04)
  else if (rlen.gt.4.D04 .and. rlen.le.5.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E04,ilap_init_4E04,nlap_init_4E04)
  else if (rlen.gt.5.D04 .and. rlen.le.6.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E04,ilap_init_5E04,nlap_init_5E04)
  else if (rlen.gt.6.D04 .and. rlen.le.7.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E04,ilap_init_6E04,nlap_init_6E04)
  else if (rlen.gt.7.D04 .and. rlen.le.8.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E04,ilap_init_7E04,nlap_init_7E04)
  else if (rlen.gt.8.D04 .and. rlen.le.9.D04) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E04,ilap_init_8E04,nlap_init_8E04)
  else if (rlen.gt.9.D04 .and. rlen.le.1.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E04,ilap_init_9E04,nlap_init_9E04)
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E05,ilap_init_1E05,nlap_init_1E05)
  else if (rlen.gt.2.D05 .and. rlen.le.3.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E05,ilap_init_2E05,nlap_init_2E05)
  else if (rlen.gt.3.D05 .and. rlen.le.4.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E05,ilap_init_3E05,nlap_init_3E05)
  else if (rlen.gt.4.D05 .and. rlen.le.5.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E05,ilap_init_4E05,nlap_init_4E05)
  else if (rlen.gt.5.D05 .and. rlen.le.6.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E05,ilap_init_5E05,nlap_init_5E05)
  else if (rlen.gt.6.D05 .and. rlen.le.7.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E05,ilap_init_6E05,nlap_init_6E05)
  else if (rlen.gt.7.D05 .and. rlen.le.8.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E05,ilap_init_7E05,nlap_init_7E05)
  else if (rlen.gt.8.D05 .and. rlen.le.9.D05) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E05,ilap_init_8E05,nlap_init_8E05)
  else if (rlen.gt.9.D05 .and. rlen.le.1.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E05,ilap_init_9E05,nlap_init_9E05)
  else if (rlen.gt.1.D06 .and. rlen.le.2.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E06,ilap_init_1E06,nlap_init_1E06)
  else if (rlen.gt.2.D06 .and. rlen.le.3.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E06,ilap_init_2E06,nlap_init_2E06)
  else if (rlen.gt.3.D06 .and. rlen.le.4.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E06,ilap_init_3E06,nlap_init_3E06)
  else if (rlen.gt.4.D06 .and. rlen.le.5.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E06,ilap_init_4E06,nlap_init_4E06)
  else if (rlen.gt.5.D06 .and. rlen.le.6.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E06,ilap_init_5E06,nlap_init_5E06)
  else if (rlen.gt.6.D06 .and. rlen.le.7.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E06,ilap_init_6E06,nlap_init_6E06)
  else if (rlen.gt.7.D06 .and. rlen.le.8.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E06,ilap_init_7E06,nlap_init_7E06)
  else if (rlen.gt.8.D06 .and. rlen.le.9.D06) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_8E06,ilap_init_8E06,nlap_init_8E06)
  else if (rlen.gt.9.D06 .and. rlen.le.1.D07) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_9E06,ilap_init_9E06,nlap_init_9E06)
  else if (rlen.gt.1.D07 .and. rlen.le.2.D07) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E07,ilap_init_1E07,nlap_init_1E07)
  else if (rlen.gt.2.D07 .and. rlen.le.3.D07) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E07,ilap_init_2E07,nlap_init_2E07)
  else if (rlen.gt.3.D07 .and. rlen.le.4.D07) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E07,ilap_init_3E07,nlap_init_3E07)
  else if (rlen.gt.4.D07 .and. rlen.le.5.D07) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E07,ilap_init_4E07,nlap_init_4E07)
  else if (rlen.gt.5.D07 .and. rlen.le.7.D07) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E07,ilap_init_5E07,nlap_init_5E07)
  else if (rlen.gt.7.D07 .and. rlen.le.1.D08) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E07,ilap_init_7E07,nlap_init_7E07)
  else if (rlen.gt.1.D08 .and. rlen.le.2.D08) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E08,ilap_init_1E08,nlap_init_1E08)
  else if (rlen.gt.2.D08 .and. rlen.le.3.D08) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E08,ilap_init_2E08,nlap_init_2E08)
  else if (rlen.gt.3.D08 .and. rlen.le.4.D08) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E08,ilap_init_3E08,nlap_init_3E08)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_4E08,ilap_init_4E08,nlap_init_4E08)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (28)                                                                                               
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.1.D08) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E08,ilap_init_1E08,nlap_init_1E08)
  else if (rlen.gt.1.D08 .and. rlen.le.2.D08) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E08,ilap_init_1E08,nlap_init_1E08)
  else if (rlen.gt.2.D08 .and. rlen.le.3.D08) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E08,ilap_init_2E08,nlap_init_2E08)
  else if (rlen.gt.3.D08 .and. rlen.le.4.D08) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E08,ilap_init_3E08,nlap_init_3E08)
  else if (rlen.gt.4.D08 .and. rlen.le.7.D08) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E08,ilap_init_4E08,nlap_init_4E08)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_7E08,ilap_init_7E08,nlap_init_7E08)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (29)                                                                                               
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.7.D08) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E08,ilap_init_7E08,nlap_init_7E08)
  else if (rlen.gt.7.D08 .and. rlen.le.1.D09) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E08,ilap_init_7E08,nlap_init_7E08)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_1E09,ilap_init_1E09,nlap_init_1E09)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (30)                                                                                               
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.7.D08) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E08,ilap_init_7E08,nlap_init_7E08)
  else if (rlen.gt.7.D08 .and. rlen.le.1.D09) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E08,ilap_init_7E08,nlap_init_7E08)
  else if (rlen.gt.1.D09 .and. rlen.le.2.D09) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E09,ilap_init_1E09,nlap_init_1E09)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_2E09,ilap_init_2E09,nlap_init_2E09)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (31)                                                                                               
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.7.D08) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E08,ilap_init_7E08,nlap_init_7E08)
  else if (rlen.gt.7.D08 .and. rlen.le.1.D09) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E08,ilap_init_7E08,nlap_init_7E08)
  else if (rlen.gt.1.D09 .and. rlen.le.2.D09) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E09,ilap_init_1E09,nlap_init_1E09)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_2E09,ilap_init_2E09,nlap_init_2E09)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (32)                                                                                               
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.7.D08) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E08,ilap_init_7E08,nlap_init_7E08)
  else if (rlen.gt.7.D08 .and. rlen.le.1.D09) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E08,ilap_init_7E08,nlap_init_7E08)
  else if (rlen.gt.1.D09 .and. rlen.le.2.D09) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E09,ilap_init_1E09,nlap_init_1E09)
  else if (rlen.gt.2.D09 .and. rlen.le.3.D09) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E09,ilap_init_2E09,nlap_init_2E09)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_3E09,ilap_init_3E09,nlap_init_3E09)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (33)                                                                                               
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.7.D08) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E08,ilap_init_7E08,nlap_init_7E08)
  else if (rlen.gt.7.D08 .and. rlen.le.1.D09) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E08,ilap_init_7E08,nlap_init_7E08)
  else if (rlen.gt.1.D09 .and. rlen.le.2.D09) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E09,ilap_init_1E09,nlap_init_1E09)
  else if (rlen.gt.2.D09 .and. rlen.le.3.D09) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E09,ilap_init_2E09,nlap_init_2E09)
  else if (rlen.gt.3.D09 .and. rlen.le.4.D09) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E09,ilap_init_3E09,nlap_init_3E09)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_4E09,ilap_init_4E09,nlap_init_4E09)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (34)                                                                                               
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.7.D08) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E08,ilap_init_7E08,nlap_init_7E08)
  else if (rlen.gt.7.D08 .and. rlen.le.1.D09) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E08,ilap_init_7E08,nlap_init_7E08)
  else if (rlen.gt.1.D09 .and. rlen.le.2.D09) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E09,ilap_init_1E09,nlap_init_1E09)
  else if (rlen.gt.2.D09 .and. rlen.le.3.D09) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E09,ilap_init_2E09,nlap_init_2E09)
  else if (rlen.gt.3.D09 .and. rlen.le.4.D09) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E09,ilap_init_3E09,nlap_init_3E09)
  else if (rlen.gt.4.D09 .and. rlen.le.5.D09) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E09,ilap_init_4E09,nlap_init_4E09)
  else if (rlen.gt.5.D09 .and. rlen.le.7.D09) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E09,ilap_init_5E09,nlap_init_5E09)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_7E09,ilap_init_7E09,nlap_init_7E09)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (35)                                                                                               
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.7.D08) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E08,ilap_init_7E08,nlap_init_7E08)
  else if (rlen.gt.7.D08 .and. rlen.le.1.D09) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E08,ilap_init_7E08,nlap_init_7E08)
  else if (rlen.gt.1.D09 .and. rlen.le.2.D09) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E09,ilap_init_1E09,nlap_init_1E09)
  else if (rlen.gt.2.D09 .and. rlen.le.3.D09) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E09,ilap_init_2E09,nlap_init_2E09)
  else if (rlen.gt.3.D09 .and. rlen.le.5.D09) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E09,ilap_init_3E09,nlap_init_3E09)
  else if (rlen.gt.5.D09 .and. rlen.le.1.D10) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E09,ilap_init_5E09,nlap_init_5E09)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_1E10,ilap_init_1E10,nlap_init_1E10)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (36)                                                                                               
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.1.D10) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E10,ilap_init_1E10,nlap_init_1E10)
  else if (rlen.gt.1.D10 .and. rlen.le.2.D10) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E10,ilap_init_1E10,nlap_init_1E10)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_2E10,ilap_init_2E10,nlap_init_2E10)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (37)                                                                                               
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.1.D10) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E10,ilap_init_1E10,nlap_init_1E10)
  else if (rlen.gt.1.D10 .and. rlen.le.2.D10) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E10,ilap_init_1E10,nlap_init_1E10)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_2E10,ilap_init_2E10,nlap_init_2E10)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (38)                                                                                               
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.1.D10) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E10,ilap_init_1E10,nlap_init_1E10)
  else if (rlen.gt.1.D10 .and. rlen.le.3.D10) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E10,ilap_init_1E10,nlap_init_1E10)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_3E10,ilap_init_3E10,nlap_init_3E10)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (39)                                                                                               
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.1.D10) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E10,ilap_init_1E10,nlap_init_1E10)
  else if (rlen.gt.1.D10 .and. rlen.le.2.D10) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E10,ilap_init_1E10,nlap_init_1E10)
  else if (rlen.gt.2.D10 .and. rlen.le.4.D10) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E10,ilap_init_2E10,nlap_init_2E10)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_4E10,ilap_init_4E10,nlap_init_4E10)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (40)                                                                                               
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.1.D10) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E10,ilap_init_1E10,nlap_init_1E10)
  else if (rlen.gt.1.D10 .and. rlen.le.2.D10) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E10,ilap_init_1E10,nlap_init_1E10)
  else if (rlen.gt.2.D10 .and. rlen.le.5.D10) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E10,ilap_init_2E10,nlap_init_2E10)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_5E10,ilap_init_5E10,nlap_init_5E10)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (41)                                                                                               
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.1.D10) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E10,ilap_init_1E10,nlap_init_1E10)
  else if (rlen.gt.1.D10 .and. rlen.le.2.D10) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E10,ilap_init_1E10,nlap_init_1E10)
  else if (rlen.gt.2.D10 .and. rlen.le.3.D10) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E10,ilap_init_2E10,nlap_init_2E10)
  else if (rlen.gt.3.D10 .and. rlen.le.7.D10) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E10,ilap_init_3E10,nlap_init_3E10)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_7E10,ilap_init_7E10,nlap_init_7E10)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (42)                                                                                               
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.1.D10) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E10,ilap_init_1E10,nlap_init_1E10)
  else if (rlen.gt.1.D10 .and. rlen.le.2.D10) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E10,ilap_init_1E10,nlap_init_1E10)
  else if (rlen.gt.2.D10 .and. rlen.le.5.D10) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E10,ilap_init_2E10,nlap_init_2E10)
  else if (rlen.gt.5.D10 .and. rlen.le.1.D11) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E10,ilap_init_5E10,nlap_init_5E10)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_1E11,ilap_init_1E11,nlap_init_1E11)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (43)                                                                                               
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.1.D11) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E11,ilap_init_1E11,nlap_init_1E11)
  else if (rlen.gt.1.D11 .and. rlen.le.2.D11) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E11,ilap_init_1E11,nlap_init_1E11)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_2E11,ilap_init_2E11,nlap_init_2E11)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (44)                                                                                               
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.1.D11) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E11,ilap_init_1E11,nlap_init_1E11)
  else if (rlen.gt.1.D11 .and. rlen.le.2.D11) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E11,ilap_init_1E11,nlap_init_1E11)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_2E11,ilap_init_2E11,nlap_init_2E11)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (45)                                                                                               
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.1.D11) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E11,ilap_init_1E11,nlap_init_1E11)
  else if (rlen.gt.1.D11 .and. rlen.le.3.D11) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E11,ilap_init_1E11,nlap_init_1E11)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_3E11,ilap_init_3E11,nlap_init_3E11)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (46)                                                                                               
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.1.D11) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E11,ilap_init_1E11,nlap_init_1E11)
  else if (rlen.gt.1.D11 .and. rlen.le.2.D11) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E11,ilap_init_1E11,nlap_init_1E11)
  else if (rlen.gt.2.D11 .and. rlen.le.4.D11) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E11,ilap_init_2E11,nlap_init_2E11)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_4E11,ilap_init_4E11,nlap_init_4E11)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (47)                                                                                               
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.1.D11) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E11,ilap_init_1E11,nlap_init_1E11)
  else if (rlen.gt.1.D11 .and. rlen.le.5.D11) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E11,ilap_init_1E11,nlap_init_1E11)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_5E11,ilap_init_5E11,nlap_init_5E11)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (48)                                                                                               
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.1.D11) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E11,ilap_init_1E11,nlap_init_1E11)
  else if (rlen.gt.1.D11 .and. rlen.le.2.D11) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E11,ilap_init_1E11,nlap_init_1E11)
  else if (rlen.gt.2.D11 .and. rlen.le.3.D11) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E11,ilap_init_2E11,nlap_init_2E11)
  else if (rlen.gt.3.D11 .and. rlen.le.7.D11) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E11,ilap_init_3E11,nlap_init_3E11)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_7E11,ilap_init_7E11,nlap_init_7E11)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (49)                                                                                               
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.1.D11) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E11,ilap_init_1E11,nlap_init_1E11)
  else if (rlen.gt.1.D11 .and. rlen.le.2.D11) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E11,ilap_init_1E11,nlap_init_1E11)
  else if (rlen.gt.2.D11 .and. rlen.le.3.D11) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E11,ilap_init_2E11,nlap_init_2E11)
  else if (rlen.gt.3.D11 .and. rlen.le.5.D11) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E11,ilap_init_3E11,nlap_init_3E11)
  else if (rlen.gt.5.D11 .and. rlen.le.1.D12) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E11,ilap_init_5E11,nlap_init_5E11)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_1E12,ilap_init_1E12,nlap_init_1E12)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (50)                                                                                               
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.2.D08) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E08,ilap_init_2E08,nlap_init_2E08)
  else if (rlen.gt.2.D08 .and. rlen.le.3.D08) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E08,ilap_init_2E08,nlap_init_2E08)
  else if (rlen.gt.3.D08 .and. rlen.le.4.D08) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E08,ilap_init_3E08,nlap_init_3E08)
  else if (rlen.gt.4.D08 .and. rlen.le.5.D08) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E08,ilap_init_4E08,nlap_init_4E08)
  else if (rlen.gt.5.D08 .and. rlen.le.7.D08) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E08,ilap_init_5E08,nlap_init_5E08)
  else if (rlen.gt.7.D08 .and. rlen.le.1.D09) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E08,ilap_init_7E08,nlap_init_7E08)
  else if (rlen.gt.1.D09 .and. rlen.le.2.D09) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E09,ilap_init_1E09,nlap_init_1E09)
  else if (rlen.gt.2.D09 .and. rlen.le.3.D09) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E09,ilap_init_2E09,nlap_init_2E09)
  else if (rlen.gt.3.D09 .and. rlen.le.4.D09) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E09,ilap_init_3E09,nlap_init_3E09)
  else if (rlen.gt.4.D09 .and. rlen.le.5.D09) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E09,ilap_init_4E09,nlap_init_4E09)
  else if (rlen.gt.5.D09 .and. rlen.le.6.D09) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E09,ilap_init_5E09,nlap_init_5E09)
  else if (rlen.gt.6.D09 .and. rlen.le.7.D09) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_6E09,ilap_init_6E09,nlap_init_6E09)
  else if (rlen.gt.7.D09 .and. rlen.le.1.D10) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E09,ilap_init_7E09,nlap_init_7E09)
  else if (rlen.gt.1.D10 .and. rlen.le.2.D10) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E10,ilap_init_1E10,nlap_init_1E10)
  else if (rlen.gt.2.D10 .and. rlen.le.3.D10) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E10,ilap_init_2E10,nlap_init_2E10)
  else if (rlen.gt.3.D10 .and. rlen.le.4.D10) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E10,ilap_init_3E10,nlap_init_3E10)
  else if (rlen.gt.4.D10 .and. rlen.le.5.D10) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E10,ilap_init_4E10,nlap_init_4E10)
  else if (rlen.gt.5.D10 .and. rlen.le.7.D10) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E10,ilap_init_5E10,nlap_init_5E10)
  else if (rlen.gt.7.D10 .and. rlen.le.1.D11) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E10,ilap_init_7E10,nlap_init_7E10)
  else if (rlen.gt.1.D11 .and. rlen.le.2.D11) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E11,ilap_init_1E11,nlap_init_1E11)
  else if (rlen.gt.2.D11 .and. rlen.le.3.D11) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E11,ilap_init_2E11,nlap_init_2E11)
  else if (rlen.gt.3.D11 .and. rlen.le.4.D11) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E11,ilap_init_3E11,nlap_init_3E11)
  else if (rlen.gt.4.D11 .and. rlen.le.5.D11) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E11,ilap_init_4E11,nlap_init_4E11)
  else if (rlen.gt.5.D11 .and. rlen.le.7.D11) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_5E11,ilap_init_5E11,nlap_init_5E11)
  else if (rlen.gt.7.D11 .and. rlen.le.1.D12) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_7E11,ilap_init_7E11,nlap_init_7E11)
  else if (rlen.gt.1.D12 .and. rlen.le.2.D12) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E12,ilap_init_1E12,nlap_init_1E12)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_2E12,ilap_init_2E12,nlap_init_2E12)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (51)                                                                                               
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.1.D12) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E12,ilap_init_1E12,nlap_init_1E12)
  else if (rlen.gt.1.D12 .and. rlen.le.2.D12) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E12,ilap_init_1E12,nlap_init_1E12)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_2E12,ilap_init_2E12,nlap_init_2E12)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (52)                                                                                               
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.1.D12) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E12,ilap_init_1E12,nlap_init_1E12)
  else if (rlen.gt.1.D12 .and. rlen.le.3.D12) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E12,ilap_init_1E12,nlap_init_1E12)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_3E12,ilap_init_3E12,nlap_init_3E12)
  end if                                                                                                 
                                                                                                         
!-----------------------------------------------------------------------------!                  
 case (53)                                                                                               
!-----------------------------------------------------------------------------!                  
  if      (rlen.gt.1.D00 .and. rlen.le.4.D11) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E11,ilap_init_4E11,nlap_init_4E11)
  else if (rlen.gt.4.D11 .and. rlen.le.1.D12) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_4E11,ilap_init_4E11,nlap_init_4E11)
  else if (rlen.gt.1.D12 .and. rlen.le.2.D12) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_1E12,ilap_init_1E12,nlap_init_1E12)
  else if (rlen.gt.2.D12 .and. rlen.le.3.D12) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_2E12,ilap_init_2E12,nlap_init_2E12)
  else if (rlen.gt.3.D12 .and. rlen.le.4.D12) then                                                       
   call lap_assign_error(errbnd,nlap,error_init_3E12,ilap_init_3E12,nlap_init_3E12)
  else                                                                                                   
   call lap_assign_error(errbnd,nlap,error_init_4E12,ilap_init_4E12,nlap_init_4E12)
  end if

!-----------------------------------------------------------------------------!
 case default
!-----------------------------------------------------------------------------!
  write(istdout,"(a)") chrdbg//"Maximum number of Laplace quadrature points is 53!"
  stop "Error"
 end select

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
!=-============================================================================!
!==============================================================================!
subroutine lap_assign_error(errbnd,nlap,error_init,ilap_init,nlap_init)
!------------------------------------------------------------------------------!
! get the pre-tabulated error for given error bound and number of quadrature
! points
!------------------------------------------------------------------------------!

 implicit none

#include "consts.h"

! dimensions:
 integer, intent(in) :: nlap_init

! input:
 integer, intent(in) :: nlap, ilap_init(nlap_init)
 real(8), intent(in) :: error_init(nlap_init)

! output:
 real(8), intent(out) :: errbnd(2)

! local:
 integer :: ilap, ileft, irght

 ! get index in *_init arrays by binary search
 ilap  = -1
 ileft = 1
 irght = nlap_init
 do while (ileft <= irght)
  ilap = ileft + ((irght - ileft) / 2)
  if (ilap_init(ilap).eq.nlap) then
   exit
  else
   if (ilap_init(ilap) > nlap) then
    irght = ilap - 1
   else
    ileft = ilap + 1
   end if
  end if
 end do

 if (ilap.eq.-1) then
  write(istdout,"(/a)") "Something is fishy! The requested number of points should be tabulated."
  stop "Bye bye!"
 end if

 errbnd(1) = error_init(ilap)
 errbnd(2) = d0

end subroutine lap_assign_error
!==============================================================================!
