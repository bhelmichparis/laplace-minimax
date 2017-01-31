!==============================================================================!
subroutine lap_numlap(errbnd,nlap,tolerr,rnge)
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
#include "error_tiny.h"

! constants:
 logical, parameter :: locdbg = .false.
 character(len=*), parameter :: chrdbg = 'lap_numlap>'

! input:
 real(8), intent(in) :: rnge(2), tolerr

! output:
 integer, intent(out) :: nlap
 real(8), intent(out) :: errbnd

! local:
 real(8) :: rlen

 if (locdbg) write(istdout,"(a)") chrdbg//"enter lap_numlap ..."

 rlen = rnge(2)/rnge(1)

 if (rlen.le.d1) then
  stop chrdbg//"Upper bound shall be greater than lower bound!"
 end if

 if      (rlen.le.1.050D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1050,ilap_init_1050,nlap_init_1050)
 else if (rlen.le.1.055D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1055,ilap_init_1055,nlap_init_1055)
 else if (rlen.le.1.060D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1060,ilap_init_1060,nlap_init_1060)
 else if (rlen.le.1.065D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1065,ilap_init_1065,nlap_init_1065)
 else if (rlen.le.1.070D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1070,ilap_init_1070,nlap_init_1070)
 else if (rlen.le.1.075D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1075,ilap_init_1075,nlap_init_1075)
 else if (rlen.le.1.080D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1080,ilap_init_1080,nlap_init_1080)
 else if (rlen.le.1.085D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1085,ilap_init_1085,nlap_init_1085)
 else if (rlen.le.1.090D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1090,ilap_init_1090,nlap_init_1090)
 else if (rlen.le.1.095D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1095,ilap_init_1095,nlap_init_1095)
 else if (rlen.le.1.100D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1100,ilap_init_1100,nlap_init_1100)
 else if (rlen.le.1.105D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1105,ilap_init_1105,nlap_init_1105)
 else if (rlen.le.1.110D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1110,ilap_init_1110,nlap_init_1110)
 else if (rlen.le.1.115D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1115,ilap_init_1115,nlap_init_1115)
 else if (rlen.le.1.120D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1120,ilap_init_1120,nlap_init_1120)
 else if (rlen.le.1.125D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1125,ilap_init_1125,nlap_init_1125)
 else if (rlen.le.1.130D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1130,ilap_init_1130,nlap_init_1130)
 else if (rlen.le.1.135D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1135,ilap_init_1135,nlap_init_1135)
 else if (rlen.le.1.140D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1140,ilap_init_1140,nlap_init_1140)
 else if (rlen.le.1.145D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1145,ilap_init_1145,nlap_init_1145)
 else if (rlen.le.1.150D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1150,ilap_init_1150,nlap_init_1150)
 else if (rlen.le.1.155D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1155,ilap_init_1155,nlap_init_1155)
 else if (rlen.le.1.160D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1160,ilap_init_1160,nlap_init_1160)
 else if (rlen.le.1.165D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1165,ilap_init_1165,nlap_init_1165)
 else if (rlen.le.1.170D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1170,ilap_init_1170,nlap_init_1170)
 else if (rlen.le.1.175D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1175,ilap_init_1175,nlap_init_1175)
 else if (rlen.le.1.180D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1180,ilap_init_1180,nlap_init_1180)
 else if (rlen.le.1.185D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1185,ilap_init_1185,nlap_init_1185)
 else if (rlen.le.1.190D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1190,ilap_init_1190,nlap_init_1190)
 else if (rlen.le.1.195D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1195,ilap_init_1195,nlap_init_1195)
 else if (rlen.le.1.200D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1200,ilap_init_1200,nlap_init_1200)
 else if (rlen.le.1.205D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1205,ilap_init_1205,nlap_init_1205)
 else if (rlen.le.1.210D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1210,ilap_init_1210,nlap_init_1210)
 else if (rlen.le.1.215D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1215,ilap_init_1215,nlap_init_1215)
 else if (rlen.le.1.220D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1220,ilap_init_1220,nlap_init_1220)
 else if (rlen.le.1.225D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1225,ilap_init_1225,nlap_init_1225)
 else if (rlen.le.1.230D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1230,ilap_init_1230,nlap_init_1230)
 else if (rlen.le.1.235D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1235,ilap_init_1235,nlap_init_1235)
 else if (rlen.le.1.240D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1240,ilap_init_1240,nlap_init_1240)
 else if (rlen.le.1.245D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1245,ilap_init_1245,nlap_init_1245)
 else if (rlen.le.1.250D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1250,ilap_init_1250,nlap_init_1250)
 else if (rlen.le.1.260D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1260,ilap_init_1260,nlap_init_1260)
 else if (rlen.le.1.270D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1270,ilap_init_1270,nlap_init_1270)
 else if (rlen.le.1.280D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1280,ilap_init_1280,nlap_init_1280)
 else if (rlen.le.1.290D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1290,ilap_init_1290,nlap_init_1290)
 else if (rlen.le.1.300D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1300,ilap_init_1300,nlap_init_1300)
 else if (rlen.le.1.310D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1310,ilap_init_1310,nlap_init_1310)
 else if (rlen.le.1.320D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1320,ilap_init_1320,nlap_init_1320)
 else if (rlen.le.1.330D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1330,ilap_init_1330,nlap_init_1330)
 else if (rlen.le.1.340D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1340,ilap_init_1340,nlap_init_1340)
 else if (rlen.le.1.350D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1350,ilap_init_1350,nlap_init_1350)
 else if (rlen.le.1.360D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1360,ilap_init_1360,nlap_init_1360)
 else if (rlen.le.1.370D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1370,ilap_init_1370,nlap_init_1370)
 else if (rlen.le.1.380D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1380,ilap_init_1380,nlap_init_1380)
 else if (rlen.le.1.390D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1390,ilap_init_1390,nlap_init_1390)
 else if (rlen.le.1.400D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1400,ilap_init_1400,nlap_init_1400)
 else if (rlen.le.1.410D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1410,ilap_init_1410,nlap_init_1410)
 else if (rlen.le.1.420D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1420,ilap_init_1420,nlap_init_1420)
 else if (rlen.le.1.430D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1430,ilap_init_1430,nlap_init_1430)
 else if (rlen.le.1.440D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1440,ilap_init_1440,nlap_init_1440)
 else if (rlen.le.1.450D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1450,ilap_init_1450,nlap_init_1450)
 else if (rlen.le.1.460D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1460,ilap_init_1460,nlap_init_1460)
 else if (rlen.le.1.470D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1470,ilap_init_1470,nlap_init_1470)
 else if (rlen.le.1.480D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1480,ilap_init_1480,nlap_init_1480)
 else if (rlen.le.1.490D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1490,ilap_init_1490,nlap_init_1490)
 else if (rlen.le.1.500D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1500,ilap_init_1500,nlap_init_1500)
 else if (rlen.le.1.510D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1510,ilap_init_1510,nlap_init_1510)
 else if (rlen.le.1.520D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1520,ilap_init_1520,nlap_init_1520)
 else if (rlen.le.1.530D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1530,ilap_init_1530,nlap_init_1530)
 else if (rlen.le.1.540D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1540,ilap_init_1540,nlap_init_1540)
 else if (rlen.le.1.550D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1550,ilap_init_1550,nlap_init_1550)
 else if (rlen.le.1.560D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1560,ilap_init_1560,nlap_init_1560)
 else if (rlen.le.1.570D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1570,ilap_init_1570,nlap_init_1570)
 else if (rlen.le.1.580D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1580,ilap_init_1580,nlap_init_1580)
 else if (rlen.le.1.590D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1590,ilap_init_1590,nlap_init_1590)
 else if (rlen.le.1.600D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1600,ilap_init_1600,nlap_init_1600)
 else if (rlen.le.1.610D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1610,ilap_init_1610,nlap_init_1610)
 else if (rlen.le.1.620D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1620,ilap_init_1620,nlap_init_1620)
 else if (rlen.le.1.630D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1630,ilap_init_1630,nlap_init_1630)
 else if (rlen.le.1.640D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1640,ilap_init_1640,nlap_init_1640)
 else if (rlen.le.1.650D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1650,ilap_init_1650,nlap_init_1650)
 else if (rlen.le.1.660D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1660,ilap_init_1660,nlap_init_1660)
 else if (rlen.le.1.670D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1670,ilap_init_1670,nlap_init_1670)
 else if (rlen.le.1.680D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1680,ilap_init_1680,nlap_init_1680)
 else if (rlen.le.1.690D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1690,ilap_init_1690,nlap_init_1690)
 else if (rlen.le.1.700D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1700,ilap_init_1700,nlap_init_1700)
 else if (rlen.le.1.710D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1710,ilap_init_1710,nlap_init_1710)
 else if (rlen.le.1.720D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1720,ilap_init_1720,nlap_init_1720)
 else if (rlen.le.1.730D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1730,ilap_init_1730,nlap_init_1730)
 else if (rlen.le.1.740D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1740,ilap_init_1740,nlap_init_1740)
 else if (rlen.le.1.750D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1750,ilap_init_1750,nlap_init_1750)
 else if (rlen.le.1.760D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1760,ilap_init_1760,nlap_init_1760)
 else if (rlen.le.1.770D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1770,ilap_init_1770,nlap_init_1770)
 else if (rlen.le.1.780D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1780,ilap_init_1780,nlap_init_1780)
 else if (rlen.le.1.790D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1790,ilap_init_1790,nlap_init_1790)
 else if (rlen.le.1.800D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1800,ilap_init_1800,nlap_init_1800)
 else if (rlen.le.1.810D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1810,ilap_init_1810,nlap_init_1810)
 else if (rlen.le.1.820D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1820,ilap_init_1820,nlap_init_1820)
 else if (rlen.le.1.830D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1830,ilap_init_1830,nlap_init_1830)
 else if (rlen.le.1.840D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1840,ilap_init_1840,nlap_init_1840)
 else if (rlen.le.1.850D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1850,ilap_init_1850,nlap_init_1850)
 else if (rlen.le.1.860D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1860,ilap_init_1860,nlap_init_1860)
 else if (rlen.le.1.870D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1870,ilap_init_1870,nlap_init_1870)
 else if (rlen.le.1.880D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1880,ilap_init_1880,nlap_init_1880)
 else if (rlen.le.1.890D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1890,ilap_init_1890,nlap_init_1890)
 else if (rlen.le.1.900D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1900,ilap_init_1900,nlap_init_1900)
 else if (rlen.le.1.910D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1910,ilap_init_1910,nlap_init_1910)
 else if (rlen.le.1.920D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1920,ilap_init_1920,nlap_init_1920)
 else if (rlen.le.1.930D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1930,ilap_init_1930,nlap_init_1930)
 else if (rlen.le.1.940D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1940,ilap_init_1940,nlap_init_1940)
 else if (rlen.le.1.950D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1950,ilap_init_1950,nlap_init_1950)
 else if (rlen.le.1.960D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1960,ilap_init_1960,nlap_init_1960)
 else if (rlen.le.1.970D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1970,ilap_init_1970,nlap_init_1970)
 else if (rlen.le.1.980D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1980,ilap_init_1980,nlap_init_1980)
 else if (rlen.le.2.D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_2E00,ilap_init_2E00,nlap_init_2E00)
 else if (rlen.le.3.D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_3E00,ilap_init_3E00,nlap_init_3E00)
 else if (rlen.le.4.D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_4E00,ilap_init_4E00,nlap_init_4E00)
 else if (rlen.le.5.D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_5E00,ilap_init_5E00,nlap_init_5E00)
 else if (rlen.le.6.D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_6E00,ilap_init_6E00,nlap_init_6E00)
 else if (rlen.le.7.D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_7E00,ilap_init_7E00,nlap_init_7E00)
 else if (rlen.le.8.D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_8E00,ilap_init_8E00,nlap_init_8E00)
 else if (rlen.le.9.D00) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_9E00,ilap_init_9E00,nlap_init_9E00)
 else if (rlen.le.1.D01) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1E01,ilap_init_1E01,nlap_init_1E01)
 else if (rlen.le.2.D01) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_2E01,ilap_init_2E01,nlap_init_2E01)
 else if (rlen.le.3.D01) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_3E01,ilap_init_3E01,nlap_init_3E01)
 else if (rlen.le.4.D01) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_4E01,ilap_init_4E01,nlap_init_4E01)
 else if (rlen.le.5.D01) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_5E01,ilap_init_5E01,nlap_init_5E01)
 else if (rlen.le.6.D01) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_6E01,ilap_init_6E01,nlap_init_6E01)
 else if (rlen.le.7.D01) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_7E01,ilap_init_7E01,nlap_init_7E01)
 else if (rlen.le.8.D01) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_8E01,ilap_init_8E01,nlap_init_8E01)
 else if (rlen.le.9.D01) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_9E01,ilap_init_9E01,nlap_init_9E01)
 else if (rlen.le.1.D02) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1E02,ilap_init_1E02,nlap_init_1E02)
 else if (rlen.le.2.D02) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_2E02,ilap_init_2E02,nlap_init_2E02)
 else if (rlen.le.3.D02) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_3E02,ilap_init_3E02,nlap_init_3E02)
 else if (rlen.le.4.D02) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_4E02,ilap_init_4E02,nlap_init_4E02)
 else if (rlen.le.5.D02) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_5E02,ilap_init_5E02,nlap_init_5E02)
 else if (rlen.le.6.D02) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_6E02,ilap_init_6E02,nlap_init_6E02)
 else if (rlen.le.7.D02) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_7E02,ilap_init_7E02,nlap_init_7E02)
 else if (rlen.le.8.D02) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_8E02,ilap_init_8E02,nlap_init_8E02)
 else if (rlen.le.9.D02) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_9E02,ilap_init_9E02,nlap_init_9E02)
 else if (rlen.le.1.D03) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1E03,ilap_init_1E03,nlap_init_1E03)
 else if (rlen.le.2.D03) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_2E03,ilap_init_2E03,nlap_init_2E03)
 else if (rlen.le.3.D03) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_3E03,ilap_init_3E03,nlap_init_3E03)
 else if (rlen.le.4.D03) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_4E03,ilap_init_4E03,nlap_init_4E03)
 else if (rlen.le.5.D03) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_5E03,ilap_init_5E03,nlap_init_5E03)
 else if (rlen.le.6.D03) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_6E03,ilap_init_6E03,nlap_init_6E03)
 else if (rlen.le.7.D03) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_7E03,ilap_init_7E03,nlap_init_7E03)
 else if (rlen.le.8.D03) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_8E03,ilap_init_8E03,nlap_init_8E03)
 else if (rlen.le.9.D03) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_9E03,ilap_init_9E03,nlap_init_9E03)
 else if (rlen.le.1.D04) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1E04,ilap_init_1E04,nlap_init_1E04)
 else if (rlen.le.2.D04) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_2E04,ilap_init_2E04,nlap_init_2E04)
 else if (rlen.le.3.D04) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_3E04,ilap_init_3E04,nlap_init_3E04)
 else if (rlen.le.4.D04) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_4E04,ilap_init_4E04,nlap_init_4E04)
 else if (rlen.le.5.D04) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_5E04,ilap_init_5E04,nlap_init_5E04)
 else if (rlen.le.6.D04) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_6E04,ilap_init_6E04,nlap_init_6E04)
 else if (rlen.le.7.D04) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_7E04,ilap_init_7E04,nlap_init_7E04)
 else if (rlen.le.8.D04) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_8E04,ilap_init_8E04,nlap_init_8E04)
 else if (rlen.le.9.D04) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_9E04,ilap_init_9E04,nlap_init_9E04)
 else if (rlen.le.1.D05) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1E05,ilap_init_1E05,nlap_init_1E05)
 else if (rlen.le.2.D05) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_2E05,ilap_init_2E05,nlap_init_2E05)
 else if (rlen.le.3.D05) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_3E05,ilap_init_3E05,nlap_init_3E05)
 else if (rlen.le.4.D05) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_4E05,ilap_init_4E05,nlap_init_4E05)
 else if (rlen.le.5.D05) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_5E05,ilap_init_5E05,nlap_init_5E05)
 else if (rlen.le.6.D05) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_6E05,ilap_init_6E05,nlap_init_6E05)
 else if (rlen.le.7.D05) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_7E05,ilap_init_7E05,nlap_init_7E05)
 else if (rlen.le.8.D05) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_8E05,ilap_init_8E05,nlap_init_8E05)
 else if (rlen.le.9.D05) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_9E05,ilap_init_9E05,nlap_init_9E05)
 else if (rlen.le.1.D06) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1E06,ilap_init_1E06,nlap_init_1E06)
 else if (rlen.le.2.D06) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_2E06,ilap_init_2E06,nlap_init_2E06)
 else if (rlen.le.3.D06) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_3E06,ilap_init_3E06,nlap_init_3E06)
 else if (rlen.le.4.D06) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_4E06,ilap_init_4E06,nlap_init_4E06)
 else if (rlen.le.5.D06) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_5E06,ilap_init_5E06,nlap_init_5E06)
 else if (rlen.le.6.D06) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_6E06,ilap_init_6E06,nlap_init_6E06)
 else if (rlen.le.7.D06) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_7E06,ilap_init_7E06,nlap_init_7E06)
 else if (rlen.le.8.D06) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_8E06,ilap_init_8E06,nlap_init_8E06)
 else if (rlen.le.9.D06) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_9E06,ilap_init_9E06,nlap_init_9E06)
 else if (rlen.le.1.D07) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1E07,ilap_init_1E07,nlap_init_1E07)
 else if (rlen.le.2.D07) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_2E07,ilap_init_2E07,nlap_init_2E07)
 else if (rlen.le.3.D07) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_3E07,ilap_init_3E07,nlap_init_3E07)
 else if (rlen.le.4.D07) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_4E07,ilap_init_4E07,nlap_init_4E07)
 else if (rlen.le.5.D07) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_5E07,ilap_init_5E07,nlap_init_5E07)
 else if (rlen.le.7.D07) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_7E07,ilap_init_7E07,nlap_init_7E07)
 else if (rlen.le.1.D08) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1E08,ilap_init_1E08,nlap_init_1E08)
 else if (rlen.le.2.D08) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_2E08,ilap_init_2E08,nlap_init_2E08)
 else if (rlen.le.3.D08) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_3E08,ilap_init_3E08,nlap_init_3E08)
 else if (rlen.le.4.D08) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_4E08,ilap_init_4E08,nlap_init_4E08)
 else if (rlen.le.5.D08) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_5E08,ilap_init_5E08,nlap_init_5E08)
 else if (rlen.le.7.D08) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_7E08,ilap_init_7E08,nlap_init_7E08)
 else if (rlen.le.1.D09) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1E09,ilap_init_1E09,nlap_init_1E09)
 else if (rlen.le.2.D09) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_2E09,ilap_init_2E09,nlap_init_2E09)
 else if (rlen.le.3.D09) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_3E09,ilap_init_3E09,nlap_init_3E09)
 else if (rlen.le.4.D09) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_4E09,ilap_init_4E09,nlap_init_4E09)
 else if (rlen.le.5.D09) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_5E09,ilap_init_5E09,nlap_init_5E09)
 else if (rlen.le.6.D09) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_6E09,ilap_init_6E09,nlap_init_6E09)
 else if (rlen.le.7.D09) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_7E09,ilap_init_7E09,nlap_init_7E09)
 else if (rlen.le.1.D10) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1E10,ilap_init_1E10,nlap_init_1E10)
 else if (rlen.le.2.D10) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_2E10,ilap_init_2E10,nlap_init_2E10)
 else if (rlen.le.3.D10) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_3E10,ilap_init_3E10,nlap_init_3E10)
 else if (rlen.le.4.D10) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_4E10,ilap_init_4E10,nlap_init_3E10)
 else if (rlen.le.5.D10) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_5E10,ilap_init_5E10,nlap_init_5E10)
 else if (rlen.le.7.D10) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_7E10,ilap_init_7E10,nlap_init_7E10)
 else if (rlen.le.1.D11) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1E11,ilap_init_1E11,nlap_init_1E11)
 else if (rlen.le.2.D11) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_2E11,ilap_init_2E11,nlap_init_2E11)
 else if (rlen.le.3.D11) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_3E11,ilap_init_3E11,nlap_init_3E11)
 else if (rlen.le.4.D11) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_4E11,ilap_init_4E11,nlap_init_4E11)
 else if (rlen.le.5.D11) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_5E11,ilap_init_5E11,nlap_init_5E11)
 else if (rlen.le.7.D11) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_7E11,ilap_init_7E11,nlap_init_7E11)
 else if (rlen.le.1.D12) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_1E12,ilap_init_1E12,nlap_init_1E12)
 else if (rlen.le.2.D12) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_2E12,ilap_init_2E12,nlap_init_2E12)
 else if (rlen.le.3.D12) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_3E12,ilap_init_3E12,nlap_init_3E12)
 else if (rlen.le.4.D12) then
  call lap_getnlap(errbnd,nlap,tolerr,error_init_4E12,ilap_init_4E12,nlap_init_4E12)
 else
  call lap_getnlap(errbnd,nlap,tolerr,error_init_inft,ilap_init_inft,nlap_init_inft)
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
subroutine lap_getnlap(errbnd,nlap, & ! out
                       tolerr,error_init,ilap_init,nlap_init)
!------------------------------------------------------------------------------!
! get number of quadrature points by scanning the arrays in error.h
!------------------------------------------------------------------------------!

 implicit none

! dimensions:
 integer, intent(in) :: nlap_init

! input:
 integer, intent(in) :: ilap_init(nlap_init)
 real(8), intent(in) :: error_init(nlap_init), tolerr

! output:
 integer, intent(out) :: nlap
 real(8), intent(out) :: errbnd

! local:
 integer :: ilap

 nlap = -1
 do ilap = 1,nlap_init
  if (error_init(ilap).le.tolerr) then
   nlap = ilap_init(ilap)
   errbnd = error_init(ilap)
   exit
  end if
 end do

end subroutine lap_getnlap
!==============================================================================!
