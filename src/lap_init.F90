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
 integer :: istart, ioff, ilap
 real(8) :: rlen
 
 if (locdbg) write(istdout,"(a)") chrdbg//"enter lap_init ..."

 rlen = rnge(2)/rnge(1)

 if (rlen .le. d1) then
  stop chrdbg//"Upper bound shall be greater than lower bound!"
 end if

!-----------------------------------------------------------------------------!
! find start address for initilization
!-----------------------------------------------------------------------------!

 istart = -99

 select case (nlap)

!-----------------------------------------------------------------------------!
 case (1)
!-----------------------------------------------------------------------------!
  !1_xk01_2E0
  if      (rlen.gt.1.D00 .and. rlen.le.2.D00) then
   istart = 1
  !1_xk01_3E0
  else if (rlen.gt.2.D00 .and. rlen.le.3.D00) then
   istart = 1
  !1_xk01_4E0
  else if (rlen.gt.3.D00 .and. rlen.le.4.D00) then
   istart = 57
  !1_xk01_5E0
  else if (rlen.gt.4.D00 .and. rlen.le.5.D00) then
   istart = 113
  !1_xk01_6E0
  else if (rlen.gt.5.D00 .and. rlen.le.6.D00) then
   istart = 169
  !1_xk01_7E0
  else if (rlen.gt.6.D00 .and. rlen.le.7.D00) then
   istart = 225
  !1_xk01_8E0
  else if (rlen.gt.7.D00 .and. rlen.le.8.D00) then
   istart = 281
  !1_xk01_9E0
  else if (rlen.gt.8.D00 .and. rlen.le.9.D00) then
   istart = 337
  !1_xk01_1E1
  else if (rlen.gt.9.D00 .and. rlen.le.1.D01) then
   istart = 393
  else
   istart = 449
  end if

!-----------------------------------------------------------------------------!
 case (2)
!-----------------------------------------------------------------------------!
  !1_xk02_2E0
  if      (rlen.gt.1.D00 .and. rlen.le.2.D00) then
   istart = 3
  !1_xk02_3E0
  else if (rlen.gt.2.D00 .and. rlen.le.3.D00) then
   istart = 3
  !1_xk02_4E0
  else if (rlen.gt.3.D00 .and. rlen.le.4.D00) then
   istart = 59
  !1_xk02_5E0
  else if (rlen.gt.4.D00 .and. rlen.le.5.D00) then
   istart = 115
  !1_xk02_6E0
  else if (rlen.gt.5.D00 .and. rlen.le.6.D00) then
   istart = 171
  !1_xk02_7E0
  else if (rlen.gt.6.D00 .and. rlen.le.7.D00) then
   istart = 227
  !1_xk02_8E0
  else if (rlen.gt.7.D00 .and. rlen.le.8.D00) then
   istart = 283
  !1_xk02_9E0
  else if (rlen.gt.8.D00 .and. rlen.le.9.D00) then
   istart = 339
  !1_xk02_1E1
  else if (rlen.gt.9.D00 .and. rlen.le.1.D01) then
   istart = 395
  !1_xk02_2E1
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then
   istart = 451
  !1_xk02_3E1
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then
   istart = 721
  !1_xk02_4E1
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then
   istart = 1025
  !1_xk02_5E1
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then
   istart = 1329
  else 
   istart = 1669
  end if

!-----------------------------------------------------------------------------!
 case (3)
!-----------------------------------------------------------------------------!
  !1_xk03_2E0
  if      (rlen.gt.1.D00 .and. rlen.le.2.D00) then
   istart = 7
  !1_xk03_3E0
  else if (rlen.gt.2.D00 .and. rlen.le.3.D00) then
   istart = 7
  !1_xk03_4E0
  else if (rlen.gt.3.D00 .and. rlen.le.4.D00) then
   istart = 63
  !1_xk03_5E0
  else if (rlen.gt.4.D00 .and. rlen.le.5.D00) then
   istart = 119
  !1_xk03_6E0
  else if (rlen.gt.5.D00 .and. rlen.le.6.D00) then
   istart = 175
  !1_xk03_7E0
  else if (rlen.gt.6.D00 .and. rlen.le.7.D00) then
   istart = 231
  !1_xk03_8E0
  else if (rlen.gt.7.D00 .and. rlen.le.8.D00) then
   istart = 287
  !1_xk03_9E0
  else if (rlen.gt.8.D00 .and. rlen.le.9.D00) then
   istart = 343
  !1_xk03_1E1
  else if (rlen.gt.9.D00 .and. rlen.le.1.D01) then
   istart = 399
  !1_xk03_2E1
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then
   istart = 455
  !1_xk03_3E1
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then
   istart = 725
  !1_xk03_4E1
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then
   istart = 1029
  !1_xk03_5E1
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then
   istart = 1333
  !1_xk03_6E1
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then
   istart = 1673
  !1_xk03_7E1
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then
   istart = 2047
  !1_xk03_8E1
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then
   istart = 2461
  !1_xk03_9E1
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then
   istart = 2875
  !1_xk03_1E2
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then
   istart = 3289
  !1_xk03_2E2
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then
   istart = 3745
  else
   istart = 4245
  end if

!-----------------------------------------------------------------------------!
 case (4)
!-----------------------------------------------------------------------------!
  !1_xk04_2E0
  if      (rlen.gt.1.D00 .and. rlen.le.2.D00) then
   istart = 13
  !1_xk04_3E0
  else if (rlen.gt.2.D00 .and. rlen.le.3.D00) then
   istart = 13
  !1_xk04_4E0
  else if (rlen.gt.3.D00 .and. rlen.le.4.D00) then
   istart = 69
  !1_xk04_5E0
  else if (rlen.gt.4.D00 .and. rlen.le.5.D00) then
   istart = 125
  !1_xk04_6E0
  else if (rlen.gt.5.D00 .and. rlen.le.6.D00) then
   istart = 181
  !1_xk04_7E0
  else if (rlen.gt.6.D00 .and. rlen.le.7.D00) then
   istart = 237
  !1_xk04_8E0
  else if (rlen.gt.7.D00 .and. rlen.le.8.D00) then
   istart = 293
  !1_xk04_9E0
  else if (rlen.gt.8.D00 .and. rlen.le.9.D00) then
   istart = 349
  !1_xk04_1E1
  else if (rlen.gt.9.D00 .and. rlen.le.1.D01) then
   istart = 405
  !1_xk04_2E1
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then
   istart = 461
  !1_xk04_3E1
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then
   istart = 731
  !1_xk04_4E1
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then
   istart = 1035
  !1_xk04_5E1
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then
   istart = 1339
  !1_xk04_6E1
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then
   istart = 1679
  !1_xk04_7E1
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then
   istart = 2053
  !1_xk04_8E1
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then
   istart = 2467
  !1_xk04_9E1
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then
   istart = 2881
  !1_xk04_1E2
  else if (rlen.gt.9.D01 .and. rlen.le.1.D01) then
   istart = 3295
  !1_xk04_2E2
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then
   istart = 3751
  !1_xk04_3E2
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
   istart = 4251
  !1_xk04_4E2
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
   istart = 4889
  !1_xk04_5E2
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   istart = 5527
  else
   istart = 6217
  end if

!-----------------------------------------------------------------------------!
 case (5)
!-----------------------------------------------------------------------------!
  !1_xk05_2E0
  if      (rlen.gt.1.D00 .and. rlen.le.2.D00) then
   istart = 21
  !1_xk05_3E0
  else if (rlen.gt.2.D00 .and. rlen.le.3.D00) then
   istart = 21
  !1_xk05_4E0
  else if (rlen.gt.3.D00 .and. rlen.le.4.D00) then
   istart = 77
  !1_xk05_5E0
  else if (rlen.gt.4.D00 .and. rlen.le.5.D00) then
   istart = 133
  !1_xk05_6E0
  else if (rlen.gt.5.D00 .and. rlen.le.6.D00) then
   istart = 189
  !1_xk05_7E0
  else if (rlen.gt.6.D00 .and. rlen.le.7.D00) then
   istart = 245
  !1_xk05_8E0
  else if (rlen.gt.7.D00 .and. rlen.le.8.D00) then
   istart = 301
  !1_xk05_9E0
  else if (rlen.gt.8.D00 .and. rlen.le.9.D00) then
   istart = 357
  !1_xk05_1E1
  else if (rlen.gt.9.D00 .and. rlen.le.1.D01) then
   istart = 413
  !1_xk05_2E1
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then
   istart = 469
  !1_xk05_3E1
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then
   istart = 739
  !1_xk05_4E1
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then
   istart = 1043
  !1_xk05_5E1
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then
   istart = 1347
  !1_xk05_6E1
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then
   istart = 1687
  !1_xk05_7E1
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then
   istart = 2061
  !1_xk05_8E1
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then
   istart = 2475
  !1_xk05_9E1
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then
   istart = 2889
  !1_xk05_1E2
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then
   istart = 3303
  !1_xk05_2E2
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then
   istart = 3759
  !1_xk05_3E2
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
   istart = 4259
  !1_xk05_4E2
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
   istart = 4897
  !1_xk05_5E2
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   istart = 5535
  !1_xk05_6E2
  else if (rlen.gt.5.D02 .and. rlen.le.6.D02) then
   istart = 6225
  !1_xk05_7E2
  else if (rlen.gt.6.D02 .and. rlen.le.7.D02) then
   istart = 6907
  !1_xk05_8E2
  else if (rlen.gt.7.D02 .and. rlen.le.8.D02) then
   istart = 7247
  !1_xk05_9E2
  else if (rlen.gt.8.D02 .and. rlen.le.9.D02) then
   istart = 7929
  !1_xk05_1E3
  else if (rlen.gt.9.D02 .and. rlen.le.1.D03) then
   istart = 8269
  !1_xk05_2E3
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
   istart = 8609
  else
   istart = 9291
  end if

!-----------------------------------------------------------------------------!
 case (6)
!-----------------------------------------------------------------------------!
  !1_xk06_2E0
  if      (rlen.gt.1.D00 .and. rlen.le.2.D00) then
   istart = 31
  !1_xk06_3E0
  else if (rlen.gt.2.D00 .and. rlen.le.3.D00) then
   istart = 31
  !1_xk06_4E0
  else if (rlen.gt.3.D00 .and. rlen.le.4.D00) then
   istart = 87
  !1_xk06_5E0
  else if (rlen.gt.4.D00 .and. rlen.le.5.D00) then
   istart = 143
  !1_xk06_6E0
  else if (rlen.gt.5.D00 .and. rlen.le.6.D00) then
   istart = 199
  !1_xk06_7E0
  else if (rlen.gt.6.D00 .and. rlen.le.7.D00) then
   istart = 255
  !1_xk06_8E0
  else if (rlen.gt.7.D00 .and. rlen.le.8.D00) then
   istart = 311
  !1_xk06_9E0
  else if (rlen.gt.8.D00 .and. rlen.le.9.D00) then
   istart = 367
  !1_xk06_1E1
  else if (rlen.gt.9.D00 .and. rlen.le.1.D01) then
   istart = 423
  !1_xk06_2E1
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then
   istart = 479
  !1_xk06_3E1
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then
   istart = 749
  !1_xk06_4E1
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then
   istart = 1053
  !1_xk06_5E1
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then
   istart = 1357
  !1_xk06_6E1
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then
   istart = 1697
  !1_xk06_7E1
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then
   istart = 2071
  !1_xk06_8E1
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then
   istart = 2485
  !1_xk06_9E1
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then
   istart = 2899
  !1_xk06_1E2
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then
   istart = 3313
  !1_xk06_2E2
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then
   istart = 3769
  !1_xk06_3E2
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
   istart = 4269
  !1_xk06_4E2
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
   istart = 4907
  !1_xk06_5E2
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   istart = 5545
  !1_xk06_6E2
  else if (rlen.gt.5.D02 .and. rlen.le.6.D02) then
   istart = 6235
  !1_xk06_7E2
  else if (rlen.gt.6.D02 .and. rlen.le.7.D02) then
   istart = 6917
  !1_xk06_8E2
  else if (rlen.gt.7.D02 .and. rlen.le.8.D02) then
   istart = 7257
  !1_xk06_9E2
  else if (rlen.gt.8.D02 .and. rlen.le.9.D02) then
   istart = 7939
  !1_xk06_1E3
  else if (rlen.gt.9.D02 .and. rlen.le.1.D03) then
   istart = 8279
  !1_xk06_2E3
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
   istart = 8619
  !1_xk06_3E3
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then
   istart = 9301
  else
   istart = 9973
  end if

!-----------------------------------------------------------------------------!
 case (7)
!-----------------------------------------------------------------------------!
  !1_xk07_2E0
  if      (rlen.gt.1.D00 .and. rlen.le.2.D00) then
   istart = 43
  !1_xk07_3E0
  else if (rlen.gt.2.D00 .and. rlen.le.3.D00) then
   istart = 43
  !1_xk07_4E0
  else if (rlen.gt.3.D00 .and. rlen.le.4.D00) then
   istart = 99
  !1_xk07_5E0
  else if (rlen.gt.4.D00 .and. rlen.le.5.D00) then
   istart = 155
  !1_xk07_6E0
  else if (rlen.gt.5.D00 .and. rlen.le.6.D00) then
   istart = 211
  !1_xk07_7E0
  else if (rlen.gt.6.D00 .and. rlen.le.7.D00) then
   istart = 267
  !1_xk07_8E0
  else if (rlen.gt.7.D00 .and. rlen.le.8.D00) then
   istart = 323
  !1_xk07_9E0
  else if (rlen.gt.8.D00 .and. rlen.le.9.D00) then
   istart = 379
  !1_xk07_1E1
  else if (rlen.gt.9.D00 .and. rlen.le.1.D01) then
   istart = 435
  !1_xk07_2E1
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then
   istart = 491
  !1_xk07_3E1
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then
   istart = 761
  !1_xk07_4E1
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then
   istart = 1065
  !1_xk07_5E1
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then
   istart = 1369
  !1_xk07_6E1
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then
   istart = 1709
  !1_xk07_7E1
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then
   istart = 2083
  !1_xk07_8E1
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then
   istart = 2497
  !1_xk07_9E1
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then
   istart = 2911
  !1_xk07_1E2
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then
   istart = 3325
  !1_xk07_2E2
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then
   istart = 3781
  !1_xk07_3E2
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
   istart = 4281
  !1_xk07_4E2
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
   istart = 4919
  !1_xk07_5E2
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   istart = 5557
  !1_xk07_6E2
  else if (rlen.gt.5.D02 .and. rlen.le.6.D02) then
   istart = 6247
  !1_xk07_7E2
  else if (rlen.gt.6.D02 .and. rlen.le.7.D02) then
   istart = 6929
  !1_xk07_8E2
  else if (rlen.gt.7.D02 .and. rlen.le.8.D02) then
   istart = 7269
  !1_xk07_9E2
  else if (rlen.gt.8.D02 .and. rlen.le.9.D02) then
   istart = 7951
  !1_xk07_1E3
  else if (rlen.gt.9.D02 .and. rlen.le.1.D03) then
   istart = 8291
  !1_xk07_2E3
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
   istart = 8631
  !1_xk07_3E3
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then
   istart = 9313
  !1_xk07_4E3
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then
   istart = 9985
  !1_xk07_5E3
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then
   istart = 10645
  !1_xk07_6E3
  else if (rlen.gt.5.D03 .and. rlen.le.6.D03) then
   istart = 11305
  !1_xk07_7E3
  else if (rlen.gt.6.D03 .and. rlen.le.7.D03) then
   istart = 11965
  else
   istart = 12333
  end if

!-----------------------------------------------------------------------------!
 case (8)
!-----------------------------------------------------------------------------!
  !1_xk08_1E1
  if      (rlen.gt.1.D00 .and. rlen.le.1.D01) then
   istart = 505
  !1_xk08_2E1
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then
   istart = 505
  !1_xk08_3E1
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then
   istart = 775
  !1_xk08_4E1
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then
   istart = 1079
  !1_xk08_5E1
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then
   istart = 1383
  !1_xk08_6E1
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then
   istart = 1723
  !1_xk08_7E1
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then
   istart = 2097
  !1_xk08_8E1
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then
   istart = 2511
  !1_xk08_9E1
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then
   istart = 2925
  !1_xk08_1E2
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then
   istart = 3339
  !1_xk08_2E2
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then
   istart = 3795
  !1_xk08_3E2
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
   istart = 4295
  !1_xk08_4E2
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
   istart = 4933
  !1_xk08_5E2
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   istart = 5571
  !1_xk08_7E2
  else if (rlen.gt.5.D02 .and. rlen.le.7.D02) then
   istart = 6261
  !1_xk08_1E3
  else if (rlen.gt.7.D02 .and. rlen.le.1.D03) then
   istart = 7283
  !1_xk08_2E3
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
   istart = 8645
  !1_xk08_3E3
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then
   istart = 9327
  !1_xk08_4E3
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then
   istart = 9999
  !1_xk08_5E3
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then
   istart = 10659
  !1_xk08_7E3
  else if (rlen.gt.5.D03 .and. rlen.le.7.D03) then
   istart = 11319
  !1_xk08_1E4
  else if (rlen.gt.7.D03 .and. rlen.le.1.D04) then
   istart = 12347
  !1_xk08_2E4
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then
   istart = 13701
  else
   istart = 14401
  end if

!-----------------------------------------------------------------------------!
 case (9)
!-----------------------------------------------------------------------------!
  !1_xk09_1E1
  if      (rlen.gt.1.D00 .and. rlen.le.1.D01) then
   istart = 521
  !1_xk09_2E1
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then
   istart = 521
  !1_xk09_3E1
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then
   istart = 791
  !1_xk09_4E1
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then
   istart = 1095
  !1_xk09_5E1
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then
   istart = 1399
  !1_xk09_6E1
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then
   istart = 1739
  !1_xk09_7E1
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then
   istart = 2113
  !1_xk09_8E1
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then
   istart = 2527
  !1_xk09_9E1
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then
   istart = 2941
  !1_xk09_1E2
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then
   istart = 3355
  !1_xk09_2E2
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then
   istart = 3811
  !1_xk09_3E2
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
   istart = 4311
  !1_xk09_4E2
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
   istart = 4949
  !1_xk09_5E2
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   istart = 5587
  !1_xk09_7E2
  else if (rlen.gt.5.D02 .and. rlen.le.7.D02) then
   istart = 6277
  !1_xk09_1E3
  else if (rlen.gt.7.D02 .and. rlen.le.1.D03) then
   istart = 7299
  !1_xk09_2E3
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
   istart = 8661
  !1_xk09_3E3
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then
   istart = 9343
  !1_xk09_4E3
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then
   istart = 10015
  !1_xk09_5E3
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then
   istart = 10675
  !1_xk09_7E3
  else if (rlen.gt.5.D03 .and. rlen.le.7.D03) then
   istart = 11335
  !1_xk09_1E4
  else if (rlen.gt.7.D03 .and. rlen.le.1.D04) then
   istart = 12363
  !1_xk09_2E4
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then
   istart = 13717
  !1_xk09_3E4
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then
   istart = 14417
  else
   istart = 15101
  end if

!-----------------------------------------------------------------------------!
 case (10)
!-----------------------------------------------------------------------------!
  !1_xk10_1E1
  if      (rlen.gt.1.D00 .and. rlen.le.1.D01) then
   istart = 539
  !1_xk10_2E1
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then
   istart = 539
  !1_xk10_3E1
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then
   istart = 809
  !1_xk10_4E1
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then
   istart = 1113
  !1_xk10_5E1
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then
   istart = 1417
  !1_xk10_6E1
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then
   istart = 1757
  !1_xk10_7E1
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then
   istart = 2131
  !1_xk10_8E1
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then
   istart = 2545
  !1_xk10_9E1
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then
   istart = 2959
  !1_xk10_1E2
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then
   istart = 3373
  !1_xk10_2E2
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then
   istart = 3829
  !1_xk10_3E2
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
   istart = 4329
  !1_xk10_4E2
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
   istart = 4967
  !1_xk10_5E2
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   istart = 5605
  !1_xk10_7E2
  else if (rlen.gt.5.D02 .and. rlen.le.7.D02) then
   istart = 6295
  !1_xk10_1E3
  else if (rlen.gt.7.D02 .and. rlen.le.1.D03) then
   istart = 7317
  !1_xk10_2E3
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
   istart = 8679
  !1_xk10_3E3
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then
   istart = 9361
  !1_xk10_4E3
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then
   istart = 10033
  !1_xk10_5E3
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then
   istart = 10693
  !1_xk10_7E3
  else if (rlen.gt.5.D03 .and. rlen.le.7.D03) then
   istart = 11353
  !1_xk10_1E4
  else if (rlen.gt.7.D03 .and. rlen.le.1.D04) then
   istart = 12381
  !1_xk10_2E4
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then
   istart = 13735
  !1_xk10_3E4
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then
   istart = 14435
  !1_xk10_5E4
  else if (rlen.gt.3.D04 .and. rlen.le.5.D04) then
   istart = 15119
  !1_xk10_1E5
  else if (rlen.gt.5.D04 .and. rlen.le.1.D05) then
   istart = 16231
  else
   istart = 17711
  end if

!-----------------------------------------------------------------------------!
 case (11)
!-----------------------------------------------------------------------------!
  !1_xk11_1E1
  if      (rlen.gt.1.D00 .and. rlen.le.1.D01) then
   istart = 559
  !1_xk11_2E1
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then
   istart = 559
  !1_xk11_3E1
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then
   istart = 829
  !1_xk11_4E1
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then
   istart = 1133
  !1_xk11_5E1
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then
   istart = 1437
  !1_xk11_6E1
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then
   istart = 1777
  !1_xk11_7E1
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then
   istart = 2151
  !1_xk11_8E1
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then
   istart = 2565
  !1_xk11_9E1
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then
   istart = 2979
  !1_xk11_1E2
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then
   istart = 3393
  !1_xk11_2E2
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then
   istart = 3849
  !1_xk11_3E2
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
   istart = 4349
  !1_xk11_4E2
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
   istart = 4987
  !1_xk11_5E2
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   istart = 5625
  !1_xk11_7E2
  else if (rlen.gt.5.D02 .and. rlen.le.7.D02) then
   istart = 6315
  !1_xk11_1E3
  else if (rlen.gt.7.D02 .and. rlen.le.1.D03) then
   istart = 7337
  !1_xk11_2E3
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
   istart = 8699
  !1_xk11_3E3
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then
   istart = 9381
  !1_xk11_4E3
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then
   istart = 10053
  !1_xk11_5E3
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then
   istart = 10713
  !1_xk11_7E3
  else if (rlen.gt.5.D03 .and. rlen.le.7.D03) then
   istart = 11373
  !1_xk11_1E4
  else if (rlen.gt.7.D03 .and. rlen.le.1.D04) then
   istart = 12401
  !1_xk11_2E4
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then
   istart = 13755
  !1_xk11_3E4
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then
   istart = 14455
  !1_xk11_5E4
  else if (rlen.gt.3.D04 .and. rlen.le.5.D04) then
   istart = 15139
  !1_xk11_1E5
  else if (rlen.gt.5.D04 .and. rlen.le.1.D05) then
   istart = 16251
  !1_xk11_2E5
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then
   istart = 17731
  else
   istart = 18377
  end if

!-----------------------------------------------------------------------------!
 case (12)
!-----------------------------------------------------------------------------!
  !1_xk12_1E1
  if      (rlen.gt.1.D00 .and. rlen.le.1.D01) then
   istart = 581
  !1_xk12_2E1
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then
   istart = 581
  !1_xk12_3E1
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then
   istart = 851
  !1_xk12_4E1
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then
   istart = 1155
  !1_xk12_5E1
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then
   istart = 1459
  !1_xk12_6E1
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then
   istart = 1799
  !1_xk12_7E1
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then
   istart = 2173
  !1_xk12_8E1
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then
   istart = 2587
  !1_xk12_9E1
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then
   istart = 3001
  !1_xk12_1E2
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then
   istart = 3415
  !1_xk12_2E2
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then
   istart = 3871
  !1_xk12_3E2
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
   istart = 4371
  !1_xk12_4E2
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
   istart = 5009
  !1_xk12_5E2
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   istart = 5647
  !1_xk12_7E2
  else if (rlen.gt.5.D02 .and. rlen.le.7.D02) then
   istart = 6337
  !1_xk12_1E3
  else if (rlen.gt.7.D02 .and. rlen.le.1.D03) then
   istart = 7359
  !1_xk12_2E3
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
   istart = 8721
  !1_xk12_3E3
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then
   istart = 9403
  !1_xk12_4E3
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then
   istart = 10075
  !1_xk12_5E3
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then
   istart = 10735
  !1_xk12_7E3
  else if (rlen.gt.5.D03 .and. rlen.le.7.D03) then
   istart = 11395
  !1_xk12_1E4
  else if (rlen.gt.7.D03 .and. rlen.le.1.D04) then
   istart = 12423
  !1_xk12_2E4
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then
   istart = 13777
  !1_xk12_3E4
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then
   istart = 14477
  !1_xk12_5E4
  else if (rlen.gt.3.D04 .and. rlen.le.5.D04) then
   istart = 15161
  !1_xk12_1E5
  else if (rlen.gt.5.D04 .and. rlen.le.1.D05) then
   istart = 16273
  !1_xk12_2E5
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then
   istart = 17753
  !1_xk12_3E5
  else if (rlen.gt.2.D05 .and. rlen.le.3.D05) then
   istart = 18399
  else
   istart = 19023
  end if

!-----------------------------------------------------------------------------!
 case (13)
!-----------------------------------------------------------------------------!
  !1_xk13_1E1
  if      (rlen.gt.1.D00 .and. rlen.le.1.D01) then
   istart = 605
  !1_xk13_2E1
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then
   istart = 605
  !1_xk13_3E1
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then
   istart = 875
  !1_xk13_4E1
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then
   istart = 1179
  !1_xk13_5E1
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then
   istart = 1483
  !1_xk13_6E1
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then
   istart = 1823
  !1_xk13_7E1
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then
   istart = 2197
  !1_xk13_8E1
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then
   istart = 2611
  !1_xk13_9E1
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then
   istart = 3025
  !1_xk13_1E2
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then
   istart = 3439
  !1_xk13_2E2
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then
   istart = 3895
  !1_xk13_3E2
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
   istart = 4395
  !1_xk13_4E2
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
   istart = 5033
  !1_xk13_5E2
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   istart = 5671
  !1_xk13_7E2
  else if (rlen.gt.5.D02 .and. rlen.le.7.D02) then
   istart = 6361
  !1_xk13_1E3
  else if (rlen.gt.7.D02 .and. rlen.le.1.D03) then
   istart = 7383
  !1_xk13_2E3
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
   istart = 8745
  !1_xk13_3E3
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then
   istart = 9427
  !1_xk13_4E3
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then
   istart = 10099
  !1_xk13_5E3
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then
   istart = 10759
  !1_xk13_7E3
  else if (rlen.gt.5.D03 .and. rlen.le.7.D03) then
   istart = 11419
  !1_xk13_1E4
  else if (rlen.gt.7.D03 .and. rlen.le.1.D04) then
   istart = 12447
  !1_xk13_2E4
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then
   istart = 13801
  !1_xk13_3E4
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then
   istart = 14501
  !1_xk13_5E4
  else if (rlen.gt.3.D04 .and. rlen.le.5.D04) then
   istart = 15185
  !1_xk13_1E5
  else if (rlen.gt.5.D04 .and. rlen.le.1.D05) then
   istart = 16297
  !1_xk13_2E5
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then
   istart = 17777
  !1_xk13_3E5
  else if (rlen.gt.2.D05 .and. rlen.le.3.D05) then
   istart = 18423
  !1_xk13_4E5
  else if (rlen.gt.3.D05 .and. rlen.le.4.D05) then
   istart = 19047
  else
   istart = 19345
  end if

!-----------------------------------------------------------------------------!
 case (14)
!-----------------------------------------------------------------------------!
  !1_xk14_1E1
  if      (rlen.gt.1.D00 .and. rlen.le.1.D01) then
   istart = 631
  !1_xk14_2E1
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then
   istart = 631
  !1_xk14_3E1
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then
   istart = 901
  !1_xk14_4E1
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then
   istart = 1205
  !1_xk14_5E1
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then
   istart = 1509
  !1_xk14_6E1
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then
   istart = 1849
  !1_xk14_7E1
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then
   istart = 2223
  !1_xk14_8E1
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then
   istart = 2637
  !1_xk14_9E1
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then
   istart = 3051
  !1_xk14_1E2
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then
   istart = 3465
  !1_xk14_2E2
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then
   istart = 3921
  !1_xk14_3E2
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
   istart = 4421
  !1_xk14_4E2
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
   istart = 5059
  !1_xk14_5E2
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   istart = 5697
  !1_xk14_7E2
  else if (rlen.gt.5.D02 .and. rlen.le.7.D02) then
   istart = 6387
  !1_xk14_1E3
  else if (rlen.gt.7.D02 .and. rlen.le.1.D03) then
   istart = 7409
  !1_xk14_2E3
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
   istart = 8771
  !1_xk14_3E3
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then
   istart = 9453
  !1_xk14_4E3
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then
   istart = 10125
  !1_xk14_5E3
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then
   istart = 10785
  !1_xk14_7E3
  else if (rlen.gt.5.D03 .and. rlen.le.7.D03) then
   istart = 11445
  !1_xk14_1E4
  else if (rlen.gt.7.D03 .and. rlen.le.1.D04) then
   istart = 12473
  !1_xk14_2E4
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then
   istart = 13827
  !1_xk14_3E4
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then
   istart = 14527
  !1_xk14_5E4
  else if (rlen.gt.3.D04 .and. rlen.le.5.D04) then
   istart = 15211
  !1_xk14_1E5
  else if (rlen.gt.5.D04 .and. rlen.le.1.D05) then
   istart = 16323
  !1_xk14_2E5
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then
   istart = 17803
  !1_xk14_3E5
  else if (rlen.gt.2.D05 .and. rlen.le.3.D05) then
   istart = 18449
  !1_xk14_4E5
  else if (rlen.gt.3.D05 .and. rlen.le.4.D05) then
   istart = 19073
  !1_xk14_5E5
  else if (rlen.gt.4.D05 .and. rlen.le.5.D05) then
   istart = 19371
  !1_xk14_7E5
  else if (rlen.gt.5.D05 .and. rlen.le.7.D05) then
   istart = 19643
  else
   istart = 20271
  end if

!-----------------------------------------------------------------------------!
 case (15)
!-----------------------------------------------------------------------------!
  !1_xk15_1E1
  if      (rlen.gt.1.D00 .and. rlen.le.1.D01) then
   istart = 659
  !1_xk15_2E1
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then
   istart = 659
  !1_xk15_3E1
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then
   istart = 929
  !1_xk15_4E1
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then
   istart = 1233
  !1_xk15_5E1
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then
   istart = 1537
  !1_xk15_6E1
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then
   istart = 1877
  !1_xk15_7E1
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then
   istart = 2251
  !1_xk15_8E1
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then
   istart = 2665
  !1_xk15_9E1
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then
   istart = 3079
  !1_xk15_1E2
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then
   istart = 3493
  !1_xk15_2E2
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then
   istart = 3949
  !1_xk15_3E2
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
   istart = 4449
  !1_xk15_4E2
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
   istart = 5087
  !1_xk15_5E2
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   istart = 5725
  !1_xk15_6E2
  else if (rlen.gt.5.D02 .and. rlen.le.6.D02) then
   istart = 6415
  !1_xk15_7E2
  else if (rlen.gt.6.D02 .and. rlen.le.7.D02) then
   istart = 6943
  !1_xk15_8E2
  else if (rlen.gt.7.D02 .and. rlen.le.8.D02) then
   istart = 7437
  !1_xk15_9E2
  else if (rlen.gt.8.D02 .and. rlen.le.9.D02) then
   istart = 7965
  !1_xk15_1E3
  else if (rlen.gt.9.D02 .and. rlen.le.1.D03) then
   istart = 8305
  !1_xk15_2E3
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
   istart = 8799
  !1_xk15_3E3
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then
   istart = 9481
  !1_xk15_4E3
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then
   istart = 10153
  !1_xk15_5E3
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then
   istart = 10813
  !1_xk15_6E3
  else if (rlen.gt.5.D03 .and. rlen.le.6.D03) then
   istart = 11473
  !1_xk15_7E3
  else if (rlen.gt.6.D03 .and. rlen.le.7.D03) then
   istart = 11979
  !1_xk15_8E3
  else if (rlen.gt.7.D03 .and. rlen.le.8.D03) then
   istart = 12501
  !1_xk15_9E3
  else if (rlen.gt.8.D03 .and. rlen.le.9.D03) then
   istart = 12993
  !1_xk15_1E4
  else if (rlen.gt.9.D03 .and. rlen.le.1.D04) then
   istart = 13347
  !1_xk15_2E4
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then
   istart = 13855
  !1_xk15_3E4
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then
   istart = 14555
  !1_xk15_4E4
  else if (rlen.gt.3.D04 .and. rlen.le.4.D04) then
   istart = 15239
  !1_xk15_5E4
  else if (rlen.gt.4.D04 .and. rlen.le.5.D04) then
   istart = 15735
  !1_xk15_7E4
  else if (rlen.gt.5.D04 .and. rlen.le.7.D04) then
   istart = 16351
  !1_xk15_1E5
  else if (rlen.gt.7.D04 .and. rlen.le.1.D05) then
   istart = 17003
  !1_xk15_2E5
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then
   istart = 17831
  !1_xk15_5E5
  else if (rlen.gt.2.D05 .and. rlen.le.5.D05) then
   istart = 18477
  !1_xk15_1E6
  else if (rlen.gt.5.D05 .and. rlen.le.1.D06) then
   istart = 19671
  !1_xk15_2E6
  else if (rlen.gt.1.D06 .and. rlen.le.2.D06) then
   istart = 20651
  else
   istart = 21197
  end if

!-----------------------------------------------------------------------------!
 case (16)
!-----------------------------------------------------------------------------!
  !1_xk16_1E1
  if      (rlen.gt.1.D00 .and. rlen.le.1.D01) then
   istart = 689
  !1_xk16_2E1
  else if (rlen.gt.1.D01 .and. rlen.le.2.D01) then
   istart = 689
  !1_xk16_3E1
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then
   istart = 959
  !1_xk16_4E1
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then
   istart = 1263
  !1_xk16_5E1
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then
   istart = 1567
  !1_xk16_6E1
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then
   istart = 1907
  !1_xk16_7E1
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then
   istart = 2281
  !1_xk16_8E1
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then
   istart = 2695
  !1_xk16_9E1
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then
   istart = 3109
  !1_xk16_1E2
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then
   istart = 3523
  !1_xk16_2E2
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then
   istart = 3979
  !1_xk16_3E2
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
   istart = 4479
  !1_xk16_4E2
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
   istart = 5117
  !1_xk16_5E2
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   istart = 5755
  !1_xk16_6E2
  else if (rlen.gt.5.D02 .and. rlen.le.6.D02) then
   istart = 6445
  !1_xk16_7E2
  else if (rlen.gt.6.D02 .and. rlen.le.7.D02) then
   istart = 6973
  !1_xk16_8E2
  else if (rlen.gt.7.D02 .and. rlen.le.8.D02) then
   istart = 7467
  !1_xk16_9E2
  else if (rlen.gt.8.D02 .and. rlen.le.9.D02) then
   istart = 7995
  !1_xk16_1E3
  else if (rlen.gt.9.D02 .and. rlen.le.1.D03) then
   istart = 8335
  !1_xk16_2E3
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
   istart = 8829
  !1_xk16_3E3
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then
   istart = 9511
  !1_xk16_4E3
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then
   istart = 10183
  !1_xk16_5E3
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then
   istart = 10843
  !1_xk16_6E3
  else if (rlen.gt.5.D03 .and. rlen.le.6.D03) then
   istart = 11503
  !1_xk16_7E3
  else if (rlen.gt.6.D03 .and. rlen.le.7.D03) then
   istart = 12009
  !1_xk16_8E3
  else if (rlen.gt.7.D03 .and. rlen.le.8.D03) then
   istart = 12531
  !1_xk16_9E3
  else if (rlen.gt.8.D03 .and. rlen.le.9.D03) then
   istart = 13023
  !1_xk16_1E4
  else if (rlen.gt.9.D03 .and. rlen.le.1.D04) then
   istart = 13377
  !1_xk16_2E4
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then
   istart = 13885
  !1_xk16_3E4
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then
   istart = 14585
  !1_xk16_4E4
  else if (rlen.gt.3.D04 .and. rlen.le.4.D04) then
   istart = 15269
  !1_xk16_5E4
  else if (rlen.gt.4.D04 .and. rlen.le.5.D04) then
   istart = 15765
  !1_xk16_7E4
  else if (rlen.gt.5.D04 .and. rlen.le.7.D04) then
   istart = 16381
  !1_xk16_1E5
  else if (rlen.gt.7.D04 .and. rlen.le.1.D05) then
   istart = 17033
  !1_xk16_2E5
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then
   istart = 17861
  !1_xk16_5E5
  else if (rlen.gt.2.D05 .and. rlen.le.5.D05) then
   istart = 18507
  !1_xk16_1E6
  else if (rlen.gt.5.D05 .and. rlen.le.1.D06) then
   istart = 19701
  !1_xk16_2E6
  else if (rlen.gt.1.D06 .and. rlen.le.2.D06) then
   istart = 20681
  !1_xk16_3E6
  else if (rlen.gt.2.D06 .and. rlen.le.3.D06) then
   istart = 21227
  else
   istart = 21743
  end if

!-----------------------------------------------------------------------------!
 case (17)
!-----------------------------------------------------------------------------!
  !1_xk17_2E1
  if      (rlen.gt.1.D00 .and. rlen.le.2.D01) then
   istart = 991
  !1_xk17_3E1
  else if (rlen.gt.2.D01 .and. rlen.le.3.D01) then
   istart = 991
  !1_xk17_4E1
  else if (rlen.gt.3.D01 .and. rlen.le.4.D01) then
   istart = 1295
  !1_xk17_5E1
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then
   istart = 1599
  !1_xk17_6E1
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then
   istart = 1939
  !1_xk17_7E1
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then
   istart = 2313
  !1_xk17_8E1
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then
   istart = 2727
  !1_xk17_9E1
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then
   istart = 3141
  !1_xk17_1E2
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then
   istart = 3555
  !1_xk17_2E2
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then
   istart = 4011
  !1_xk17_3E2
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
   istart = 4511
  !1_xk17_4E2
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
   istart = 5149
  !1_xk17_5E2
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   istart = 5787
  !1_xk17_6E2
  else if (rlen.gt.5.D02 .and. rlen.le.6.D02) then
   istart = 6477
  !1_xk17_7E2
  else if (rlen.gt.6.D02 .and. rlen.le.7.D02) then
   istart = 7005
  !1_xk17_8E2
  else if (rlen.gt.7.D02 .and. rlen.le.8.D02) then
   istart = 7499
  !1_xk17_9E2
  else if (rlen.gt.8.D02 .and. rlen.le.9.D02) then
   istart = 8027
  !1_xk17_1E3
  else if (rlen.gt.9.D02 .and. rlen.le.1.D03) then
   istart = 8367
  !1_xk17_2E3
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
   istart = 8861
  !1_xk17_3E3
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then
   istart = 9543
  !1_xk17_4E3
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then
   istart = 10215
  !1_xk17_5E3
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then
   istart = 10875
  !1_xk17_6E3
  else if (rlen.gt.5.D03 .and. rlen.le.6.D03) then
   istart = 11535
  !1_xk17_7E3
  else if (rlen.gt.6.D03 .and. rlen.le.7.D03) then
   istart = 12041
  !1_xk17_8E3
  else if (rlen.gt.7.D03 .and. rlen.le.8.D03) then
   istart = 12563
  !1_xk17_9E3
  else if (rlen.gt.8.D03 .and. rlen.le.9.D03) then
   istart = 13055
  !1_xk17_1E4
  else if (rlen.gt.9.D03 .and. rlen.le.1.D04) then
   istart = 13409
  !1_xk17_2E4
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then
   istart = 13917
  !1_xk17_3E4
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then
   istart = 14617
  !1_xk17_4E4
  else if (rlen.gt.3.D04 .and. rlen.le.4.D04) then
   istart = 15301
  !1_xk17_5E4
  else if (rlen.gt.4.D04 .and. rlen.le.5.D04) then
   istart = 15797
  !1_xk17_7E4
  else if (rlen.gt.5.D04 .and. rlen.le.7.D04) then
   istart = 16413
  !1_xk17_1E5
  else if (rlen.gt.7.D04 .and. rlen.le.1.D05) then
   istart = 17065
  !1_xk17_2E5
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then
   istart = 17893
  !1_xk17_5E5
  else if (rlen.gt.2.D05 .and. rlen.le.5.D05) then
   istart = 18539
  !1_xk17_1E6
  else if (rlen.gt.5.D05 .and. rlen.le.1.D06) then
   istart = 19733
  !1_xk17_2E6
  else if (rlen.gt.1.D06 .and. rlen.le.2.D06) then
   istart = 20713
  !1_xk17_3E6
  else if (rlen.gt.2.D06 .and. rlen.le.3.D06) then
   istart = 21259
  !1_xk17_4E6
  else if (rlen.gt.3.D06 .and. rlen.le.4.D06) then
   istart = 21775
  else
   istart = 22209
  end if

!-----------------------------------------------------------------------------!
 case (18)
!-----------------------------------------------------------------------------!
  !1_xk18_4E1
  if      (rlen.gt.1.D00 .and. rlen.le.4.D01) then
   istart = 1633
  !1_xk18_5E1
  else if (rlen.gt.4.D01 .and. rlen.le.5.D01) then
   istart = 1633
  !1_xk18_6E1
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then
   istart = 1973
  !1_xk18_7E1
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then
   istart = 2347
  !1_xk18_8E1
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then
   istart = 2761
  !1_xk18_9E1
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then
   istart = 3175
  !1_xk18_1E2
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then
   istart = 3589
  !1_xk18_2E2
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then
   istart = 4045
  !1_xk18_3E2
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
   istart = 4545
  !1_xk18_4E2
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
   istart = 5183
  !1_xk18_5E2
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   istart = 5821
  !1_xk18_6E2
  else if (rlen.gt.5.D02 .and. rlen.le.6.D02) then
   istart = 6511
  !1_xk18_7E2
  else if (rlen.gt.6.D02 .and. rlen.le.7.D02) then
   istart = 7039
  !1_xk18_8E2
  else if (rlen.gt.7.D02 .and. rlen.le.8.D02) then
   istart = 7533
  !1_xk18_9E2
  else if (rlen.gt.8.D02 .and. rlen.le.9.D02) then
   istart = 8061
  !1_xk18_1E3
  else if (rlen.gt.9.D02 .and. rlen.le.1.D03) then
   istart = 8401
  !1_xk18_2E3
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
   istart = 8895
  !1_xk18_3E3
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then
   istart = 9577
  !1_xk18_4E3
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then
   istart = 10249
  !1_xk18_5E3
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then
   istart = 10909
  !1_xk18_6E3
  else if (rlen.gt.5.D03 .and. rlen.le.6.D03) then
   istart = 11569
  !1_xk18_7E3
  else if (rlen.gt.6.D03 .and. rlen.le.7.D03) then
   istart = 12075
  !1_xk18_8E3
  else if (rlen.gt.7.D03 .and. rlen.le.8.D03) then
   istart = 12597
  !1_xk18_9E3
  else if (rlen.gt.8.D03 .and. rlen.le.9.D03) then
   istart = 13089
  !1_xk18_1E4
  else if (rlen.gt.9.D03 .and. rlen.le.1.D04) then
   istart = 13443
  !1_xk18_2E4
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then
   istart = 13951
  !1_xk18_3E4
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then
   istart = 14651
  !1_xk18_4E4
  else if (rlen.gt.3.D04 .and. rlen.le.4.D04) then
   istart = 15335
  !1_xk18_5E4
  else if (rlen.gt.4.D04 .and. rlen.le.5.D04) then
   istart = 15831
  !1_xk18_7E4
  else if (rlen.gt.5.D04 .and. rlen.le.7.D04) then
   istart = 16447
  !1_xk18_1E5
  else if (rlen.gt.7.D04 .and. rlen.le.1.D05) then
   istart = 17099
  !1_xk18_2E5
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then
   istart = 17927
  !1_xk18_5E5
  else if (rlen.gt.2.D05 .and. rlen.le.5.D05) then
   istart = 18573
  !1_xk18_1E6
  else if (rlen.gt.5.D05 .and. rlen.le.1.D06) then
   istart = 19767
  !1_xk18_2E6
  else if (rlen.gt.1.D06 .and. rlen.le.2.D06) then
   istart = 20747
  !1_xk18_3E6
  else if (rlen.gt.2.D06 .and. rlen.le.3.D06) then
   istart = 21293
  !1_xk18_4E6
  else if (rlen.gt.3.D06 .and. rlen.le.4.D06) then
   istart = 21809
  !1_xk18_5E6
  else if (rlen.gt.4.D06 .and. rlen.le.5.D06) then
   istart = 22243
  !1_xk18_7E6
  else if (rlen.gt.5.D06 .and. rlen.le.7.D06) then
   istart = 22643
  else
   istart = 23147
  end if

!-----------------------------------------------------------------------------!
 case (19)
!-----------------------------------------------------------------------------!
  !1_xk19_5E1
  if      (rlen.gt.1.D00 .and. rlen.le.5.D01) then
   istart = 2009
  !1_xk19_6E1
  else if (rlen.gt.5.D01 .and. rlen.le.6.D01) then
   istart = 2009
  !1_xk19_7E1
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then
   istart = 2383
  !1_xk19_8E1
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then
   istart = 2797
  !1_xk19_9E1
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then
   istart = 3211
  !1_xk19_1E2
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then
   istart = 3625
  !1_xk19_2E2
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then
   istart = 4081
  !1_xk19_3E2
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
   istart = 4581
  !1_xk19_4E2
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
   istart = 5219
  !1_xk19_5E2
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   istart = 5857
  !1_xk19_6E2
  else if (rlen.gt.5.D02 .and. rlen.le.6.D02) then
   istart = 6547
  !1_xk19_7E2
  else if (rlen.gt.6.D02 .and. rlen.le.7.D02) then
   istart = 7075
  !1_xk19_8E2
  else if (rlen.gt.7.D02 .and. rlen.le.8.D02) then
   istart = 7569
  !1_xk19_9E2
  else if (rlen.gt.8.D02 .and. rlen.le.9.D02) then
   istart = 8097
  !1_xk19_1E3
  else if (rlen.gt.9.D02 .and. rlen.le.1.D03) then
   istart = 8437
  !1_xk19_2E3
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
   istart = 8931
  !1_xk19_3E3
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then
   istart = 9613
  !1_xk19_4E3
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then
   istart = 10285
  !1_xk19_5E3
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then
   istart = 10945
  !1_xk19_6E3
  else if (rlen.gt.5.D03 .and. rlen.le.6.D03) then
   istart = 11605
  !1_xk19_7E3
  else if (rlen.gt.6.D03 .and. rlen.le.7.D03) then
   istart = 12111
  !1_xk19_8E3
  else if (rlen.gt.7.D03 .and. rlen.le.8.D03) then
   istart = 12633
  !1_xk19_9E3
  else if (rlen.gt.8.D03 .and. rlen.le.9.D03) then
   istart = 13125
  !1_xk19_1E4
  else if (rlen.gt.9.D03 .and. rlen.le.1.D04) then
   istart = 13479
  !1_xk19_2E4
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then
   istart = 13987
  !1_xk19_3E4
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then
   istart = 14687
  !1_xk19_4E4
  else if (rlen.gt.3.D04 .and. rlen.le.4.D04) then
   istart = 15371
  !1_xk19_5E4
  else if (rlen.gt.4.D04 .and. rlen.le.5.D04) then
   istart = 15867
  !1_xk19_7E4
  else if (rlen.gt.5.D04 .and. rlen.le.7.D04) then
   istart = 16483
  !1_xk19_1E5
  else if (rlen.gt.7.D04 .and. rlen.le.1.D05) then
   istart = 17135
  !1_xk19_2E5
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then
   istart = 17963
  !1_xk19_5E5
  else if (rlen.gt.2.D05 .and. rlen.le.5.D05) then
   istart = 18609
  !1_xk19_1E6
  else if (rlen.gt.5.D05 .and. rlen.le.1.D06) then
   istart = 19803
  !1_xk19_2E6
  else if (rlen.gt.1.D06 .and. rlen.le.2.D06) then
   istart = 20783
  !1_xk19_3E6
  else if (rlen.gt.2.D06 .and. rlen.le.3.D06) then
   istart = 21329
  !1_xk19_4E6
  else if (rlen.gt.3.D06 .and. rlen.le.4.D06) then
   istart = 21845
  !1_xk19_5E6
  else if (rlen.gt.4.D06 .and. rlen.le.5.D06) then
   istart = 22279
  !1_xk19_7E6
  else if (rlen.gt.5.D06 .and. rlen.le.7.D06) then
   istart = 22679
  !1_xk19_1E7
  else if (rlen.gt.7.D06 .and. rlen.le.1.D07) then
   istart = 23183
  else
   istart = 23655
  end if

!-----------------------------------------------------------------------------!
 case (20)
!-----------------------------------------------------------------------------!
  !1_xk20_5E1
  if      (rlen.gt.1.D00 .and. rlen.le.6.D01) then
   istart = 2421
  !1_xk20_7E1
  else if (rlen.gt.6.D01 .and. rlen.le.7.D01) then
   istart = 2421
  !1_xk20_8E1
  else if (rlen.gt.7.D01 .and. rlen.le.8.D01) then
   istart = 2835
  !1_xk20_9E1
  else if (rlen.gt.8.D01 .and. rlen.le.9.D01) then
   istart = 3249
  !1_xk20_1E2
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then
   istart = 3663
  !1_xk20_2E2
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then
   istart = 4119
  !1_xk20_3E2
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
   istart = 4619
  !1_xk20_4E2
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
   istart = 5257
  !1_xk20_5E2
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   istart = 5895
  !1_xk20_6E2
  else if (rlen.gt.5.D02 .and. rlen.le.6.D02) then
   istart = 6585
  !1_xk20_7E2
  else if (rlen.gt.6.D02 .and. rlen.le.7.D02) then
   istart = 7113
  !1_xk20_8E2
  else if (rlen.gt.7.D02 .and. rlen.le.8.D02) then
   istart = 7607
  !1_xk20_9E2
  else if (rlen.gt.8.D02 .and. rlen.le.9.D02) then
   istart = 8135
  !1_xk20_1E3
  else if (rlen.gt.9.D02 .and. rlen.le.1.D03) then
   istart = 8475
  !1_xk20_2E3
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
   istart = 8969
  !1_xk20_3E3
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then
   istart = 9651
  !1_xk20_4E3
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then
   istart = 10323
  !1_xk20_5E3
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then
   istart = 10983
  !1_xk20_6E3
  else if (rlen.gt.5.D03 .and. rlen.le.6.D03) then
   istart = 11643
  !1_xk20_7E3
  else if (rlen.gt.6.D03 .and. rlen.le.7.D03) then
   istart = 12149
  !1_xk20_8E3
  else if (rlen.gt.7.D03 .and. rlen.le.8.D03) then
   istart = 12671
  !1_xk20_9E3
  else if (rlen.gt.8.D03 .and. rlen.le.9.D03) then
   istart = 13163
  !1_xk20_1E4
  else if (rlen.gt.9.D03 .and. rlen.le.1.D04) then
   istart = 13517
  !1_xk20_2E4
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then
   istart = 14025
  !1_xk20_3E4
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then
   istart = 14725
  !1_xk20_4E4
  else if (rlen.gt.3.D04 .and. rlen.le.4.D04) then
   istart = 15409
  !1_xk20_5E4
  else if (rlen.gt.4.D04 .and. rlen.le.5.D04) then
   istart = 15905
  !1_xk20_7E4
  else if (rlen.gt.5.D04 .and. rlen.le.7.D04) then
   istart = 16521
  !1_xk20_1E5
  else if (rlen.gt.7.D04 .and. rlen.le.1.D05) then
   istart = 17173
  !1_xk20_2E5
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then
   istart = 18001
  !1_xk20_5E5
  else if (rlen.gt.2.D05 .and. rlen.le.5.D05) then
   istart = 18647
  !1_xk20_1E6
  else if (rlen.gt.5.D05 .and. rlen.le.1.D06) then
   istart = 19841
  !1_xk20_2E6
  else if (rlen.gt.1.D06 .and. rlen.le.2.D06) then
   istart = 20821
  !1_xk20_3E6
  else if (rlen.gt.2.D06 .and. rlen.le.3.D06) then
   istart = 21367
  !1_xk20_4E6
  else if (rlen.gt.3.D06 .and. rlen.le.4.D06) then
   istart = 21883
  !1_xk20_5E6
  else if (rlen.gt.4.D06 .and. rlen.le.5.D06) then
   istart = 22317
  !1_xk20_7E6
  else if (rlen.gt.5.D06 .and. rlen.le.7.D06) then
   istart = 22717
  !1_xk20_1E7
  else if (rlen.gt.7.D06 .and. rlen.le.1.D07) then
   istart = 23221
  !1_xk20_2E7
  else if (rlen.gt.1.D07 .and. rlen.le.2.D07) then
   istart = 23693
  else
   istart = 24069
  end if

!-----------------------------------------------------------------------------!
 case (21)
!-----------------------------------------------------------------------------!
  !1_xk21_9E1
  if      (rlen.gt.1.D00 .and. rlen.le.9.D01) then
   istart = 3703
  !1_xk21_1E2
  else if (rlen.gt.9.D01 .and. rlen.le.1.D02) then
   istart = 3703
  !1_xk21_2E2
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then
   istart = 4159
  !1_xk21_3E2
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
   istart = 4659
  !1_xk21_4E2
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
   istart = 5297
  !1_xk21_5E2
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   istart = 5935
  !1_xk21_6E2
  else if (rlen.gt.5.D02 .and. rlen.le.6.D02) then
   istart = 6625
  !1_xk21_7E2
  else if (rlen.gt.6.D02 .and. rlen.le.7.D02) then
   istart = 7153
  !1_xk21_8E2
  else if (rlen.gt.7.D02 .and. rlen.le.8.D02) then
   istart = 7647
  !1_xk21_9E2
  else if (rlen.gt.8.D02 .and. rlen.le.9.D02) then
   istart = 8175
  !1_xk21_1E3
  else if (rlen.gt.9.D02 .and. rlen.le.1.D03) then
   istart = 8515
  !1_xk21_2E3
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
   istart = 9009
  !1_xk21_3E3
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then
   istart = 9691
  !1_xk21_4E3
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then
   istart = 10363
  !1_xk21_5E3
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then
   istart = 11023
  !1_xk21_6E3
  else if (rlen.gt.5.D03 .and. rlen.le.6.D03) then
   istart = 11683
  !1_xk21_7E3
  else if (rlen.gt.6.D03 .and. rlen.le.7.D03) then
   istart = 12189
  !1_xk21_8E3
  else if (rlen.gt.7.D03 .and. rlen.le.8.D03) then
   istart = 12711
  !1_xk21_9E3
  else if (rlen.gt.8.D03 .and. rlen.le.9.D03) then
   istart = 13203
  !1_xk21_1E4
  else if (rlen.gt.9.D03 .and. rlen.le.1.D04) then
   istart = 13557
  !1_xk21_2E4
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then
   istart = 14065
  !1_xk21_3E4
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then
   istart = 14765
  !1_xk21_4E4
  else if (rlen.gt.3.D04 .and. rlen.le.4.D04) then
   istart = 15449
  !1_xk21_5E4
  else if (rlen.gt.4.D04 .and. rlen.le.5.D04) then
   istart = 15945
  !1_xk21_7E4
  else if (rlen.gt.5.D04 .and. rlen.le.7.D04) then
   istart = 16561
  !1_xk21_1E5
  else if (rlen.gt.7.D04 .and. rlen.le.1.D05) then
   istart = 17213
  !1_xk21_2E5
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then
   istart = 18041
  !1_xk21_5E5
  else if (rlen.gt.2.D05 .and. rlen.le.5.D05) then
   istart = 18687
  !1_xk21_1E6
  else if (rlen.gt.5.D05 .and. rlen.le.1.D06) then
   istart = 19881
  !1_xk21_2E6
  else if (rlen.gt.1.D06 .and. rlen.le.2.D06) then
   istart = 20861
  !1_xk21_3E6
  else if (rlen.gt.2.D06 .and. rlen.le.3.D06) then
   istart = 21407
  !1_xk21_4E6
  else if (rlen.gt.3.D06 .and. rlen.le.4.D06) then
   istart = 21923
  !1_xk21_5E6
  else if (rlen.gt.4.D06 .and. rlen.le.5.D06) then
   istart = 22357
  !1_xk21_7E6
  else if (rlen.gt.5.D06 .and. rlen.le.7.D06) then
   istart = 22757
  !1_xk21_1E7
  else if (rlen.gt.7.D06 .and. rlen.le.1.D07) then
   istart = 23261
  !1_xk21_2E7
  else if (rlen.gt.1.D07 .and. rlen.le.2.D07) then
   istart = 23733
  !1_xk21_3E7
  else if (rlen.gt.2.D07 .and. rlen.le.3.D07) then
   istart = 24109
  else
   istart = 24395
  end if

!-----------------------------------------------------------------------------!
 case (22)
!-----------------------------------------------------------------------------!
  !1_xk22_1E2
  if      (rlen.gt.1.D00 .and. rlen.le.1.D02) then
   istart = 4201
  !1_xk22_2E2
  else if (rlen.gt.1.D02 .and. rlen.le.2.D02) then
   istart = 4201
  !1_xk22_3E2
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
   istart = 4701
  !1_xk22_4E2
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
   istart = 5339
  !1_xk22_5E2
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   istart = 5977
  !1_xk22_7E2
  else if (rlen.gt.5.D02 .and. rlen.le.7.D02) then
   istart = 6667
  !1_xk22_1E3
  else if (rlen.gt.7.D02 .and. rlen.le.1.D03) then
   istart = 7689
  !1_xk22_2E3
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
   istart = 9051
  !1_xk22_3E3
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then
   istart = 9733
  !1_xk22_4E3
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then
   istart = 10405
  !1_xk22_5E3
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then
   istart = 11065
  !1_xk22_7E3
  else if (rlen.gt.5.D03 .and. rlen.le.7.D03) then
   istart = 11725
  !1_xk22_1E4
  else if (rlen.gt.7.D03 .and. rlen.le.1.D04) then
   istart = 12753
  !1_xk22_2E4
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then
   istart = 14107
  !1_xk22_3E4
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then
   istart = 14807
  !1_xk22_4E4
  else if (rlen.gt.3.D04 .and. rlen.le.4.D04) then
   istart = 15491
  !1_xk22_5E4
  else if (rlen.gt.4.D04 .and. rlen.le.5.D04) then
   istart = 15987
  !1_xk22_7E4
  else if (rlen.gt.5.D04 .and. rlen.le.7.D04) then
   istart = 16603
  !1_xk22_1E5
  else if (rlen.gt.7.D04 .and. rlen.le.1.D05) then
   istart = 17255
  !1_xk22_2E5
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then
   istart = 18083
  !1_xk22_3E5
  else if (rlen.gt.2.D05 .and. rlen.le.3.D05) then
   istart = 18729
  !1_xk22_4E5
  else if (rlen.gt.3.D05 .and. rlen.le.4.D05) then
   istart = 19101
  !1_xk22_5E5
  else if (rlen.gt.4.D05 .and. rlen.le.5.D05) then
   istart = 19399
  !1_xk22_7E5
  else if (rlen.gt.5.D05 .and. rlen.le.7.D05) then
   istart = 19923
  !1_xk22_1E6
  else if (rlen.gt.7.D05 .and. rlen.le.1.D06) then
   istart = 20299
  !1_xk22_2E6
  else if (rlen.gt.1.D06 .and. rlen.le.2.D06) then
   istart = 20903
  !1_xk22_3E6
  else if (rlen.gt.2.D06 .and. rlen.le.3.D06) then
   istart = 21449
  !1_xk22_4E6
  else if (rlen.gt.3.D06 .and. rlen.le.4.D06) then
   istart = 21965
  !1_xk22_5E6
  else if (rlen.gt.4.D06 .and. rlen.le.5.D06) then
   istart = 22399
  !1_xk22_7E6
  else if (rlen.gt.5.D06 .and. rlen.le.7.D06) then
   istart = 22799
  !1_xk22_1E7
  else if (rlen.gt.7.D06 .and. rlen.le.1.D07) then
   istart = 23303
  !1_xk22_2E7
  else if (rlen.gt.1.D07 .and. rlen.le.2.D07) then
   istart = 23775
  !1_xk22_3E7
  else if (rlen.gt.2.D07 .and. rlen.le.3.D07) then
   istart = 24151
  !1_xk22_4E7
  else if (rlen.gt.3.D07 .and. rlen.le.4.D07) then
   istart = 24437
  else
   istart = 24681
  end if

!-----------------------------------------------------------------------------!
 case (23)
!-----------------------------------------------------------------------------!
  !1_xk23_2E2
  if      (rlen.gt.1.D00 .and. rlen.le.2.D02) then
   istart = 4745
  !1_xk23_3E2
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
   istart = 4745
  !1_xk23_4E2
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
   istart = 5383
  !1_xk23_5E2
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   istart = 6021
  !1_xk23_7E2
  else if (rlen.gt.5.D02 .and. rlen.le.7.D02) then
   istart = 6711
  !1_xk23_1E3
  else if (rlen.gt.7.D02 .and. rlen.le.1.D03) then
   istart = 7733
  !1_xk23_2E3
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
   istart = 9095
  !1_xk23_3E3
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then
   istart = 9777
  !1_xk23_4E3
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then
   istart = 10449
  !1_xk23_5E3
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then
   istart = 11109
  !1_xk23_7E3
  else if (rlen.gt.5.D03 .and. rlen.le.7.D03) then
   istart = 11769
  !1_xk23_1E4
  else if (rlen.gt.7.D03 .and. rlen.le.1.D04) then
   istart = 12797
  !1_xk23_2E4
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then
   istart = 14151
  !1_xk23_3E4
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then
   istart = 14851
  !1_xk23_4E4
  else if (rlen.gt.3.D04 .and. rlen.le.4.D04) then
   istart = 15535
  !1_xk23_5E4
  else if (rlen.gt.4.D04 .and. rlen.le.5.D04) then
   istart = 16031
  !1_xk23_7E4
  else if (rlen.gt.5.D04 .and. rlen.le.7.D04) then
   istart = 16647
  !1_xk23_1E5
  else if (rlen.gt.7.D04 .and. rlen.le.1.D05) then
   istart = 17299
  !1_xk23_2E5
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then
   istart = 18127
  !1_xk23_3E5
  else if (rlen.gt.2.D05 .and. rlen.le.3.D05) then
   istart = 18773
  !1_xk23_4E5
  else if (rlen.gt.3.D05 .and. rlen.le.4.D05) then
   istart = 19145
  !1_xk23_5E5
  else if (rlen.gt.4.D05 .and. rlen.le.5.D05) then
   istart = 19443
  !1_xk23_7E5
  else if (rlen.gt.5.D05 .and. rlen.le.7.D05) then
   istart = 19967
  !1_xk23_1E6
  else if (rlen.gt.7.D05 .and. rlen.le.1.D06) then
   istart = 20343
  !1_xk23_2E6
  else if (rlen.gt.1.D06 .and. rlen.le.2.D06) then
   istart = 20947
  !1_xk23_3E6
  else if (rlen.gt.2.D06 .and. rlen.le.3.D06) then
   istart = 21493
  !1_xk23_4E6
  else if (rlen.gt.3.D06 .and. rlen.le.4.D06) then
   istart = 22009
  !1_xk23_5E6
  else if (rlen.gt.4.D06 .and. rlen.le.5.D06) then
   istart = 22443
  !1_xk23_7E6
  else if (rlen.gt.5.D06 .and. rlen.le.7.D06) then
   istart = 22843
  !1_xk23_1E7
  else if (rlen.gt.7.D06 .and. rlen.le.1.D07) then
   istart = 23347
  !1_xk23_2E7
  else if (rlen.gt.1.D07 .and. rlen.le.2.D07) then
   istart = 23819
  !1_xk23_3E7
  else if (rlen.gt.2.D07 .and. rlen.le.3.D07) then
   istart = 24195
  !1_xk23_4E7
  else if (rlen.gt.3.D07 .and. rlen.le.4.D07) then
   istart = 24481
  !1_xk23_5E7
  else if (rlen.gt.4.D07 .and. rlen.le.5.D07) then
   istart = 24725
  !1_xk23_7E7
  else if (rlen.gt.5.D07 .and. rlen.le.7.D07) then
   istart = 24925
  else
   istart = 25175
  end if

!-----------------------------------------------------------------------------!
 case (24)
!-----------------------------------------------------------------------------!
  !1_xk24_2E2
  if      (rlen.gt.1.D00 .and. rlen.le.2.D02) then
   istart = 4791
  !1_xk24_3E2
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
   istart = 4791
  !1_xk24_4E2
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
   istart = 5429
  !1_xk24_5E2
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   istart = 6067
  !1_xk24_7E2
  else if (rlen.gt.5.D02 .and. rlen.le.7.D02) then
   istart = 6757
  !1_xk24_1E3
  else if (rlen.gt.7.D02 .and. rlen.le.1.D03) then
   istart = 7779
  !1_xk24_2E3
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
   istart = 9141
  !1_xk24_3E3
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then
   istart = 9823
  !1_xk24_4E3
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then
   istart = 10495
  !1_xk24_5E3
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then
   istart = 11155
  !1_xk24_7E3
  else if (rlen.gt.5.D03 .and. rlen.le.7.D03) then
   istart = 11815
  !1_xk24_1E4
  else if (rlen.gt.7.D03 .and. rlen.le.1.D04) then
   istart = 12843
  !1_xk24_2E4
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then
   istart = 14197
  !1_xk24_3E4
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then
   istart = 14897
  !1_xk24_4E4
  else if (rlen.gt.3.D04 .and. rlen.le.4.D04) then
   istart = 15581
  !1_xk24_5E4
  else if (rlen.gt.4.D04 .and. rlen.le.5.D04) then
   istart = 16077
  !1_xk24_7E4
  else if (rlen.gt.5.D04 .and. rlen.le.7.D04) then
   istart = 16693
  !1_xk24_1E5
  else if (rlen.gt.7.D04 .and. rlen.le.1.D05) then
   istart = 17345
  !1_xk24_2E5
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then
   istart = 18173
  !1_xk24_3E5
  else if (rlen.gt.2.D05 .and. rlen.le.3.D05) then
   istart = 18819
  !1_xk24_4E5
  else if (rlen.gt.3.D05 .and. rlen.le.4.D05) then
   istart = 19191
  !1_xk24_5E5
  else if (rlen.gt.4.D05 .and. rlen.le.5.D05) then
   istart = 19489
  !1_xk24_7E5
  else if (rlen.gt.5.D05 .and. rlen.le.7.D05) then
   istart = 20013
  !1_xk24_1E6
  else if (rlen.gt.7.D05 .and. rlen.le.1.D06) then
   istart = 20389
  !1_xk24_2E6
  else if (rlen.gt.1.D06 .and. rlen.le.2.D06) then
   istart = 20993
  !1_xk24_3E6
  else if (rlen.gt.2.D06 .and. rlen.le.3.D06) then
   istart = 21539
  !1_xk24_4E6
  else if (rlen.gt.3.D06 .and. rlen.le.4.D06) then
   istart = 22055
  !1_xk24_5E6
  else if (rlen.gt.4.D06 .and. rlen.le.5.D06) then
   istart = 22489
  !1_xk24_7E6
  else if (rlen.gt.5.D06 .and. rlen.le.7.D06) then
   istart = 22889
  !1_xk24_1E7
  else if (rlen.gt.7.D06 .and. rlen.le.1.D07) then
   istart = 23393
  !1_xk24_2E7
  else if (rlen.gt.1.D07 .and. rlen.le.2.D07) then
   istart = 23865
  !1_xk24_3E7
  else if (rlen.gt.2.D07 .and. rlen.le.3.D07) then
   istart = 24241
  !1_xk24_4E7
  else if (rlen.gt.3.D07 .and. rlen.le.4.D07) then
   istart = 24527
  !1_xk24_5E7
  else if (rlen.gt.4.D07 .and. rlen.le.5.D07) then
   istart = 24771
  !1_xk24_7E7
  else if (rlen.gt.5.D07 .and. rlen.le.7.D07) then
   istart = 24971
  !1_xk24_1E8
  else if (rlen.gt.7.D07 .and. rlen.le.1.D08) then
   istart = 25221
  else
   istart = 25375
  end if

!-----------------------------------------------------------------------------!
 case (25)
!-----------------------------------------------------------------------------!
  !1_xk25_2E2
  if      (rlen.gt.1.D00 .and. rlen.le.2.D02) then
   istart = 4839
  !1_xk25_3E2
  else if (rlen.gt.2.D02 .and. rlen.le.3.D02) then
   istart = 4839
  !1_xk25_4E2
  else if (rlen.gt.3.D02 .and. rlen.le.4.D02) then
   istart = 5477
  !1_xk25_5E2
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   istart = 6115
  !1_xk25_7E2
  else if (rlen.gt.5.D02 .and. rlen.le.7.D02) then
   istart = 6805
  !1_xk25_1E3
  else if (rlen.gt.7.D02 .and. rlen.le.1.D03) then
   istart = 7827
  !1_xk25_2E3
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
   istart = 9189
  !1_xk25_3E3
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then
   istart = 9871
  !1_xk25_4E3
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then
   istart = 10543
  !1_xk25_5E3
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then
   istart = 11203
  !1_xk25_6E3
  else if (rlen.gt.5.D03 .and. rlen.le.6.D03) then
   istart = 11863
  !1_xk25_7E3
  else if (rlen.gt.6.D03 .and. rlen.le.7.D03) then
   istart = 12231
  !1_xk25_8E3
  else if (rlen.gt.7.D03 .and. rlen.le.8.D03) then
   istart = 12891
  !1_xk25_9E3
  else if (rlen.gt.8.D03 .and. rlen.le.9.D03) then
   istart = 13245
  !1_xk25_1E4
  else if (rlen.gt.9.D03 .and. rlen.le.1.D04) then
   istart = 13599
  !1_xk25_2E4
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then
   istart = 14245
  !1_xk25_5E4
  else if (rlen.gt.2.D04 .and. rlen.le.5.D04) then
   istart = 14945
  !1_xk25_1E5
  else if (rlen.gt.5.D04 .and. rlen.le.1.D05) then
   istart = 16741
  !1_xk25_2E5
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then
   istart = 18221
  !1_xk25_5E5
  else if (rlen.gt.2.D05 .and. rlen.le.5.D05) then
   istart = 18867
  !1_xk25_1E6
  else if (rlen.gt.5.D05 .and. rlen.le.1.D06) then
   istart = 20061
  !1_xk25_2E6
  else if (rlen.gt.1.D06 .and. rlen.le.2.D06) then
   istart = 21041
  !1_xk25_5E6
  else if (rlen.gt.2.D06 .and. rlen.le.5.D06) then
   istart = 21587
  !1_xk25_1E7
  else if (rlen.gt.5.D06 .and. rlen.le.1.D07) then
   istart = 22937
  !1_xk25_5E7
  else if (rlen.gt.1.D07 .and. rlen.le.5.D07) then
   istart = 23913
  !1_xk25_1E8
  else if (rlen.gt.5.D07 .and. rlen.le.1.D08) then
   istart = 25019
  !1_xk25_2E8
  else if (rlen.gt.1.D08 .and. rlen.le.2.D08) then
   istart = 25423
  else
   istart = 25635
  end if

!-----------------------------------------------------------------------------!
 case (26)
!-----------------------------------------------------------------------------!
  !1_xk26_4E2
  if      (rlen.gt.1.D00 .and. rlen.le.4.D02) then
   istart = 6165
  !1_xk26_5E2
  else if (rlen.gt.4.D02 .and. rlen.le.5.D02) then
   istart = 6165
  !1_xk26_6E2
  else if (rlen.gt.5.D02 .and. rlen.le.6.D02) then
   istart = 6855
  !1_xk26_7E2
  else if (rlen.gt.6.D02 .and. rlen.le.7.D02) then
   istart = 7195
  !1_xk26_8E2
  else if (rlen.gt.7.D02 .and. rlen.le.8.D02) then
   istart = 7877
  !1_xk26_9E2
  else if (rlen.gt.8.D02 .and. rlen.le.9.D02) then
   istart = 8217
  !1_xk26_1E3
  else if (rlen.gt.9.D02 .and. rlen.le.1.D03) then
   istart = 8557
  !1_xk26_2E3
  else if (rlen.gt.1.D03 .and. rlen.le.2.D03) then
   istart = 9239
  !1_xk26_3E3
  else if (rlen.gt.2.D03 .and. rlen.le.3.D03) then
   istart = 9921
  !1_xk26_4E3
  else if (rlen.gt.3.D03 .and. rlen.le.4.D03) then
   istart = 10593
  !1_xk26_5E3
  else if (rlen.gt.4.D03 .and. rlen.le.5.D03) then
   istart = 11253
  !1_xk26_6E3
  else if (rlen.gt.5.D03 .and. rlen.le.6.D03) then
   istart = 11913
  !1_xk26_7E3
  else if (rlen.gt.6.D03 .and. rlen.le.7.D03) then
   istart = 12281
  !1_xk26_8E3
  else if (rlen.gt.7.D03 .and. rlen.le.8.D03) then
   istart = 12941
  !1_xk26_9E3
  else if (rlen.gt.8.D03 .and. rlen.le.9.D03) then
   istart = 13295
  !1_xk26_1E4
  else if (rlen.gt.9.D03 .and. rlen.le.1.D04) then
   istart = 13649
  !1_xk26_2E4
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then
   istart = 14295
  !1_xk26_3E4
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then
   istart = 14995
  !1_xk26_4E4
  else if (rlen.gt.3.D04 .and. rlen.le.4.D04) then
   istart = 15629
  !1_xk26_5E4
  else if (rlen.gt.4.D04 .and. rlen.le.5.D04) then
   istart = 16125
  !1_xk26_6E4
  else if (rlen.gt.5.D04 .and. rlen.le.6.D04) then
   istart = 16791
  !1_xk26_7E4
  else if (rlen.gt.6.D04 .and. rlen.le.7.D04) then
   istart = 16897
  !1_xk26_8E4
  else if (rlen.gt.7.D04 .and. rlen.le.8.D04) then
   istart = 17393
  !1_xk26_9E4
  else if (rlen.gt.8.D04 .and. rlen.le.9.D04) then
   istart = 17499
  !1_xk26_1E5
  else if (rlen.gt.9.D04 .and. rlen.le.1.D05) then
   istart = 17605
  !1_xk26_2E5
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then
   istart = 18271
  !1_xk26_3E5
  else if (rlen.gt.2.D05 .and. rlen.le.3.D05) then
   istart = 18917
  !1_xk26_4E5
  else if (rlen.gt.3.D05 .and. rlen.le.4.D05) then
   istart = 19239
  !1_xk26_5E5
  else if (rlen.gt.4.D05 .and. rlen.le.5.D05) then
   istart = 19537
  !1_xk26_7E5
  else if (rlen.gt.5.D05 .and. rlen.le.7.D05) then
   istart = 20111
  !1_xk26_1E6
  else if (rlen.gt.7.D05 .and. rlen.le.1.D06) then
   istart = 20437
  !1_xk26_2E6
  else if (rlen.gt.1.D06 .and. rlen.le.2.D06) then
   istart = 21091
  !1_xk26_3E6
  else if (rlen.gt.2.D06 .and. rlen.le.3.D06) then
   istart = 21637
  !1_xk26_4E6
  else if (rlen.gt.3.D06 .and. rlen.le.4.D06) then
   istart = 22103
  !1_xk26_5E6
  else if (rlen.gt.4.D06 .and. rlen.le.5.D06) then
   istart = 22537
  !1_xk26_7E6
  else if (rlen.gt.5.D06 .and. rlen.le.7.D06) then
   istart = 22987
  !1_xk26_1E7
  else if (rlen.gt.7.D06 .and. rlen.le.1.D07) then
   istart = 23441
  !1_xk26_2E7
  else if (rlen.gt.1.D07 .and. rlen.le.2.D07) then
   istart = 23963
  !1_xk26_3E7
  else if (rlen.gt.2.D07 .and. rlen.le.3.D07) then
   istart = 24289
  !1_xk26_4E7
  else if (rlen.gt.3.D07 .and. rlen.le.4.D07) then
   istart = 24575
  !1_xk26_5E7
  else if (rlen.gt.4.D07 .and. rlen.le.5.D07) then
   istart = 24819
  !1_xk26_7E7
  else if (rlen.gt.5.D07 .and. rlen.le.7.D07) then
   istart = 25069
  !1_xk26_1E8
  else if (rlen.gt.7.D07 .and. rlen.le.1.D08) then
   istart = 25269
  !1_xk26_2E8
  else if (rlen.gt.1.D08 .and. rlen.le.2.D08) then
   istart = 25473
  !1_xk26_3E8
  else if (rlen.gt.2.D08 .and. rlen.le.3.D08) then
   istart = 25685
  else
   istart = 25947
  end if

!-----------------------------------------------------------------------------!
 case (27)
!-----------------------------------------------------------------------------!
  !1_xk27_1E4
  if      (rlen.gt.1.D00 .and. rlen.le.1.D04) then
   istart = 14347
  !1_xk27_2E4
  else if (rlen.gt.1.D04 .and. rlen.le.2.D04) then
   istart = 14347
  !1_xk27_3E4
  else if (rlen.gt.2.D04 .and. rlen.le.3.D04) then
   istart = 15047
  !1_xk27_4E4
  else if (rlen.gt.3.D04 .and. rlen.le.4.D04) then
   istart = 15681
  !1_xk27_5E4
  else if (rlen.gt.4.D04 .and. rlen.le.5.D04) then
   istart = 16177
  !1_xk27_6E4
  else if (rlen.gt.5.D04 .and. rlen.le.6.D04) then
   istart = 16843
  !1_xk27_7E4
  else if (rlen.gt.6.D04 .and. rlen.le.7.D04) then
   istart = 16949
  !1_xk27_8E4
  else if (rlen.gt.7.D04 .and. rlen.le.8.D04) then
   istart = 17445
  !1_xk27_9E4
  else if (rlen.gt.8.D04 .and. rlen.le.9.D04) then
   istart = 17551
  !1_xk27_1E5
  else if (rlen.gt.9.D04 .and. rlen.le.1.D05) then
   istart = 17657
  !1_xk27_2E5
  else if (rlen.gt.1.D05 .and. rlen.le.2.D05) then
   istart = 18323
  !1_xk27_3E5
  else if (rlen.gt.2.D05 .and. rlen.le.3.D05) then
   istart = 18969
  !1_xk27_4E5
  else if (rlen.gt.3.D05 .and. rlen.le.4.D05) then
   istart = 19291
  !1_xk27_5E5
  else if (rlen.gt.4.D05 .and. rlen.le.5.D05) then
   istart = 19589
  !1_xk27_6E5
  else if (rlen.gt.5.D05 .and. rlen.le.6.D05) then
   istart = 20163
  !1_xk27_7E5
  else if (rlen.gt.6.D05 .and. rlen.le.7.D05) then
   istart = 20217
  !1_xk27_8E5
  else if (rlen.gt.7.D05 .and. rlen.le.8.D05) then
   istart = 20489
  !1_xk27_9E5
  else if (rlen.gt.8.D05 .and. rlen.le.9.D05) then
   istart = 20543
  !1_xk27_1E6
  else if (rlen.gt.9.D05 .and. rlen.le.1.D06) then
   istart = 20597
  !1_xk27_2E6
  else if (rlen.gt.1.D06 .and. rlen.le.2.D06) then
   istart = 21143
  !1_xk27_3E6
  else if (rlen.gt.2.D06 .and. rlen.le.3.D06) then
   istart = 21689
  !1_xk27_4E6
  else if (rlen.gt.3.D06 .and. rlen.le.4.D06) then
   istart = 22155
  !1_xk27_5E6
  else if (rlen.gt.4.D06 .and. rlen.le.5.D06) then
   istart = 22589
  !1_xk27_6E6
  else if (rlen.gt.5.D06 .and. rlen.le.6.D06) then
   istart = 23039
  !1_xk27_7E6
  else if (rlen.gt.6.D06 .and. rlen.le.7.D06) then
   istart = 23093
  !1_xk27_8E6
  else if (rlen.gt.7.D06 .and. rlen.le.8.D06) then
   istart = 23493
  !1_xk27_9E6
  else if (rlen.gt.8.D06 .and. rlen.le.9.D06) then
   istart = 23547
  !1_xk27_1E7
  else if (rlen.gt.9.D06 .and. rlen.le.1.D07) then
   istart = 23601
  !1_xk27_2E7
  else if (rlen.gt.1.D07 .and. rlen.le.2.D07) then
   istart = 24015
  !1_xk27_3E7
  else if (rlen.gt.2.D07 .and. rlen.le.3.D07) then
   istart = 24341
  !1_xk27_4E7
  else if (rlen.gt.3.D07 .and. rlen.le.4.D07) then
   istart = 24627
  !1_xk27_5E7
  else if (rlen.gt.4.D07 .and. rlen.le.5.D07) then
   istart = 24871
  !1_xk27_7E7
  else if (rlen.gt.5.D07 .and. rlen.le.7.D07) then
   istart = 25121
  !1_xk27_1E8
  else if (rlen.gt.7.D07 .and. rlen.le.1.D08) then
   istart = 25321
  !1_xk27_2E8
  else if (rlen.gt.1.D08 .and. rlen.le.2.D08) then
   istart = 25525
  !1_xk27_3E8
  else if (rlen.gt.2.D08 .and. rlen.le.3.D08) then
   istart = 25737
  !1_xk27_4E8
  else if (rlen.gt.3.D08 .and. rlen.le.4.D08) then
   istart = 25999
  else
   istart = 26209
  end if

!-----------------------------------------------------------------------------!
 case (28)
!-----------------------------------------------------------------------------!
  !1_xk28_1E8
  if      (rlen.gt.1.D00 .and. rlen.le.1.D08) then
   istart = 25579
  !1_xk28_2E8
  else if (rlen.gt.1.D08 .and. rlen.le.2.D08) then
   istart = 25579
  !1_xk28_3E8
  else if (rlen.gt.2.D08 .and. rlen.le.3.D08) then
   istart = 25791
  !1_xk28_4E8
  else if (rlen.gt.3.D08 .and. rlen.le.4.D08) then
   istart = 26053
  !1_xk28_7E8
  else if (rlen.gt.4.D08 .and. rlen.le.7.D08) then
   istart = 26263
  else
   istart = 26519
  end if

!-----------------------------------------------------------------------------!
 case (29)
!-----------------------------------------------------------------------------!
  !1_xk29_7E8
  if      (rlen.gt.1.D00 .and. rlen.le.7.D08) then
   istart = 26575
  !1_xk29_1E9
  else if (rlen.gt.7.D08 .and. rlen.le.1.D09) then
   istart = 26575
  else
   istart = 27123
  end if

!-----------------------------------------------------------------------------!
 case (30)
!-----------------------------------------------------------------------------!
  !1_xk30_7E8
  if      (rlen.gt.1.D00 .and. rlen.le.7.D08) then
   istart = 26633
  !1_xk30_1E9
  else if (rlen.gt.7.D08 .and. rlen.le.1.D09) then
   istart = 26633
  !1_xk30_2E9
  else if (rlen.gt.1.D09 .and. rlen.le.2.D09) then
   istart = 27181
  else
   istart = 27671
  end if

!-----------------------------------------------------------------------------!
 case (31)
!-----------------------------------------------------------------------------!
  !1_xk31_7E8
  if      (rlen.gt.1.D00 .and. rlen.le.7.D08) then
   istart = 26693
  !1_xk31_1E9
  else if (rlen.gt.7.D08 .and. rlen.le.1.D09) then
   istart = 26693
  !1_xk31_2E9
  else if (rlen.gt.1.D09 .and. rlen.le.2.D09) then
   istart = 27241
  else
   istart = 27731
  end if

!-----------------------------------------------------------------------------!
 case (32)
!-----------------------------------------------------------------------------!
  !1_xk32_7E8
  if      (rlen.gt.1.D00 .and. rlen.le.7.D08) then
   istart = 26755
  !1_xk32_1E9
  else if (rlen.gt.7.D08 .and. rlen.le.1.D09) then
   istart = 26755
  !1_xk32_2E9
  else if (rlen.gt.1.D09 .and. rlen.le.2.D09) then
   istart = 27303
  !1_xk32_3E9
  else if (rlen.gt.2.D09 .and. rlen.le.3.D09) then
   istart = 27793
  else
   istart = 28161
  end if

!-----------------------------------------------------------------------------!
 case (33)
!-----------------------------------------------------------------------------!
  !1_xk33_7E8
  if      (rlen.gt.1.D00 .and. rlen.le.7.D08) then
   istart = 26819
  !1_xk33_1E9
  else if (rlen.gt.7.D08 .and. rlen.le.1.D09) then
   istart = 26819
  !1_xk33_2E9
  else if (rlen.gt.1.D09 .and. rlen.le.2.D09) then
   istart = 27367
  !1_xk33_3E9
  else if (rlen.gt.2.D09 .and. rlen.le.3.D09) then
   istart = 27857
  !1_xk33_4E9
  else if (rlen.gt.3.D09 .and. rlen.le.4.D09) then
   istart = 28225
  else
   istart = 28529
  end if

!-----------------------------------------------------------------------------!
 case (34)
!-----------------------------------------------------------------------------!
  !1_xk34_7E8
  if      (rlen.gt.1.D00 .and. rlen.le.7.D08) then
   istart = 26885
  !1_xk34_1E9
  else if (rlen.gt.7.D08 .and. rlen.le.1.D09) then
   istart = 26885
  !1_xk34_2E9
  else if (rlen.gt.1.D09 .and. rlen.le.2.D09) then
   istart = 27433
  !1_xk34_3E9
  else if (rlen.gt.2.D09 .and. rlen.le.3.D09) then
   istart = 27923
  !1_xk34_4E9
  else if (rlen.gt.3.D09 .and. rlen.le.4.D09) then
   istart = 28291
  !1_xk34_5E9
  else if (rlen.gt.4.D09 .and. rlen.le.5.D09) then
   istart = 28595
  !1_xk34_7E9
  else if (rlen.gt.5.D09 .and. rlen.le.7.D09) then
   istart = 28763
  else
   istart = 29101
  end if

!-----------------------------------------------------------------------------!
 case (35)
!-----------------------------------------------------------------------------!
  !1_xk35_7E8
  if      (rlen.gt.1.D00 .and. rlen.le.7.D08) then
   istart = 26953
  !1_xk35_1E9
  else if (rlen.gt.7.D08 .and. rlen.le.1.D09) then
   istart = 26953
  !1_xk35_2E9
  else if (rlen.gt.1.D09 .and. rlen.le.2.D09) then
   istart = 27501
  !1_xk35_3E9
  else if (rlen.gt.2.D09 .and. rlen.le.3.D09) then
   istart = 27991
  !1_xk35_5E9
  else if (rlen.gt.3.D09 .and. rlen.le.5.D09) then
   istart = 28359
  !1_xk35_1E10
  else if (rlen.gt.5.D09 .and. rlen.le.1.D10) then
   istart = 28831
  else
   istart = 29269
  end if

!-----------------------------------------------------------------------------!
 case (36)
!-----------------------------------------------------------------------------!
  !1_xk36_1E10
  if      (rlen.gt.1.D00 .and. rlen.le.1.D10) then
   istart = 29339
  !1_xk36_2E10
  else if (rlen.gt.1.D10 .and. rlen.le.2.D10) then
   istart = 29339
  else
   istart = 29985
  end if

!-----------------------------------------------------------------------------!
 case (37)
!-----------------------------------------------------------------------------!
  !1_xk37_1E10
  if      (rlen.gt.1.D00 .and. rlen.le.1.D10) then
   istart = 29411
  !1_xk37_2E10
  else if (rlen.gt.1.D10 .and. rlen.le.2.D10) then
   istart = 29411
  else
   istart = 30057
  end if

!-----------------------------------------------------------------------------!
 case (38)
!-----------------------------------------------------------------------------!
  !1_xk38_1E10
  if      (rlen.gt.1.D00 .and. rlen.le.1.D10) then
   istart = 29485
  !1_xk38_3E10
  else if (rlen.gt.1.D10 .and. rlen.le.3.D10) then
   istart = 29485
  else
   istart = 30555
  end if

!-----------------------------------------------------------------------------!
 case (39)
!-----------------------------------------------------------------------------!
  !1_xk39_1E10
  if      (rlen.gt.1.D00 .and. rlen.le.1.D10) then
   istart = 29561
  !1_xk39_2E10
  else if (rlen.gt.1.D10 .and. rlen.le.2.D10) then
   istart = 29561
  !1_xk39_4E10
  else if (rlen.gt.2.D10 .and. rlen.le.4.D10) then
   istart = 30131
  else
   istart = 30813
  end if

!-----------------------------------------------------------------------------!
 case (40)
!-----------------------------------------------------------------------------!
  !1_xk40_1E10
  if      (rlen.gt.1.D00 .and. rlen.le.1.D10) then
   istart = 29639
  !1_xk40_2E10
  else if (rlen.gt.1.D10 .and. rlen.le.2.D10) then
   istart = 29639
  !1_xk40_5E10
  else if (rlen.gt.2.D10 .and. rlen.le.5.D10) then
   istart = 30209
  else
   istart = 30991
  end if

!-----------------------------------------------------------------------------!
 case (41)
!-----------------------------------------------------------------------------!
  !1_xk41_1E10
  if      (rlen.gt.1.D00 .and. rlen.le.1.D10) then
   istart = 29719
  !1_xk41_2E10
  else if (rlen.gt.1.D10 .and. rlen.le.2.D10) then
   istart = 29719
  !1_xk41_3E10
  else if (rlen.gt.2.D10 .and. rlen.le.3.D10) then
   istart = 30289
  !1_xk41_7E10
  else if (rlen.gt.3.D10 .and. rlen.le.7.D10) then
   istart = 30631
  else
   istart = 31255
  end if

!-----------------------------------------------------------------------------!
 case (42)
!-----------------------------------------------------------------------------!
  !1_xk42_1E10
  if      (rlen.gt.1.D00 .and. rlen.le.1.D10) then
   istart = 29801
  !1_xk42_2E10
  else if (rlen.gt.1.D10 .and. rlen.le.2.D10) then
   istart = 29801
  !1_xk42_5E10
  else if (rlen.gt.2.D10 .and. rlen.le.5.D10) then
   istart = 30371
  !1_xk42_1E11
  else if (rlen.gt.5.D10 .and. rlen.le.1.D11) then
   istart = 31071
  else
   istart = 31437
  end if

!-----------------------------------------------------------------------------!
 case (43)
!-----------------------------------------------------------------------------!
  !1_xk43_1E11
  if      (rlen.gt.1.D00 .and. rlen.le.1.D11) then
   istart = 31521
  !1_xk43_2E11
  else if (rlen.gt.1.D11 .and. rlen.le.2.D11) then
   istart = 31521
  else
   istart = 32265
  end if

!-----------------------------------------------------------------------------!
 case (44)
!-----------------------------------------------------------------------------!
  !1_xk44_1E11
  if      (rlen.gt.1.D00 .and. rlen.le.1.D11) then
   istart = 31607
  !1_xk44_2E11
  else if (rlen.gt.1.D11 .and. rlen.le.2.D11) then
   istart = 31607
  else
   istart = 32351
  end if

!-----------------------------------------------------------------------------!
 case (45)
!-----------------------------------------------------------------------------!
  !1_xk45_1E11
  if      (rlen.gt.1.D00 .and. rlen.le.1.D11) then
   istart = 31695
  !1_xk45_3E11
  else if (rlen.gt.1.D11 .and. rlen.le.3.D11) then
   istart = 31695
  else
   istart = 32825
  end if

!-----------------------------------------------------------------------------!
 case (46)
!-----------------------------------------------------------------------------!
  !1_xk46_1E11
  if      (rlen.gt.1.D00 .and. rlen.le.1.D11) then
   istart = 31785
  !1_xk46_2E11
  else if (rlen.gt.1.D11 .and. rlen.le.2.D11) then
   istart = 31785
  !1_xk46_4E11
  else if (rlen.gt.2.D11 .and. rlen.le.4.D11) then
   istart = 32439
  else
   istart = 33209
  end if

!-----------------------------------------------------------------------------!
 case (47)
!-----------------------------------------------------------------------------!
  !1_xk47_1E11
  if      (rlen.gt.1.D00 .and. rlen.le.1.D11) then
   istart = 31877
  !1_xk47_5E11
  else if (rlen.gt.1.D11 .and. rlen.le.5.D11) then
   istart = 31877
  else
   istart = 33507
  end if

!-----------------------------------------------------------------------------!
 case (48)
!-----------------------------------------------------------------------------!
  !1_xk48_1E11
  if      (rlen.gt.1.D00 .and. rlen.le.1.D11) then
   istart = 31971
  !1_xk48_2E11
  else if (rlen.gt.1.D11 .and. rlen.le.2.D11) then
   istart = 31971
  !1_xk48_3E11
  else if (rlen.gt.2.D11 .and. rlen.le.3.D11) then
   istart = 32531
  !1_xk48_7E11
  else if (rlen.gt.3.D11 .and. rlen.le.7.D11) then
   istart = 32915
  else
   istart = 33799
  end if

!-----------------------------------------------------------------------------!
 case (49)
!-----------------------------------------------------------------------------!
  !1_xk49_1E11
  if      (rlen.gt.1.D00 .and. rlen.le.1.D11) then
   istart = 32067
  !1_xk49_2E11
  else if (rlen.gt.1.D11 .and. rlen.le.2.D11) then
   istart = 32067
  !1_xk49_3E11
  else if (rlen.gt.2.D11 .and. rlen.le.3.D11) then
   istart = 32627
  !1_xk49_5E11
  else if (rlen.gt.3.D11 .and. rlen.le.5.D11) then
   istart = 33011
  !1_xk49_1E12
  else if (rlen.gt.5.D11 .and. rlen.le.1.D12) then
   istart = 33601
  else
   istart = 33995
  end if

!-----------------------------------------------------------------------------!
 case (50)
!-----------------------------------------------------------------------------!
  !1_xk50_2E8
  if      (rlen.gt.1.D00 .and. rlen.le.2.D08) then
   istart = 25847
  !1_xk50_3E8
  else if (rlen.gt.2.D08 .and. rlen.le.3.D08) then
   istart = 25847
  !1_xk50_4E8
  else if (rlen.gt.3.D08 .and. rlen.le.4.D08) then
   istart = 26109
  !1_xk50_5E8
  else if (rlen.gt.4.D08 .and. rlen.le.5.D08) then
   istart = 26319
  !1_xk50_7E8
  else if (rlen.gt.5.D08 .and. rlen.le.7.D08) then
   istart = 26419
  !1_xk50_1E9
  else if (rlen.gt.7.D08 .and. rlen.le.1.D09) then
   istart = 27023
  !1_xk50_2E9
  else if (rlen.gt.1.D09 .and. rlen.le.2.D09) then
   istart = 27571
  !1_xk50_3E9
  else if (rlen.gt.2.D09 .and. rlen.le.3.D09) then
   istart = 28061
  !1_xk50_4E9
  else if (rlen.gt.3.D09 .and. rlen.le.4.D09) then
   istart = 28429
  !1_xk50_5E9
  else if (rlen.gt.4.D09 .and. rlen.le.5.D09) then
   istart = 28663
  !1_xk50_6E9
  else if (rlen.gt.5.D09 .and. rlen.le.6.D09) then
   istart = 28901
  !1_xk50_7E9
  else if (rlen.gt.6.D09 .and. rlen.le.7.D09) then
   istart = 29001
  !1_xk50_1E10
  else if (rlen.gt.7.D09 .and. rlen.le.1.D10) then
   istart = 29169
  !1_xk50_2E10
  else if (rlen.gt.1.D10 .and. rlen.le.2.D10) then
   istart = 29885
  !1_xk50_3E10
  else if (rlen.gt.2.D10 .and. rlen.le.3.D10) then
   istart = 30455
  !1_xk50_4E10
  else if (rlen.gt.3.D10 .and. rlen.le.4.D10) then
   istart = 30713
  !1_xk50_5E10
  else if (rlen.gt.4.D10 .and. rlen.le.5.D10) then
   istart = 30891
  !1_xk50_7E10
  else if (rlen.gt.5.D10 .and. rlen.le.7.D10) then
   istart = 31155
  !1_xk50_1E11
  else if (rlen.gt.7.D10 .and. rlen.le.1.D11) then
   istart = 31337
  !1_xk50_2E11
  else if (rlen.gt.1.D11 .and. rlen.le.2.D11) then
   istart = 32165
  !1_xk50_3E11
  else if (rlen.gt.2.D11 .and. rlen.le.3.D11) then
   istart = 32725
  !1_xk50_4E11
  else if (rlen.gt.3.D11 .and. rlen.le.4.D11) then
   istart = 33109
  !1_xk50_5E11
  else if (rlen.gt.4.D11 .and. rlen.le.5.D11) then
   istart = 33301
  !1_xk50_7E11
  else if (rlen.gt.5.D11 .and. rlen.le.7.D11) then
   istart = 33699
  !1_xk50_1E12
  else if (rlen.gt.7.D11 .and. rlen.le.1.D12) then
   istart = 33895
  !1_xk50_2E12
  else if (rlen.gt.1.D12 .and. rlen.le.2.D12) then
   istart = 34093
  else
   istart = 34505
  end if

!-----------------------------------------------------------------------------!
 case (51)
!-----------------------------------------------------------------------------!
  !1_xk51_1E12
  if      (rlen.gt.1.D00 .and. rlen.le.1.D12) then
   istart = 34193
  !1_xk51_2E12
  else if (rlen.gt.1.D12 .and. rlen.le.2.D12) then
   istart = 34193
  else
   istart = 34605
  end if

!-----------------------------------------------------------------------------!
 case (52)
!-----------------------------------------------------------------------------!
  !1_xk52_1E12
  if      (rlen.gt.1.D00 .and. rlen.le.1.D12) then
   istart = 34295
  !1_xk52_3E12
  else if (rlen.gt.1.D12 .and. rlen.le.3.D12) then
   istart = 34295
  else
   istart = 34813
  end if

!-----------------------------------------------------------------------------!
 case (53)
!-----------------------------------------------------------------------------!
  !1_xk53_4E11
  if      (rlen.gt.1.D00 .and. rlen.le.4.D11) then
   istart = 33401
  !1_xk53_1E12
  else if (rlen.gt.4.D11 .and. rlen.le.1.D12) then
   istart = 33401
  !1_xk53_2E12
  else if (rlen.gt.1.D12 .and. rlen.le.2.D12) then
   istart = 34399
  !1_xk53_3E12
  else if (rlen.gt.2.D12 .and. rlen.le.3.D12) then
   istart = 34707
  !1_xk53_4E12
  else if (rlen.gt.3.D12 .and. rlen.le.4.D12) then
   istart = 34917
  else
   istart = 35023
  end if

!-----------------------------------------------------------------------------!
 case default
!-----------------------------------------------------------------------------!
  write(istdout,"(a)") chrdbg//"Maximum number of Laplace quadrature points is 53!"
  stop "Error"
 end select

!-----------------------------------------------------------------------------!
! copy data
!-----------------------------------------------------------------------------!
 ioff = istart-1
 do ilap = 1,nlap
  wghts(1,ilap) = laplace_init(ioff+ilap)
  wghts(2,ilap) = d0
 end do 

 ioff = ioff+nlap
 do ilap = 1,nlap
  xpnts(1,ilap) = laplace_init(ioff+ilap)
  xpnts(2,ilap) = d0
 end do 

 if (locdbg) then
  write(istdout,*)  chrdbg//" quadrature points>",nlap
  write(istdout,*)  chrdbg//" index range>",istart,istart+2*nlap-1
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
