!------------------------------------------------------------------------------!
!
! arithmetic operations for double-double arithmetics
!
!  - stored as two real(8)floats stored consequtively in memory
!
!  - implemented operations and mathematical functions:
!
!       +  -  *  /  exp()
!
!
! BHP, winter 2016
!------------------------------------------------------------------------------!
!==============================================================================!
subroutine dd128_quick_two_sum(ddsum,dval1,dval2)
!------------------------------------------------------------------------------!
! d1 + d2;  d1 > d2
!------------------------------------------------------------------------------!

 implicit none
 
 real(8), intent(in)  :: dval1, dval2
 real(8), dimension(2), intent(out) :: ddsum

 ddsum(1) = dval1+dval2
 ddsum(2) = dval2-(ddsum(1)-dval1)

end subroutine dd128_quick_two_sum
!==============================================================================!
!==============================================================================!
subroutine dd128_quick_two_sum_assign(dval)
!------------------------------------------------------------------------------!
! d1 + d2;  d1 > d2
!------------------------------------------------------------------------------!

 implicit none
 
 real(8), dimension(2), intent(inout) :: dval
 real(8)  :: dtmp

 dtmp    = dval(1)
 dval(1) = dval(1)+dval(2)
 dval(2) = dval(2)-(dval(1)-dtmp)

end subroutine dd128_quick_two_sum_assign
!==============================================================================!
!==============================================================================!
subroutine dd128_two_sum(ddsum,dval1,dval2)
!------------------------------------------------------------------------------!
! (x,y) = a + b :
! ---------------
!  x  = a+b
!  bv = x-a
!  y  = (a-(x-bv))+(b-bv)
!------------------------------------------------------------------------------!

  implicit none
  
  real(8), intent(in)  :: dval1, dval2
  real(8), dimension(2), intent(out) :: ddsum

  real(8) :: dtmp

  ddsum(1) = dval1+dval2
  dtmp = ddsum(1) - dval1
  ddsum(2) = (dval1-(ddsum(1)-dtmp)) + (dval2-dtmp)

 end subroutine dd128_two_sum
!==============================================================================!
!==============================================================================!
 subroutine dd128_two_sub(ddsub,dval1,dval2)
!------------------------------------------------------------------------------!
! (x,y) = a - b :
! ---------------
!  x  = a-b
!  bv = a-x
!  y  = (a-(x+bv))+(bv-b)
!------------------------------------------------------------------------------!

  implicit none
  
  real(8), intent(in)  :: dval1, dval2
  real(8), dimension(2), intent(out) :: ddsub

  real(8) :: dtmp

  ddsub(1) = dval1-dval2
  dtmp = dval1 - ddsub(1)
  ddsub(2) = (dval1-(ddsub(1)+dtmp)) + (dtmp-dval2)

 end subroutine dd128_two_sub
!==============================================================================!
!==============================================================================!
 subroutine dd128_add(ddadd,ddval1,ddval2)
!------------------------------------------------------------------------------!
! dd1 + dd2 
!------------------------------------------------------------------------------!

  implicit none
  
  real(8), dimension(2), intent(in)  :: ddval1, ddval2
  real(8), dimension(2), intent(out) :: ddadd

  real(8), dimension(2) :: ddtmp

  call dd128_two_sum(ddadd,ddval1(1),ddval2(1))
  call dd128_two_sum(ddtmp,ddval1(2),ddval2(2))

  ddadd(2) = ddadd(2)+ddtmp(1)
  call dd128_quick_two_sum_assign(ddadd)

  ddadd(2) = ddadd(2)+ddtmp(2)
  call dd128_quick_two_sum_assign(ddadd)

 end subroutine dd128_add
!==============================================================================!
!==============================================================================!
 subroutine dd128_add_assign(ddval1,ddval2)
!------------------------------------------------------------------------------!
! dd1 = dd1 + dd2 
!------------------------------------------------------------------------------!

  implicit none
  
  real(8), dimension(2), intent(in)    :: ddval2
  real(8), dimension(2), intent(inout) :: ddval1

  real(8), dimension(2) :: ddtmp1, ddtmp2

  call dd128_two_sum(ddtmp1,ddval1(1),ddval2(1))
  call dd128_two_sum(ddtmp2,ddval1(2),ddval2(2))

  ddval1(1) = ddtmp1(1)
  ddval1(2) = ddtmp1(2)+ddtmp2(1)
  call dd128_quick_two_sum_assign(ddval1)

  ddval1(2) = ddval1(2)+ddtmp2(2)
  call dd128_quick_two_sum_assign(ddval1)

 end subroutine dd128_add_assign
!==============================================================================!
!==============================================================================!
 subroutine dd128_fma_two_prod(ddfma,dval1,dval2)
!------------------------------------------------------------------------------!
! d1 * d2  for fused-multiply-addition (FMA)
!------------------------------------------------------------------------------!

  implicit none
  
  real(8), intent(in)  :: dval1, dval2
  real(8), dimension(2), intent(out) :: ddfma

  ddfma(1) = dval1*dval2
  ddfma(2) = dval1*dval2 - ddfma(1)

 end subroutine dd128_fma_two_prod
!==============================================================================!
!==============================================================================!
 subroutine dd128_two_prod(ddmul,dval1,dval2)
!------------------------------------------------------------------------------!
! d1 * d2
!------------------------------------------------------------------------------!

  implicit none
  
  real(8), intent(in)  :: dval1, dval2
  real(8), dimension(2), intent(out) :: ddmul

  real(8), dimension(2) :: ddsp1, ddsp2

  ddmul(1) = dval1*dval2
  
  call dd128_split(ddsp1,dval1)
  call dd128_split(ddsp2,dval2)

  ddmul(2) = ( ( ddsp1(1)*ddsp2(1) - ddmul(1) ) &
              +  ddsp1(1)*ddsp2(2)  + ddsp1(2)*ddsp2(1) ) &
              +  ddsp1(2)*ddsp2(2) 

 end subroutine dd128_two_prod
!==============================================================================!
!==============================================================================!
 subroutine dd128_two_sqr(ddmul,dval1)
!------------------------------------------------------------------------------!
! d1 * d1
!------------------------------------------------------------------------------!

  implicit none
  
  real(8), intent(in)  :: dval1
  real(8), dimension(2), intent(out) :: ddmul

  real(8), dimension(2) :: ddsp1

  ddmul(1) = dval1*dval1
  
  call dd128_split(ddsp1,dval1)

  ddmul(2) = ( ( ddsp1(1)*ddsp1(1) - ddmul(1) ) &
              +  ddsp1(1)*ddsp1(2) + ddsp1(1)*ddsp1(2) )         &
              +  ddsp1(2)*ddsp1(2) 

 end subroutine dd128_two_sqr
!==============================================================================!
!==============================================================================!
 subroutine dd128_split(ddval,dval)
!------------------------------------------------------------------------------!
! split double precision real
!------------------------------------------------------------------------------!
   implicit none

   real(8), parameter :: dsplit = 134217729.d0   ! 2^27+1

   real(8), intent(in)  :: dval
   real(8), dimension(2), intent(out) :: ddval

   real(8) :: dtmp

   dtmp = dval*dsplit
   ddval(1) = dtmp-(dtmp-dval)
   ddval(2) = dval-ddval(1)
   
 end subroutine dd128_split
!==============================================================================!
!==============================================================================!
 subroutine dd128_mul_doub(ddmul,ddval1,dval2)
!------------------------------------------------------------------------------!
! dd1 * d2
!------------------------------------------------------------------------------!

  implicit none
  
  real(8), intent(in)  :: dval2
  real(8), dimension(2), intent(in)  :: ddval1
  real(8), dimension(2), intent(out) :: ddmul

  real(8) :: dc11, dc21, dc2, dt1, dt2
  real(8), dimension(2) :: dda(2), ddb(2)

  call dd128_split(dda,ddval1(1))
  call dd128_split(ddb,dval2)

  dc11 = ddval1(1)*dval2
  dc21 = (((dda(1)*ddb(1)-dc11 ) +dda(1)*ddb(2) ) +dda(2)*ddb(1) ) +dda(2)*ddb(2)

  dc2  = ddval1(2)*dval2

  dt1 = dc11+dc2
  dt2 = (dc2-(dt1-dc11))+dc21

  ddmul(1) = dt1+dt2
  ddmul(2) = dt2-(ddmul(1)-dt1)

 end subroutine dd128_mul_doub
!==============================================================================!
!==============================================================================!
 subroutine dd128_mul(ddmul,ddval1,ddval2)
!------------------------------------------------------------------------------!
! dd1 * dd2
!------------------------------------------------------------------------------!
  implicit none

  real(8), dimension(2), intent(in)  :: ddval1, ddval2
  real(8), dimension(2), intent(out) :: ddmul

  real(8), dimension(2) :: ddtmp

  call dd128_two_prod(ddtmp,ddval1(1),ddval2(1))

  ddtmp(2) = ddtmp(2) + ddval1(1) * ddval2(2)
  ddtmp(2) = ddtmp(2) + ddval1(2) * ddval2(1)

  call dd128_quick_two_sum(ddmul,ddtmp(1),ddtmp(2))

 end subroutine dd128_mul
!==============================================================================!
!==============================================================================!
 subroutine dd128_sqr(ddmul,ddval1)
!------------------------------------------------------------------------------!
! dd1 * dd1
!------------------------------------------------------------------------------!
  implicit none

  real(8), dimension(2), intent(in)  :: ddval1
  real(8), dimension(2), intent(out) :: ddmul

  real(8), dimension(2) :: ddtmp

  call dd128_two_sqr(ddtmp,ddval1(1))

  ddtmp(2) = ddtmp(2) + ddval1(1) * ddval1(2)
  ddtmp(2) = ddtmp(2) + ddval1(2) * ddval1(1)

  call dd128_quick_two_sum(ddmul,ddtmp(1),ddtmp(2))

 end subroutine dd128_sqr
!==============================================================================!
!==============================================================================!
 subroutine dd128_sqr_assign(ddval1)
!------------------------------------------------------------------------------!
! dd1 * dd1
!------------------------------------------------------------------------------!
  implicit none

  real(8), dimension(2), intent(inout)  :: ddval1

  real(8), dimension(2) :: ddtmp

  call dd128_two_sqr(ddtmp,ddval1(1))

  ddtmp(2) = ddtmp(2) + ddval1(1) * ddval1(2)
  ddtmp(2) = ddtmp(2) + ddval1(2) * ddval1(1)

  call dd128_quick_two_sum(ddval1,ddtmp(1),ddtmp(2))

 end subroutine dd128_sqr_assign
!==============================================================================!
!==============================================================================!
 subroutine dd128_mul_assign(ddval1,ddval2)
!------------------------------------------------------------------------------!
! dd1 = dd1 * dd2
!------------------------------------------------------------------------------!
  implicit none

  real(8), dimension(2), intent(in)    :: ddval2
  real(8), dimension(2), intent(inout) :: ddval1

  real(8), dimension(2) :: ddtmp

  call dd128_two_prod(ddtmp,ddval1(1),ddval2(1))

  ddtmp(2) = ddtmp(2) + ddval1(1) * ddval2(2)
  ddval1(2) = ddtmp(2) + ddval1(2) * ddval2(1)
  ddval1(1) = ddtmp(1)

  call dd128_quick_two_sum_assign(ddval1)

 end subroutine dd128_mul_assign
!==============================================================================!
!==============================================================================!
 subroutine dd128_sub(ddsub,ddval1,ddval2)
!------------------------------------------------------------------------------!
! dd1 - dd2
!
!  could be done more elegantly ...
!
!------------------------------------------------------------------------------!
  implicit none
  
  real(8), dimension(2), intent(in)  :: ddval1, ddval2
  real(8), dimension(2), intent(out) :: ddsub

  real(8), dimension(2) :: ddtmp

  ddtmp(1) = -ddval2(1); ddtmp(2) = -ddval2(2)
  call dd128_add(ddsub,ddval1,ddtmp)

 end subroutine dd128_sub
!==============================================================================!
!==============================================================================!
 subroutine dd128_sub_assign(ddval1,ddval2)
!------------------------------------------------------------------------------!
! dd1 = dd1 - dd2
!
!  could be done more elegantly ...
!
!------------------------------------------------------------------------------!
  implicit none
  
  real(8), dimension(2), intent(in)  :: ddval2
  real(8), dimension(2), intent(out) :: ddval1

  real(8), dimension(2) :: ddtmp

  ddtmp(1) = -ddval2(1); ddtmp(2) = -ddval2(2)
  call dd128_add_assign(ddval1,ddtmp)

 end subroutine dd128_sub_assign
!==============================================================================!
!==============================================================================!
 subroutine dd128_div_doub(dddiv,ddval,dval)
!------------------------------------------------------------------------------!
! dd / d
!------------------------------------------------------------------------------!

  implicit none
  
! input / output:
  real(8), intent(in)  :: dval
  real(8), dimension(2), intent(in)  :: ddval
  real(8), dimension(2), intent(out) :: dddiv

! local:
  real(8) :: dt1, dt2, dt12, dt22, der
  real(8), dimension(2) :: ddt1, ddb

  dt1 =  ddval(1) / dval

  call dd128_split(ddt1,dt1)
  call dd128_split(ddb,dval)

  dt12 = dt1 * dval
  dt22 = (((ddt1(1)*ddb(1)-dt12)+ddt1(1)*ddb(2))+ddt1(2)*ddb(1))+ddt1(2)*ddb(2)

  ddt1(1) = ddval(1) - dt12
  der = ddt1(1)-ddval(1) 
  ddt1(2) = ((-dt12-der)+(ddval(1)-(ddt1(1)-der)))+ddval(2)-dt22

  dt2 = (ddt1(1)+ddt1(2))/dval
  
  dddiv(1) = dt1+dt2
  dddiv(2) = dt2-(dddiv(1)-dt1)

 end subroutine dd128_div_doub
!==============================================================================!
!==============================================================================!
 subroutine dd128_div(dddiv,ddval1,ddval2)
!------------------------------------------------------------------------------!
! dd1 / dd2
!------------------------------------------------------------------------------!
  implicit none
  
  real(8), parameter :: d1 = 1.d0, d0 = 0.d0

  real(8), dimension(2), intent(in)  :: ddval1, ddval2
  real(8), dimension(2), intent(out) :: dddiv

  real(8) :: dxval, dyval
  real(8), dimension(2) :: ddtmp1, ddtmp2

  dxval = d1 / ddval2(1)
  dyval = ddval1(1) * dxval

  ddtmp1(1) = dyval; ddtmp1(2) = d0
  call dd128_mul(ddtmp2,ddval2,ddtmp1)
  
  call dd128_sub(ddtmp1,ddval1,ddtmp2)

  call dd128_two_prod(ddtmp2,dxval,ddtmp1(1))

  ddtmp1(1) = dyval; ddtmp1(2) = d0
  call dd128_add(dddiv,ddtmp1,ddtmp2)

 end subroutine dd128_div
!==============================================================================!
!==============================================================================!
 subroutine dd128_div_assign(ddval1,ddval2)
!------------------------------------------------------------------------------!
! dd1 = dd1 / dd2
!------------------------------------------------------------------------------!
  implicit none
  
  real(8), parameter :: d1 = 1.d0, d0 = 0.d0

  real(8), dimension(2), intent(in)    :: ddval2
  real(8), dimension(2), intent(inout) :: ddval1

  real(8) :: dxval, dyval
  real(8), dimension(2) :: ddtmp1, ddtmp2

  dxval = d1 / ddval2(1)
  dyval = ddval1(1) * dxval

  ddtmp1(1) = dyval; ddtmp1(2) = d0
  call dd128_mul(ddtmp2,ddval2,ddtmp1)
  
  call dd128_sub(ddtmp1,ddval1,ddtmp2)

  call dd128_two_prod(ddtmp2,dxval,ddtmp1(1))

  ddtmp1(1) = dyval; ddtmp1(2) = d0
  call dd128_add(ddval1,ddtmp1,ddtmp2)

 end subroutine dd128_div_assign
!==============================================================================!
!==============================================================================!
 subroutine dd128_div_doub_assign(ddval,dval)
!------------------------------------------------------------------------------!
! dd = dd / d
!------------------------------------------------------------------------------!

  implicit none

! input / output:
  real(8), intent(in)  :: dval
  real(8), dimension(2), intent(inout)  :: ddval

! local:
  real(8) :: dt1, dt2, dt12, dt22, der
  real(8), dimension(2) :: ddt1, ddb

  dt1 =  ddval(1) / dval

  call dd128_split(ddt1,dt1)
  call dd128_split(ddb,dval)

  dt12 = dt1 * dval
  dt22 = (((ddt1(1)*ddb(1)-dt12)+ddt1(1)*ddb(2))+ddt1(2)*ddb(1))+ddt1(2)*ddb(2)

  ddt1(1) = ddval(1) - dt12
  der = ddt1(1)-ddval(1)
  ddt1(2) = ((-dt12-der)+(ddval(1)-(ddt1(1)-der)))+ddval(2)-dt22

  dt2 = (ddt1(1)+ddt1(2))/dval

  ddval(1) = dt1+dt2
  ddval(2) = dt2-(ddval(1)-dt1)

 end subroutine dd128_div_doub_assign
!==============================================================================!
!==============================================================================!
 logical pure function dd128_eq(ddval1,ddval2)
!------------------------------------------------------------------------------!
! dd1 .eq. dd2
!------------------------------------------------------------------------------!
  implicit none

  real(8), dimension(2), intent(in) :: ddval1, ddval2

  dd128_eq = ((ddval1(1).eq.ddval2(1)) .and. (ddval1(2).eq.ddval2(2)))

 end function dd128_eq
!==============================================================================!
!==============================================================================!
 logical pure function dd128_ne(ddval1,ddval2)
!------------------------------------------------------------------------------!
! dd1 .ne. dd2
!------------------------------------------------------------------------------!
  implicit none

  real(8), dimension(2), intent(in) :: ddval1, ddval2

  dd128_ne = ((ddval1(1).ne.ddval2(1)) .or. (ddval1(2).ne.ddval2(2)))

 end function dd128_ne
!==============================================================================!
!==============================================================================!
 logical pure function dd128_gt(ddval1,ddval2)
!------------------------------------------------------------------------------!
! dd1 .gt. dd2
!------------------------------------------------------------------------------!
  implicit none

  real(8), dimension(2), intent(in) :: ddval1, ddval2

  if (ddval1(1).gt.ddval2(1)) then
   dd128_gt = .true.
  else if (ddval1(1).eq.ddval2(1)) then
   if (ddval1(2).gt.ddval2(2)) then
    dd128_gt = .true.
   else
    dd128_gt = .false.
   end if
  else
   dd128_gt = .false.
  end if

 end function dd128_gt
!==============================================================================!
!==============================================================================!
 logical pure function dd128_lt(ddval1,ddval2)
!------------------------------------------------------------------------------!
! dd1 .lt. dd2
!------------------------------------------------------------------------------!
  implicit none

  real(8), dimension(2), intent(in) :: ddval1, ddval2

  if (ddval1(1).lt.ddval2(1)) then
   dd128_lt = .true.
  else if (ddval1(1).eq.ddval2(1)) then
   if (ddval1(2).lt.ddval2(2)) then
    dd128_lt = .true.
   else
    dd128_lt = .false.
   end if
  else
   dd128_lt = .false.
  end if

 end function dd128_lt
!==============================================================================!
!==============================================================================!
 logical pure function dd128_ge(ddval1,ddval2)
!------------------------------------------------------------------------------!
! dd1 .ge. dd2
!------------------------------------------------------------------------------!
  implicit none

  real(8), dimension(2), intent(in) :: ddval1, ddval2

  if (ddval1(1).gt.ddval2(1)) then
   dd128_ge = .true.
  else if (ddval1(1).eq.ddval2(1)) then
   if (ddval1(2).gt.ddval2(2) .or. ddval1(2).eq.ddval2(2)) then
    dd128_ge = .true.
   else
    dd128_ge = .false.
   end if
  else
   dd128_ge = .false.
  end if

 end function dd128_ge
!==============================================================================!
!==============================================================================!
 logical pure function dd128_le(ddval1,ddval2)
!------------------------------------------------------------------------------!
! dd1 .le. dd2
!------------------------------------------------------------------------------!
  implicit none

  real(8), dimension(2), intent(in) :: ddval1, ddval2

  if (ddval1(1).lt.ddval2(1)) then
   dd128_le = .true.
  else if (ddval1(1).eq.ddval2(1)) then
   if (ddval1(2).lt.ddval2(2) .or. ddval1(2).eq.ddval2(2)) then
    dd128_le = .true.
   else
    dd128_le = .false.
   end if
  else
   dd128_le = .false.
  end if

 end function dd128_le
!==============================================================================!
!==============================================================================!
 subroutine dd128_sqrt(ddsqrt,ddval)
!------------------------------------------------------------------------------!
! dd = sqrt(dd)
!------------------------------------------------------------------------------!

  implicit none

  real(8), parameter :: d1 = 1.d0, d0 = 0.d0

  real(8), dimension(2), intent(in) :: ddval
  real(8), dimension(2), intent(out) :: ddsqrt

  ! local
  real(8) :: dxval, dyval
  real(8), dimension(2) :: ddtmp1, ddtmp2

  dxval = d1 / sqrt(ddval(1))
  dyval = ddval(1) * dxval

  call dd128_two_prod(ddtmp1,dyval,dyval)

  call dd128_sub(ddtmp2,ddval,ddtmp1)

  call dd128_two_prod(ddtmp1,dxval,ddtmp2(1))

  ddtmp1(1) = ddtmp1(1)/2; ddtmp1(2) = ddtmp1(2)/2

  ddtmp2(1) = dyval; ddtmp2(2) = d0
  call dd128_add(ddsqrt,ddtmp2,ddtmp1)

 end subroutine dd128_sqrt
!==============================================================================!
!==============================================================================!
 subroutine dd128_exp(ddexp,ddval)
!------------------------------------------------------------------------------!
! dde = exp(dd)
!------------------------------------------------------------------------------!
  implicit none
  
  integer, parameter :: maxiter = 1000
  real(8), parameter :: d1 = 1.d0, d0 = 0.d0
  real(8), parameter :: tolexp = 1.d-30
  real(8), parameter :: xmin(2) = (/ -512.d0, 0.d0 /)

  real(8), dimension(2), intent(in)  :: ddval
  real(8), dimension(2), intent(out) :: ddexp

  ! local: 
  integer :: iscal, norder, iter
  real(8) :: dtmp, dxval
  real(8), dimension(2) :: ddtmp1, ddtmp2

  ! function:
  logical :: dd128_gt, dd128_lt

  ! check range of x
  if (dd128_lt(ddval,xmin)) then
   ddexp(1) = d0
   ddexp(2) = d0
   return
  end if

  dxval = tolexp*exp(ddval(1))

  ! get order for scaling and squaring
  iscal = 1
  norder = 0
  do 
   dtmp = dble(iscal)
   ddtmp1(1) = dtmp; ddtmp1(2) = d0
   if (dd128_gt(ddtmp1,ddval)) exit
   iscal = iscal*2
   norder = norder+1
  end do 

  ! for faster convergence of Taylor expansion (to be assessed)
  iscal = iscal*256
  norder = norder+8

  ! initialization
  ddexp(1) = d0; ddexp(2) = d0
  ddtmp1(1) = d1; ddtmp1(2) = d0

  ! Taylor expansion
  do iter = 1,maxiter
   call dd128_add_assign(ddexp,ddtmp1)
   if (abs(ddtmp1(1)).lt.dxval) exit

   call dd128_mul(ddtmp2,ddtmp1,ddval)

   dtmp = dble(iscal*iter)
   call dd128_div_doub(ddtmp1,ddtmp2,dtmp)
  end do

  ! squaring
  do iter = 1,norder
   call dd128_sqr_assign(ddexp)
  end do

 end subroutine dd128_exp
!==============================================================================!
