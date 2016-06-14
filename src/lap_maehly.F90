!==================================================================================================================================!
subroutine lap_maehly(niter,xpts,&
                      rnge,maxiter,tolres,xpnts,wghts,stpmax,delta,&
                      nxpts,nlap)
!---------------------------------------------------------------------------------------!
!
! find all 2k-1 extremum points of error distribution functional
!  with Maehly's procedure (Newton + deflation for exponentials)
!
!
! B. Helmich-Paris, summer 2015
!
!----------------------------------------------------------------------------------------!

 implicit none

#include "consts.h"

! parameter:
 logical, parameter :: locdbg = .false.
 character(len=*), parameter :: chrdbg = 'lap_maehly>'

 real(8), parameter :: alpha(2) = (/ d1, d0 /)
 real(8), parameter ::  beta(2) = (/ d0, d0 /)

! dimensions:
 integer, intent(in) :: nxpts, nlap

! input:
 integer, intent(in) :: maxiter
 real(8), intent(in) :: rnge(2), tolres, xpnts(2,nlap), wghts(2,nlap), stpmax, delta

! output:
 integer, intent(out) :: niter
 real(8), intent(inout) :: xpts(2,nxpts)

! local:
 logical :: converged
 integer :: irt, jrt, iter, niter0
 real(8), dimension(2) :: xval, xval0, denom, grad, func, delx, &
    temp1, temp2, temp3, tolrs0, stpmx0, dswap

! functions:
 logical :: dd128_lt, dd128_gt

 niter = 0
 xval0(1) = rnge(1)
 xval0(2) = 0.d0

 tolrs0(1) = tolres
 tolrs0(2) = 0.d0

 xpts_loop: do irt = 1,nxpts

  niter0 = 0
  xval(1) = xval0(1)
  xval(2) = xval0(2)

  if (irt.eq.1) then
   stpmx0(1) = stpmax
  else if (irt.eq.2) then
   stpmx0(1) = stpmax*(xpts(1,1)-rnge(1))
  else 
   stpmx0(1) = stpmax*(xpts(1,irt-1)-xpts(1,irt-2))
  end if
  stpmx0(2) = d0

  ! find xpts by NR procedure
  iter_loop: do iter = 1, maxiter
 
   ! compute value of functional and its gradient
   call lap_functional1(func,grad,xval,wghts,xpnts,nlap)

   ! compute denominator
   denom(1) = d0
   denom(2) = d0
   do jrt = 1,irt-1
    call dd128_sub(temp2,xval,xpts(1,jrt))
    call dd128_div(temp3,alpha,temp2)
    call dd128_add_assign(denom,temp3)
   end do

   call dd128_mul(temp1,denom,func)
   call dd128_sub(temp2,grad,temp1)

   grad(1) = temp2(1)
   grad(2) = temp2(2)

   ! step length
   call dd128_div(delx,func,grad)

   ! scale update vector
   temp2(1) = abs(delx(1))
   temp2(2) = abs(delx(2))

   if (dd128_gt(temp2,stpmx0)) then
    delx(1) = stpmx0(1)*sign(d1,delx(1))
    delx(2) = d0
   end if

   ! save solution vector
   call dd128_sub_assign(xval,delx)
 
   if (locdbg) write(istdout,*) chrdbg,iter,xval(1),func(1),grad(1),delx(1)

   converged = (dd128_lt(temp2,tolrs0))
   if (converged) exit

  end do iter_loop

  ! check convergence
  if (.not.converged) goto 100

  ! number of Newton iterations
  niter0 = niter0+iter

  ! check convergence
  if (niter0.gt.maxiter) goto 100

  ! polish extremum points by perfoming NR with extremum points
  ! of deflated function
  if (irt.gt.1) then
   call lap_newton1(iter,xval,maxiter,tolrs0,stpmx0,xpnts,wghts,nlap)
   niter0 = niter0+iter

   ! check convergence
   if (niter0.gt.maxiter) goto 100
  end if

  niter = niter+niter0

  xpts(1,irt) = xval(1)
  xpts(2,irt) = xval(2)

  xval0(1) = xpts(1,irt)*(d1+delta)
  xval0(1) = min(xval0(1),rnge(2))
  xval0(1) = max(xval0(1),rnge(1))
  xval0(2) = d0

 end do xpts_loop

 ! sort extremum points in ascending order
 do jrt = 2,nxpts
  do irt = 1,jrt-1
   if (dd128_gt(xpts(1,irt),xpts(1,jrt))) then
    dswap(1) = xpts(1,jrt) 
    dswap(2) = xpts(2,jrt) 
    xpts(1,jrt) = xpts(1,irt)
    xpts(2,jrt) = xpts(2,irt)
    xpts(1,irt) = dswap(1)
    xpts(2,irt) = dswap(2)
   end if
  end do
 end do

 if (locdbg) then
  write(istdout,*) chrdbg,"Extremum points of error distribution:"
  write(istdout,"(2(E55.40,1x))") xpts
 end if

 ! check range for extremum points
 if (xpts(1,1).le.rnge(1)) then
  stop chrdbg//"lowest extremum point below 1!"
 end if

 if (xpts(1,nxpts).ge.rnge(2)) then
  stop chrdbg//"largest extremum point greater R!"
 end if

 return

 100 write(istdout,'(/,2a,/)') &
      chrdbg,'                  ERROR'
     write(istdout,'(2a,1x,i3,1x,a,/)') &
      chrdbg, "Newton algorithm did not converge within ",maxiter, " iterations!"
 stop "Error"

end subroutine lap_maehly
!==================================================================================================================================!
!==================================================================================================================================!
subroutine lap_functional1(func,grad,xval,wghts,xpnts,nlap)
!---------------------------------------------------------------------------------------!
!
! do the summation over quadrature points by using Kahan summation
!
!---------------------------------------------------------------------------------------!

 implicit none

#include "consts.h"

! paramter:
 real(8), parameter :: alpha(2) = (/ d1, d0 /)
 real(8), parameter ::  beta(2) = (/ d0, d0 /)

! input:
 integer, intent(in) :: nlap
 real(8), intent(in) :: wghts(2,nlap), xpnts(2,nlap)
 real(8), dimension(2), intent(in) :: xval

! output:
 real(8), dimension(2), intent(out) :: func, grad

! local:
 integer :: ilap
 real(8), dimension(2) :: temp1, temp2, temp3

 call dd128_sqr(temp2,xval)

 call dd128_div(func,alpha,temp2)

 call dd128_mul_doub(temp3,func,-d2)

 call dd128_div(grad,temp3,xval)
 
 do ilap = 1, nlap
  temp2(1) = -xpnts(1,ilap)
  temp2(2) = -xpnts(2,ilap)
  call dd128_mul(temp1,xval,temp2)
  call dd128_exp(temp2,temp1)

  call dd128_mul(temp1,xpnts(1,ilap),wghts(1,ilap))
  call dd128_mul(temp3,temp1,temp2)
  call dd128_sub_assign(func,temp3)

  call dd128_mul(temp2,temp3,xpnts(1,ilap))
  call dd128_add_assign(grad,temp2)
  
 end do

end subroutine lap_functional1
!==================================================================================================================================!
!==================================================================================================================================!
subroutine lap_newton1(niter,xval,&
                       maxiter,tolrs0,stpmx0,xpnts,wghts,&
                       nlap)
!---------------------------------------------------------------------------------------!
!
! Find extremum points in one dimension by Newton procedure.
!
!---------------------------------------------------------------------------------------!
 
 implicit none

#include "consts.h"

! parameter
 logical, parameter :: locdbg = .false.
 character(len=12), parameter :: chrdbg = 'lap_newton1>'

 ! dimensions:
 integer, intent(in) :: nlap

 ! input:
 integer, intent(in) :: maxiter
 real(8), intent(in) :: xpnts(2,nlap), wghts(2,nlap), tolrs0(2), stpmx0(2)

 ! output:
 integer, intent(out) :: niter
 real(8), dimension(2), intent(inout) :: xval

 ! local:
 logical :: converged
 real(8), dimension(2) :: func, grad, delx, temp

! functions:
 logical :: dd128_gt, dd128_lt

 converged = .false.

 do niter = 1, maxiter

  ! compute value of distribution
  ! and gradient
  call lap_functional1(func,grad,xval,wghts,xpnts,nlap)

  ! step length
  if (grad(1).eq.d0) then
   grad(1) = 1.d+10
   grad(2) = d0
  end if

  call dd128_div(delx,func,grad)

  ! scale update vector
  temp(1) = abs(delx(1))
  temp(2) = abs(delx(2))
  if (dd128_gt(temp,stpmx0)) then
   delx(1) = stpmx0(1)*sign(d1,delx(1))
   delx(2) = d0
  end if

  ! save solution vector
  call dd128_sub_assign(xval,delx)

  if (locdbg) write(istdout,*) chrdbg,niter,xval(1),func(1),grad(1),delx(1)

  converged = (dd128_lt(temp,tolrs0))
  if (converged) exit

 end do

 if (.not.converged) then
  write(istdout,'(/,2a,/)') chrdbg,'                  ERROR'
  write(istdout,'(2a,1x,i3,1x,a,/)') chrdbg, "Newton algorithm did not converge within ",maxiter, " iterations!"
  write(istdout,'(2a,1x,i3,1x,a,/)') chrdbg, "current step width:",delx(1)
  stop "Error"
 end if
   
end subroutine lap_newton1
!==================================================================================================================================!
