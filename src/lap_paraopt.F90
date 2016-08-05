!==================================================================================================================================
subroutine lap_paraopt(niter,weight,expon,error,&                 !out
                       ipiv,jaco,func,xold,xnew,delx,grad,&       !i/o
                       expts,nexpts,maxiter,tolres,stpmax,afact,& !inp
                       nlap,nxpts)                                !dim
!----------------------------------------------------------------------------------------------------------------------------------!
!
! optimize the Laplace exponents and weights with a
! Newton-Raphson algorithm by finding the roots of 
! the set of equations:
!  
!  n(x'_i,w_j,a_j) + n(x'_i+1,w_j,a_j) = 0
!
!----------------------------------------------------------------------------------------------------------------------------------!

 implicit none

#include "consts.h"

! parameter:
 logical, parameter :: locdbg = .false.
 character(len=*), parameter :: chrdbg = 'lap_paraopt>'
 real(8), parameter :: alpha(2) = (/ d1, d0 /)
 real(8), parameter ::  beta(2) = (/ d0, d0 /)

! dimensions:
 integer,intent(in) :: nlap,nexpts,nxpts

! input:
 integer,intent(in) :: maxiter
 real(8),intent(in) :: stpmax, tolres, afact, expts(2,nexpts)

! scratch:
 integer, intent(inout) :: ipiv(nxpts)
 real(8), intent(inout) :: jaco(2,nxpts,nxpts), func(2,nxpts), xold(2,nxpts), &
                           xnew(2,nxpts), delx(2,nxpts), grad(2,nxpts)

! output:
 integer,intent(out)   :: niter
 real(8),intent(inout) :: weight(2,nlap), expon(2,nlap), error(2)

! local:
 logical :: converged, check
 integer :: ilap, iter, idx
 real(8), dimension(2) :: fval, fval0, delxnrm, slope, stpmx0, temp, tolrs0

! functions:
 logical :: dd128_lt, dd128_gt

 if (locdbg) write(istdout,"(a)") "entered lap_paraopt ..."

 converged = .false.

 tolrs0(1) = tolres
 tolrs0(2) = d0

 ! copy parameter locally
 do ilap = 1,nlap
  xnew(1,ilap)      = expon(1,ilap)
  xnew(2,ilap)      = expon(2,ilap)
  xnew(1,nlap+ilap) = weight(1,ilap)
  xnew(2,nlap+ilap) = weight(2,ilap)
 end do
 xnew(1,nxpts) = error(1)
 xnew(2,nxpts) = error(2)
 
 slope(1) = 999.d0
 slope(2) = d0
 stpmx0(1) = stpmax
 stpmx0(2) = d0

 ! compute initial functional:
 call lap_functional(func,jaco,1,nlap,expts,xnew,nxpts)

 call dd128_dot(temp,nxpts,func,1,func,1)
 call dd128_mul_doub(fval,temp,dp5)

 do iter = 1, maxiter

  ! compute Jacobian
  call lap_functional(func,jaco,2,nlap,expts,xnew,nxpts)

  if (locdbg) then
   ! print functional
   write(istdout,*) chrdbg,"functional:"
   write(istdout,"(2(E55.40,1x))") func

   ! print Jacobian
   write(istdout,*) chrdbg,"Jacobian:"
   write(istdout,"(2(E55.40,1x))") jaco
  end if

  ! compute gradient of scalar functional 
  call dd128_gemv('t',nxpts,nxpts,alpha,jaco,nxpts,func,1,beta,grad,1)

  ! save negative functional for NR
  do idx = 1,nxpts
   delx(1,idx) = - func(1,idx)
   delx(2,idx) = - func(2,idx)
  enddo

  call dd128_ludcmp(jaco,nxpts,nxpts,ipiv,temp)
  call dd128_lubksb(jaco,nxpts,nxpts,ipiv,delx) 

  if (locdbg) then
   ! print solution vector
   write(istdout,*) chrdbg,"update vector:"
   write(istdout,"(2(E55.40,1x))") delx
   write(istdout,*) chrdbg,"functional gradient vector:"
   write(istdout,"(2(E55.40,1x))") grad
   call dd128_dot(temp,nxpts,grad,1,delx,1)
   write(istdout,*) chrdbg,"functional slope:",temp
  end if

  ! save solution vector
  call dd128_copy(nxpts,xnew,1,xold,1)

  ! RMS error
  call dd128_nrm2(delxnrm,nxpts,delx,1)

  ! scale update vector
  if (dd128_gt(delxnrm,stpmx0)) then
   call dd128_div(temp,stpmx0,delxnrm)
   call dd128_scal(nxpts,temp,delx,1)
  end if

  ! update parameter:
  fval0(1) = fval(1)
  fval0(2) = fval(2)
  call lap_lnsrch(check,xnew,func,fval,&
                  xold,delx,maxiter,fval0,tolres,expts,grad,afact,&
                  nxpts,nlap,nexpts)

  if (locdbg) write(istdout,*) chrdbg,'Para opt:', iter,delxnrm,fval,xnew(1:2,nxpts)

  converged = dd128_lt(delxnrm,tolrs0)
  if (converged) exit

  if (locdbg) then
   write(istdout,*) chrdbg,'grad. of functional:',iter,slope(1:2)

   call dd128_sub(temp,fval,fval0)
   write(istdout,*) chrdbg,'difference of functional values:',iter,temp(1:2)

   write(istdout,*) chrdbg,"new exponents:"
   write(istdout,"(2(E55.40,1x))") xnew(1:2,1:nlap)
  
   write(istdout,*) chrdbg,"new weights:"
   write(istdout,"(2(E55.40,1x))") xnew(1:2,nlap+1:2*nlap)
  end if

  if (check) then
   call dd128_dot(temp,nxpts,grad,1,delx,1)
   write(istdout,*) chrdbg,"functional slope:",temp
   stop "Functional is minimal but solution vector not found with given accuracy!"
  end if

 end do

 ! copy 2k+1 parameter back
 do ilap = 1,nlap
  expon(1,ilap)  = xnew(1,ilap)
  expon(2,ilap)  = xnew(2,ilap)
  weight(1,ilap) = xnew(1,nlap+ilap) 
  weight(2,ilap) = xnew(2,nlap+ilap) 
 end do
 error(1) = xnew(1,nxpts)
 error(2) = xnew(2,nxpts)
 
 niter = iter

 if (locdbg.or. .not.converged) then
  write(istdout,*) "Solving Laplace exponents and weights"
  write(istdout,*) "   with Newton-Raphson algorithm"
  if (converged) then
    write(istdout,'(/,a,/)') "CONVERGED"
  else
    write(istdout,'(/,a,/)') "FAILED"
  end if
  write(istdout,*) "within", niter," iterations!"

  write(istdout,*) "new exponents:"
  write(istdout,"(2(E55.40,1x))") expon 

  write(istdout,*) "new weights:"
  write(istdout,"(2(E55.40,1x))") weight

 end if

 if (.not.converged)  stop "not converged"

 if (locdbg) write(istdout,"(a)") "... left lap_paraopt"

end subroutine lap_paraopt
!==================================================================================================================================!
!==================================================================================================================================!
subroutine lap_lnsrch(check,xnew,func,fval,&
                      xold,delx,maxiter,fval0,tolres,expts,grad,afact,&
                      ndim,nlap,nexpts)

 implicit none

#include "consts.h"

! parameter:
 logical, parameter :: locdbg = .false.
 character(len=*), parameter :: chrdbg = 'lap_lnsrch>'

! dimension:
 integer, intent(in) :: nlap, ndim, nexpts

! input:
 integer, intent(in) :: maxiter
 real(8), intent(in) :: xold(2,ndim), grad(2,ndim), delx(2,ndim), expts(2,nexpts),&
                        fval0(2), tolres, afact

! output:
 logical, intent(out) :: check
 real(8), intent(out) :: fval(2), xnew(2,ndim), func(2,ndim)
 
! local:
 integer :: iter, ilap
 real(8), dimension(2) :: alam, alam2, alamin, aval, bval, rhs1, rhs2, &
    slope, denom, disc, fval2, lambda, temp, temp2, atmp1, atmp2

! functions:
 logical :: dd128_le, dd128_eq, dd128_lt, dd128_gt, dd128_ge

 if (locdbg) write(istdout,"(a)") "entered lap_lnsrch ..."

 ! check for local minimum in func.
 check = .false.

 ! compute slope
 call dd128_dot(slope,ndim,grad,1,delx,1)

 temp(1) = d0
 temp(2) = d0
 if (dd128_gt(slope,temp)) then
  stop 'Slope must not be negative!'
 end if

 alamin(1) = tolres
 alamin(2) = d0
 alam(1) = d1
 alam(2) = d0
 do iter = 1, maxiter

  if (locdbg) write(istdout,*) chrdbg,iter,alam,fval,fval0+afact*alam*slope
 
  ! compute new parameter
  do ilap = 1,ndim
   call dd128_mul(temp,alam,delx(1,ilap))
   call dd128_add(xnew(1,ilap),xold(1,ilap),temp)
  end do

  ! compute functional value of new parameter
  call lap_functional(func,temp,1,nlap,expts,xnew,ndim)

  call dd128_dot(temp,ndim,func,1,func,1)
  call dd128_mul_doub(fval,temp,dp5)

  ! converged NR algorithm
  call dd128_mul(temp,alam,slope)
  call dd128_mul_doub(temp2,temp,afact)
  call dd128_add_assign(temp2,fval0)
  if (dd128_le(alam,alamin)) then
   call dd128_copy(ndim,xold,1,xnew,1)
   check = .true.
   go to 100
  ! sufficient function minimization
  else if (dd128_le(fval,temp2)) then
   go to 100
  ! backtrack
  else
   ! 1st backtrack
   temp(1) = d1
   temp(2) = d0
   if (dd128_eq(alam,temp)) then
    call dd128_sub(aval,fval,fval0)
    call dd128_sub_assign(aval,slope)
    call dd128_mul_doub(lambda,slope,-dp5)
    call dd128_div_assign(lambda,aval)
   ! subsequent backtracks
   else
    call dd128_sub(rhs1,fval,fval0)
    call dd128_mul(temp,alam,slope)
    call dd128_sub_assign(rhs1,temp)
    call dd128_sub(rhs2,fval2,fval0)
    call dd128_mul(temp,alam2,slope)
    call dd128_sub_assign(rhs2,temp)
    call dd128_sub(denom,alam,alam2)
    call dd128_sqr(temp,alam)
    call dd128_div(atmp1,rhs1,temp)
    call dd128_sqr(temp,alam2)
    call dd128_div(atmp2,rhs2,temp)
    call dd128_sub(aval,atmp1,atmp2)
    call dd128_div_assign(aval,denom)
    call dd128_mul(bval,alam ,atmp2)
    call dd128_mul(temp,alam2,atmp1)
    call dd128_sub_assign(bval,temp)
    call dd128_div_assign(bval,denom)
    temp(1) = d0
    temp(2) = d0
    if (dd128_eq(aval,temp)) then
     call dd128_mul_doub(lambda,slope,-dp5)
     call dd128_div_assign(lambda,bval)
    else
     call dd128_sqr(disc,bval)
     call dd128_mul_doub(temp,aval,-d3)
     call dd128_mul_assign(temp,slope)
     call dd128_add_assign(disc,temp)
     temp(1) = d0
     temp(2) = d0
     if (dd128_lt(disc,temp)) then
      call dd128_mul_doub(lambda,alam,dp5)
     else if (dd128_le(bval,temp)) then
      call dd128_sqrt(temp,disc)
      call dd128_sub(lambda,temp,bval)
      call dd128_mul_doub(temp,aval,d3)
      call dd128_div_assign(lambda,temp)
     else
      call dd128_sqrt(temp,disc)
      call dd128_add_assign(temp,bval)
      call dd128_div(lambda,slope,temp)
      lambda(1) = -lambda(1)
      lambda(2) = -lambda(2)
     end if
    end if
    
    call dd128_mul_doub(temp,alam,dp5)
    if (dd128_gt(lambda,temp)) then
     lambda(1) = temp(1)
     lambda(2) = temp(2)
    end if

   end if
  end if

  alam2(1) = alam(1)
  alam2(2) = alam(2)
  fval2(1) = fval(1)
  fval2(2) = fval(2)
  call dd128_mul_doub(temp,alam,0.1d0)
  if (dd128_ge(lambda,temp)) then
   alam(1) = lambda(1)
   alam(2) = lambda(2)
  else
   alam(1) = temp(1)
   alam(2) = temp(2)
  end if

 end do

 100 continue

 if (locdbg) write(istdout,"(a)") "... left lap_lnsrch"

end subroutine lap_lnsrch
!==================================================================================================================================!
!==================================================================================================================================!
subroutine lap_functional(func,jaco,iopt,nlap,xpts,xval,ndim)
!----------------------------------------------------------------------------------------------------------------------------------!
!
! iopt = 1    Func   --
!        2     --   Jaco
!        3    Func  Jaco
!
!----------------------------------------------------------------------------------------------------------------------------------!

 implicit none

#include "consts.h"

! parameter:
 logical, parameter :: locdbg = .false.
 character(len=*), parameter :: chrdbg = 'lap_functional>'

! dimensions:
 integer, intent(in) :: ndim

! input:
 integer, intent(in) :: iopt, nlap
 real(8), intent(in) :: xpts(2,ndim), xval(2,ndim)

! output:
 real(8), intent(inout) :: func(2,ndim), jaco(2,ndim,ndim)

! local:
 integer :: iext, ilap
 real(8) :: sgn
 real(8),dimension(2) :: temp1, temp2

 if (locdbg) write(istdout,"(a)") "enter lap_functional ..."

 ! compute functional
 if (iand(iopt,1).eq.1) then
  sgn=-d1
  do iext = 1, ndim
   call dd128_mul_doub(temp1,xval(1,ndim),sgn)
   call dd128_mul(temp2,temp1,xpts(1,iext))
   temp1(1) = d1
   temp1(2) = d0
   call dd128_sub_assign(temp2,temp1)
   call dd128_div(func(1,iext),temp2,xpts(1,iext))
   do ilap = 1,nlap
    call dd128_mul(temp1,xval(1,ilap),xpts(1,iext))
    temp1(1) = -temp1(1)
    temp1(2) = -temp1(2)
    call dd128_exp(temp2,temp1)
    call dd128_mul_assign(temp2,xval(1,nlap+ilap))
    call dd128_add_assign(func(1,iext),temp2)
   end do
   sgn=-sgn
  end do
 ! compute Jacobian
 else if (iand(iopt,2).eq.2) then
  sgn=-d1
  do iext = 1, ndim
   do ilap = 1,nlap
    call dd128_mul(temp1,xval(1,ilap),xpts(1,iext))
    temp1(1) = -temp1(1)
    temp1(2) = -temp1(2)
    call dd128_exp(temp2,temp1)
    call dd128_mul(temp1,temp2,xval(1,nlap+ilap))
    temp1(1) = -temp1(1)
    temp1(2) = -temp1(2)
    call dd128_mul(jaco(1,iext,ilap),temp1,xpts(1,iext))
    temp1(1) = -temp1(1)
    temp1(2) = -temp1(2)
    call dd128_div(jaco(1,iext,nlap+ilap),temp1,xval(1,nlap+ilap))
   end do
   jaco(1,iext,ndim) = sgn
   jaco(2,iext,ndim) = d0
   sgn=-sgn
  end do
 ! else ???
 else
  stop chrdbg//'invalid iopt! Should be 1-3!'
 end if

 if (locdbg) write(istdout,"(a)") " ... left lap_functional"

end subroutine lap_functional
!==================================================================================================================================!
