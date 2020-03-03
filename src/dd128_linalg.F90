!==============================================================================!
subroutine dd128_copy(n,ddx,incx,ddy,incy)
!------------------------------------------------------------------------------!
!
! double-double presicion copy command
!
!     copies a vector, x, to a vector, y.
!     uses unrolled loops for increments equal to one.
!
! BHP, winter2016
!
!------------------------------------------------------------------------------!

! dimensions:
 integer, intent(in) :: incx,incy,n

! input:
 real(8), intent(in)  :: ddx(2,(n-1)*abs(incx)+1)

! output:
 real(8), intent(out) :: ddy(2,(n-1)*abs(incy)+1)

! local:
 integer :: i,ix,iy,m,mp1

 if (n.le.0) return
!
 if (incx.eq.1 .and. incy.eq.1) then
!
!        code for both increments equal to 1
!
  m = mod(n,7)
  if (m.ne.0) then
   do i = 1,m
    ddy(1,i) = ddx(1,i)
    ddy(2,i) = ddx(2,i)
   enddo
  endif
  if (n.ge.7) then
   mp1 = m + 1
   do i = mp1,n,7
    ddy(1,i)   = ddx(1,i) 
    ddy(2,i)   = ddx(2,i) 
    ddy(1,i+1) = ddx(1,i+1)
    ddy(2,i+1) = ddx(2,i+1)
    ddy(1,i+2) = ddx(1,i+2)
    ddy(2,i+2) = ddx(2,i+2)
    ddy(1,i+3) = ddx(1,i+3)
    ddy(2,i+3) = ddx(2,i+3)
    ddy(1,i+4) = ddx(1,i+4)
    ddy(2,i+4) = ddx(2,i+4)
    ddy(1,i+5) = ddx(1,i+5)
    ddy(2,i+5) = ddx(2,i+5)
    ddy(1,i+6) = ddx(1,i+6)
    ddy(2,i+6) = ddx(2,i+6)
   enddo
  endif
!
 else
!
!        code for unequal increments or equal increments
!          not equal to 1
!
  ix = 1
  iy = 1
  if (incx.lt.0) ix = (-n+1)*incx + 1
  if (incy.lt.0) iy = (-n+1)*incy + 1
  do i = 1,n
   ddy(1,iy) = ddx(1,ix)
   ddy(2,iy) = ddx(2,ix)
   ix = ix + incx
   iy = iy + incy
  end do
 endif
!
end subroutine dd128_copy
!==============================================================================!
!==============================================================================!
subroutine dd128_dot(ddz,n,ddx,incx,ddy,incy)
!------------------------------------------------------------------------------!
!  dot product of two vector that contain double double precision elements
!
! BHP, winter2016
!
!------------------------------------------------------------------------------!

 implicit none

! parameter:
 integer :: ncache = 5

! input:
 integer, intent(in) :: incx,incy,n
 real(8), intent(in) ::  ddx(2,(n-1)*abs(incx)+1),ddy(2,(n-1)*abs(incy)+1)

! output:
 real(8), dimension(2), intent(out) :: ddz

! local:
 real(8), dimension(2) :: ddtmp1, ddtmp2
 integer :: i,ix,iy,m,mp1

 ddz(1) = 0.0d0; ddz(2) = 0.0d0

 if (n.gt.0) then
  ddtmp1(1) = 0.0d0; ddtmp1(2) = 0.0d0
  if (incx.ne.1 .or. incy.ne.1) then
!
!        code for unequal increments or equal increments
!          not equal to 1
!
   ix = 1
   iy = 1
   if (incx.lt.0) ix = (-n+1)*incx + 1
   if (incy.lt.0) iy = (-n+1)*incy + 1
   do i = 1,n
    call dd128_mul(ddtmp2,ddx(1,ix),ddy(1,iy))
    call dd128_add_assign(ddtmp1,ddtmp2)
    ix = ix + incx
    iy = iy + incy
   end do
   ddz(1) = ddtmp1(1); ddz(2) = ddtmp1(2)
  else
!
!        code for both increments equal to 1
!
!
!        clean-up loop
!
    m = mod(n,ncache)
    if (m.eq.0) go to 40
    do i = 1,m
     call dd128_mul(ddtmp2,ddx(1,i  ),ddy(1,i  ))
     call dd128_add_assign(ddtmp1,ddtmp2)
    end do
    if (n.lt.ncache) go to 60
 40 mp1 = m + 1
    do i = mp1,n,ncache
     call dd128_mul(ddtmp2,ddx(1,i  ),ddy(1,i  ))
     call dd128_add_assign(ddtmp1,ddtmp2)
     call dd128_mul(ddtmp2,ddx(1,i+1),ddy(1,i+1))
     call dd128_add_assign(ddtmp1,ddtmp2)
     call dd128_mul(ddtmp2,ddx(1,i+2),ddy(1,i+2))
     call dd128_add_assign(ddtmp1,ddtmp2)
     call dd128_mul(ddtmp2,ddx(1,i+3),ddy(1,i+3))
     call dd128_add_assign(ddtmp1,ddtmp2)
     call dd128_mul(ddtmp2,ddx(1,i+4),ddy(1,i+4))
     call dd128_add_assign(ddtmp1,ddtmp2)
    end do
  60  ddz(1) = ddtmp1(1); ddz(2) = ddtmp1(2)
  end if
 end if

end subroutine dd128_dot
!==============================================================================!
!==============================================================================!
subroutine dd128_gemv(trans,m,n,alpha,dda,lda,ddx,incx,beta,ddy,incy)
!------------------------------------------------------------------------------!
!
!   double-double precision version of gemv Netlib BLAS routine
!
!
!  purpose
!  =======
!
!  dd128_gemv  performs one of the matrix-vector operations
!
!     y := alpha*a*x + beta*y,   or   y := alpha*a'*x + beta*y,
!
!  where alpha and beta are scalars, x and y are vectors and a is an
!  m by n matrix.
!
!  arguments
!  ==========
!
!  trans  - character*1.
!           on entry, trans specifies the operation to be performed as
!           follows:
!
!              trans = 'n' or 'n'   y := alpha*a*x + beta*y.
!
!              trans = 't' or 't'   y := alpha*a'*x + beta*y.
!
!              trans = 'c' or 'c'   y := alpha*a'*x + beta*y.
!
!           unchanged on exit.
!
!  m      - integer.
!           on entry, m specifies the number of rows of the matrix a.
!           m must be at least zero.
!           unchanged on exit.
!
!  n      - integer.
!           on entry, n specifies the number of columns of the matrix a.
!           n must be at least zero.
!           unchanged on exit.
!
!  alpha  - double double precision.
!           on entry, alpha specifies the scalar alpha.
!           unchanged on exit.
!
!  dda    - double double precision array of dimension ( lda, n ).
!           before entry, the leading m by n part of the array a must
!           contain the matrix of coefficients.
!           unchanged on exit.
!
!  lda    - integer.
!           on entry, lda specifies the first dimension of a as declared
!           in the calling (sub) program. lda must be at least
!           max( 1, m ).
!           unchanged on exit.
!
!  ddx    - double double precision array of dimension at least
!           ( 1 + ( n - 1 )*abs( incx ) ) when trans = 'n' or 'n'
!           and at least
!           ( 1 + ( m - 1 )*abs( incx ) ) otherwise.
!           before entry, the incremented array x must contain the
!           vector x.
!           unchanged on exit.
!
!  incx   - integer.
!           on entry, incx specifies the increment for the elements of
!           x. incx must not be zero.
!           unchanged on exit.
!
!  beta   - double double precision.
!           on entry, beta specifies the scalar beta. when beta is
!           supplied as zero then y need not be set on input.
!           unchanged on exit.
!
!  y      - double precision array of dimension at least
!           ( 1 + ( m - 1 )*abs( incy ) ) when trans = 'n' or 'n'
!           and at least
!           ( 1 + ( n - 1 )*abs( incy ) ) otherwise.
!           before entry with beta non-zero, the incremented array y
!           must contain the vector y. on exit, y is overwritten by the
!           updated vector y.
!
!  incy   - integer.
!           on entry, incy specifies the increment for the elements of
!           y. incy must not be zero.
!           unchanged on exit.
!
!
!  level 2 blas routine.
!
!  -- written on 22-october-1986.
!     jack dongarra, argonne national lab.
!     jeremy du croz, nag central office.
!     sven hammarling, nag central office.
!     richard hanson, sandia national labs.
!
!------------------------------------------------------------------------------!

 implicit none

! parameters:
 real(8), parameter :: one = 1.0d+0, zero = 0.0d+0

! dimension:
 integer, intent(in) :: incx,incy,lda,m,n

! input
 character(len=1), intent(in) :: trans
 real(8), dimension(2), intent(in) :: alpha, beta
 real(8), intent(in) :: dda(2,lda,*), ddx(2,*)

! in/output
 real(8), intent(inout) :: ddy(2,*)

! local scalars:
 real(8), dimension(2) :: temp, temp2
 integer :: i,info,ix,iy,j,jx,jy,kx,ky,lenx,leny

! external functions:
 logical, external :: dd128_lsame
!
!     test the input parameters.
!
 info = 0
 if (.not.dd128_lsame(trans,'n') .and. .not.dd128_lsame(trans,'t') .and. .not.dd128_lsame(trans,'c')) then
  info = 1
 else if (m.lt.0) then
  info = 2
 else if (n.lt.0) then
  info = 3
 else if (lda.lt.max(1,m)) then
  info = 6
 else if (incx.eq.0) then
  info = 8
 else if (incy.eq.0) then
  info = 11
 end if
 if (info.ne.0) then
  call dd128_xerbla('dd128_gemv ',info)
  return
 end if
!
!     quick return if possible.
!
 if ((m.eq.0) .or. (n.eq.0) .or. ((alpha(1).eq.zero).and. (beta(1).eq.one))) return
!
!     set  lenx  and  leny, the lengths of the vectors x and y, and set
!     up the start points in  x  and  y.
!
 if (dd128_lsame(trans,'n')) then
  lenx = n
  leny = m
 else
  lenx = m
  leny = n
 end if
 if (incx.gt.0) then
  kx = 1
 else
  kx = 1 - (lenx-1)*incx
 end if
 if (incy.gt.0) then
  ky = 1
 else
  ky = 1 - (leny-1)*incy
 end if
!
!     start the operations. in this version the elements of a are
!     accessed sequentially with one pass through a.
!
!     first form  y := beta*y.
!
 if (beta(1).ne.one) then
  if (incy.eq.1) then
   if (beta(1).eq.zero) then
    do i = 1,leny
     ddy(1,i) = zero; ddy(2,i) = zero
    end do
   else
    do i = 1,leny
     call dd128_mul_assign(ddy(1,i),beta)
    end do
   end if
  else
   iy = ky
   if (beta(1).eq.zero) then
    do i = 1,leny
     ddy(1,iy) = zero; ddy(2,iy) = zero
     iy = iy + incy
    end do
   else
    do i = 1,leny
     call dd128_mul_assign(ddy(1,iy),beta)
     iy = iy + incy
    end do
   end if
  end if
 end if
 if (alpha(1).eq.zero) return
 if (dd128_lsame(trans,'n')) then
!
!        form  y := alpha*a*x + y.
!
  jx = kx
  if (incy.eq.1) then
   do j = 1,n
    if (ddx(1,jx).ne.zero) then
     call dd128_mul(temp,ddx(1,jx),alpha)
     do i = 1,m
      call dd128_mul(temp2,temp,dda(1,i,j))
      call dd128_add_assign(ddy(1,i),temp2)
     end do
    end if
    jx = jx + incx
   end do
  else
   do j = 1,n
    if (ddx(1,jx).ne.zero) then
     call dd128_mul(temp,ddx(1,jx),alpha)
     iy = ky
     do i = 1,m
      call dd128_mul(temp2,temp,dda(1,i,j))
      call dd128_add_assign(ddy(1,iy),temp2)
      iy = iy + incy
     end do
    end if
    jx = jx + incx
   end do
  end if
 else
!
! form  y := alpha*a'*x + y.
!
  jy = ky
  if (incx.eq.1) then
   do j = 1,n
    temp(1) = zero
    temp(2) = zero
    do i = 1,m
     call dd128_mul(temp2,ddx(1,i),dda(1,i,j))
     call dd128_add_assign(temp,temp2)
    end do
    call dd128_mul(temp2,alpha,temp)
    call dd128_add_assign(ddy(1,jy),temp2)
    jy = jy + incy
   end do
  else
   do j = 1,n
    temp(1) = zero; temp(2) = zero
    ix = kx
    do i = 1,m
     call dd128_mul(temp2,dda(1,i,j),ddx(1,ix))
     call dd128_add_assign(temp,temp2)
     ix = ix + incx
    end do
    call dd128_mul(temp2,alpha,temp)
    call dd128_add_assign(ddy(1,jy),temp2)
    jy = jy + incy
   end do
  end if
 end if
end subroutine dd128_gemv
!==============================================================================!
!==============================================================================!
subroutine dd128_lubksb(a,n,np,indx,b) 
!------------------------------------------------------------------------------!
! solves the set of n linear equations a â· x = b. here a is input
! not as the matrix a but rather as its lu decomposition, determined 
! by the routine ludcmp. indx is input as the permutation vector returned 
! by ludcmp. b(1:n) is input as the right-hand side vector b, and returns 
! with the solution vector x. a, n, np, and indx are not modified by this 
! routine and can be left in place for successive calls with different 
! right-hand sides b. this routine takes into account the possibility that 
! b will begin with many zero elements, so it is efficient for use in matrix 
! inversion.
!------------------------------------------------------------------------------!
 
 implicit none

! parameter:
 real(8), parameter :: d0 = 0.d0

! dimension:
 integer, intent(in) :: n,np

! input:
 integer, intent(in) :: indx(n)
 real(8), intent(in) :: a(2,np,n)

! in/output:
 real(8), intent(inout) :: b(2,n)

! local:
 integer :: i,ii,j,ll 
 real(8),dimension(2) :: sum, temp

 ii=0
 do i=1,n
  ll=indx(i) 
  sum(1) = b(1,ll); sum(2) = b(2,ll)
  b(1,ll)=b(1,i); b(2,ll)=b(2,i)
  if (ii.ne.0)then
 !when ii is set to a positive value, 
 !it will become the in- dex of the first nonvanishing element of b. 
 ! we now do the forward substitution, equation (2.3.6). 
 ! the only new wrinkle is to unscramble the permutation as we go.
   do j=ii,i-1 
    call dd128_mul(temp,a(1,i,j),b(1,j))
    call dd128_sub_assign(sum,temp)
   end do
  else if (sum(1).ne.d0) then
   ii=i 
  endif
  b(1,i) = sum(1); b(2,i) = sum(2)
 enddo

 do i=n,1,-1 
  sum(1) = b(1,i); sum(2) = b(2,i)
  do j=i+1,n
   call dd128_mul(temp,a(1,i,j),b(1,j))
   call dd128_sub_assign(sum,temp)
  enddo
  call dd128_div(b(1,i),sum,a(1,i,i))
 enddo

end subroutine dd128_lubksb
!==============================================================================!
!==============================================================================!
subroutine dd128_ludcmp(a,n,np,indx,d)
!------------------------------------------------------------------------------!
! 
! perform a LU decompostion of square matrix a stored as
! 
!  DOUBLE DOUBLE PRECISION array
!
! BHP, winter 2016
!
!  Given a matrix a(1:n,1:n), with physical dimension np by np, 
!  this routine replaces it by the LU decomposition of a rowwise permutation 
!  of itself. a and n are input. a is output, arranged as in equation (2.3.14) 
!  above; indx(1:n) is an output vector that records the row permutation 
!  effected by the partial pivoting; d is output as Â±1 depending on whether
!  the number of row interchanges was even or odd, respectively. 
!  This routine is used in combination with lubksb to solve linear 
!  equations or invert a matrix.
!
!------------------------------------------------------------------------------!

 implicit none

! parameter:
 integer, parameter :: nmax = 500
 real(8), parameter :: tiny = 1.0d-20
 real(8), parameter :: d1 = 1.d0, d0 = 0.d0

! dimensions:
 integer, intent(in) :: np, n

! scratch:
 real(8), intent(inout) :: a(2,np,n)

! output:
 integer, intent(out) :: indx(n)
 real(8), intent(out) :: d

! local:
 integer :: i,imax,j,k
 real(8), dimension(2) :: aamax,dum,sum, ddtmp
 real(8) :: vv(2,nmax)  ! array in stack

! function:
 logical :: dd128_ge

 imax = -99
 d=d1
 do i=1,n
  aamax(1) = d0; aamax(2) = d0
  do j=1,n
   if ( abs(a(1,i,j)).gt.aamax(1) .or. &
       (abs(a(1,i,j)).eq.aamax(1) .and. abs(a(2,i,j)).gt.aamax(2))) then
    aamax(1)=abs(a(1,i,j)); aamax(2)=abs(a(2,i,j)) 
   end if
  end do
  if (aamax(1).eq.d0) then
   stop 'singular matrix in ludcmp' !No nonzero largest element.
  end if
  ddtmp(1) = d1; ddtmp(2) = d0
  call dd128_div(vv(1,i),ddtmp,aamax)
 end do
 do j=1,n
  do i=1,j-1 
   sum(1) = a(1,i,j); sum(2) = a(2,i,j)
   do k=1,i-1
    call dd128_mul(ddtmp,a(1,i,k),a(1,k,j))
    call dd128_sub_assign(sum,ddtmp)
   enddo
   a(1,i,j) = sum(1); a(2,i,j) = sum(2)
  enddo
  aamax(1) = d0; aamax(2) = d0
  do i=j,n 
   sum(1) = a(1,i,j); sum(2) = a(2,i,j)
   do k=1,j-1
    call dd128_mul(ddtmp,a(1,i,k),a(1,k,j))
    call dd128_sub_assign(sum,ddtmp)
   enddo
   a(1,i,j) = sum(1); a(2,i,j) = sum(2)
   ddtmp(1) = abs(sum(1)); ddtmp(2) = abs(sum(2))
   call dd128_mul(dum,vv(1,i),ddtmp)
   if (dd128_ge(dum,aamax)) then
    imax=i
    aamax(1) = dum(1); aamax(2) = dum(2)
   endif
  enddo
  if (j.ne.imax)then
   do k=1,n 
    dum(1) = a(1,imax,k); dum(2) = a(2,imax,k)
    a(1,imax,k)=a(1,j,k); a(2,imax,k)=a(2,j,k)
    a(1,j,k) = dum(1); a(2,j,k) = dum(2)
   enddo
   d=-d
   vv(1,imax) = vv(1,j); vv(2,imax) = vv(2,j)
  endif
  indx(j)=imax 
  if (a(1,j,j).eq.d0) then
   a(1,j,j)=tiny; a(2,j,j) = d0
  end if
!If the pivot element is zero the matrix is singular (at least to the precision of the al- gorithm). For some applications on singular matrices, it is desirable to substitute TINY for zero.
!Sample page from NUMERICAL RECIPES IN FORTRAN 77: THE ART OF SCIENTIFIC COMPUTING (ISBN 0-521-43064-X)
  if(j.ne.n) then 
   ddtmp(1) = d1; ddtmp(2) = d0
   call dd128_div(dum,ddtmp,a(1,j,j))
   do i=j+1,n 
    call dd128_mul_assign(a(1,i,j),dum)
   enddo
  endif
 enddo

end subroutine dd128_ludcmp
!==============================================================================!
!==============================================================================!
subroutine dd128_nrm2(ddnrm2,n,x,incx)
!------------------------------------------------------------------------------!
!
!  purpose
!  =======
!
!  dd128_nrm2 returns the euclidean norm of a vector via the function
!  name, so that
!
!     dd128_nrm2 := sqrt( x'*x )
!
!
!  -- this version written on 25-october-1982.
!     modified on 14-october-1993 to inline the call to dlassq.
!     sven hammarling, nag ltd.
!
!------------------------------------------------------------------------------!

 implicit none

! parameters:
 real(8), parameter :: one = 1.d0, zero = 0.d0

! input:
 integer, intent(in) :: incx,n
 real(8), intent(in) :: x(2,(n-1)*abs(incx)+1)

! output:
 real(8), dimension(2), intent(out) :: ddnrm2

! local:
 integer :: ix
 real(8), dimension(2) :: absxi,scale,ssq, ddtmp1, ddtmp2

! functions:
 logical :: dd128_lt

 if (n.lt.1 .or. incx.lt.1) then
  ddnrm2(1) = zero; ddnrm2(2) = zero 
 else if (n.eq.1) then
  ddnrm2(1) = abs(x(1,1)); ddnrm2(2) = abs(x(2,1))
 else
  scale(1) = zero; scale(2) = zero
  ssq(1) = one; ssq(2) = zero
  do ix = 1,1 + (n-1)*incx,incx
   if (x(1,ix).ne.zero .or. x(2,ix).ne.zero) then
    absxi(1) = abs(x(1,ix)); absxi(2) = abs(x(2,ix))
    if (dd128_lt(scale,absxi)) then
     call dd128_div(ddtmp1,scale,absxi)
     call dd128_sqr_assign(ddtmp1)
     call dd128_mul_assign(ddtmp1,ssq)
     ddtmp2(1) = one; ddtmp2(2) = zero
     call dd128_add(ssq,ddtmp2,ddtmp1)
     scale(1) = absxi(1); scale(2) = absxi(2)
    else
     call dd128_div(ddtmp1,absxi,scale)
     call dd128_sqr_assign(ddtmp1)
     call dd128_add_assign(ssq,ddtmp1)
    end if
   end if
  enddo
  call dd128_sqrt(ddtmp1,ssq)
  call dd128_mul(ddnrm2,scale,ddtmp1)
 end if

end subroutine dd128_nrm2
!==============================================================================!
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at 
!            http://www.netlib.org/lapack/explore-html/ 
!
!  Definition:
!  ===========
!
!       LOGICAL FUNCTION LSAME(CA,CB)
! 
!       .. Scalar Arguments ..
!       CHARACTER CA,CB
!       ..
!  
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!> LSAME returns .TRUE. if CA is the same letter as CB regardless of
!> case.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> \param[in] CA
!> \verbatim
!>          CA is CHARACTER*1
!> \endverbatim
!>
!> \param[in] CB
!> \verbatim
!>          CB is CHARACTER*1
!>          CA and CB specify the single characters to be compared.
!> \endverbatim
!
!  Authors:
!  ========
!
!> \author Univ. of Tennessee 
!> \author Univ. of California Berkeley 
!> \author Univ. of Colorado Denver 
!> \author NAG Ltd. 
!
!> \date November 2011
!
!> \ingroup aux_blas
!
!  =====================================================================
      LOGICAL FUNCTION dd128_lsame(CA,CB)
!
!  -- Reference BLAS level1 routine (version 3.1) --
!  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2011
!
!     .. Scalar Arguments ..
      CHARACTER ca,cb
!     ..
!
! =====================================================================
!
!     .. Intrinsic Functions ..
      INTRINSIC ichar
!     ..
!     .. Local Scalars ..
      INTEGER inta,intb,zcode
!     ..
!
!     Test if the characters are equal
!
      dd128_lsame = ca .EQ. cb
      IF (dd128_lsame) RETURN
!
!     Now test for equivalence if both characters are alphabetic.
!
      zcode = ichar('Z')
!
!     Use 'Z' rather than 'A' so that ASCII can be detected on Prime
!     machines, on which ICHAR returns a value with bit 8 set.
!     ICHAR('A') on Prime machines returns 193 which is the same as
!     ICHAR('A') on an EBCDIC machine.
!
      inta = ichar(ca)
      intb = ichar(cb)
!
      IF (zcode.EQ.90 .OR. zcode.EQ.122) THEN
!
!        ASCII is assumed - ZCODE is the ASCII code of either lower or
!        upper case 'Z'.
!
          IF (inta.GE.97 .AND. inta.LE.122) inta = inta - 32
          IF (intb.GE.97 .AND. intb.LE.122) intb = intb - 32
!
      ELSE IF (zcode.EQ.233 .OR. zcode.EQ.169) THEN
!
!        EBCDIC is assumed - ZCODE is the EBCDIC code of either lower or
!        upper case 'Z'.
!
          IF (inta.GE.129 .AND. inta.LE.137 .OR.                        &
     &        inta.GE.145 .AND. inta.LE.153 .OR.                        &
     &        inta.GE.162 .AND. inta.LE.169) inta = inta + 64
          IF (intb.GE.129 .AND. intb.LE.137 .OR.                        &
     &        intb.GE.145 .AND. intb.LE.153 .OR.                        &
     &        intb.GE.162 .AND. intb.LE.169) intb = intb + 64
!
      ELSE IF (zcode.EQ.218 .OR. zcode.EQ.250) THEN
!
!        ASCII is assumed, on Prime machines - ZCODE is the ASCII code
!        plus 128 of either lower or upper case 'Z'.
!
          IF (inta.GE.225 .AND. inta.LE.250) inta = inta - 32
          IF (intb.GE.225 .AND. intb.LE.250) intb = intb - 32
      END IF
      dd128_lsame = inta .EQ. intb
!
      RETURN
!
!     End of dd128_LSAME
!
      END

!> \brief \b XERBLA
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at 
!            http://www.netlib.org/lapack/explore-html/ 
!
!  Definition:
!  ===========
!
!       SUBROUTINE XERBLA( SRNAME, INFO )
! 
!       .. Scalar Arguments ..
!       CHARACTER*(*)      SRNAME
!       INTEGER            INFO
!       ..
!  
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!> XERBLA  is an error handler for the LAPACK routines.
!> It is called by an LAPACK routine if an input parameter has an
!> invalid value.  A message is printed and execution stops.
!>
!> Installers may consider modifying the STOP statement in order to
!> call system-specific exception-handling facilities.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> \param[in] SRNAME
!> \verbatim
!>          SRNAME is CHARACTER*(*)
!>          The name of the routine which called XERBLA.
!> \endverbatim
!>
!> \param[in] INFO
!> \verbatim
!>          INFO is INTEGER
!>          The position of the invalid parameter in the parameter list
!>          of the calling routine.
!> \endverbatim
!
!  Authors:
!  ========
!
!> \author Univ. of Tennessee 
!> \author Univ. of California Berkeley 
!> \author Univ. of Colorado Denver 
!> \author NAG Ltd. 
!
!> \date November 2011
!
!> \ingroup aux_blas
!
!  =====================================================================
      SUBROUTINE dd128_xerbla( SRNAME, INFO )
!
!  -- Reference BLAS level1 routine (version 3.4.0) --
!  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2011
!
!     .. Scalar Arguments ..
      CHARACTER*(*)      SRNAME
      INTEGER            INFO
!     ..
!
! =====================================================================
!
!     .. Intrinsic Functions ..
      INTRINSIC          len_trim
!     ..
!     .. Executable Statements ..
!
      WRITE( *, fmt = 9999 )srname( 1:len_trim( srname ) ), info
!
      stop
!
 9999 FORMAT( ' ** On entry to ', a, ' parameter number ', i2, ' had ', &
     &      'an illegal value' )
!
!     End of XERBLA
!
      END
!==============================================================================!
!==============================================================================!
subroutine dd128_scal(n,dda,ddx,incx)
!------------------------------------------------------------------------------!
!
! double-double precision version of Netlib BLAS dscal
!
!     scales a vector by a constant.
!     uses unrolled loops for increment equal to one.
!     jack dongarra, linpack, 3/11/78.
!     modified to correct problem with negative increment, 8/21/90.
!
!------------------------------------------------------------------------------!

 implicit none

! dimension:
 integer, intent(in) :: n

! input:
 integer, intent(in) :: incx
 real(8), dimension(2), intent(in) :: dda

! in/output:
 real(8), intent(inout) :: ddx(2,(n-1)*abs(incx)+1)

! local:
 integer :: i,ix,m,mp1

 if ( n.le.0 ) return
 if ( incx.eq.1 ) go to 20
!        code for increment not equal to 1

 ix = 1
 if(incx.lt.0)ix = (-n+1)*incx + 1
 do i = 1,n
  !dx(ix) = da*dx(ix)
  call dd128_mul_assign(ddx(1,ix),dda)
  ix = ix + incx
 end do
 return
!        code for increment equal to 1
!
!
!        clean-up loop
 20 m = mod(n,5)
 if( m .eq. 0 ) go to 40
 do i = 1,m
  !dx(i) = da*dx(i)
  call dd128_mul_assign(ddx(1,i),dda)
 end do
 if( n .lt. 5 ) return
 40 mp1 = m + 1
 do i = mp1,n,5
  !dx(i) = da*dx(i)
  !dx(i + 1) = da*dx(i + 1)
  !dx(i + 2) = da*dx(i + 2)
  !dx(i + 3) = da*dx(i + 3)
  !dx(i + 4) = da*dx(i + 4)
  call dd128_mul_assign(ddx(1,i  ),dda)
  call dd128_mul_assign(ddx(1,i+1),dda)
  call dd128_mul_assign(ddx(1,i+2),dda)
  call dd128_mul_assign(ddx(1,i+3),dda)
  call dd128_mul_assign(ddx(1,i+4),dda)
 end do

end subroutine dd128_scal
!==============================================================================!
