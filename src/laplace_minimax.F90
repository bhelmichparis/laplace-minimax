!==============================================================================!
subroutine laplace_minimax(errmax,xpnts,wghts,nlap,ymin,ymax,&
                           mxiter,iprint,stepmx,tolrng,tolpar,tolerr,&
                           delta,afact,do_rmsd,do_init,do_nlap)
!------------------------------------------------------------------------------!
!
! Routine computes the numerical Laplace transformation of the orbital energy 
! denominator:
!
!   1 / x = \sum_k  w_k * exp( -a_k * x )
!
! The Remez algorithm is employed to compute the minimax solution.
! The Newton-Maehly algorithm is used to find all extremum points. Please
! cite the following paper if you use the library:
!
! (1) A. Takatsuka et al., 
!      The Journal of Chemical Physics 129, 044112 (2008); 
!       doi: 10.1063/1.2958921
!
! (2) B. Helmich-Paris and L. Visscher,
!      Journal of Computational Physics 321, 927 - 931 (2016);
!       doi: 10.1016/j.jcp.2016.06.011
!
! 
!  variable             explanation
!  --------             -----------
!
!   xpnts                (output) double precision array with 
!                          Laplace exponents (a_k)
!
!   wghts                (output) double presicion array with 
!                          Laplace weights   (w_k)
!
!   ymin                 (input) lower bound of orbital energy denominator
!
!   ymax                 (input) upper bound of orbital energy denominator
!
!   nlap                 (in/output)  number of quadrature (Laplace) points
!
!   mxiter               (optional) maximum number of iterations. Used for each
!                           of the iterative prodecures (Remez + Newton(-Maehly))
!
!   iprint               (optional) print level
!
!   stepmx               (optional) maximum step length used for each of the 
!                           Newton type procedures
!
!   tolrng               (optional) tolerance threshold for the Newton-Maehly
!                           procedure that determines the extremum points
!
!   tolpar               (optional) tolerance threshold for the Newton procedure
!                           that computes the Laplace parameters at each extremum
!                           point
!
!   tolerr               (optional) tolerance threshold for the maximum quadrature
!                           error obtained by the minimax algorithm
!
!   delta                (optional) shift parameter for initializating the next
!                           extremum point to be determined by Newton-Maehly
!
!   afact                (optional) factor for the line search algorithm used
!                           in combination with the Newton algorithm to determine
!                           the Laplace parameters
!
!   do_rmsd              (optional) compute an RMS error after the optimization
!                           of the Laplace parameters
!
!   do_init              (optional) initialize Laplace parameters with pre-tabu-
!                           lated values. Otherwise start with what is available
!                           on xpnts and wghts
!
!   do_nlap              (optional) obtain the number of quadrature points by
!                           comparing tabulated errors to threshold
!
!
!
! Benjamin Helmich-Paris, summer 2015
!
!------------------------------------------------------------------------------!

 implicit none

#include "consts.h"

! constants:
 logical, parameter :: locdbg = .false.
 character(len=*), parameter :: chrdbg = 'laplace_minimax>'
 
! input:
 real(8), intent(in) :: ymin, ymax

! optional arguments:
 logical, intent(in), optional :: do_rmsd, do_init, do_nlap
 integer, intent(in), optional :: mxiter, iprint
 real(8), intent(in), optional :: stepmx, tolrng, tolpar, delta, afact, tolerr

! in/output:
 integer, intent(inout) :: nlap
 real(8), intent(inout) :: xpnts(*), wghts(*)

!output:
 real(8), intent(out) :: errmax

! local scalars:
 logical :: do_rmsd0, do_init0, do_nlap0
 integer :: iter, niter, nxpts, ilap, iprnt0, mxitr0
 real(8) :: rnge(2), errmsd, afct0, stpmx0, delt0, tlrng0, tlpar0, tlerr0

! local arrays:
 integer :: ipiv(2*mxlap+1)
 real(8) :: xpts(2,2*mxlap+1), xpnts2(2,mxlap), wghts2(2,mxlap), &
            func(2,2*mxlap+1), &
            xold(2,2*mxlap+1), xnew(2,2*mxlap+1), delx(2,2*mxlap+1), &
            grad(2,2*mxlap+1), errbnd(2) 

! LV2023: jaco defined as allocatable instead of static to avoid compiler warnings
 real(8), allocatable :: jaco(:,:,:) 

 allocate(jaco(2,2*mxlap+1,2*mxlap+1))

 ! process optional arguments
 if (present(mxiter)) then
  mxitr0 = mxiter
 else
  mxitr0 = 50
 end if

 if (present(iprint)) then
  iprnt0 = iprint
 else
  iprnt0 = 0
 end if

 if (present(stepmx)) then
  stpmx0 = stepmx
 else
  stpmx0 = 0.3D0
 end if

 if (present(tolrng)) then
  tlrng0 = tolrng
 else
  tlrng0 = 1.D-10
 end if

 if (present(tolpar)) then
  tlpar0 = tolpar
 else
  tlpar0 = 1.D-15
 end if

 if (present(delta)) then
  delt0 = delta
 else
  delt0 = 1.d-6
 end if

 if (present(afact)) then
  afct0 = afact
 else
  afct0 = 1.d-4
 end if

 if (present(do_rmsd)) then
  do_rmsd0 = do_rmsd
 else
  do_rmsd0 = .false.
 end if

 if (present(do_init)) then
  do_init0 = do_init
 else
  do_init0 = .true.
 end if

 if (present(do_nlap)) then
  do_nlap0 = do_nlap
  if (do_nlap .and. (.not.(present(tolerr)))) then
   write(istdout,"(/a/)") "To determine the number quadrature points, we need accuracy threshold."
   stop "Bye bye !"
  end if
  if (do_nlap0) then
   tlerr0 = tolerr
  else
   tlerr0 = -9.D+99
  end if
 else
  do_nlap0 = .false.
 end if

 if (locdbg .or. (iand(iprnt0,8).eq.8)) write(istdout,"(a)") "entered laplace_minimax .."

 ! print pre-amble
 if (iand(iprnt0,1).eq.1) then
  write(istdout,'(//,a,//)')             '     Numerical    L A P L A C E    quadrature'
 end if

 ! print range
 if (iand(iprnt0,1).eq.1) then
  write(istdout,'(a,2(1x,e12.3))')     '     range or orbital energy denominator:',ymin,ymax
 end if
 
 ! numerical quadrature is done within bounds [1,R]
 rnge(1) = d1
 rnge(2) = ymax / ymin

 if (do_nlap0) then
  call lap_numlap(errbnd(1),nlap,tlerr0,rnge)
 else
  ! initialize maximum error
  errbnd(1) = -10.**(-nlap)
 end if
 errbnd(2) = d0

 ! print number of Laplace points
 if (iand(iprnt0,1).eq.1) then
  write(istdout,'(a,1x,i2)') '             number of quadrature points:',nlap
 end if

 ! either start values for predefined boundaries from file 
 if (do_init0) then
  call lap_rddata(xpnts2,wghts2,rnge,nlap)
  call lap_rderror(errbnd,rnge,nlap)
 ! .. or use existing parameters in range [x_min; x_max]
 else
  do ilap = 1,nlap
   xpnts2(1,ilap) = xpnts(ilap) * ymin
   xpnts2(2,ilap) = d0
   wghts2(1,ilap) = wghts(ilap) * ymin
   wghts2(2,ilap) = d0
  end do
 end if

 ! number of extremum points
 nxpts  = 2*nlap+1

 if (locdbg .or. (iand(iprnt0,2).eq.2)) then
  write(istdout,*) chrdbg,"range:",rnge
 end if

 if (locdbg .or. (iand(iprnt0,4).eq.4)) then
  write(istdout,*) chrdbg,"initial max. error:",errbnd(1:2)
  write(istdout,*) chrdbg,"initial exponents:"
  write(istdout,"(2(E55.40,1x))") xpnts2(1:2,1:nlap)
 
  write(istdout,*) chrdbg,"inital weights:"
  write(istdout,"(2(E55.40,1x))") wghts2(1:2,1:nlap)

  write(istdout,*) chrdbg,"tlrng0:",tlrng0
  write(istdout,*) chrdbg,"tlpar0:",tlpar0
  write(istdout,*) chrdbg,"mxitr0:",mxitr0
  write(istdout,*) chrdbg,"stpmx0:",stpmx0
 end if

 if (iand(iprnt0,8).eq.8) then
  write(istdout,"(/a)") "----------------------"
  write(istdout,"(a)")  "  # MACRO     # MICRO "
  write(istdout,"(a)")  "----------------------"
 end if

 do iter = 1, mxitr0

  ! compute 2k-1 extremum points of error distribution
  call lap_maehly(niter,xpts(1,2),&
                  rnge,mxitr0,tlrng0,xpnts2,wghts2,stpmx0,delt0,&
                  nxpts-2,nlap)

  ! boundary points are global extremum points
  xpts(1,1)     = rnge(1)
  xpts(2,1)     = d0
  xpts(1,nxpts) = rnge(2)
  xpts(2,nxpts) = d0
 
  ! optimize Laplace parameter
  call lap_paraopt(niter,wghts2,xpnts2,errbnd,&
                   ipiv,jaco,func,xold,xnew,delx,grad,&
                   xpts,nxpts,mxitr0,tlpar0,stpmx0,afct0,&
                   nlap,nxpts)

  ! report number of iterations
  if (iand(iprnt0,8).eq.8) write(istdout,"(2(1x,i5))") iter,niter

  if (niter.eq. 1) exit

 end do

 ! transform back to orignal range [x_min;x_max]
 do ilap = 1,nlap
  call dd128_div_doub_assign(xpnts2(1,ilap),ymin)
  call dd128_div_doub_assign(wghts2(1,ilap),ymin)
 end do

 ! compute the RMSD error
 if (do_rmsd0) then
  write(istdout,'(/,a,/)') "         WARNING"
  write(istdout,'(a)') "  RMSD calculation disabled!"
  errmsd = 9.d99
!  call lap_rmsd(errmsd,eig,xpnts2,wghts2,istro,iendo,istrv,iendv,neig,nlap)
 end if

 do ilap = 1,nlap
  xpnts(ilap) = xpnts2(1,ilap)
  wghts(ilap) = wghts2(1,ilap)
 end do

 ! pass maximum error to calling routine
 errmax = errbnd(1)

 ! print output
 if (iand(iprnt0,1).eq.1) then
  write(istdout,'(a,2(1x,e12.3))')     '  maximum absolute error of distribution:',abs(errbnd(1))
  if (do_rmsd0) &
   write(istdout,'(a,2(1x,e12.3))')    '              RMSD error of distribution:',errmsd
  write(istdout,'(/a)')      '                  exponents              weights'
  write(istdout,'(a)')       '    ================================================='
  do ilap = 1,nlap
   write(istdout,'(5x,i3,2(1x,f20.10))') ilap,xpnts(ilap),wghts(ilap)
  end do
  write(istdout,'(/)')
  call flush(istdout)
 end if

 if (locdbg .or. (iand(iprnt0,4).eq.4)) write(istdout,"(a)") "left laplace_minimax ..."

end subroutine laplace_minimax
!==============================================================================!
