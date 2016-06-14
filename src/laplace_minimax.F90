!==============================================================================!
subroutine laplace_minimax(xpnts,wghts,nlap,eig,neig,istro,iendo,istrv,iendv,&
                           mxiter,iprint,stepmx,tolrng,tolpar,delta,afact,&
                           do_rmsd)
!------------------------------------------------------------------------------!
!
! compute the numerical Laplace transformation of the orbital energy 
! denominator by using the Remez algorithm to compute the minimax solution.
! (see Takatsuka et al. JCP ...)
!
! BHP, summer 2015
!
!------------------------------------------------------------------------------!


 implicit none

#include "consts.h"

! constants:
 logical, parameter :: locdbg = .false.
 character(len=*), parameter :: chrdbg = 'laplace_minimax>'
 
! dimensions:
 integer, intent(in) :: nlap, neig

! input:
 integer, intent(in) :: istro, istrv, iendo, iendv
 real(8), intent(in) :: eig(neig)

! optional arguments:

 logical, intent(in), optional :: do_rmsd
 integer, intent(in), optional :: mxiter, iprint
 real(8), intent(in), optional :: stepmx, tolrng, tolpar, delta, afact

!output:
 real(8), intent(inout) :: xpnts(nlap), wghts(nlap)

! local scalars:
 logical :: do_rmsd0
 integer :: iter, niter, nxpts, ilap, iprnt0, mxitr0
 real(8) :: errbnd, rnge(2), bounds(2), elow, ehigh, ehomo, elumo, errmsd, &
            afct0, stpmx0, tlrng0, tlpar0, delt0

! local arrays:
 integer :: ipiv(2*mxlap+1)
 real(8) :: xpts(2,2*mxlap+1), xpnts2(2,mxlap), wghts2(2,mxlap), &
            jaco(2,2*mxlap+1,2*mxlap+1), func(2,2*mxlap+1), &
            xold(2,2*mxlap+1), xnew(2,2*mxlap+1), delx(2,2*mxlap+1), &
            grad(2,2*mxlap+1)

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

 if (locdbg .or. iprnt0 .gt. 8) write(istdout,"(a)") "entered laplace_minimax .."

 ! print pre-amble
 if (iprnt0.gt.0) then
  write(istdout,'(//,a,//)')             '     Numerical    L A P L A C E    quadrature'
 end if

 nxpts  = 2*nlap+1
 errbnd = -10.**(-nlap)

 elow  = eig(istro)
 ehomo = eig(iendo)
 elumo = eig(istrv)
 ehigh = eig(iendv)

 ! bounds of numerical quadrature [x_min,x_max]
 bounds(1) = d2*(elumo-ehomo)
 bounds(2) = d2*(ehigh-elow )

 ! numerical quadrature is done within bounds [1,R]
 rnge(1) = d1
 rnge(2) = bounds(2) / bounds(1)

 ! print range
 if (iprnt0 .gt. 0) then
  write(istdout,'(a,2(1x,e12.3))')     '  range or orbital energy denominator:',bounds(1:2)
 end if
 
 if (locdbg .or. iprnt0.gt.4) then
  write(istdout,*) chrdbg,"elow,ehomo,elumo,ehigh",elow,ehomo,elumo,ehigh
  write(istdout,*) chrdbg,"bounds:",bounds
  write(istdout,*) chrdbg,"range:",rnge
 end if

 !--------------------------------------------------!
 ! start values for predefined boundaries from file 
 !--------------------------------------------------!
 call lap_init(xpnts2,wghts2,rnge,nlap)

 if (locdbg .or. iprnt0 .gt. 8) then
  write(istdout,*) chrdbg,"initial max. error:",errbnd
  write(istdout,*) chrdbg,"initial exponents:"
  write(istdout,"(2(E55.40,1x))") xpnts2(1:2,1:nlap)
 
  write(istdout,*) chrdbg,"inital weights:"
  write(istdout,"(2(E55.40,1x))") wghts2(1:2,1:nlap)

  write(istdout,*) chrdbg,"tlrng0:",tlrng0
  write(istdout,*) chrdbg,"tlpar0:",tlpar0
  write(istdout,*) chrdbg,"mxitr0:",mxitr0
  write(istdout,*) chrdbg,"stpmx0:",stpmx0
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

  if (niter.eq. 1) exit

 end do

 ! transform back to orignal range [x_min;x_max]
 do ilap = 1,nlap
  call dd128_div_doub_assign(xpnts2(1,ilap),bounds(1))
  call dd128_div_doub_assign(wghts2(1,ilap),bounds(1))
 end do

 do ilap = 1,nlap
  xpnts(ilap) = xpnts2(1,ilap)
  wghts(ilap) = wghts2(1,ilap)
 end do

 ! compute the RMSD error
 if (do_rmsd0) &
 & call lap_rmsd(errmsd,eig,xpnts,wghts,istro,iendo,istrv,iendv,neig,nlap)

 ! print output
 if (iprnt0 .gt. 0) then
  write(istdout,'(a,2(1x,e12.3))')     '  maximum absolute error of distribution:',abs(errbnd)
  if (do_rmsd0) &
   write(istdout,'(a,2(1x,e12.3))')     '  RMSD error of distribution:            ',errmsd
  write(istdout,'(/a)')      '                  exponents              weights'
  write(istdout,'(a)')       '    ================================================='
  do ilap = 1,nlap
   write(istdout,'(5x,i3,2(1x,f20.10))') ilap,xpnts(ilap),wghts(ilap)
  end do
  write(istdout,'(/)')
  call flush(istdout)
 end if

 if (locdbg .or. iprnt0 .gt. 8) write(istdout,"(a)") "left laplace_minimax ..."

end subroutine laplace_minimax
!==============================================================================!
