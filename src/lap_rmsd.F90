!==============================================================================!
subroutine lap_rmsd(errmsd,eig,xpnts2,wghts2,istro,iendo,istrv,iendv,neig,nlap)
!------------------------------------------------------------------------------!
!
! compute the RMSD value of the Laplace error distribution function
!
!  preliminary brute-force implementation
!
!
! BHP, summer 2015
!
!------------------------------------------------------------------------------!
 
 implicit none

#include "consts.h"

! constants:
 character(len=*), parameter :: chrdbg = 'lap_rmsd>'
 logical, parameter :: locdbg = .false.

! dimensions:
 integer, intent(in) :: nlap, neig

! input:
 integer, intent(in) :: istro, iendo, istrv, iendv
 real(8), intent(in) :: eig(neig), xpnts2(2,nlap), wghts2(2,nlap)

! output:
 real(8), intent(out) :: errmsd

! local:
 integer :: moa, mob, imo, jmo, ilap, noct, nvirct
 real(8) :: err(2), eigbja(2), eigbj(2), denom(2), errtmp(2), dtmp(2), dtmp2(2)

 if (locdbg) write(istdout,"(a)") "entered lap_rmsd ..."

 err(1) = d0
 err(2) = d0

 do mob = istrv,iendv
  do jmo = istro,iendo
   call dd128_two_sub(eigbj,eig(mob),eig(jmo))
   do moa = istrv,iendv
    dtmp(1) = eig(moa)
    dtmp(2) = d0
    call dd128_add(eigbja,eigbj,dtmp)
    do imo = istro,iendo
     dtmp(1) = eig(imo)
     dtmp(2) = d0
     call dd128_sub(denom,eigbja,dtmp)
     dtmp(1) = d1
     dtmp(2) = d0
     call dd128_div(errtmp,dtmp,denom)
     do ilap = 1,nlap
      call dd128_mul(dtmp,xpnts2(1,ilap),denom)
      dtmp(1) = -dtmp(1)
      dtmp(2) = -dtmp(2)
      call dd128_exp(dtmp2,dtmp)
      call dd128_mul_assign(dtmp2,wghts2(1,ilap))
      call dd128_sub_assign(errtmp,dtmp2)
     end do
     call dd128_sqr(err,errtmp)
    end do
   end do
  end do
 end do

 noct   = iendo-istro+1
 nvirct = iendv-istrv+1
 dtmp(1)  = real(noct*noct,8)
 dtmp2(1) = real(nvirct*nvirct,8)

 call dd128_two_prod(denom,dtmp(1),dtmp2(1))
 call dd128_div(errtmp,err,denom)
 call dd128_sqrt(err,errtmp)

 errmsd = err(1)

 if (locdbg) write(istdout,*) chrdbg,'RMSD error:',errmsd

 if (locdbg) write(istdout,"(a)") "... left lap_rmsd"

end subroutine lap_rmsd
!==============================================================================!
