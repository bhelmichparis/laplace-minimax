!==============================================================================!
subroutine lap_rmsd(errmsd,eig,xpnts,wghts,istro,iendo,istrv,iendv,neig,nlap)
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
 real(8), intent(in) :: eig(neig), xpnts(nlap), wghts(nlap)

! output:
 real(8), intent(out) :: errmsd

! local:
 integer :: moa, mob, imo, jmo, ilap, noct, nvirct
 real(8) :: eigbja, eigbj, denom, errtmp, dtmp

 if (locdbg) write(istdout,"(a)") "entered lap_rmsd ..."

 errmsd = d0
 do mob = istrv,iendv
  do jmo = istro,iendo
   eigbj = eig(mob)-eig(jmo)
   do moa = istrv,iendv
    eigbja = eigbj+eig(moa)
    do imo = istro,iendo
     denom = eigbja-eig(imo)
     errtmp = d1 / denom
     do ilap = 1,nlap
      dtmp = min(xpnts(ilap)*denom,40.d0)
      errtmp = errtmp - wghts(ilap)*exp(-dtmp)
     end do
     errmsd = errmsd + errtmp*errtmp
    end do
   end do
  end do
 end do

 noct   = iendo-istro+1
 nvirct = iendv-istrv+1
 errmsd = ((errmsd/real(noct*noct))/real(nvirct*nvirct))
 errmsd = sqrt(errmsd)

 if (locdbg) write(istdout,*) chrdbg,'RMSD error:',errmsd

 if (locdbg) write(istdout,"(a)") "... left lap_rmsd"

end subroutine lap_rmsd
!==============================================================================!
