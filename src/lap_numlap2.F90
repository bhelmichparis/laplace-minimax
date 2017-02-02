!==============================================================================!
subroutine lap_numlap2(errbnd,nlap,tolerr,rnge)
!------------------------------------------------------------------------------!
!
! Read laplace data file of pre-tabulated Laplace exponents and weights
!  of Takatsuka, Ten-no, and Hackbusch.
! Those initial values depend on the interval.
!
!------------------------------------------------------------------------------!

 implicit none

#include "consts.h"

! constants:
 logical, parameter :: locdbg = .true.
 character(len=*), parameter :: chrdbg = 'lap_numlap>'

 integer, parameter :: luinit = 11

! input:
 real(8), intent(in)   :: rnge(2), tolerr

! output:
 integer, intent(out)  :: nlap
 real(8), intent(out)  :: errbnd(2)

! local:
 character(len=80) :: line
 character(len=512) :: rootdir, filerr
 integer :: ios, itmp1, itmp2
 logical :: found_token
 real(8) :: rlen, dtmp
 
 if (locdbg) write(istdout,"(a)") chrdbg//"enter lap_numlap2 ..."

 ! get absolute path to data files
 call getenv("LAPLACE_ROOT",rootdir)
 rootdir = adjustl(rootdir)
 filerr  = trim(rootdir)//"/data/init_error.txt"

 rlen = rnge(2)/rnge(1)

 open(unit=luinit,file=filerr,status='old',action='read')

 found_token = .false.

 do while (.not.found_token)

  read(luinit,'(a)',iostat=ios) line
  if (ios.eq.-1) exit

  line = adjustl(line)
  line = trim(line)
  
  ! check for comment line
  if (line(1:4).ne."1_xk") cycle

  read(line(8:8),"(I1)") itmp1
  read(line(10:11),"(I2)") itmp2
  dtmp = real(itmp1,8)*10.d0**itmp2

  if (dtmp.gt.rlen) then
   read(line(12:21),"(ES10.3)") errbnd(1)
   read(line(5:6),"(I2)") nlap
   if (errbnd(1).le.tolerr) then
    found_token = .true.
   end if
  end if

 end do

 close(unit=luinit)

 errbnd(2) = d0

 if (locdbg) then
  write(istdout,*) chrdbg//"rlen  :",rlen
  write(istdout,*) chrdbg//"nlap  :",nlap
  write(istdout,*) chrdbg//"errbnd:",errbnd
 end if

 if (.not.found_token) then
  write(istdout,'(/a,/,a,e10.3,a,/,a,e10.3,a/)') &
    '          W A R N I N G    ','accuracy too strict:',tolerr,&
    ' not in the range of pre-tabulated values!',' Take ',errbnd(1),'instead!'
 end if
 
end subroutine lap_numlap2
!==============================================================================!
