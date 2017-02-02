!==============================================================================!
subroutine lap_rderror(errbnd,rnge,nlap)
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
 character(len=*), parameter :: chrdbg = 'lap_rderror>'

 integer, parameter :: luinit = 11

! dimensions:
 integer, intent(in)    :: nlap

! input:
 real(8), intent(in)    :: rnge(2)

! in/output:
 real(8), intent(inout) :: errbnd(2)

! local:
 integer :: iopt, ios, itmp1, itmp2
 logical :: found_token, lower
 character(len=5)  :: ctmp
 character(len=11) :: token, cdummy
 character(len=80) :: line
 character(len=512) :: rootdir, filerr
 real(8) :: rlen, dtmp
 
 if (locdbg) write(istdout,"(a)") chrdbg//"enter lap_rderror ..."

 ! get absolute path to data files
 call getenv("LAPLACE_ROOT",rootdir)
 rootdir = adjustl(rootdir)
 filerr  = trim(rootdir)//"/data/init_error.txt"

 rlen = rnge(2)/rnge(1)

 ! prepare token for reading
 token(1:4) = '1_xk'
 write(token(5:6),'(I2.2)') nlap
 
 open(unit=luinit,file=filerr,status='old',action='read')

 found_token = .false.

 lower = .false.

 do while (.not.found_token)

  ! check for tokens with number of Laplace points
  read(luinit,'(a)',iostat=ios) line
  if (ios.eq.-1) exit
  iopt = index(line,token(1:6))

  ! find tokens with upper bound
  if (iopt.gt.0) then
   line = adjustl(line)
   line = trim(line)
   read(line(8:8),*) itmp1
   read(line(10:12),*) itmp2
   dtmp = real(itmp1)*10.d0**itmp2
   if (dtmp.le.rlen) then
    ctmp(1:5) = line(7:11)
    lower = .true.
   else
    if (.not.lower) then
     token(7:11) = line(7:11)
    else
     token(7:11) = ctmp(1:5)
    end if
    found_token = .true.
   end if
  end if

 end do

 if (locdbg) write(istdout,*) chrdbg, "token:", token(1:10)

 if (.not.found_token) then
  write(istdout,'(/a,/,a,e10.3,a,/,a,e10.3,a/)') &
    '          W A R N I N G    ','Range of denominator 1.0 -- ',rlen,&
    ' not in the range of pre-tabulated values!',' Take ',dtmp,'instead!'
  token(7:11) = ctmp(1:5)
 end if

 rewind(luinit)

 do
  ! check now for the full token
  read(luinit,'(a)',iostat=ios) line
  if (ios.eq.-1) exit
  iopt = index(line,token(1:10))

  if (iopt.gt.0) then
   read(line,"(a10,1x,ES10.3)") cdummy, errbnd(1)
   errbnd(2) = d0
   exit
  end if
 end do

 close(unit=luinit)
 
end subroutine lap_rderror
!==============================================================================!
