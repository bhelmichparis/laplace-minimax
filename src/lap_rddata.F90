!==============================================================================!
subroutine lap_rddata(xpnts,wghts,rnge,nlap)
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
 character(len=*), parameter :: chrdbg = 'lap_rddata>'

 integer, parameter :: luinit = 11

! dimensions:
 integer, intent(in)    :: nlap

! input:
 real(8), intent(in)    :: rnge(2)

! in/output:
 real(8), intent(inout) :: xpnts(2,nlap), wghts(2,nlap)

! local:
 integer :: iopt, ios, itmp1, itmp2, ichr, jopt, nlapa, nlapo
 logical :: found_token, lower
 character(len=5)  :: ctmp
 character(len=11) :: token
 character(len=80) :: line
 character(len=512) :: rootdir, fildat !, filerr
 real(8) :: rlen, dtmp
 
 if (locdbg) write(istdout,"(a)") chrdbg//"enter lap_rddata ..."

 ! get absolute path to data files
 call getenv("LAPLACE_ROOT",rootdir)
 rootdir = adjustl(rootdir)
 fildat  = trim(rootdir)//"/data/init_para.txt"

 rlen = rnge(2)/rnge(1)

 ! prepare token for reading
 token(1:4) = '1_xk'
 write(token(5:6),'(I2.2)') nlap
 
 open(unit=luinit,file=fildat,status='old',action='read')

 found_token = .false.
 lower = .false.

 token_loop: do while (.not.found_token)

  ! check for tokens with number of Laplace points
  read(luinit,'(a)',iostat=ios) line
  if (ios.eq.-1) exit token_loop
  iopt = index(line,token(1:6))

  ! find tokens with upper bound
  if (iopt.gt.0) then
   line = adjustl(line)
   line = trim(line)

   read(line(8:8),"(I1)") itmp1
   if (line(9:9).eq."E") then
    read(line(10:11),"(I2)") itmp2
    dtmp = real(itmp1)*10.d0**itmp2
   else
    read(line(9:11),"(I3)") itmp2
    dtmp = real(itmp1,8)+real(itmp2,8)/1000.d0
   end if

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

 end do token_loop

 if (locdbg) write(istdout,*) chrdbg, "token:",token(1:11)

 if (.not.found_token) then
  write(istdout,'(/a,/,a,e10.3,a,/,a,e10.3,a/)') &
    '          W A R N I N G    ','Range of denominator 1.0 -- ',rlen,&
    ' not in the range of pre-tabulated values!',' Take ',dtmp,'instead!'
  token(7:11) = ctmp(1:5)
 end if

 rewind(luinit)

 read_loop: do
  ! check now for the full token
  read(luinit,'(a)',iostat=ios) line
  if (ios.eq.-1) exit read_loop
  iopt = index(line,token(1:11))

  if (iopt.gt.0) then
   nlapo = 0; nlapa = 0
   do
    read(luinit,'(a)',iostat=ios) line
    if (ios.eq.-1) exit read_loop

    line = adjustl(line)
    line = trim(line)

    iopt = index(line,'omega')
    if (iopt>0) then
     nlapo = nlapo+1
     do ichr = 1,len(line)
      if (line(ichr:ichr).eq.' ') exit
     end do
     if (nlapo.gt.nlap) goto 300
     read(line(1:ichr),*) wghts(1,nlapo)
     wghts(2,nlapo) = d0
    end if

    jopt = index(line,'alpha')
    if (jopt>0) then
     nlapa = nlapa+1
     do ichr = 1,len(line)
      if (line(ichr:ichr).eq.' ') exit
     end do
     if (nlapa.gt.nlap) goto 200
     read(line(1:ichr),*) xpnts(1,nlapa)
     xpnts(2,nlapa) = d0
    end if

    if (iopt+jopt.eq.0) exit
   end do

   ! check consistency in number of Laplace points
   if (nlapa.ne.nlap) goto 200
   if (nlapo.ne.nlap) goto 300

  end if
 end do read_loop

 return

 close(unit=luinit)

 200 write(istdout,'(a,/,2a,/)') 'Number of Laplace points not equal to ',&
                           'number of exponents in file ',fildat
     goto 999

 300 write(istdout,'(a,/,2a,/)') 'Number of Laplace points not equal to ',&
                           'number of weights in file ',fildat
     goto 999

 999 close(unit=luinit)

 stop
 
end subroutine lap_rddata
!==============================================================================!
