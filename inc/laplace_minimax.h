!==============================================================================!
 interface 
  subroutine laplace_minimax(xpnts,wghts,nlap,eig,neig,istro,iendo,istrv,iendv,&
  &                          mxiter,iprint,stepmx,tolrng,tolpar,delta,afact,&
  &                          do_rmsd)
 
   integer, intent(in) :: nlap, neig
   integer, intent(in) :: istro, istrv, iendo, iendv
   real(8), intent(in) :: eig(neig)
 
   logical, intent(in), optional :: do_rmsd
   integer, intent(in), optional :: mxiter, iprint
   real(8), intent(in), optional :: stepmx, tolrng, tolpar, delta, afact
 
   real(8),intent(inout) :: xpnts(nlap), wghts(nlap)
  
  end subroutine laplace_minimax

 end interface
!==============================================================================!
