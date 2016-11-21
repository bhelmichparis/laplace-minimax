!==============================================================================!
 interface 
  subroutine laplace_minimax(errmax,xpnts,wghts,nlap,ymin,ymax,&
  &                          mxiter,iprint,stepmx,tolrng,tolpar,delta,afact,&
  &                          do_rmsd,do_init)
 
   integer, intent(in) :: nlap
   real(8), intent(in) :: ymin, ymax
 
   logical, intent(in), optional :: do_rmsd, do_init
   integer, intent(in), optional :: mxiter, iprint
   real(8), intent(in), optional :: stepmx, tolrng, tolpar, delta, afact
 
   real(8),intent(inout) :: errmax
   real(8),intent(inout) :: xpnts(nlap), wghts(nlap)
  
  end subroutine laplace_minimax

 end interface
!==============================================================================!
