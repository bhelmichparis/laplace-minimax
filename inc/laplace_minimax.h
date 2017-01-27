!==============================================================================!
 interface 
  subroutine laplace_minimax(errmax,xpnts,wghts,nlap,ymin,ymax,&
  &                          mxiter,iprint,stepmx,tolrng,tolpar,tolerr,&
  &                          delta,afact,do_rmsd,do_init,do_nlap)
 
   real(8), intent(in) :: ymin, ymax
 
   logical, intent(in), optional :: do_rmsd, do_init, do_nlap
   integer, intent(in), optional :: mxiter, iprint
   real(8), intent(in), optional :: stepmx, tolrng, tolpar, delta, afact, tolerr
 
   integer, intent(inout) :: nlap
   real(8), intent(inout) :: xpnts(*), wghts(*)
  
   real(8), intent(out) :: errmax

  end subroutine laplace_minimax

 end interface
!==============================================================================!
