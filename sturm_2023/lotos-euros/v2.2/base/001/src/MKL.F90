!#################################################################
!
! Interface to MKL routines.
!
!### macro's #####################################################
!
#include "le.inc"
!
!#################################################################

#ifdef with_mkl
module MKL
      
  include "mkl_spblas.fi"

end module MKL
#endif
