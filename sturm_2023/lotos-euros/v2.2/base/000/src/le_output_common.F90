!###############################################################################
!
! NAME
!
!   LE_Output_Common  -  LOTOS-EUROS output tools and global variables
!
!
!
!###############################################################################
!
#define TRACEBACK write (gol,'("in ",a," (",a,", line",i5,")")') rname, __FILE__, __LINE__; call goErr
#define IF_NOTOK_RETURN(action) if (status/=0) then; TRACEBACK; action; return; end if
#define IF_ERROR_RETURN(action) if (status> 0) then; TRACEBACK; action; return; end if
!
#define IF_NF90_NOTOK_RETURN(action) if (status/=NF90_NOERR) then; gol=nf90_strerror(status); call goErr; TRACEBACK; action; return; end if
!
#include "le.inc"
!
!###############################################################################

module LE_Output_Common

  use GO, only : gol, goPr, goErr
  use GO, only : TDate

#ifdef with_netcdf  
  use NetCDF, only : NF90_StrError, NF90_NOERR
#endif

  implicit none


  ! --- in/out -----------------------------
  
  public
  
  public  ::  T_LE_Output_Common
  
  public  ::  Init, Done
  public  ::  PutOut_GlobalAttributes

  ! --- const ------------------------------
    
  character(len=*), parameter   ::  mname = 'LE_Output_Common'
  
    
  ! --- types ------------------------------
  
  type T_LE_Output_Common
    ! output directory:
    character(len=256)          ::  outdir
    ! run directory           
    character(len=512)          ::  packscript
    ! description:
    character(len=64)           ::  author
    character(len=64)           ::  institution
    character(len=64)           ::  version
    ! experiment:
    character(len=32)           ::  model
    character(len=64)           ::  expid
    ! CF convention:
    character(len=32)           ::  CF_convention
    ! t0
    type(Tdate)                 ::  t0
  end type T_LE_Output_Common
  
  
  ! --- interfaces -------------------------
  
  interface Init
    module procedure leoc_common_Init
  end interface
  
  interface Done
    module procedure leoc_common_Done
  end interface
  
  interface PutOut_GlobalAttributes
    module procedure leoc_PutOut_GlobalAttributes
  end interface

  
contains


  ! ====================================================
  
  
  subroutine leoc_common_Init( leoc, rcfile, rckey, status )

    use GO     , only : TrcFile, Init, Done, ReadRc
    use GO     , only : TDate
    use GO     , only : NewDate
    use GO     , only : GoTranslate
    
    use LE_Config, only : outputdir
  
    ! --- in/out --------------------------------
    
    type(T_LE_Output_Common), intent(out)   ::  leoc
    character(len=*), intent(in)            ::  rcfile
    character(len=*), intent(in)            ::  rckey
    integer, intent(out)                    ::  status
    
    ! --- const ----------------------------
    
    character(len=*), parameter   ::  rname = mname//'/leoc_common_Init'
    
    ! --- local ---------------------------------
    
    type(TrcFile)         ::  rcF
    character(len=32)     ::  stime
    integer               ::  minu, seco
    integer               ::  yy_s, mm_s, dd_s, hh_s

    ! --- begin ---------------------------------
    
    ! open rcfile:
    call Init( rcF, rcfile, status )
    IF_NOTOK_RETURN(status=1)
    
    ! copy output directory:
    leoc%outdir = trim(outputdir)
    
    ! run directory:    
    call ReadRc( rcF, 'output.pack.script', leoc%packscript, status )
    IF_NOTOK_RETURN(status=1)
    
    ! description:
    call ReadRc( rcF, trim(rckey)//'.author'     , leoc%author     , status )
    IF_NOTOK_RETURN(status=1)
    call ReadRc( rcF, trim(rckey)//'.institution', leoc%institution, status )
    IF_NOTOK_RETURN(status=1)
    call ReadRc( rcF, trim(rckey)//'.version'    , leoc%version    , status )
    IF_NOTOK_RETURN(status=1)

    ! read keys for filename etc:
    call ReadRc( rcF, trim(rckey)//'.model', leoc%model, status )
    IF_NOTOK_RETURN(status=1)
    call ReadRc( rcF, trim(rckey)//'.expid', leoc%expid, status )
    IF_NOTOK_RETURN(status=1)

    ! CF convention: "CF-1.5"
    call ReadRc( rcF, 'cf.convention', leoc%CF_convention, status )
    IF_NOTOK_RETURN(status=1)

    ! reference time for out put file (seconds from January first of the start year of simulation)   
    call ReadRc( rcF, 'timerange.start' , stime, status )
    IF_NOTOK_RETURN(status=1)
    call goTranslate( stime, '/-:', ' ', status )
    IF_NOTOK_RETURN(status=1)
    read (stime,*,iostat=status) yy_s, mm_s, dd_s, hh_s, minu, seco
    if ( status /= 0 ) then
      write (gol,'("could not read start time from : ",a)') trim(stime); call goErr
      TRACEBACK; status=1; return
    end if
    leoc%t0 = NewDate( time6=(/yy_s,01,01,00,00,00/) )
    
    ! close:
    call Done( rcF, status )
    IF_NOTOK_RETURN(status=1)
    
    ! ok
    status = 0
    
  end subroutine leoc_common_Init
  
  
  ! ***
  

  subroutine leoc_common_Done( leoc, status )
  
    ! --- in/out --------------------------------
    
    type(T_LE_Output_Common), intent(inout)   ::  leoc
    integer, intent(out)                      ::  status
    
    ! --- const ----------------------------
    
    character(len=*), parameter   ::  rname = mname//'/leoc_common_Done'
    
    ! --- begin ---------------------------------
    
    ! dummy to avoid warnings about unused variables ...
    leoc%outdir = ''
    
    ! ok
    status = 0
    
  end subroutine leoc_common_Done
  
  
  ! ***
  
  
  subroutine leoc_PutOut_GlobalAttributes( leoc, ncid, status )

#ifdef with_netcdf  
    use NetCDF , only : NF90_Put_Att, NF90_GLOBAL
#endif
  
    ! --- in/out ------------------------------
    
    type(T_LE_Output_Common), intent(inout)   ::  leoc
    integer, intent(in)                       ::  ncid
    integer, intent(out)                      ::  status
    
    ! --- const ----------------------------
    
    character(len=*), parameter   ::  rname = mname//'/leoc_PutOut_GlobalAttributes'
    
    ! --- begin ---------------------------------
    
#ifndef with_netcdf
    ! check ...
    stop 'not compiled with netcdf support'
#endif
    
#ifdef with_netcdf  
    ! data origin:    
    status = NF90_Put_Att( ncid, NF90_GLOBAL, 'author', trim(leoc%author) )
    IF_NF90_NOTOK_RETURN(status=1)
    status = NF90_Put_Att( ncid, NF90_GLOBAL, 'institution', trim(leoc%institution) )
    IF_NF90_NOTOK_RETURN(status=1)
    status = NF90_Put_Att( ncid, NF90_GLOBAL, 'version', trim(leoc%version) )
    IF_NF90_NOTOK_RETURN(status=1)
    
    ! model and experiment info:
    status = NF90_Put_Att( ncid, NF90_GLOBAL, 'model', trim(leoc%model) )
    IF_NF90_NOTOK_RETURN(status=1)
    status = NF90_Put_Att( ncid, NF90_GLOBAL, 'expid', trim(leoc%expid) )
    IF_NF90_NOTOK_RETURN(status=1)
    
    ! CF convention
    status = NF90_Put_Att( ncid, NF90_GLOBAL, 'Conventions', trim(leoc%CF_convention) )
    IF_NF90_NOTOK_RETURN(status=1)
#else
    ! dummy to avoid warnings about unused variables ...
    print *, 'leoc_PutOut_GlobalAttributes: ', ncid, trim(leoc%model)
#endif

    ! ok
    status = 0
    
  end subroutine leoc_PutOut_GlobalAttributes

  
end module le_output_common
