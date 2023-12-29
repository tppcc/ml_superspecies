!###############################################################################
!
! vbs - routines to perform vbs calculations for detemining soa production and aging
!
!
! DESCRIPTION
!
!   Implementation of VBS scheme.
!
!   Original project code saved as "v1.7/proj/VBS/v1.7.005" .
!   Many tests seemed to be implemented, check documentation (if available)
!   on why certain settings did not make it to the final code.
!
!
! BUG IN PAR LOSS ?
!
!   In "v1.7/proj/VBS/v1.7.005/src/chemistry.F90" the following code is used
!   to calculate the total loss of PAR:
!
!      ! to determine the amount of PAR that was lost cannot be done via keeping track of the concentration
!      ! before and after the reaction step, because there is both loss and production of PAR (this is different
!      ! for OLE, TOL, XYL, ISO and TERP so for those gases it is easier to calculate it from the difference).
!      ! Note that the term below includes both loss terms and 'negative' production terms
!      drog(1) = y(i_par) * (yl(i_par) + y(i_ole) * (rk(52)*y(i_oh) + rk(53)*y(i_o3) + rk(54)*y(i_no3)) )
!
!   The factors at the right hand side come from the production and loss terms,
!   and are the contributions of the direct loss and the negative-production
!   terms that are regarded as losses too:
!   
!     yp(i) = -1.0*(rk(52)*y(i_oh)+rk(53)*y(i_o3)+rk(54)*y(i_no3))*y(i_ole) ...   # conc/s
!     yl(i) = rk(50) * y(i_oh) * (1.0 + ftmp)                                     #  1/s
!
!   The units can be deduced from the original reaction equation:
!     dy/dt = yp + yl*y [conc/s]
!
!   Rewriting the equation for "drog" shows that there is something wrong with its units:
!
!      drog(1) =    rk(50)       * y(i_oh)              * y(i_par)    # [conc/s]
!                 + rk(50)*ftmp  * y(i_oh)              * y(i_par)    # [conc/s]
!                 + rk(52)       * y(i_oh ) * y(i_ole)  * y(i_par)    # [conc/s] * [conc]
!                 + rk(53)       * y(i_o3 ) * y(i_ole)  * y(i_par)    # [conc/s] * [conc]
!                 + rk(54)       * y(i_no3) * y(i_ole)  * y(i_par)    # [conc/s] * [conc]
!
!   Quick fix could be:
!
!      drog(1) = y(i_par) * yl(i_par) + y(i_ole) * (rk(52)*y(i_oh) + rk(53)*y(i_o3) + rk(54)*y(i_no3))
!
!   This is not yet implemented however, since it would be much better to generate
!   the required code automatically. Scripting to do this is probably similar to
!   that used for the total chemical production and loss that is used in the labeling.
!
!
! OTHER THINGS TO BE DONE
!
!   - Implement distribution of POM emissions over VBS tracers using composition tables.
!   - Don't use 'rk(ireac_Rnn)' constants, since these depend on the chemistry scheme.
!     Instead let the required code be generated automatically.
!   - All concentrations in mlc/cm3 ? This is the native unit in the CBM scheme.
!   - No pressure dependency yet in reaction rates and conversions.
!   - Aging by OH is now explicitly coded using solution of system of equations
!     obtained from Mathematica. This is not very robust, not much experience
!     with Mathematica, and difficult to check the implementation.
!     Better add reactions to table and let solver compute the solution.
!
!
! HISTORY
!  2012-03, Joost Beltman
!    New implementation as project with LE v1.7.5 .
!  2012-11, Arjo Segers
!    Adapted to be included in the base v1.8.8 .
!    Version needs some serious improvement before it can be used.
!  2013-07, Richard Kranenburg
!    BUG in loss of PAR solved
!
!  2014-12, Anne Walter/Astrid Manders
!  Changed distribution of emissions over C* values to have first 4
!  classes sum up to 1 to be more comparable to primary pom emissions.
!
!   2015-12 Astrid Manders
!  Changed terpene yields to 0, since they and the terpene emissions are highly uncertain
!  leading to large mismatches between observations and model results. This decision is
!  is in agreement with current understanding of the VBS modules in other models
!
!  2019-04, Ruud Janssen
!    Modified to calculate SVOC/SOA partitioning for various precuror classes:
!    anthropogenic, biogenic, semi- and intermediate volatile, and primary
!    Moved yield calculation to le_chem_work.F90  
!    Moved aging reactions to reactions.csv and le_chem_work.F90
!    Fixed Tref and SOA formation from terpenes
!
!  2021-06, Obin Sturm
!    Fixed minor bug that occurs during checkall, prevents molar mass specmolm and 
!    species concentration ch variables from accessing negative index ispec_cg
!    when ispec_cg = -999, which is assigned when there are fewer bins in the 
!    vbs class (n_vbs_i) than the maximum number of bins over all vbs classes (n_vbs_bins).
!    Removes conditional statement checking if bin index k > n_vbs_i, in nested for loop ~ln 465
!    Subroutine to upload compression/decompression matrices, and compression/decompression
!    subroutines to transform tracers to superspecies over whole domain, and vice versa.
!###############################################################################
!
#define TRACEBACK write (gol,'("in ",a," (",a,", line",i5,")")') rname, __FILE__, __LINE__; call goErr
#define IF_NOTOK_RETURN(action) if (status/=0) then; TRACEBACK; action; return; end if
#define IF_ERROR_RETURN(action) if (status> 0) then; TRACEBACK; action; return; end if
!
#include "le.inc"
!
!###############################################################################

#ifdef with_vbs

module LE_VBS

  use GO, only : gol, goPr, goErr
  
  use Indices, only : n_vbs_bins, n_soa_prec, ispecs_soa_prec, n_vbs_pog, n_vbs_sisog, n_vbs_asog, n_vbs_bsog

  implicit none


  ! --- in/out -----------------------------------
  
  private
  
  public  ::  LE_VBS_Init, LE_VBS_Done, LE_VBS_ML_Init
  public  ::  LE_VBS_Apply, LE_VBS_Compress, LE_VBS_Decompress
  
  public  ::  vbs_emisfac

  !public  ::  n_soa_prec
  public  ::  i_soa_prec_par, i_soa_prec_ole, i_soa_prec_tol, i_soa_prec_xyl, i_soa_prec_iso, i_soa_prec_terp

  public  ::  with_vbs_soa_chemistry


  ! --- const ------------------------------------

  character(len=*), parameter ::  mname = 'LE_VBS'
  
  ! saturation vapor pressure of vbs classes [ug/m3]
  ! at these concentrations half is in gas phase, half in aerosol
  ! SVOCs: psat < ~10^3 ug/m3
  ! IVOCs: psat ~10^3-10^6 ug/m3
  ! VOCs: psat > ~10^6 ug/m3
  real, parameter ::  psat(n_vbs_bins) = (/0.01, 0.1, 1.0, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0/)

  ! number of soa precursors
  ! soa precursors: isoprene (iso), terpenes (terp), alkanes (par), olefines (ole), aromatics (toluene + xylene)
  !integer, parameter :: n_soa_prec = 6
  
  ! --------------------------------------------------------------------------------------------------
  ! -NOTE: the order of these tracers must be the same order as they are in the tracers.csv input file
  ! --------------------------------------------------------------------------------------------------
  ! indices:  
  integer, parameter :: i_soa_prec_par  = 1
  integer, parameter :: i_soa_prec_ole  = 2
  integer, parameter :: i_soa_prec_tol  = 3
  integer, parameter :: i_soa_prec_xyl  = 4
  integer, parameter :: i_soa_prec_iso  = 5
  integer, parameter :: i_soa_prec_terp = 6

  !!! molecular weights of svocs (ug/mol) from parent voc: par     ole   tol     xyl   iso     terp !rj
  !!! real, parameter ::  mw_svoc(n_soa_prec) = (/135e6, 120e6, 150e6, 150e6, 136e6, 180e6/) !rj
  ! molecular weights of soa_prec (ug/mol)        par     ole   tol     xyl   iso     terp
  ! for par and ole, the MWs of the simplest alkane (ethane)) and alkene (ethene), respectively, are used ! rj
  ! afterwards, scaling with the avarage chain lenght is applied !rj
  !real, parameter :: mw_soa_prec(n_soa_prec) = (/ 135e6, 120e6, 150e6, 150e6, 136e6, 180e6 /) ! original values from Joost Beltman's work !rj
  real, parameter :: mw_soa_prec(n_soa_prec) = (/ 30e6, 28e6, 92e6, 106e6, 68e6, 136e6 /) !updated values: mw of parent voc's !rj

  ! conversion factor between PAR and alkanes based on average chain length of alkanes
  !real, parameter ::  conv_par_alkanes = 1.0
  !real, parameter ::  conv_ole_alkenes = 1.0
  real, parameter ::  conv_par_alkanes = 10.0
  real, parameter ::  conv_ole_alkenes = 10.0
  !real, parameter ::  conv_par_alkanes = 9.0
  !real, parameter ::  conv_ole_alkenes = 9.0

!!!  aging reactions moved to reactions.csv
!!!  ! reaction rate of aging reactions with OH (cm^3/molecule/s)
!!!  !real, parameter     ::  agingrate = 4e-11
  
  ! mass fraction of VBS_cg tracer in emitted POM [(kg cg)/(kg pom)]
  ! total seems to be 2.5 (kg cg)/(kg pom).
  ! CHANGED: sum of first 4 classes = 1, sum of the other 5 casses = 1.5 - totalsum = 2.5
  real, parameter     ::  vbs_emisfac(n_vbs_bins) = (/0.1, 0.2, 0.3, 0.4, 0.1, 0.2, 0.30, 0.40, 0.50/)
  !                                                    original emission factors (Shrivastava et al., 2008) !rj
  !                                                    0.03, 0.06, 0.09, 0.14, 0.18, 0.30, 0.40, 0.50, 0.80 !rj 
  ! conversion:
  real, parameter     ::  kg2ug = 1.0e9    ! ug/kg
  
  ! matrices compressing to/decompressing from machine learning superspecies 
  real, dimension(n_vbs_pog,2)    :: decompress_poa
  real, dimension(n_vbs_sisog,2)  :: decompress_sisoa
  real, dimension(n_vbs_asog,2)   :: decompress_asoa
  real, dimension(n_vbs_bsog,2)   :: decompress_bsoa
  
  real, dimension(n_vbs_pog,2)    :: decompress_pog
  real, dimension(n_vbs_sisog,2)  :: decompress_sisog
  real, dimension(n_vbs_asog,2)   :: decompress_asog
  real, dimension(n_vbs_bsog,2)   :: decompress_bsog 
  
  real, dimension(2,n_vbs_pog)    :: compress_poa
  real, dimension(2,n_vbs_sisog)  :: compress_sisoa
  real, dimension(2,n_vbs_asog)   :: compress_asoa
  real, dimension(2,n_vbs_bsog)   :: compress_bsoa
  
  real, dimension(2,n_vbs_pog)    :: compress_pog
  real, dimension(2,n_vbs_sisog)  :: compress_sisog
  real, dimension(2,n_vbs_asog)   :: compress_asog
  real, dimension(2,n_vbs_bsog)   :: compress_bsog  
  ! --- var --------------------------------------

  ! VBS gas-to-aerosol condensation:
  logical               ::  with_vbs_soa_chemistry

  ! timers:
  integer   ::  itim_vbs_ML

contains


  ! ====================================================================


  subroutine LE_VBS_Init( rcF, status )

    use GO, only : TRcFile, ReadRc
    use Indices

    ! --- in/out ---------------------------------

    type(TrcFile), intent(in)       ::  rcF
    integer, intent(out)                ::  status

    ! --- const ----------------------------------

    character(len=*), parameter ::  rname = mname//'/LE_VBS_Init'

    ! --- begin ----------------------------------
    
    ! enabled ?
    if ( n_vbs > 0 ) then

      ! open rcfile:
      call ReadRc( rcF, 'le.vbs.soa.chemistry', with_vbs_soa_chemistry, status )  
      IF_NOTOK_RETURN(status=1)                                                   

      ! check if index numbers of soa precursors is consitent with global spec indices
      if ( ispecs_soa_prec(i_soa_prec_par)  /= ispec_par .or. &
           ispecs_soa_prec(i_soa_prec_ole)  /= ispec_ole .or. &
           ispecs_soa_prec(i_soa_prec_tol)  /= ispec_tol .or. &
           ispecs_soa_prec(i_soa_prec_xyl)  /= ispec_xyl .or. &
           ispecs_soa_prec(i_soa_prec_iso)  /= ispec_iso .or. &
           ispecs_soa_prec(i_soa_prec_terp) /= ispec_terp ) then

        write (gol,'("VBS: Index numbering of SOA precursors is not consistent with global index numbering.")'); call goErr
        write (gol,'("      global,  precursor numbering")' ); call goErr
        write (gol,'(" par   ",i2.2, "        ", i2.2)') ispec_par,  ispecs_soa_prec(i_soa_prec_par) ; call goErr  
        write (gol,'(" ole   ",i2.2, "        ", i2.2)') ispec_ole,  ispecs_soa_prec(i_soa_prec_ole) ; call goErr
        write (gol,'(" tol   ",i2.2, "        ", i2.2)') ispec_tol,  ispecs_soa_prec(i_soa_prec_tol) ; call goErr
        write (gol,'(" xyl   ",i2.2, "        ", i2.2)') ispec_xyl,  ispecs_soa_prec(i_soa_prec_xyl) ; call goErr
        write (gol,'(" iso   ",i2.2, "        ", i2.2)') ispec_iso,  ispecs_soa_prec(i_soa_prec_iso) ; call goErr
        write (gol,'(" terp  ",i2.2, "        ", i2.2)') ispec_terp, ispecs_soa_prec(i_soa_prec_terp); call goErr
        TRACEBACK; status=1; return
      end if
      
    end if

    ! ok
    status = 0

  end subroutine LE_VBS_Init
  

  subroutine LE_VBS_ML_Init( status )
  
    use Indices, only : n_vbs_pog, n_vbs_sisog, n_vbs_asog, n_vbs_bsog
    use GO   , only : GO_Timer_Def
    
    ! --- in/out ---------------------------------

    integer, intent(out)                ::  status
    
    ! --- const ----------------------------------

    character(len=*), parameter ::  rname = mname//'/LE_VBS_ML_Init'
    
    ! --- local ----------------------------------
    
    character(len = 200)												::	path = '/tsn.tno.nl/Data/SV/sv-059025_unix/ProjectData/VBS-machinelearning/users/Obin/Code/lotos-euros/v2.2/proj/vbs-ML/001/data/composition_matrices/' 
    integer                             ::  i, j

    ! --- begin ----------------------------------
    status = 0 
    do while (status == 0)
      
      ! open aerosol decompression matrices
      open(101, file=trim(path)//'decompress_poa.csv',status='old',form="formatted",iostat=status)
      open(102, file=trim(path)//'decompress_sisoa.csv',status='old',form="formatted",iostat=status)
			open(103, file=trim(path)//'decompress_asoa.csv',status='old',form="formatted",iostat=status)
			open(104, file=trim(path)//'decompress_bsoa.csv',status='old',form="formatted",iostat=status)
			
			! open gas decompression matrices
		  open(105, file=trim(path)//'decompress_pog.csv',status='old',form="formatted",iostat=status)
      open(106, file=trim(path)//'decompress_sisog.csv',status='old',form="formatted",iostat=status)
			open(107, file=trim(path)//'decompress_asog.csv',status='old',form="formatted",iostat=status)
			open(108, file=trim(path)//'decompress_bsog.csv',status='old',form="formatted",iostat=status)

      ! open aerosol compression matrices
      open(109, file=trim(path)//'compress_poa.csv',status='old',form="formatted",iostat=status)
      open(110, file=trim(path)//'compress_sisoa.csv',status='old',form="formatted",iostat=status)
			open(111, file=trim(path)//'compress_asoa.csv',status='old',form="formatted",iostat=status)
			open(112, file=trim(path)//'compress_bsoa.csv',status='old',form="formatted",iostat=status)
			
			! open gas compression matrices
		  open(113, file=trim(path)//'compress_pog.csv',status='old',form="formatted",iostat=status)
      open(114, file=trim(path)//'compress_sisog.csv',status='old',form="formatted",iostat=status)
			open(115, file=trim(path)//'compress_asog.csv',status='old',form="formatted",iostat=status)
			open(116, file=trim(path)//'compress_bsog.csv',status='old',form="formatted",iostat=status)
		  		  
      ! read aerosol decompression matrices
			read(101,*,iostat=status)  ((decompress_poa(i,j), j = 1,2), i = 1,n_vbs_pog)
			read(102,*,iostat=status)  ((decompress_sisoa(i,j), j = 1,2), i = 1,n_vbs_sisog)
			read(103,*,iostat=status)  ((decompress_asoa(i,j), j = 1,2), i = 1,n_vbs_asog)
			read(104,*,iostat=status)  ((decompress_bsoa(i,j), j = 1,2), i = 1,n_vbs_bsog)
			
			! read gas decompression matrices
			read(105,*,iostat=status)  ((decompress_pog(i,j), j = 1,2), i = 1,n_vbs_pog)
			read(106,*,iostat=status)  ((decompress_sisog(i,j), j = 1,2), i = 1,n_vbs_sisog)
			read(107,*,iostat=status)  ((decompress_asog(i,j), j = 1,2), i = 1,n_vbs_asog)
			read(108,*,iostat=status)  ((decompress_bsog(i,j), j = 1,2), i = 1,n_vbs_bsog)
			
			! read aerosol compression matrices
			read(109,*,iostat=status)  ((compress_poa(i,j), j = 1,n_vbs_pog), i = 1,2)
			read(110,*,iostat=status)  ((compress_sisoa(i,j), j = 1,n_vbs_sisog), i = 1,2)
			read(111,*,iostat=status)  ((compress_asoa(i,j), j = 1,n_vbs_asog), i = 1,2)
			read(112,*,iostat=status)  ((compress_bsoa(i,j), j = 1,n_vbs_bsog), i = 1,2)
			
			! read gas compression matrices
			read(113,*,iostat=status)  ((compress_pog(i,j), j = 1,n_vbs_pog), i = 1,2)
			read(114,*,iostat=status)  ((compress_sisog(i,j), j = 1,n_vbs_sisog), i = 1,2)
			read(115,*,iostat=status)  ((compress_asog(i,j), j = 1,n_vbs_asog), i = 1,2)
			read(116,*,iostat=status)  ((compress_bsog(i,j), j = 1,n_vbs_bsog), i = 1,2)
			
			! debugging check, write the biogenic gas compression matrix
      do i = 1,2
        print *, "biogenic gas compression matrix:" ,(compress_bsog(i,j), j = 1,n_vbs_bsog)
      enddo
      
      ! escape the while loop
      status =-13314 
      
    enddo
    
    ! ok
    ! status is -13314 to terminate the while loop, iostat probably doesn't have that key
    if (status == -13314) then
      status = 0
    endif
    
    ! define a new timer, itim_vbs_ML, used for measuring compression and decompression
    call GO_Timer_Def( itim_vbs_ML, 'vbs machine learning'  , status )
    IF_NOTOK_RETURN(status=1)
    
  end subroutine LE_VBS_ML_Init


  ! ***


  subroutine LE_VBS_Done( status )

    ! --- in/out ---------------------------------

    integer, intent(out)                ::  status

    ! --- const ----------------------------------

    character(len=*), parameter ::  rname = mname//'/LE_VBS_Done'

    ! --- begin ----------------------------------

    ! ok
    status = 0

  end subroutine LE_VBS_Done


  ! ***


  !
  ! VBS gas-to-aerosol chemistry.
  ! Should only be called if n_vbs_cg == n_vbs_soa
  !

  subroutine LE_VBS_Apply( ch, soa_prec_reacted, nreac, rk, pk, tk, deltat )

    use Indices, only : nspec

    ! --- in/out ---------------------------------

    real, intent(inout) ::  ch(nspec)                     ! concentration array
    real, intent(in)    ::  soa_prec_reacted(n_soa_prec)  ! array with semi-volatile organics (ppb)
    integer, intent(in) ::  nreac                         ! number of reactions ! not used rj
    real(8), intent(in) ::  rk(nreac)                     ! reaction rates ! not used rj
    real, intent(in)    ::  pk                            ! pressure in cell (Pa)
    real, intent(in)    ::  tk                            ! temperature in cell
    real, intent(in)    ::  deltat                        ! time step of calculations (min)

    ! --- local ----------------------------------

    real                ::  curpsat(n_vbs_bins)           ! current saturation vapor pressure per bin
    real                ::  vbs_product_ugm3(n_vbs_bins)  ! amount of vbs product in ug/m^3
    real                ::  curyields(n_soa_prec, n_vbs_bins)  ! current yields

    ! --- begin ----------------------------------

    ! calculate psat for all vbs classes at current ambient temperature (needed for partitioning below)
    call clausiusclapeyron(curpsat, tk)

    !!!! now done in le_chem_work:
    !!!! convert from amount of precursor reacted to amount of product per vbs class (using mass yields)
    !!!call prec_to_vbsclass(soa_prec_reacted, curyields, ch, pk, tk )

    ! calculate partitioning for each vbs class at current psat
    call partition_iter( ch, curpsat, pk, tk )

  end subroutine LE_VBS_Apply


  ! this subroutine calculates the saturation vapor pressures at the current temperature
  ! using the clausius clapeyron equation:
  !
  ! P2 = P1 * exp( DHvap / R * ( 1/T1 - 1/T2 ) ), where
  !
  ! T1 is reference temperature (in K)
  ! T2 is current temperature (in K)
  ! R is the universal gas constant (in J/mol/K)
  ! DHvap is the enthalpy of vaporisation (in J/mol)
  ! P1 is the saturation vapor pressure at reference temperature T1
  ! P2 is the saturation vapor pressure at current temperature T2
  !
  subroutine clausiusclapeyron(curpsat,tk)
  
    use Binas, only : Rgas

    ! --- in/out ---------------------------------

    real, intent(inout) ::  curpsat(n_vbs_bins)  ! temperature dependent saturation vapor pressure
    real, intent(in)    ::  tk                    ! temperature in cell

    ! --- local ----------------------------------
    !!! todo: make dhvap volatility and precursor class dependent
    real, parameter ::  dhvap(n_vbs_bins)  = 30000.0   ! heat of vaporisation of volatile compounds (J/mol)
    real, parameter ::  Tref                = 298.0     ! reference temperature (K)

    ! --- begin ----------------------------------

    curpsat = psat * exp(dhvap/Rgas * ( 1/Tref - 1/tk))

    return

  end subroutine clausiusclapeyron


  subroutine partition_iter(ch, curpsat, pk, tk )
  
    use Binas, only : Rgas
    use Indices, only : nspec, n_vbs_cg, specmolm
    use Indices, only : ispecs_vbs_cg, ispecs_vbs_soa
    use Indices, only : n_vbs_pog, n_vbs_sisog, n_vbs_asog, n_vbs_bsog

    use Indices, only : ispec_vbs_bsog_sup1, ispec_vbs_bsog_sup2, ispec_vbs_bsoa_sup1, ispec_vbs_bsoa_sup2
    ! --- in/out ---------------------------------

    real, intent(inout) ::  ch(nspec)             ! concentration array
    real, intent(in)    ::  curpsat(n_vbs_bins)   ! temperature dependent saturation vapor pressure
    real, intent(in)    ::  pk                    ! pressure in cell (Pa)
    real, intent(in)    ::  tk                    ! temperature in cell (K)

    ! --- local ----------------------------------

    integer             ::  k, l, n, i            ! loop variables
    integer             ::  ispec_cg, ispec_soa
    integer, parameter  ::  n_vbs_class=4 ! 4: poa/pog, sisoa/sisog, asoa/asog, bsoa/bsog
    integer             ::  maxiter=100           ! maximum number of iterations to calculate partitioning
    real                ::  aerprecision=1e-4     ! precision of calculation of total organic aerosol load (ug/m^3)
    real                ::  totaer ! total concentration of organic material in aerosol phase (ug/m^3 air)
    real                ::  totaerold ! to keep track of totaer from previous iteration
    real, parameter     ::  mintotaer = 1e-4      ! minimum concentration of soa (ug/m^3 air)
    real                ::  totorgi(n_vbs_bins)   ! total concentration of organic material in gas phase + aerosol per bin (ug/m^3 air)
    real                ::  xi(n_vbs_bins)        ! fraction of mass in aerosol phase for each vbs bin
    integer             ::  n_vbs(n_vbs_class)
    real                ::  totorg(n_vbs_bins, n_vbs_class) ! concentration of organic mass for each bin & class
    real                ::  mw_cg ! molecular weight of each class (ug mol-1)
    integer             ::  n_vbs_i
    integer             ::  ispecs_vbs_cg_tmp(n_vbs_bins, n_vbs_class)
    integer             ::  ispecs_vbs_soa_tmp(n_vbs_bins, n_vbs_class)
    real                ::  frac_class(n_vbs_bins, n_vbs_class) ! fraction of organic material in each class (sums up to 1 for each bin)
    real                ::  tiny=1e-20
    ! --- begin ----------------------------------
    
    n_vbs = (/ n_vbs_pog, n_vbs_sisog, n_vbs_asog, n_vbs_bsog /) 
    
    ! set temporary indices 
    ! TODO: MOVE TO INIT
    n=0
    do i=1, n_vbs_class
      n_vbs_i = n_vbs(i)
      do k=1,n_vbs_bins
         if (k > n_vbs_i) then
           ispecs_vbs_cg_tmp(k,i)  = -999
           ispecs_vbs_soa_tmp(k,i) = -999
         else
           n=n+1
           ispecs_vbs_cg_tmp(k,i)  = ispecs_vbs_cg(n)
           ispecs_vbs_soa_tmp(k,i) = ispecs_vbs_soa(n)
         endif
      enddo
    enddo

    ! first determine total previous concentration in the aerosol (totaer)
    totaer = 0
    do i=1, n_vbs_class
      do k=1, n_vbs_bins
        ispec_soa = ispecs_vbs_soa_tmp(k,i)
        if ( ispec_soa>0 ) totaer = totaer + ch(ispec_soa) 
      enddo
    enddo

    ! totaer needs to have a minimum value to enable building up of concentration
    ! (otherwise division by zero below)
    totaer = max(totaer, mintotaer)

    ! determine total organic mass (ug m-3) for each bin in each class
    do i=1, n_vbs_class ! 4
      n_vbs_i = n_vbs(i)
      do k=1, n_vbs_bins ! 9
         if (k > n_vbs_i) then
           totorg(k,i) = 0.0
         else
           ispec_cg    = ispecs_vbs_cg_tmp(k,i)
           ispec_soa   = ispecs_vbs_soa_tmp(k,i)
           mw_cg       = specmolm(ispec_cg) 
           totorg(k,i) = ch(ispec_soa) + ch(ispec_cg) * pk * mw_cg / ( Rgas * tk ) ! ug/m3
         endif
      enddo
    enddo

    ! determine facrtion of total organic mass in each class
    do i=1, n_vbs_class ! 4
      n_vbs_i = n_vbs(i)
      do k=1, n_vbs_bins
        totorgi(k)      = SUM(totorg(k,:))
          if (k > n_vbs_i) then
            frac_class(k,i) = 0.0
          else
            frac_class(k,i) = totorg(k,i)/(totorgi(k)+tiny)
          endif
      enddo
    enddo

    !iteratively determine the aerosol fraction xi for each vbs bin
    totaerold = totaer
    do l=1, maxiter
      ! calculate aerosol fraction for each vbs class
      xi = 1 / ( 1 + curpsat/totaer )

      ! re-determine total concentration in aerosol with current partitioning
      totaer = 0
      do k=1, n_vbs_bins
        totaer = totaer + totorgi(k) * xi(k)
      enddo

      ! totaer needs to have a minimum value to enable building up of concentration
      totaer = max(totaer, mintotaer)
      if (abs(totaer - totaerold) <= aerprecision) exit
      totaerold = totaer
    enddo
             
    ! loop over vbs tracers and assign mass to each aerosol and gas phase species
    do i=1, n_vbs_class ! 4
      n_vbs_i = n_vbs(i)
      do k = 1, n_vbs_i ! changed May 2021 from n_vbs_bins to class-specific n_vbs_i
        ispec_cg  = ispecs_vbs_cg_tmp(k,i)
        ispec_soa = ispecs_vbs_soa_tmp(k,i)
        mw_cg     = specmolm(ispec_cg)

        ! the concentrations in the aerosol phase (ug/m^3 air) is now given by
        ch(ispec_soa) = totorg(k,i) * frac_class(k,i) * xi(k)
        ! the rest ends up in the gas phase (convert back from ug/m^3 to ppb)
        ch(ispec_cg ) = (totorg(k,i) - ch(ispec_soa)) * Rgas * tk / (mw_cg * pk)

      enddo
    enddo
    
  end subroutine partition_iter
  
  subroutine LE_VBS_Compress(c, status)

    use dims   , only : nx, ny, nz, nspec
    use Indices, only : ispecs_vbs_pog, ispecs_vbs_sisog, ispecs_vbs_asog, ispecs_vbs_bsog
    use Indices, only : n_vbs_pog, n_vbs_sisog, n_vbs_asog, n_vbs_bsog, ispecs_vbs_soa
    use Indices, only : ispec_vbs_pog_sup1, ispec_vbs_pog_sup2, ispec_vbs_poa_sup1, ispec_vbs_poa_sup2
    use Indices, only : ispec_vbs_sisog_sup1, ispec_vbs_sisog_sup2, ispec_vbs_sisoa_sup1, ispec_vbs_sisoa_sup2
    use Indices, only : ispec_vbs_asog_sup1, ispec_vbs_asog_sup2, ispec_vbs_asoa_sup1, ispec_vbs_asoa_sup2
    use Indices, only : ispec_vbs_bsog_sup1, ispec_vbs_bsog_sup2, ispec_vbs_bsoa_sup1, ispec_vbs_bsoa_sup2
    use GO   , only : GO_Timer_Start, GO_Timer_End
    
    ! --- in/out ---------------------------------
    
    real, intent(inout)       ::  c(nx,ny,nz,nspec)           ! concentration array
    integer, intent(out)                ::  status
    
    ! --- const ----------------------------------

    character(len=*), parameter ::  rname = mname//'/LE_VBS_Compress'
    
    ! --- local ----------------------------------

    integer                     ::  i, j, k           ! loop variables    
    real                ::  tiny = 1e-20
    integer             :: ispecs_vbs_poa(n_vbs_pog)
    integer             :: ispecs_vbs_sisoa(n_vbs_sisog)
    integer             :: ispecs_vbs_asoa(n_vbs_asog)
    integer             :: ispecs_vbs_bsoa(n_vbs_bsog)
    
    ! box concentration array:
    real        ::  ch(nspec)
    
    ! --- begin ----------------------------------
    
    ! start timing:
    call GO_Timer_Start( itim_vbs_ML, status )
    IF_NOTOK_RETURN(status=1)
    
    
    ! get aerosol indices
    ispecs_vbs_poa = ispecs_vbs_soa(1:n_vbs_pog)
    ispecs_vbs_sisoa = ispecs_vbs_soa((n_vbs_pog+1):(n_vbs_pog + n_vbs_sisog))
    ispecs_vbs_asoa = ispecs_vbs_soa((n_vbs_pog + n_vbs_sisog + 1):(n_vbs_pog + n_vbs_sisog + n_vbs_asog))
    ispecs_vbs_bsoa = ispecs_vbs_soa((n_vbs_pog + n_vbs_sisog +n_vbs_asog + 1):(n_vbs_pog + n_vbs_sisog + n_vbs_asog + n_vbs_bsog))
    
    do k = 1, nz
      do j = 1, ny
        do i = 1, nx
        
          ! Set conc's in help array:
          ch = max( 0.0, c(i,j,k,:) )  ! ppb, ug/m3
          
          ! calculate pog superspecies
          ch(ispec_vbs_pog_sup1:ispec_vbs_pog_sup2) = matmul(compress_pog,ch(ispecs_vbs_pog))
          ch(ispec_vbs_pog_sup1:ispec_vbs_pog_sup2) = sum(ch(ispecs_vbs_pog)) / ( sum(ch(ispec_vbs_pog_sup1:ispec_vbs_pog_sup2)) + tiny ) * ch(ispec_vbs_pog_sup1:ispec_vbs_pog_sup2)
          
          ! calculate sisog superspecies
          ch(ispec_vbs_sisog_sup1:ispec_vbs_sisog_sup2) = matmul(compress_sisog,ch(ispecs_vbs_sisog))
          ch(ispec_vbs_sisog_sup1:ispec_vbs_sisog_sup2) = sum(ch(ispecs_vbs_sisog)) / ( sum(ch(ispec_vbs_sisog_sup1:ispec_vbs_sisog_sup2)) + tiny ) * ch(ispec_vbs_sisog_sup1:ispec_vbs_sisog_sup2)
          
          ! calculate asog superspecies
          ch(ispec_vbs_asog_sup1:ispec_vbs_asog_sup2) = matmul(compress_asog,ch(ispecs_vbs_asog))
          ch(ispec_vbs_asog_sup1:ispec_vbs_asog_sup2) = sum(ch(ispecs_vbs_asog)) / ( sum(ch(ispec_vbs_asog_sup1:ispec_vbs_asog_sup2)) + tiny ) * ch(ispec_vbs_asog_sup1:ispec_vbs_asog_sup2)
          
          ! calculate bsog superspecies
          ch(ispec_vbs_bsog_sup1:ispec_vbs_bsog_sup2) = matmul(compress_bsog,ch(ispecs_vbs_bsog))
          ch(ispec_vbs_bsog_sup1:ispec_vbs_bsog_sup2) = sum(ch(ispecs_vbs_bsog)) / ( sum(ch(ispec_vbs_bsog_sup1:ispec_vbs_bsog_sup2)) + tiny ) * ch(ispec_vbs_bsog_sup1:ispec_vbs_bsog_sup2)
          
          ! calculate poa superspecies
          ch(ispec_vbs_poa_sup1:ispec_vbs_poa_sup2) = matmul(compress_poa,ch(ispecs_vbs_poa))
          ch(ispec_vbs_poa_sup1:ispec_vbs_poa_sup2) = sum(ch(ispecs_vbs_poa)) / (sum(ch(ispec_vbs_poa_sup1:ispec_vbs_poa_sup2)) + tiny ) * ch(ispec_vbs_poa_sup1:ispec_vbs_poa_sup2)
          
          ! calculate sisoa superspecies
          ch(ispec_vbs_sisoa_sup1:ispec_vbs_sisoa_sup2) = matmul(compress_sisoa,ch(ispecs_vbs_sisoa))
          ch(ispec_vbs_sisoa_sup1:ispec_vbs_sisoa_sup2) = sum(ch(ispecs_vbs_sisoa)) / ( sum(ch(ispec_vbs_sisoa_sup1:ispec_vbs_sisoa_sup2)) + tiny ) * ch(ispec_vbs_sisoa_sup1:ispec_vbs_sisoa_sup2)
          
          ! calculate asoa superspecies
          ch(ispec_vbs_asoa_sup1:ispec_vbs_asoa_sup2) = matmul(compress_asoa,ch(ispecs_vbs_asoa))
          ch(ispec_vbs_asoa_sup1:ispec_vbs_asoa_sup2) = sum(ch(ispecs_vbs_asoa)) / ( sum(ch(ispec_vbs_asoa_sup1:ispec_vbs_asoa_sup2)) + tiny ) * ch(ispec_vbs_asoa_sup1:ispec_vbs_asoa_sup2)
          
          ! calculate bsoa superspecies
          ch(ispec_vbs_bsoa_sup1:ispec_vbs_bsoa_sup2) = matmul(compress_bsoa,ch(ispecs_vbs_bsoa))
          ch(ispec_vbs_bsoa_sup1:ispec_vbs_bsoa_sup2) = sum(ch(ispecs_vbs_bsoa)) / ( sum(ch(ispec_vbs_bsoa_sup1:ispec_vbs_bsoa_sup2)) + tiny ) * ch(ispec_vbs_bsoa_sup1:ispec_vbs_bsoa_sup2)
         
    
          ! restore concentrations:
          c(i,j,k,:) = ch(:)
        enddo ! i
      enddo ! j
    enddo ! k

    print *, "cabauw surf poa tracers      PRE ADVECTION", c(68,40,1,ispecs_vbs_poa)
    print *, "cabauw surf poa superspecies PRE ADVECTION", c(68,40,1,ispec_vbs_poa_sup1:ispec_vbs_poa_sup2)
    
    ! stop timing:
    call GO_Timer_End( itim_vbs_ML, status )
    IF_NOTOK_RETURN(status=1) 
    
    ! okay
    status = 0     
      
  end subroutine LE_VBS_Compress
  
  subroutine LE_VBS_Decompress(c, status)
    
    use dims   , only : nx, ny, nz, nspec
    use Indices, only : ispecs_vbs_pog, ispecs_vbs_sisog, ispecs_vbs_asog, ispecs_vbs_bsog
    use Indices, only : n_vbs_pog, n_vbs_sisog, n_vbs_asog, n_vbs_bsog
    use Indices, only : ispecs_vbs_soa
    use Indices, only : ispec_vbs_pog_sup1, ispec_vbs_pog_sup2, ispec_vbs_poa_sup1, ispec_vbs_poa_sup2
    use Indices, only : ispec_vbs_sisog_sup1, ispec_vbs_sisog_sup2, ispec_vbs_sisoa_sup1, ispec_vbs_sisoa_sup2
    use Indices, only : ispec_vbs_asog_sup1, ispec_vbs_asog_sup2, ispec_vbs_asoa_sup1, ispec_vbs_asoa_sup2
    use Indices, only : ispec_vbs_bsog_sup1, ispec_vbs_bsog_sup2, ispec_vbs_bsoa_sup1, ispec_vbs_bsoa_sup2
    use GO   , only : GO_Timer_Start, GO_Timer_End
    
    ! --- in/out ---------------------------------
    
    real, intent(inout)       ::  c(nx,ny,nz,nspec)           ! concentration array
    integer, intent(out)                ::  status
    
    ! --- const ----------------------------------

    character(len=*), parameter ::  rname = mname//'/LE_VBS_Decompress'
    
    ! --- local ----------------------------------

    integer                     ::  i, j, k           ! loop variables    
    real                ::  tiny = 1e-20
    integer             :: ispecs_vbs_poa(n_vbs_pog)
    integer             :: ispecs_vbs_sisoa(n_vbs_sisog)
    integer             :: ispecs_vbs_asoa(n_vbs_asog)
    integer             :: ispecs_vbs_bsoa(n_vbs_bsog)
    
    ! box concentration array:
    real                ::  ch(nspec)
        
    ! --- begin ----------------------------------
    
    ! start timing:
    call GO_Timer_Start( itim_vbs_ML, status )
    IF_NOTOK_RETURN(status=1)
    
    ! get aerosol indices
    ispecs_vbs_poa = ispecs_vbs_soa(1:n_vbs_pog)
    ispecs_vbs_sisoa = ispecs_vbs_soa((n_vbs_pog+1):(n_vbs_pog + n_vbs_sisog))
    ispecs_vbs_asoa = ispecs_vbs_soa((n_vbs_pog + n_vbs_sisog + 1):(n_vbs_pog + n_vbs_sisog + n_vbs_asog))
    ispecs_vbs_bsoa = ispecs_vbs_soa((n_vbs_pog + n_vbs_sisog +n_vbs_asog + 1):(n_vbs_pog + n_vbs_sisog + n_vbs_asog + n_vbs_bsog))
    

    print *, "cabauw surf poa tracers      POST ADVECTION", c(68,40,1,ispecs_vbs_poa)
    print *, "cabauw surf poa superspecies POST ADVECTION", c(68,40,1,ispec_vbs_poa_sup1:ispec_vbs_poa_sup2)    
    
    do k = 1, nz
      do j = 1, ny
        do i = 1, nx
        
          ! Set conc's in help array:
          ch = max( 0.0, c(i,j,k,:) )  ! ppb, ug/m3 
             
          ! decompress pog superspecies into pog tracers
          ch(ispecs_vbs_pog) = matmul(decompress_pog,ch(ispec_vbs_pog_sup1:ispec_vbs_pog_sup2) )
          ch(ispecs_vbs_pog) = ( sum(ch(ispec_vbs_pog_sup1:ispec_vbs_pog_sup2)) /(sum(ch(ispecs_vbs_pog))+ tiny ) ) * ch(ispecs_vbs_pog)
          
          ! decompress sisog superspecies into sisog tracers
          ch(ispecs_vbs_sisog) = matmul(decompress_sisog,ch(ispec_vbs_sisog_sup1:ispec_vbs_sisog_sup2) )
          ch(ispecs_vbs_sisog) = ( sum(ch(ispec_vbs_sisog_sup1:ispec_vbs_sisog_sup2)) /(sum(ch(ispecs_vbs_sisog))+ tiny ) ) * ch(ispecs_vbs_sisog)
          
          ! decompress asog superspecies into asog tracers
          ch(ispecs_vbs_asog) = matmul(decompress_asog,ch(ispec_vbs_asog_sup1:ispec_vbs_asog_sup2) )
          ch(ispecs_vbs_asog) = ( sum(ch(ispec_vbs_asog_sup1:ispec_vbs_asog_sup2)) /(sum(ch(ispecs_vbs_asog))+ tiny ) ) * ch(ispecs_vbs_asog)
          
          ! decompress bsog superspecies into bsog tracers
          ch(ispecs_vbs_bsog) = matmul(decompress_bsog,ch(ispec_vbs_bsog_sup1:ispec_vbs_bsog_sup2) )
          ch(ispecs_vbs_bsog) = ( sum(ch(ispec_vbs_bsog_sup1:ispec_vbs_bsog_sup2)) /(sum(ch(ispecs_vbs_bsog))+ tiny ) ) * ch(ispecs_vbs_bsog)

          ! decompress poa superspecies into poa tracers
          ch(ispecs_vbs_poa) = matmul(decompress_poa,ch(ispec_vbs_poa_sup1:ispec_vbs_poa_sup2) )
          ch(ispecs_vbs_poa) = ( sum(ch(ispec_vbs_poa_sup1:ispec_vbs_poa_sup2)) /(sum(ch(ispecs_vbs_poa))+ tiny ) ) * ch(ispecs_vbs_poa)
          
          ! decompress sisoa superspecies into sisoa tracers
          ch(ispecs_vbs_sisoa) = matmul(decompress_sisoa,ch(ispec_vbs_sisoa_sup1:ispec_vbs_sisoa_sup2) )
          ch(ispecs_vbs_sisoa) = ( sum(ch(ispec_vbs_sisoa_sup1:ispec_vbs_sisoa_sup2)) /(sum(ch(ispecs_vbs_sisoa))+ tiny ) ) * ch(ispecs_vbs_sisoa)
          
          ! decompress asoa superspecies into asoa tracers
          ch(ispecs_vbs_asoa) = matmul(decompress_asoa,ch(ispec_vbs_asoa_sup1:ispec_vbs_asoa_sup2) )
          ch(ispecs_vbs_asoa) = ( sum(ch(ispec_vbs_asoa_sup1:ispec_vbs_asoa_sup2)) /(sum(ch(ispecs_vbs_asoa))+ tiny ) ) * ch(ispecs_vbs_asoa)
          
          ! decompress bsoa superspecies into bsoa tracers
          ch(ispecs_vbs_bsoa) = matmul(decompress_bsoa,ch(ispec_vbs_bsoa_sup1:ispec_vbs_bsoa_sup2) )
          ch(ispecs_vbs_bsoa) = ( sum(ch(ispec_vbs_bsoa_sup1:ispec_vbs_bsoa_sup2)) /(sum(ch(ispecs_vbs_bsoa))+ tiny ) ) * ch(ispecs_vbs_bsoa)
          
          ! restore concentrations:
          c(i,j,k,:) = ch(:)
        enddo ! i
      enddo ! j
    enddo ! k          
    
    print *, "cabauw surf poa tracers      POST DECOMPRESSION", c(68,40,1,ispecs_vbs_poa)
    
    
    ! stop timing:
    call GO_Timer_End( itim_vbs_ML, status )
    IF_NOTOK_RETURN(status=1) 
    
    ! okay 
    status = 0
    
  end subroutine LE_VBS_Decompress
  

end module LE_VBS

#endif
