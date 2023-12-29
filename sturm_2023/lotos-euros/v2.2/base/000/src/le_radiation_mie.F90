!###############################################################################
!
! Mie properties
!
! History
!   2012, Astrid Manders, TNO
!     Original implementation in "le_radiation.F90"
!   2015-06, Arjo Segers, TNO
!     Splitted this module off from "le_radiation.F90".
!     Introduced interpolation in lookup table.
!     Fixed computation of volume mean diameter.
!
!###############################################################################
!
#define TRACEBACK write (gol,'("in ",a," (",a,", line",i5,")")') rname, __FILE__, __LINE__; call goErr
#define IF_NOTOK_RETURN(action) if (status/=0) then; TRACEBACK; action; return; end if
#define IF_ERROR_RETURN(action) if (status> 0) then; TRACEBACK; action; return; end if
!
#include "le.inc"
!
!###############################################################################

module LE_Radiation_Mie
 
  use GO, only : gol, goPr, goErr
  
  use Indices, only : N_AEROSOL_MODES

  implicit none


  ! --- in/out -----------------------------------

  private

  public  ::  T_AerosolMode, T_AerosolModes
  public  ::  T_RefracIndex
  public  ::  calc_properties_mie


  ! --- const ------------------------------------

  character(len=*), parameter ::  mname = 'LE_Radiation_Mie'

  real, parameter :: le_RH_cutoff = 0.95
  

  ! --- types ----------------------------------

  ! properties of aerosol mode:
  type T_AerosolMode
    ! numer density definition:
    real        ::  Dg        ! number geometric mean [m]
    real        ::  sigmag    ! geometric std.dev.
    ! computed diameters:
    real        ::  Da2     ! 2nd moment arithmetic mean diameter (surface, cross-section)
    real        ::  Da3     ! 3rd moment arithmetic mean diameter (surface, cross-section)
  contains
    procedure   ::  Init      => AerosolMode_Init
    procedure   ::  Done      => AerosolMode_Done
  end type T_AerosolMode
  
  ! *

  ! collection of multiple aerosol modes:
  type T_AerosolModes
    ! number of modes:
    integer                             ::  nmode
    ! definitions:
    type(T_AerosolMode), allocatable    ::  mode(:)  ! (nmode)
  contains
    procedure   ::  Init      => AerosolModes_Init
    procedure   ::  Done      => AerosolModes_Done
  end type T_AerosolModes
  
  ! *

  type T_RefracIndex
    complex, allocatable  ::  PerSpecie(:,:)   ! (swbands%n,n_aerosol)
    complex, allocatable  ::  Water(:)         ! (swbands%n)
  contains
    procedure   ::  Init      => RefracIndex_Init
    procedure   ::  Done      => RefracIndex_Done
  end type T_RefracIndex


contains


  ! ====================================================================
  ! ===
  ! === aerosol modes
  ! ===
  ! ====================================================================


!  subroutine Aer_Init( self, rcF, status )     
!
!    use GO     , only : TrcFile, ReadRc
!    use Indices, only : AEROSOL_FINE_MODES, AEROSOL_COARSE_MODE
!    use Indices, only : AEROSOL_FF_MODES, AEROSOL_CCC_MODE, AEROSOL_CC_MODE
!    use LE_Particle_Data, only : partsize_modes
!
!    ! --- in/out ---------------------------------
!
!    class(T_Aer), intent(out)     ::  self
!    type(TrcFile), intent(in)     ::  rcF
!    integer, intent(out)          ::  status
!
!    ! --- const ----------------------------------
!
!    character(len=*), parameter   ::  rname = mname//'/Aer_Init'
!
!    ! --- local ----------------------------------
!    
!    !real      ::  factor_f, factor_c
!
!    ! --- begin ----------------------------------
!
!    ! Original names:
!    !!! ~ first tests:
!    !!le_Dpg(LE_MODE_1) = 159E-9 ! in meters
!    !!le_sigma(LE_MODE_1) = 1.59
!    !! ~ second test:
!    !le_Dpg(LE_MODE_1) = 300E-9 ! in meters
!    !le_sigma(LE_MODE_1) = 1.35
!    !! ~ no tests ..
!    !le_Dpg  (LE_MODE_2) = 2.0E-6
!    !le_sigma(LE_MODE_2) = 2.0
!    !! compute volume:
!    !le_Dv = exp(log(aer%Dpg) + 3*(log(aer%sigma)**2.0)) * exp((log(aer%sigma)**2.0)/2.0)
!
!    ! new names, init to dummy first:
!    self%Dpg  (:) = 1.0
!    self%sigma(:) = 1.0
!  
!    ! Choice for particle size is important and rather uncertain.
!    ! AM 2-2-2-15
!    ! Particle size in LOTOS-EUROS is set representative  for wet particles and used to calculate
!    ! deposition velocity. For radiation, particle number is more relevant and water uptake is calculated 
!    ! explicitly. In first coupling with RACMO some parameter values were tested, 2 modes only 
!    !!! ~ first tests:
!    !!le_Dpg(LE_MODE_1) = 159E-9 ! in meters
!    !!le_sigma(LE_MODE_1) = 1.59
!    !! ~ second test:
!    !le_Dpg(LE_MODE_1) = 300E-9 ! in meters
!    !le_sigma(LE_MODE_1) = 1.35
!    !! ~ no tests ..
!    !le_Dpg  (LE_MODE_2) = 2.0E-6
!    !le_sigma(LE_MODE_2) = 2.0
!    !! compute volume:
!    !le_Dv = exp(log(self%Dpg) + 3*(log(self%sigma)**2.0)) * exp((log(self%sigma)**2.0)/2.0)
!    
!    ! the first test values gave better results than the second test, 
!    ! although the second choice was adopted.
!    ! Here, we try the assumption that for particle number/radiaton 
!    ! the representative diameter is half of that of the diameter that 
!    ! is appropriate for deposition. 
!    ! ~ fine mode:
!    self%Dpg  (AEROSOL_FF_MODES)    = partsize_modes(1)/2  ! in meters
!    self%sigma(AEROSOL_FF_MODES)    = 1.35
!    self%Dpg  (AEROSOL_FINE_MODES)  = partsize_modes(2)/2  ! in meters
!    self%sigma(AEROSOL_FINE_MODES)  = 1.35
!    ! ~ coarse mode:
!    self%Dpg  (AEROSOL_CCC_MODE)    = partsize_modes(3)/2 
!    self%sigma(AEROSOL_CCC_MODE)    = 2.0 
!    self%Dpg  (AEROSOL_CC_MODE)     = partsize_modes(4)/2 
!    self%sigma(AEROSOL_CC_MODE)     = 2.0
!    self%Dpg  (AEROSOL_COARSE_MODE) = partsize_modes(5)/2
!    self%sigma(AEROSOL_COARSE_MODE) = 2.0
!    
!    !! ... ADHOC FIX ...
!    !! factors:
!    !call ReadRc( rcF, 'le.radiation.partsize.factor.f', factor_f, status )
!    !IF_NOTOK_RETURN(status=1)
!    !call ReadRc( rcF, 'le.radiation.partsize.factor.c', factor_c, status )
!    !IF_NOTOK_RETURN(status=1)
!    !! apply:
!    !self%Dpg(AEROSOL_FF_MODES   ) = self%Dpg(AEROSOL_FF_MODES   ) * factor_f
!    !self%Dpg(AEROSOL_FINE_MODES ) = self%Dpg(AEROSOL_FINE_MODES ) * factor_f
!    !self%Dpg(AEROSOL_CCC_MODE   ) = self%Dpg(AEROSOL_CCC_MODE   ) * factor_c
!    !self%Dpg(AEROSOL_CC_MODE    ) = self%Dpg(AEROSOL_CC_MODE    ) * factor_c
!    !self%Dpg(AEROSOL_COARSE_MODE) = self%Dpg(AEROSOL_COARSE_MODE) * factor_c
!    !! ..................
!    
!    !! testing ...
!    !! diameter mean:
!    !self%Dp = ParticleDiameter_Median2Mean( self%Dpg, self%sigma )
!    !! surface median and mean:
!    !self%DpgS = ParticleDiameter_Median2SurfaceMedian( self%Dpg, sigma )
!    !self%DpS  = ParticleDiameter_Median2Mean( self%DpgS, self%sigma )
!    !! volume median and mean:
!    !self%DpgV = ParticleDiameter_Median2VolumeMedian( self%Dpg, sigma )
!    !self%DpV  = ParticleDiameter_Median2Mean( self%DpgV, self%sigma )
!
!    ! compute volume mean radius and surface mean radius:
!    self%Dv = exp( log(self%Dpg) + 3.0*(log(self%sigma)**2.0) ) * exp((log(self%sigma)**2.0)/2.0)  
!    self%Ds = exp( log(self%Dpg) + 2.0*(log(self%sigma)**2.0) ) * exp((log(self%sigma)**2.0)/2.0)
!    
!    ! now defined above as parameter:
!    !le_RH_cutoff = 0.95
!
!    !if (LE_DEBUG) print '("[I] LE_init: Dpg: ",2(En10.2," "))', le_Dpg
!    !if (LE_DEBUG) print '("[I] LE_init: Sigma: ",2(F4.2," "))', le_sigma
!    !if (LE_DEBUG) print '("[I] LE_init: RH_cutoff: ", F4.2)', le_RH_cutoff
!
!    ! ok
!    status = 0
!    
!  end subroutine Aer_Init
!
!
!  ! ***
!
!
!  subroutine Aer_Done( self, status )     
!
!    ! --- in/out ---------------------------------
!
!    class(T_Aer), intent(inout)   ::  self
!    integer, intent(out)          ::  status
!
!    ! --- const ----------------------------------
!
!    character(len=*), parameter   ::  rname = mname//'/Aer_Done'
!
!    ! --- local ----------------------------------
!
!    ! --- begin ----------------------------------
!
!    ! ok
!    status = 0
!    
!  end subroutine Aer_Done
!  
!  
!  ! ***


  subroutine AerosolMode_Init( self, Dg, sigmag, status )     

    use JAQL, only : ParticleMode_AMeanDiameter_Moment

    ! --- in/out ---------------------------------

    class(T_AerosolMode), intent(out)     ::  self
    real, intent(in)                      ::  Dg, sigmag
    integer, intent(out)                  ::  status

    ! --- const ----------------------------------

    character(len=*), parameter   ::  rname = mname//'/AerosolMode_Init'

    ! --- local ----------------------------------
    
    ! --- begin ----------------------------------
    
    ! store:
    self%Dg     = Dg
    self%sigmag = sigmag

    ! BUG fixed: 
    !   Wrong computation of surface/volume mean diameters:
    !     Dv = exp( log(Dpg) + 3.0*(log(sigma)**2.0) ) * exp((log(sigma)**2.0)/2.0)  
    !     Ds = exp( log(Dpg) + 2.0*(log(sigma)**2.0) ) * exp((log(sigma)**2.0)/2.0)
    !   This seems a combination of formula (8.53) and (8.50) for Dg,
    !   and (8.44) for arithmetic mean in Seinfeld&Pands (2nd Edition).
    !   However, arithmetic mean diameter of a moment should be computed differently,
    !   see new implementation in JAQL_Particles module.
    
    ! arithmetic mean diameters representative for 
    ! mean 2nd moment (surface,cross-section) and 3rd moment (volume):
    self%Da2 = ParticleMode_AMeanDiameter_Moment( self%Dg, self%sigmag, 2 )
    self%Da3 = ParticleMode_AMeanDiameter_Moment( self%Dg, self%sigmag, 3 )

    ! ok
    status = 0
    
  end subroutine AerosolMode_Init


  ! ***


  subroutine AerosolMode_Done( self, status )     

    ! --- in/out ---------------------------------

    class(T_AerosolMode), intent(inout)   ::  self
    integer, intent(out)                  ::  status

    ! --- const ----------------------------------

    character(len=*), parameter   ::  rname = mname//'/AerosolMode_Done'

    ! --- local ----------------------------------

    ! --- begin ----------------------------------

    ! ok
    status = 0
    
  end subroutine AerosolMode_Done
  
  
  ! ***


  subroutine AerosolModes_Init( self, status )     

    use Indices         , only : nspec, specname, specmode
    use Indices         , only : N_AEROSOL_MODES
    use Indices         , only : NO_AEROSOL_MODE
    use Indices         , only : AEROSOL_FINE_MODES, AEROSOL_COARSE_MODE
    use Indices         , only : AEROSOL_FF_MODES, AEROSOL_CCC_MODE, AEROSOL_CC_MODE
    use LE_Particle_Data, only : partsize_modes

    ! --- in/out ---------------------------------

    class(T_AerosolModes), intent(out)    ::  self
    integer, intent(out)                  ::  status

    ! --- const ----------------------------------

    character(len=*), parameter   ::  rname = mname//'/AerosolModes_Init'

    ! --- local ----------------------------------
    
    integer     ::  ispec
    integer     ::  imode
    
    ! --- begin ----------------------------------
    
    ! storage:
    allocate( self%mode(N_AEROSOL_MODES), stat=status )
    IF_NOTOK_RETURN(status=1)

    ! Choice for particle size is important and rather uncertain.
    ! AM 2-2-2-15
    ! Particle size in LOTOS-EUROS is set representative  for wet particles and used to calculate
    ! deposition velocity. For radiation, particle number is more relevant and water uptake is calculated 
    ! explicitly. In first coupling with RACMO some parameter values were tested, 2 modes only 
    !!! ~ first tests:
    !!le_Dpg(LE_MODE_1) = 159E-9 ! in meters
    !!le_sigma(LE_MODE_1) = 1.59
    !! ~ second test:
    !le_Dpg(LE_MODE_1) = 300E-9 ! in meters
    !le_sigma(LE_MODE_1) = 1.35
    !! ~ no tests ..
    !le_Dpg  (LE_MODE_2) = 2.0E-6
    !le_sigma(LE_MODE_2) = 2.0
    !! compute volume:
    !le_Dv = exp(log(self%Dpg) + 3*(log(self%sigma)**2.0)) * exp((log(self%sigma)**2.0)/2.0)
    
    ! the first test values gave better results than the second test, 
    ! although the second choice was adopted.
    ! Here, we try the assumption that for particle number/radiaton 
    ! the representative diameter is half of that of the diameter that 
    ! is appropriate for deposition. 
    
    ! loop over specs:
    do ispec = 1, nspec

      ! current mode:
      imode = specmode(ispec)
      ! skip non-aerosols:
      if ( imode == NO_AEROSOL_MODE ) cycle

      ! switch:
      select case ( imode )
        !
        ! fine modes:                     Dg [m]          sigmag
        case ( AEROSOL_FF_MODES )
          call self%mode(imode)%Init( partsize_modes(1)/2, 1.35, status )
          IF_NOTOK_RETURN(status=1)
        case ( AEROSOL_FINE_MODES )
          call self%mode(imode)%Init( partsize_modes(2)/2, 1.35, status )
          IF_NOTOK_RETURN(status=1)
        !
        ! ~ coarse modes:                 Dg [m]          sigmag
        case ( AEROSOL_CCC_MODE )
          call self%mode(imode)%Init( partsize_modes(3)/2, 2.00, status )
          IF_NOTOK_RETURN(status=1)
        case ( AEROSOL_CC_MODE )
          call self%mode(imode)%Init( partsize_modes(4)/2, 2.00, status )
          IF_NOTOK_RETURN(status=1)
        case ( AEROSOL_COARSE_MODE )
          call self%mode(imode)%Init( partsize_modes(5)/2, 2.00, status )
          IF_NOTOK_RETURN(status=1)
        !
        case default
          write (gol,'("unsupported specmode ",i0," for spec `",a,"`")') imode, trim(specname(ispec)); call goErr
          TRACEBACK; status=1; return
      end select

    end do  ! spec
    
    ! ok
    status = 0
    
  end subroutine AerosolModes_Init


  ! ***


  subroutine AerosolModes_Done( self, status )     

    ! --- in/out ---------------------------------

    class(T_AerosolModes), intent(inout)    ::  self
    integer, intent(out)                    ::  status

    ! --- const ----------------------------------

    character(len=*), parameter   ::  rname = mname//'/AerosolModes_Done'

    ! --- local ----------------------------------

    ! --- begin ----------------------------------
    
    ! clear:
    deallocate( self%mode, stat=status )
    IF_NOTOK_RETURN(status=1)

    ! ok
    status = 0
    
  end subroutine AerosolModes_Done


  ! ====================================================================
  ! ===
  ! === RefracIndex
  ! ===
  ! ====================================================================


  subroutine RefracIndex_Init( self, lut, swbands, status )     

    use Indices
    use LE_Radiation_LUT    , only : T_Radiation_LUT
    use LE_Radiation_SWBands, only : T_SWBands

    ! --- in/out ---------------------------------

    class(T_RefracIndex), intent(out)     ::  self
    class(T_Radiation_LUT), intent(in)    ::  lut
    class(T_SWBands), intent(in)          ::  swbands
    integer, intent(out)                  ::  status

    ! --- const ----------------------------------

    character(len=*), parameter   ::  rname = mname//'/RefracIndex_Init'

    ! --- local ----------------------------------

    integer                         ::  i, j
    integer                         ::  bandwidth
    real, allocatable               ::  tmp1(:), tmp2(:)
    real                            ::  Re, Im
    integer                         ::  i_aerosol, ispec

    ! --- begin ----------------------------------

    ! storage:
    allocate( self%PerSpecie(swbands%n,n_aerosol), stat=status )
    IF_NOTOK_RETURN(status=1)
    allocate( self%Water(swbands%n), stat=status )
    IF_NOTOK_RETURN(status=1)
    
    ! loop over wavelength bands:
    do j = 1, swbands%n

      ! bandwith:
      bandwidth = nint( swbands%width(j)*1000.0 ) ! nm
      ! check ...
      if ( bandwidth < 1 ) then
        write (gol,'("bandwidth ",i0," seem less than 1 nm :",f8.3)') &
                        j, swbands%width(j)*1000.0; call goErr
        TRACEBACK; status=1; return
      end if
      !print *, 'aaa0 j ', j, ' bandwidth ', bandwidth, ' nm'
      
      ! subgrid of wavelengths per nm:
      allocate( tmp1(bandwidth) )
      do i = 1, bandwidth
        tmp1(i) = swbands%bounds(j,1) + (i-1.0) / 1000.0
      end do
      ! storage for interpolated values:
      allocate( tmp2(bandwidth) )
      
      ! loop over aersol tracers:
      do i_aerosol = 1, n_aerosol
        ! global index:
        ispec = ispecs_aerosol(i_aerosol)
        
        ! switch per aerosol:
        select case ( ispec )
        
          !
          ! opac table sets BC, SALT, SO4, NO3
          !
          
          case ( ispec_SO4a_f, ispec_SO4a_c,  ispec_NH4a_f, ispec_NO3a_f, ispec_NO3a_c )

            call rad_interpol( lut%opac(2,:), lut%opac(1,:), tmp1, tmp2 )
            Re = sum(tmp2)/bandwidth
            call rad_interpol(lut%opac(3,:), lut%opac(1,:), tmp1, tmp2)
            Im = sum(tmp2)/bandwidth
            
        
          case ( ispec_EC_f, ispec_EC_c )

            call rad_interpol(lut%opac(4,:), lut%opac(1,:), tmp1, tmp2)
            Re = sum(tmp2)/bandwidth
            call rad_interpol(lut%opac(5,:), lut%opac(1,:), tmp1, tmp2)
            Im = sum(tmp2)/bandwidth
           
           
        
          case (ispec_Na_ff, ispec_Na_f, ispec_Na_ccc, ispec_Na_cc, ispec_Na_c )

            call rad_interpol(lut%opac(6,:), lut%opac(1,:), tmp1, tmp2)
            Re = sum(tmp2)/bandwidth
            call rad_interpol(lut%opac(7,:), lut%opac(1,:), tmp1, tmp2)
            Im = sum(tmp2)/bandwidth
            
          !
          ! echam_ham table sets PPM25 and DUST refractive indexes
          !
          
          case ( ispec_ppm_f, ispec_ppm_c, ispec_pom_f, ispec_pom_c )

            call rad_interpol(lut%echam(2,:), lut%echam(1,:), tmp1, tmp2)
            Re = sum(tmp2)/bandwidth
            call rad_interpol(lut%echam(3,:), lut%echam(1,:), tmp1, tmp2)
            Im = sum(tmp2)/bandwidth
            
        
          case ( ispec_dust_ff,ispec_dust_f, ispec_dust_ccc, ispec_dust_cc, ispec_dust_c )

            call rad_interpol(lut%echam(4,:), lut%echam(1,:), tmp1, tmp2)
            Re = sum(tmp2)/bandwidth
            call rad_interpol(lut%echam(5,:), lut%echam(1,:), tmp1, tmp2)
            Im = sum(tmp2)/bandwidth
            

          case default
            write (gol,'(a,": could not fill RefracIndex%PerSpecie for aerosol ",i6," (",a,")")') rname, ispec, trim(specname(ispec)); call goErr
            TRACEBACK; status=1; return
        end select

        ! store:
        self%PerSpecie(j,i_aerosol) = cmplx(Re,Im)
        
      end do  ! aerosols

      ! Segelstein is about the water
      call rad_interpol(lut%segelstein(2,:), lut%segelstein(1,:), tmp1, tmp2)
      Re = sum(tmp2)/bandwidth
      call rad_interpol(lut%segelstein(3,:), lut%segelstein(1,:), tmp1, tmp2)
      Im = sum(tmp2)/bandwidth
      self%Water(j)=cmplx(Re,Im)

      ! clear:
      deallocate(tmp1,tmp2)
     
    end do   ! bands
    
    ! ok
    status = 0
    
  end subroutine RefracIndex_Init


  ! ***


  subroutine RefracIndex_Done( self, status )     

    ! --- in/out ---------------------------------

    class(T_RefracIndex), intent(inout)   ::  self
    integer, intent(out)                  ::  status

    ! --- const ----------------------------------

    character(len=*), parameter   ::  rname = mname//'/RefracIndex_Done'

    ! --- local ----------------------------------

    ! --- begin ----------------------------------

    ! clear:
    deallocate( self%PerSpecie, stat=status )
    IF_NOTOK_RETURN(status=1)
    deallocate( self%Water, stat=status )
    IF_NOTOK_RETURN(status=1)
    
    ! ok
    status = 0
    
  end subroutine RefracIndex_Done
  

  ! ====================================================================
  ! ===
  ! === Mie
  ! ===
  ! ====================================================================
    

  subroutine calc_properties_mie( c, rh, swbands, lookup, lut, aer, RefracIndex, &
                                  ext, scat, asym, &
                                  refr_f, refr_c, &
                                  volf, volc,crosfine, croscoarse, &
                                  Nfine, Ncoarse, status, &
                                  sizeparam, refr_spec, extf_spec )

    use Binas               , only : pi
    use Binas               , only : massfrac_Na_in_seasalt
    use JAQL                , only : Particle_Volume_D, Particle_CrossSection_D
    use Num                 , only : T_LookUp
    use Indices
    use LE_Radiation_SWBands, only : T_SWBands
    use LE_Radiation_LUT    , only : T_Radiation_LUT
  
!  use dims, only: rh, h
!  use binas, only: pi

    ! --- in/out ---------------------------------

    real, intent(in)                    ::  c(:)   ! (nspec) gas (ppb) and aerosol concentrations [ug/m3]
    real, intent(in)                    ::  rh     ! relative humidity [%]
    class(T_SWBands), intent(in)        ::  swbands
    class(T_LookUp), intent(inout)      ::  lookup
    class(T_Radiation_LUT), intent(in)  ::  lut
    class(T_AerosolModes), intent(in)   ::  aer
    class(T_RefracIndex), intent(in)    ::  RefracIndex
    real, intent(out)                   ::  ext(:)     ! (bands%n) extinction coefficent
    real, intent(out)                   ::  scat(:)    ! (bands%n) single scattering albedo
    real, intent(out)                   ::  asym(:)    ! (bands%n) asymmetry factor
    complex, intent(out)                ::  refr_f(:)    ! (bands%n)
    complex, intent(out)                ::  refr_c(:)    ! (bands%n)
    real, intent(out)                   ::  volf, volc, crosfine, croscoarse, Nfine, Ncoarse
    integer, intent(out)                ::  status
    
    ! testing ...
    real, intent(out), optional         ::  sizeparam(:,:)  ! (bands%n,nspec)
    complex, intent(out), optional      ::  refr_spec(:,:)  ! (bands%n,nspec)
    real, intent(out), optional         ::  extf_spec(:,:)  ! (bands%n,nspec)

    ! --- const ----------------------------------

    character(len=*), parameter   ::  rname = mname//'/calc_properties_mie'
    
    real, parameter  ::  kg_to_g = 1.0e3
    real, parameter  ::  mug_to_g= 1.0e-6

    logical, parameter :: print_warning = .true.

    ! --- local ----------------------------------
    
    logical                ::  mode_is_used(N_AEROSOL_MODES)

    real                   ::  VolumePerSpecie(nspec)
    real                   ::  WaterVolPerMode(N_AEROSOL_MODES)
    real                   ::  DryVolPerMode  (N_AEROSOL_MODES)
    real                   ::  Growth         (N_AEROSOL_MODES)
    real                   ::  Kappa          (N_AEROSOL_MODES)
    real                   ::  TotVolPerMode  (N_AEROSOL_MODES)
    real                   ::  NumberConcentration(N_AEROSOL_MODES)
    real                   ::  MedianWetRadius(N_AEROSOL_MODES)
    !real                   ::  MedianWetCrossection(N_AEROSOL_MODES)
    real                   ::  MeanWetCrossSection
    complex, allocatable   ::  RefracInd(:,:)  ! (N_AEROSOL_MODES,RRTM_NO_SWBANDS)
    complex                ::  RInd
    real                   ::  x
    real                   ::  k_ext(N_AEROSOL_MODES), k_sca(N_AEROSOL_MODES), AF(N_AEROSOL_MODES)
    real                   ::  lambda
    real                   ::  lu_ext, lu_a, lu_g
    real                   ::  rhl
    real                   ::  scattering
    real                   ::  totvolume, totnumber
    integer                ::  ispec
    integer                ::  imode
    integer                ::  jband
    integer                ::  i_aerosol
    real                   ::  csa

    ! --- begin ----------------------------------
    
    ! storage:
    allocate( RefracInd(N_AEROSOL_MODES,swbands%n), stat=status )
    IF_NOTOK_RETURN(status=1)

    ! Calculate volume concentrations of the species, and calculate total volume per mode

    ! init flags:
    mode_is_used = .false.
    ! init sum:
    DryVolPerMode = 0.0
    VolumePerSpecie = 0.0
    !scat=0.0
    !asym=0.0
    ! loop over species:
    do ispec = 1, nspec
      ! current mode:
      imode = specmode(ispec)
      ! skip ?
      if ( imode == NO_AEROSOL_MODE ) cycle

      ! set flag:
      mode_is_used(imode) = .true.
      
      ! always valid data ..
      !if (.NOT. NoData(l)) then
        !       m3                g/ug       ug/m3         kg/m3            g/kg
        VolumePerSpecie(ispec) = mug_to_g * c(ispec) / ( aerdens(ispec) * kg_to_g )
      !else 
      !  VolumePerSpecie(spec) = 0.0 
      !endif

      ! convert from Na to seasalt if necessary:
      if ( (ispec == ispec_Na_f) .or. (ispec == ispec_Na_ff) &
           .or. (ispec == ispec_Na_ccc).or. (ispec == ispec_Na_cc).or. (ispec == ispec_Na_c)) then
        VolumePerSpecie(ispec) = VolumePerSpecie(ispec) / massfrac_Na_in_seasalt  ! Na -> SS
      end if

      ! add contribution per mode:
      DryVolPerMode(imode) = DryVolPerMode(imode) + VolumePerSpecie(ispec)
      
    end do !species

    ! Calculate average hygroscopic parameter kappa per mode, 
    ! and afterwards determine hygroscopic growth factor
    Kappa = 0.0
    ! loop over species:
    do ispec = 1, nspec
      ! current mode:
      imode = specmode(ispec)
      ! skip ?
      if ( imode == NO_AEROSOL_MODE ) cycle
      ! check value:
      if (DryVolPerMode(imode) > 0.0) then
         Kappa(imode) = Kappa(imode) +  (VolumePerSpecie(ispec) / DryVolPerMode(imode)) * aerhygro(ispec)
      end if
    end do !species

    ! init:
    Growth = 0.0
    WaterVolPerMode = 0.0
    TotVolPerMode = 0.0
    NumberConcentration = 0.0
    volf=0.
    volc=0.
    crosfine   = 0.0
    croscoarse = 0.0
    Nfine      = 0.0
    Ncoarse    = 0.0
    
    !
    ! Calculate water uptake and sum to total aerosol volume
    !
    
    ! RH from % to fraction:
    rhl = rh / 100.0  ! 0-1
    ! truncate:
    if ( rhl > le_RH_cutoff ) rhl = le_rh_cutoff
    if ( rhl < 0            ) rhl = 0.0
    
    ! loop over aerosol modes:
    do imode = 1, N_AEROSOL_MODES
      ! skip if not used:
      if ( .not. mode_is_used(imode) ) cycle

      ! compute growth:
      Growth(imode) = (1 + Kappa(imode) * (rhl / (1 - rhl))) ** 0.333 !check units RH

      ! Calculate water uptake:
      WaterVolPerMode(imode) = (Growth(imode)**3.0 -1 ) * DryVolPerMode(imode)

      ! Calculate sum to total aerosol volume
      TotVolPerMode(imode) = DryVolPerMode(imode) + WaterVolPerMode(imode)

      ! calculate number concentration [count/m3]
      NumberConcentration(imode) = DryVolPerMode(imode) / Particle_Volume_D( aer%mode(imode)%Da3 )

      ! part 6, calculate median radius of the humified aerosols (per mode)
      !MedianWetRadius(imode)      =      0.5 * aer%Dpg(imode) * Growth(imode)
      MedianWetRadius(imode)      =      0.5 * aer%mode(imode)%Dg * Growth(imode)

      ! mean cross-section of the humified aersool:
      !MedianWetCrossection(imode) = pi*( 0.5 *  aer%Ds(imode) * Growth(imode) )**2    ! AJS: wrong name, Ds is the mean, not the median!
      MeanWetCrossSection = Particle_CrossSection_D( aer%mode(imode)%Da2 * Growth(imode) )
      
      ! switch:
      select case (imode)
        case( AEROSOL_FINE_MODES, AEROSOL_FF_MODES )
          volf       = volf       + TotVolPerMode(imode)
          !crosfine   = crosfine   + NumberConcentration(imode) * Medianwetcrossection(imode)
          crosfine   = crosfine   + NumberConcentration(imode) * MeanWetCrossSection
          Nfine      = Nfine      + Numberconcentration(imode)
        case( AEROSOL_CCC_MODE, AEROSOL_CC_MODE, AEROSOL_COARSE_MODE )
          volc       = volc       + TotVolPerMode(imode)
          !croscoarse = croscoarse + Numberconcentration(imode) * Medianwetcrossection(imode)
          croscoarse = croscoarse + NumberConcentration(imode) * MeanWetCrossSection
          Ncoarse    = Ncoarse    + Numberconcentration(imode)
        case default
          write (gol,'("unsupported imode ",i0)') imode; call goErr
          TRACEBACK; status=1; return
       end select

    end do ! modes
    
    ! init:
    RefracInd = (0.0,0.0)
      
    do jband = 1, swbands%n

      !! loop over tracers:
      !aerospecno=0
      !do ispec = 1, nspec         
      !  imode = specmode(ispec)        
      !  ! skip ?
      !  if ( imode == NO_AEROSOL_MODE ) cycle
      !  ! increase counter:
      !  aerospecno=aerospecno+1
      !  ! check ...
      !  if ( aerospecno > n_aerosol ) then
      !    write (gol,'("aerspecno > N_aerosol")'); call goErr
      !    TRACEBACK; status=1; return
      !  end if
      !  ! fill:
      !  RefracInd(imode,jband) = RefracInd(imode,jband) + VolumePerSpecie(ispec) * RefracIndex%PerSpecie(jband,aerospecno)
      !end do !species

      ! loop over aersol tracers:
      do i_aerosol = 1, n_aerosol
        ! global tracer index:
        ispec = ispecs_aerosol(i_aerosol) 
        ! aerosol mode:
        imode = specmode(ispec) 
        ! add contribution:
        RefracInd(imode,jband) = RefracInd(imode,jband) + &
                                   VolumePerSpecie(ispec) * RefracIndex%PerSpecie(jband,i_aerosol)
      end do ! aerosol bands        
      
      ! loop over size modes:
      do imode = 1, N_AEROSOL_MODES
        ! add water contribution:
        RefracInd(imode,jband) = RefracInd(imode,jband) + WaterVolPerMode(imode) * RefracIndex%Water(jband)
        ! normalize:
        if ( TotVolPerMode(imode) > 0.0 ) then
          RefracInd(imode,jband) = RefracInd(imode,jband) / TotVolPerMode(imode)
        else
          RefracInd(imode,jband) = (0.0,0.0)
        end if
      end do ! modes

    end do ! bands

    ! part 7. extract information from look-up tables, interpolate   
    ext    = 0.0  
    asym   = 0.0
    scat   = 0.0
    refr_f = (0.0,0.0)
    refr_c = (0.0,0.0)

    ! testing ...
    if ( present(sizeparam) ) sizeparam = 0.0
    if ( present(refr_spec) ) refr_spec = 0.0
    if ( present(extf_spec) ) extf_spec = 0.0
    
    ! loop over bands:
    do jband = 1, swbands%n 

      scattering=0.
      totvolume =0.   
      totnumber =0.
      ! loop over modes:
      do imode = 1, N_AEROSOL_MODES
        ! skip if not used:
        if ( .not. mode_is_used(imode) ) cycle

        ! extract:
        RInd   = RefracInd(imode,jband)

        ! wavelength in m
        lambda = 1.0D-6 * swbands%lambda(jband)

        ! size parameter [JadB-2013, eq. (6.9) or (6.19)],
        ! based on the geometric mean (median) radius,
        ! used as index in the lookup tables:
        x = 2.0 * pi * MedianWetRadius(imode) / lambda
        
        ! set interpolation indices:
        call lookup%InterpolSet( (/x,real(RInd),aimag(RInd)/), status )
        IF_NOTOK_RETURN(status=1)

        ! switch:
        ! modes with sigma=1.59 (or smaller):
        if ( (abs(aer%mode(imode)%sigmag - 1.59) < 0.01) .or. &
             (abs(aer%mode(imode)%sigmag - 1.35) < 0.01)      ) then
                
          ! apply interpolations in lookup tables:
          call lookup%InterpolApply( lut%ext_159, lu_ext, status )
          IF_NOTOK_RETURN(status=1)
          call lookup%InterpolApply( lut%a_159  , lu_a  , status )
          IF_NOTOK_RETURN(status=1)
          call lookup%InterpolApply( lut%g_159  , lu_g  , status )
          IF_NOTOK_RETURN(status=1)
        
        ! modes with sigma=2.00
        else if ( (abs(aer%mode(imode)%sigmag - 2.00) < 0.01) ) then

          ! apply interpolations in lookup tables:
          call lookup%InterpolApply( lut%ext_200, lu_ext, status )
          IF_NOTOK_RETURN(status=1)
          call lookup%InterpolApply( lut%a_200  , lu_a  , status )
          IF_NOTOK_RETURN(status=1)
          call lookup%InterpolApply( lut%g_200  , lu_g  , status )
          IF_NOTOK_RETURN(status=1)
          
        else
          write (gol,'("unsupported sigma ",f0.2," for mode ",i6)') aer%mode(imode)%sigmag, imode; call goErr
          TRACEBACK; status=1; return
        end if

        ! fill:
        k_ext(imode) = lu_ext * lambda * lambda * NumberConcentration(imode)
        k_sca(imode) = lu_a * k_ext(imode)
        AF(imode)    = lu_g
        
        ! update sums:
        ext(jband)  = ext(jband)  + k_ext(imode)
        scattering  = scattering  + k_sca(imode)
        asym(jband) = asym(jband) + k_sca(imode)*AF(imode)

        ! switch:
        select case (imode)
          !
          ! fine modes:
          case( AEROSOL_FINE_MODES, AEROSOL_FF_MODES )
            ! refractive index:
            refr_f(jband) = refr_f(jband) + RInd * TotVolPerMode(imode)
          !
          ! coarse modes:
          case( AEROSOL_CCC_MODE, AEROSOL_CC_MODE, AEROSOL_COARSE_MODE )
            ! refractive index:
            refr_c(jband) =   refr_c(jband) + RInd * TotVolPerMode(imode)
          !
          case default
            write (gol,'("unsupported mode ",i6)') imode; call goErr
            TRACEBACK; status=1; return
        end select

        ! testing ...
        if ( present(sizeparam) .or. present(refr_spec) .or. present(extf_spec) ) then
          ! median cross-section:
          csa = pi * MedianWetRadius(imode)**2
          ! loop over specs:
          do ispec = 1, nspec
            ! current mode?
            if ( (specmode(ispec) == imode) .and. (NumberConcentration(imode) > 0.0) ) then
              if ( present(sizeparam) ) sizeparam(jband,ispec) = x
              if ( present(refr_spec) ) refr_spec(jband,ispec) = RInd
              if ( present(extf_spec) ) extf_spec(jband,ispec) = extf_spec(jband,ispec) + lu_ext * lambda**2 / csa
            end if
          end do ! specs
        end if  ! store
        
        !totvolume      =   totvolume+TotVolPerMode(imode)
        !totnumber      =   totnumber+(MedianWetRadius(imode)**2)*NumberConcentration(imode)
      end do ! loop imode

      ! Finally determine extinction coefficient, AOD (tau), 
      ! single scattering coefficient ssa and asymmetry factor asy      
      ! should be altered when there are more than 2 modes
      if ( ext(jband) > 1.0d-7 ) then
        scat(jband)  = scattering/ ext(jband)
        if (scat(jband) > 0.000000001) then
          asym(jband)  = asym(jband) / scattering
        else 
          asym(jband)  = 1.0
        end if
      else 
        scat(jband)  = 0.0
        asym(jband)  = 1.0
      end if
    
    end do ! loop j over swbands
    
    ! clear:
    deallocate( RefracInd, stat=status )
    IF_NOTOK_RETURN(status=1)
    
    ! ok
    status = 0

  end subroutine calc_properties_mie

  !-----------------------------------------------------------------

  subroutine rad_interpol(y,x,u,v)
  
    real, intent(in)    :: x(:), y(:)   ! (M) values and data
    real, intent(in)    :: u(:)         ! (N) target values
    real, intent(out)   :: v(:)         ! (N) interpolated data

    integer :: N, M, i, j

    N = size(x) 
    M = size(u)

    !print *, 'n,m,u',n,m,u
    !print *, 'x1,xn,', x(1),x(N)

    ! loop over output values:
    do j = 1, M 

      ! set to first data if below minimum:
      if ( u(j) < x(1) ) then
        ! set to first value:
        v(j) = y(1)
        ! next target value:
        cycle
      end if

      ! set to last data if above maximum:
      if ( u(j) > x(N) ) then
        ! set to last value:
        v(j) = y(N)
        ! warning ...
        !print *,'piep', v(j), y(N), j,N
        ! next target value:
        cycle
      end if

      ! loop over begin values of intervals:
      do i = 1, N-1
        ! target value in interval?
        if (u(j)>=x(i) .and. u(j)<x(i+1)) then
          ! interplate:
          v(j) = y(i) + (u(j)-x(i)) * (y(i+1)-y(i))/(x(i+1)-x(i))
          ! found, next target value:
          exit
        end if
      end do ! intervals

    end do ! output values

  end subroutine rad_interpol

end module LE_Radiation_Mie
