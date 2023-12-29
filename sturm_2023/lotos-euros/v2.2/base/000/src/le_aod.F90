!###############################################################################
!
! LOTOS-EUROS AOD calculation
! ---------------------------
!
!    Copied from p24 of:
!      Koelemeijer RBA ; Schaap M ; Timmermans RMA ; Homan CD ; Matthijsen J ; 
!           Kassteele J van de ; Builtjes PJH
!      Monitoring aerosol concentrations and optical thickness over Europe 
!          - PARMA final report
!      MNP report 555034001/2006
!      januari 2007
!      http://www.mnp.nl/bibliotheek/rapporten/555034001.pdf
!
!  The AOT is computed from the dry aerosol mass concentrations derived from the LOTOS-
!  EUROS model using the approach of Kiehl and Briegleb (1993):
!
!      AOTi(lambda) = f (RH,lambda) * Ai(lambda) * Bi (lambda)
!
!  where 
!    Ai(lambda)    is the mass extinction efficiency of the compound i; 
!    Bi            is the column burden of the compound i; 
!    f (RH,lambda) is a function describing the variation of the scattering coefficient
!                    with relative humidity (RH) and wavelength (lambda). 
!  To compute Ai(lambda) for dry inorganic particles, a Mie (Mie, 1908) code has been used, 
!  assuming the aerosol size distribution to be log-normal, with a geometric mean radius of 0.05um, 
!  a geometric standard deviation of 2.0 and a sulphate dry particle density of 1.7 g cm3 
!  (Kiehl and Briegleb, 1993). 
!  Most aerosol particles absorb or release water vapour when the relative humidity (RH) changes. 
!  Thus the size and composition of the particles change, resulting in different light scattering 
!  properties. To account for the variation of the aerosol scattering coefficient with RH, 
!  the factor f(RH, ?), derived from humidity controlled nephelometry (Veefkind et al., 1996), 
!  is used (see Figure 2.2). Similar functions for f(RH) have been reported by Day et al. (2001) 
!  for various locations in the United States. Effects due to hysteresis (e.g. Tang, 1997) 
!  are not accounted for. The wavelength dependence of f (RH) can be ignored (Veefkind et al., 1999). 
!  The scattering calculations were made with RH values taken from the analysed meteorological
!  data file that is used as input to the LOTOS-EUROS model, including the variations of RH
!  with height. 
!  For the organic aerosol components an Ai of 9 for EC and 7 for OC is assumed
!  (Tegen et al., 1997). For these components the growth as function of RH has been neglected.
!
!         component      RH growth    Ai   Bi
!         EC                           9             elementary carbon; BC
!         OC                           7             organic carbon; PM2.5
!
!    Kiehl, J. T., and B. P. Briegleb, 1993,
!    The relative roles of sulfate aerosols and greenhouse gases in climate forcing, 
!    Science, 260, 311-314.
!
!    Veefkind J.P., J.C.H. van der Hage, and H. M. ten Brink, 1996. 
!    Nephelometer derived and directly measured aerosol optical thickness 
!    of the atmospheric boundary layer,
!    Atmos. Res., 41, 217-228.
!
!    Tegen, I., P. Hollrig, M.Chin, I. Fung, D. Jacob, J. Penner, 1997, 
!    Contribution of different aerosol species to the global aerosol extinction 
!    optical thickness: Estimates from model results. 
!    J. Geophys. Res. 102, 23,895-23,915.
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  AJS, 2009-07
!  Checked (Tegen et al., 1997), values seem to be taken from table 1 :
!
!    ----------------------------------------------------------------------------------
!    Type                               B (=Ai ?)
!                                        [m2/g]
!    ----------------------------------------------------------------------------------
!    Sea salt                          0.2 -  0.4 (0.3)  a)
!    Soil dust ( 1-10 um)              0.2 -  0.4 (0.3)
!    Soil dust (<1    um)              1   -  2   (1.5)
!    Sulfate (H2SO4)        b)         5   - 12   (8.0)
!    Carboneaceous aerosol  d)         5   - 12   (8.0)
!    Black carbon                      8   - 12   (9.0)
!    ----------------------------------------------------------------------------------
!    a) Number in parenteheses is used to calculate global optical thicknesses;
!       values are for 0.55 um wavelength .
!    b) Sulfate aerosol assumed to consist of H2SO4 .
!    c) Values are 5-8.5 (6.0) m2/g over land and 7-14 (10) m2/g over oceans.
!    d) Carbonaceous aerosol includes black carbon
!    ----------------------------------------------------------------------------------
!
!  The value '7' for Organic Carbon was not found in the article,
!  probably simply set as a value 'lower than 9' .
!  
!  The formula is implemented:
!
!     AOD =  f(RH)   *   Ai   *  (   c   * Mi * dh / 1e6 )
!             0-1       m2/g     ( ug/m3   1    m   g/ug )
!      1      0-1       m2/g               g/m2               <-- ? AOD not in m ?
!
!  where Mi is a mass ratio between the modelled aerosol and the aerosol
!  for which Ai is valid.
!
!  ------------+----------------------------------+-------------------------------------------
!  LE version: |             v1.5.31              |    derivied from table1
!  Type        |         Mi              Ai       |           Mi             Ai
!  ------------+----------------------------------+-------------------------------------------
!  SO4a        | f  (NH4)2-SO4 / SO4    5.199645  | f  (NH4)2-SO4 / SO4    5.199645  a) b1)
!  NO3a        | f  (NH4)  NO3 / NO3    5.199645  | f  (NH4)  NO3 / NO3    5.199645  a)
!  BC          |                        9.0       |                        9.0       a)
!  PM2.5       |                        7.0       |                        7.0       a)
!  Na_f        | f                      5.199645  | f                      2.7              c)
!  Na_c        |                                  | f                      0.3          b2)
!  dust_f      |                                  |                        1.5          b3) c)
!  dust_c      |                                  |                        0.3          b2)
!  -----------------------------------------------+-------------------------------------------
!
!  a) Keep current formula.
!
!  b1) (Tegen et al., 1997) table 1 suggests:
!        fRh,   Mi = H2SO4 / SO4  ,  Ai = 8.0
!     Some doubt if this values does contains some kind of average humidity factor,
!     therfore keep using the original value
!
!  b2) Value from table.
!
!  b3) Alternative following (Tegen et al., 1997) table 1, 
!     distrubuted original values 1.5 (0-1um) and 0.3 (1-10um) 
!     over interval 1-2.5um   : (1.0 * 1.5 + 1.5 * 0.3 )/2.5 ~ 0.8
!
!  c) Extiction coefficients computed by Lyana Curier ('Lie' code ?)
!     Sea Salt fine mode
!       Input
!         LOG NORMAL DISTRIBUTION, r_g= 0.4590 [ln(sigma_g)]**2=0.1570  (volume distribution)
!         LAMBDA = 0.5500
!         refractive index real part = 1.350   refractive index imaginarypart = 0.00100
!       Output
!         CEXT=0.274748D+01
!     Desert Dust fine mode
!       size distribution parameters out of OPAC database
!       Input
!         LOG NORMAL DISTRIBUTION, r_g= 0.2700 [ln(sigma_g)]**2=0.4460
!         LAMBDA  = 0.5500
!         Refractive index real part = 1.530 Refractive index imaginary part == 0.00100
!       Output:
!         CEXT=0.151282D+01 
!
!----------------------------
! Extension: explicitly track extinction coefficents to be put out in le_output_aod, AM 29-1-2014

!###############################################################################
!
#define TRACEBACK write (gol,'("in ",a," (",a,", line",i5,")")') rname, __FILE__, __LINE__; call goErr
#define IF_NOTOK_RETURN(action) if (status/=0) then; TRACEBACK; action; return; end if
#define IF_ERROR_RETURN(action) if (status> 0) then; TRACEBACK; action; return; end if
!
#include "le.inc"
!
!###############################################################################


module LE_AOD


  use GO, only : gol, goPr, goErr
  
  implicit none
  
  
  ! --- in/out ---------------------------------
  
  private
  
  public    ::  LE_AOD_calc3d


  ! --- const ------------------------------------

  character(len=*), parameter ::  mname = 'LE_AOD'
  

contains


  ! =============================================================
  
  !
  ! Increase of extinction efficency (or cross-section)
  ! with increasing relative humidity.
  ! Based on (Veefkind et al, 1996)
  !
  !    Veefkind J.P., J.C.H. van der Hage, and H. M. ten Brink, 1996. 
  !    Nephelometer derived and directly measured aerosol optical thickness 
  !    of the atmospheric boundary layer,
  !    Atmos. Res., 41, 217-228.
  !

  elemental real function Rh_Growth( rh )
  
    ! --- in/out ---------------------------------
    
    real, intent(in)      ::  rh   ! relative humidity [%]
    
    ! --- const ----------------------------------
    
    ! fit parameters growth funtion
    real, parameter :: ap(0:6) = (/0.196581266 ,  0.100650483  , -0.00389645252 , 5.63596671e-5, &
                                   6.4988432e-8, -7.95507834e-9,  4.95445298e-11 /)
  
    ! --- begin ----------------------------------
    
    Rh_Growth = ap(0) + ap(1)*Rh    + ap(2)*Rh**2 + ap(3)*Rh**3 +  &
                        ap(4)*Rh**4 + ap(5)*Rh**5 + ap(6)*Rh**6

  end function Rh_Growth
  
  
  ! ***

  
  subroutine LE_AOD_calc3d( c, AOD, extinction, status )

    use dims, only : runF
    use dims, only : nx, ny, nz, nspec
    use LE_Data, only : LE_Data_GetPointer
    use indices
    
    ! --- in/out -----------------------------------

    real, intent(in)      ::  c(nx,ny,nz,nspec)
    real, intent(out)     ::  AOD(nx, ny,nz)
    real, intent(out)     ::  extinction(nx, ny, nz)
    integer, intent(out)  ::  status
    
    ! --- const ----------------------------------

    character(len=*), parameter ::  rname = mname//'/LE_AOD_calc3d'

    ! --- local -----------------------------------
    
    real      ::  frh(nx,ny,nz)
    real      ::  Mi

    real, pointer          ::  RH     (:,:,:)   ! (lon,lat,nz)
    real, pointer          ::  delta_h(:,:,:)   ! (lon,lat,nz)
    
    ! --- begin ------------------------------------
    
    ! access meteo:
    call LE_Data_GetPointer( 'rh', RH     , status, check_units ='%' )
    IF_NOTOK_RETURN(status=1) 
    call LE_Data_GetPointer( 'dh', delta_h, status, check_units ='m' )
    IF_NOTOK_RETURN(status=1)    
    
    ! init output:
    AOD = 0.0
    extinction=0.0
    ! growth factor:
    frh = Rh_Growth( Rh )
    ! truncate:
    frh = min( max( 1.0, frh ), 10.0 )

    ! AOD calculation at 550nm
    !
    !   AOD =   conc   Ai   dh  1/1e6
    !    1     ug/m3  m2/g  m   g/ug 
    !
    ! ammonium-sulphate:
    if ( any((/i_so4a_f,i_nh4a_f/)>0) ) then
      ! check ..
      if ( .not. all((/i_so4a_f,i_nh4a_f/)>0) ) then
        write (gol,'("For AOD calculation, both SO4a and NH4a should be present")'); call goErr
        TRACEBACK; status=1; return
      end if
      ! conversion factor from sulphate (SO4) to ammonium-sulphate (NH4)2(SO4)
      Mi = ( specmolm(i_nh4a_f)*2 + specmolm(i_so4a_f) ) / specmolm(i_so4a_f)
      ! add contribution:
      AOD = AOD + frh * c(1:nx,1:ny,1:nz,i_so4a_f) * Mi * 5.199645 * delta_h(1:nx,1:ny,1:nz)/1e6
      extinction=extinction+frh*c(1:nx,1:ny,1:nz,i_so4a_f) * Mi * 5.199645/1e6
    end if
    ! 
    ! ammonium-nitrate:
    if ( any((/i_no3a_f,i_nh4a_f/)>0) ) then
      ! check ..
      if ( .not. all((/i_no3a_f,i_nh4a_f/)>0) ) then
        write (gol,'("For AOD calculation, both SO4a and NH4a should be present")'); call goErr
        TRACEBACK; status=1; return
      end if
      ! conversion factor from nitrate (NO3) to ammonium-nitrate (NH4)(NO3)
      Mi = ( specmolm(i_nh4a_f) + specmolm(i_no3a_f) ) / specmolm(i_no3a_f)
      ! add contribution:
      AOD = AOD + frh * c(1:nx,1:ny,1:nz,i_no3a_f) *  Mi * 5.199645 * delta_h(1:nx,1:ny,1:nz)/1e6
      extinction = extinction + frh * c(1:nx,1:ny,1:nz,i_no3a_f) *  Mi * 5.199645/1e6
    end if
    !
    ! elementary carbon:
    if (i_ec_f  >0) AOD = AOD +       c(1:nx,1:ny,1:nz,i_ec_f  ) * 9.0 * delta_h(1:nx,1:ny,1:nz)/1e6
    if (i_ec_f  >0) extinction = extinction +       c(1:nx,1:ny,1:nz,i_ec_f  ) * 9.0 /1e6
    !
    ! primary pm:
    if (i_ppm_f >0) AOD = AOD +       c(1:nx,1:ny,1:nz,i_ppm_f ) * 7.0 * delta_h(1:nx,1:ny,1:nz)/1e6
    if (i_ppm_f >0) extinction = extinction +       c(1:nx,1:ny,1:nz,i_ppm_f ) * 7.0 /1e6
    !
    ! sea-salt:
    if (i_na_f  >0) AOD = AOD + frh * c(1:nx,1:ny,1:nz,i_na_f  ) * 2.7 * delta_h(1:nx,1:ny,1:nz)/1e6
    if (i_na_f  >0) extinction = extinction+ frh * c(1:nx,1:ny,1:nz,i_na_f  ) * 2.7/1e6
    if (i_na_ff  >0) AOD = AOD + frh * c(1:nx,1:ny,1:nz,i_na_ff  ) * 2.7 * delta_h(1:nx,1:ny,1:nz)/1e6
    if (i_na_ff  >0) extinction = extinction+ frh * c(1:nx,1:ny,1:nz,i_na_ff  ) * 2.7/1e6
    
    if (i_na_c  >0) AOD = AOD + frh * c(1:nx,1:ny,1:nz,i_na_c  ) * 0.3 * delta_h(1:nx,1:ny,1:nz)/1e6
    if (i_na_c  >0) extinction = extinction + frh * c(1:nx,1:ny,1:nz,i_na_cc  ) * 0.3/1e6
    if (i_na_cc  >0) AOD = AOD + frh * c(1:nx,1:ny,1:nz,i_na_cc  ) * 0.3 * delta_h(1:nx,1:ny,1:nz)/1e6
    if (i_na_cc  >0) extinction = extinction + frh * c(1:nx,1:ny,1:nz,i_na_cc  ) * 0.3/1e6
    if (i_na_ccc  >0) AOD = AOD + frh * c(1:nx,1:ny,1:nz,i_na_ccc  ) * 0.3 * delta_h(1:nx,1:ny,1:nz)/1e6
    if (i_na_ccc  >0) extinction = extinction + frh * c(1:nx,1:ny,1:nz,i_na_ccc  ) * 0.3/1e6
    !
    ! dust:
    if (i_dust_f>0) AOD = AOD +       c(1:nx,1:ny,1:nz,i_dust_f) * 1.5 * delta_h(1:nx,1:ny,1:nz)/1e6
    if (i_dust_f>0) extinction = extinction +       c(1:nx,1:ny,1:nz,i_dust_f) * 1.5 /1e6
    if (i_dust_ff>0) AOD = AOD +       c(1:nx,1:ny,1:nz,i_dust_ff) * 1.5 * delta_h(1:nx,1:ny,1:nz)/1e6
    if (i_dust_ff>0) extinction = extinction +       c(1:nx,1:ny,1:nz,i_dust_ff) * 1.5 /1e6
    if (i_dust_c>0) AOD = AOD +       c(1:nx,1:ny,1:nz,i_dust_c) * 0.3 * delta_h(1:nx,1:ny,1:nz)/1e6
    if (i_dust_c>0) extinction = extinction +       c(1:nx,1:ny,1:nz,i_dust_c) * 0.3 /1e6
    if (i_dust_cc>0) AOD = AOD +       c(1:nx,1:ny,1:nz,i_dust_cc) * 0.3 * delta_h(1:nx,1:ny,1:nz)/1e6
    if (i_dust_cc>0) extinction = extinction +       c(1:nx,1:ny,1:nz,i_dust_cc) * 0.3 /1e6
    if (i_dust_ccc>0) AOD = AOD +       c(1:nx,1:ny,1:nz,i_dust_ccc) * 0.3 * delta_h(1:nx,1:ny,1:nz)/1e6
    if (i_dust_ccc>0) extinction = extinction +       c(1:nx,1:ny,1:nz,i_dust_ccc) * 0.3 /1e6
    ! ok
    status = 0

  end subroutine LE_AOD_calc3d
  

end module LE_AOD
