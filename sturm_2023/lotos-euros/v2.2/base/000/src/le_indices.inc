  ! number of tracers, including special:
  integer, parameter  ::  nspec_all = 69

  ! number of tracers, excluding special:
  integer, parameter  ::  nspec = 54

  ! tracer indices;
  ! value -1 for disabled tracers:
  integer, parameter  ::  ispec_NO2      =   1  ! Nitrogen dioxide
  integer, parameter  ::  ispec_NO       =   2  ! Nitric oxide
  integer, parameter  ::  ispec_O3       =   3  ! Ozone
  integer, parameter  ::  ispec_CO       =   4  ! Carbon monoxide
  integer, parameter  ::  ispec_FORM     =   5  ! Formaldehyde
  integer, parameter  ::  ispec_ALD      =   6  ! Aldehyde
  integer, parameter  ::  ispec_PAN      =   7  ! Peroxyacetyl nitrate
  integer, parameter  ::  ispec_MGLY     =   8  ! Methylglyoxal and other aromatic products
  integer, parameter  ::  ispec_PAR      =   9  ! Paraffin carbon bond (C-C)
  integer, parameter  ::  ispec_OLE      =  10  ! Terminal olefin carbon bond (R-C=C)
  integer, parameter  ::  ispec_ETH      =  11  ! Ethene
  integer, parameter  ::  ispec_TOL      =  12  ! Toluene and other monoalkyl aromatics
  integer, parameter  ::  ispec_CRES     =  13  ! Cresol and higher molecular weight phenols
  integer, parameter  ::  ispec_XYL      =  14  ! Xylene and other polyalkyl aromatics
  integer, parameter  ::  ispec_CO2      =  -1  ! Carbon dioxide
  integer, parameter  ::  ispec_SO4a_f   =  15  ! Sulfate aerosol fine mode
  integer, parameter  ::  ispec_SO2      =  16  ! Sulfur dioxide
  integer, parameter  ::  ispec_HNO2     =  17  ! Nitrous acid
  integer, parameter  ::  ispec_CH4      =  18  ! Methane
  integer, parameter  ::  ispec_NH3      =  19  ! Ammonia
  integer, parameter  ::  ispec_NO3      =  20  ! Nitrate radical
  integer, parameter  ::  ispec_OH       =  21  ! Hydroxyl radical
  integer, parameter  ::  ispec_HO2      =  22  ! Hydroperoxy radical
  integer, parameter  ::  ispec_N2O5     =  23  ! Dinitrogen pentoxide
  integer, parameter  ::  ispec_SO4a_c   =  24  ! Sulfate aerosol coarse mode
  integer, parameter  ::  ispec_NH4a_f   =  25  ! Ammonium aerosol fine mode
  integer, parameter  ::  ispec_NO3a_f   =  26  ! Nitrate aerosol fine mode
  integer, parameter  ::  ispec_NO3a_c   =  27  ! Nitrate aerosol coarse mode
  integer, parameter  ::  ispec_C2O3     =  28  ! Acetylperoxy radical
  integer, parameter  ::  ispec_XO2      =  29  ! NO to NO2 conversion from alkylperoxy (RO2) radical
  integer, parameter  ::  ispec_XO2N     =  30  ! NO to organic nitrate conversion from alkylperoxy (RO2) radical
  integer, parameter  ::  ispec_CRO      =  31  ! Methylphenoxy radical
  integer, parameter  ::  ispec_HNO3     =  32  ! Nitric acid
  integer, parameter  ::  ispec_H2O2     =  33  ! Hydrogen peroxide
  integer, parameter  ::  ispec_ISO      =  34  ! Isoprene
  integer, parameter  ::  ispec_ISPD     =  35  ! Isoprene product (lumped methacrolein/methyl vinyl/ketone etc.)
  integer, parameter  ::  ispec_TO2      =  36  ! Toluene-hydroxyl radical adduct
  integer, parameter  ::  ispec_OPEN     =  37  ! Aromatic ring opening product
  integer, parameter  ::  ispec_TERP     =  38  ! Terpene
  integer, parameter  ::  ispec_SF6      =  -2  ! Sulfur hexafluoride
  integer, parameter  ::  ispec_EC_f     =  39  ! elementary carbon aerosol fine mode
  integer, parameter  ::  ispec_EC_c     =  40  ! elementary carbon aerosol coarse mode
  integer, parameter  ::  ispec_POM_f    =  41  ! primary organic matter aerosol fine mode
  integer, parameter  ::  ispec_POM_c    =  42  ! primary organic matter aerosol coarse mode
  integer, parameter  ::  ispec_PPM_f    =  43  ! primary particulate matter fine mode
  integer, parameter  ::  ispec_PPM_c    =  44  ! primary particulate matter coarse mode
  integer, parameter  ::  ispec_PPM_1    =  -3  ! particulate matter ultra_fine mode
  integer, parameter  ::  ispec_PPM_1_25 =  -4  ! particulate matter ultra_fine to fine mode
  integer, parameter  ::  ispec_PPM_25_4 =  -5  ! particulate matter fine mode to medium mode
  integer, parameter  ::  ispec_PPM_4_10 =  -6  ! particulate matter medium mode to coarse mode
  integer, parameter  ::  ispec_EC_1     =  -7  ! Elemental carbon ultra_fine mode
  integer, parameter  ::  ispec_EC_1_25  =  -8  ! Elemental carbon ultra_fine mode to fine mode
  integer, parameter  ::  ispec_Na_ff    =  45  ! Sodium aerosol fine mode
  integer, parameter  ::  ispec_Na_f     =  46  ! Sodium aerosol fine mode
  integer, parameter  ::  ispec_Na_c     =  47  ! Sodium aerosol coarse mode
  integer, parameter  ::  ispec_Na_cc    =  48  ! Sodium aerosol coarse mode
  integer, parameter  ::  ispec_Na_ccc   =  49  ! Sodium aerosol coarse mode
  integer, parameter  ::  ispec_dust_ff  =  50  ! dust aerosol fine mode
  integer, parameter  ::  ispec_dust_f   =  51  ! dust aerosol fine mode
  integer, parameter  ::  ispec_dust_c   =  52  ! dust aerosol coarse mode
  integer, parameter  ::  ispec_dust_cc  =  53  ! dust aerosol coarse mode
  integer, parameter  ::  ispec_dust_ccc =  54  ! dust aerosol coarse mode
  integer, parameter  ::  ispec_pol_b    =  -9  ! birch pollen
  integer, parameter  ::  ispec_pol_g    = -10  ! grass pollen
  integer, parameter  ::  ispec_pol_o    = -11  ! olive pollen
  integer, parameter  ::  ispec_h2so4    = -12  ! sulphuric acid
  integer, parameter  ::  ispec_so4ns    = -13  ! sulphate aerosol nucle. mode soluble
  integer, parameter  ::  ispec_so4ks    = -14  ! sulphate aerosol aitken mode soluble
  integer, parameter  ::  ispec_so4as    = -15  ! sulphate aerosol accum. mode soluble
  integer, parameter  ::  ispec_so4cs    = -16  ! sulphate aerosol coarse mode soluble
  integer, parameter  ::  ispec_bcks     = -17  ! black carbon aerosol aitken mode soluble
  integer, parameter  ::  ispec_bcas     = -18  ! black carbon aerosol accum. mode soluble
  integer, parameter  ::  ispec_bccs     = -19  ! black carbon aerosol coarse mode soluble
  integer, parameter  ::  ispec_bcki     = -20  ! black carbon aerosol aitken mode insoluble
  integer, parameter  ::  ispec_ocks     = -21  ! organic carbon aerosol aitken mode soluble
  integer, parameter  ::  ispec_ocas     = -22  ! organic carbon aerosol accum. mode soluble
  integer, parameter  ::  ispec_occs     = -23  ! organic carbon aerosol coarse mode soluble
  integer, parameter  ::  ispec_ocki     = -24  ! organic carbon aerosol aitken mode insoluble
  integer, parameter  ::  ispec_ssas     = -25  ! sea salt aerosol accum. mode soluble
  integer, parameter  ::  ispec_sscs     = -26  ! sea salt aerosol coarse mode soluble
  integer, parameter  ::  ispec_duas     = -27  ! dust aerosol accum. mode soluble
  integer, parameter  ::  ispec_ducs     = -28  ! dust aerosol coarse mode soluble
  integer, parameter  ::  ispec_duai     = -29  ! dust aerosol accum. mode insoluble
  integer, parameter  ::  ispec_duci     = -30  ! dust aerosol coarse mode insoluble
  integer, parameter  ::  ispec_nnus     = -31  ! number concentration nucle. mode soluble
  integer, parameter  ::  ispec_nais     = -32  ! number concentration aitken mode soluble
  integer, parameter  ::  ispec_nacs     = -33  ! number concentration accum. mode soluble
  integer, parameter  ::  ispec_ncos     = -34  ! number concentration coarse mode soluble
  integer, parameter  ::  ispec_naii     = -35  ! number concentration aitken mode insoluble
  integer, parameter  ::  ispec_naci     = -36  ! number concentration accum. mode insoluble
  integer, parameter  ::  ispec_ncoi     = -37  ! number concentration coarse mode insoluble
  integer, parameter  ::  ispec_Ca_f     = -38  ! calcium aerosol fine mode
  integer, parameter  ::  ispec_Ca_c     = -39  ! calcium aerosol coarse mode
  integer, parameter  ::  ispec_K_f      = -40  ! potasium aerosol fine mode
  integer, parameter  ::  ispec_K_c      = -41  ! potasium aerosol coarse mode
  integer, parameter  ::  ispec_Mg_f     = -42  ! magnesium aerosol fine mode
  integer, parameter  ::  ispec_Mg_c     = -43  ! magnesium aerosol coarse mode
  integer, parameter  ::  ispec_vbs_cg1  = -44  ! vbs condensable gas 1
  integer, parameter  ::  ispec_vbs_cg2  = -45  ! vbs condensable gas 2
  integer, parameter  ::  ispec_vbs_cg3  = -46  ! vbs condensable gas 3
  integer, parameter  ::  ispec_vbs_cg4  = -47  ! vbs condensable gas 4
  integer, parameter  ::  ispec_vbs_cg5  = -48  ! vbs condensable gas 5
  integer, parameter  ::  ispec_vbs_cg6  = -49  ! vbs condensable gas 6
  integer, parameter  ::  ispec_vbs_cg7  = -50  ! vbs condensable gas 7
  integer, parameter  ::  ispec_vbs_cg8  = -51  ! vbs condensable gas 8
  integer, parameter  ::  ispec_vbs_cg9  = -52  ! vbs condensable gas 9
  integer, parameter  ::  ispec_vbs_soa1 = -53  ! vbs secondary organic aerosol 1
  integer, parameter  ::  ispec_vbs_soa2 = -54  ! vbs secondary organic aerosol 1
  integer, parameter  ::  ispec_vbs_soa3 = -55  ! vbs secondary organic aerosol 1
  integer, parameter  ::  ispec_vbs_soa4 = -56  ! vbs secondary organic aerosol 1
  integer, parameter  ::  ispec_vbs_soa5 = -57  ! vbs secondary organic aerosol 1
  integer, parameter  ::  ispec_vbs_soa6 = -58  ! vbs secondary organic aerosol 1
  integer, parameter  ::  ispec_vbs_soa7 = -59  ! vbs secondary organic aerosol 1
  integer, parameter  ::  ispec_vbs_soa8 = -60  ! vbs secondary organic aerosol 1
  integer, parameter  ::  ispec_vbs_soa9 = -61  ! vbs secondary organic aerosol 1
  integer, parameter  ::  ispec_Pb_f     = -62  ! lead aerosol fine mode
  integer, parameter  ::  ispec_Pb_c     = -63  ! lead aerosol coarse mode
  integer, parameter  ::  ispec_Cd_f     = -64  ! cadmium aerosol fine mode
  integer, parameter  ::  ispec_Cd_c     = -65  ! cadmium aerosol coarse mode
  integer, parameter  ::  ispec_O        = -66  ! Oxygen atom in the O3(P) electronic state
  integer, parameter  ::  ispec_O1D      = -67  ! Oxygen atom in the O1(D) electronic state
  integer, parameter  ::  ispec_PNA      = -68  ! Peroxynitric acid (HNO4)
  integer, parameter  ::  ispec_ALD2     = -69  ! Acetaldehyde
  integer, parameter  ::  ispec_ALDX     = -70  ! Propionaldehyde and higher aldehydes
  integer, parameter  ::  ispec_CXO3     = -71  ! C3 and higher acylperoxy radicals
  integer, parameter  ::  ispec_PANX     = -72  ! C3 and higher peroxyacyl nitrates
  integer, parameter  ::  ispec_NTR      = -73  ! Organic nitrate (RNO3)
  integer, parameter  ::  ispec_ETOH     = -74  ! Ethanol
  integer, parameter  ::  ispec_MEO2     = -75  ! Methylperoxy radical
  integer, parameter  ::  ispec_MEOH     = -76  ! Methanol
  integer, parameter  ::  ispec_MEPX     = -77  ! Methylhydroperoxide
  integer, parameter  ::  ispec_FACD     = -78  ! Formic acid
  integer, parameter  ::  ispec_ETHA     = -79  ! Ethane
  integer, parameter  ::  ispec_ROOH     = -80  ! Higher organic peroxide
  integer, parameter  ::  ispec_AACD     = -81  ! Acetic and higher carboxylic acids
  integer, parameter  ::  ispec_PACD     = -82  ! Peroxyacetic and higher peroxycarboxylic acids
  integer, parameter  ::  ispec_ROR      = -83  ! Secondary alkoxy radical
  integer, parameter  ::  ispec_IOLE     = -84  ! Internal olefin carbon bond (R-C=C-R)
  integer, parameter  ::  ispec_SULF     = -85  ! Sulfuric acid (gaseous)
  integer, parameter  ::  ispec_HCO3     = -86  ! Bicarbonate
  integer, parameter  ::  ispec_tpm10    =  55  ! total particulate matter 0-10um
  integer, parameter  ::  ispec_tpm25    =  56  ! total particulate matter 0-2.5um
  integer, parameter  ::  ispec_tss      =  57  ! total seasalt aerosol
  integer, parameter  ::  ispec_tdust    =  58  ! total dust aerosol
  integer, parameter  ::  ispec_tc       =  59  ! total carbon aerosol
  integer, parameter  ::  ispec_tcg      =  60  ! total concensible gasses
  integer, parameter  ::  ispec_tsoa     =  61  ! total secondary organic aerosol
  integer, parameter  ::  ispec_tnmvoc   =  62  ! total nmvoc
  integer, parameter  ::  ispec_SOxa     =  63  ! SOx aerosol
  integer, parameter  ::  ispec_NOya     =  64  ! NOy aerosol
  integer, parameter  ::  ispec_NHxa     =  65  ! NHx aerosol
  integer, parameter  ::  ispec_NO2_obs  =  66  ! Nitrogen dioxide reported by observation devices
  integer, parameter  ::  ispec_tpm10_biascorr =  67  ! total particulate matter 0-10um biascorrected
  integer, parameter  ::  ispec_tpm25_biascorr =  68  ! total particulate matter 0-2.5um biascorrected
  integer, parameter  ::  ispec_o3_biascorr =  69  ! ozone biascorrected

  ! tracer indices as variables, for backwards compatibility:
  integer             ::  i_NO2      =   1  ! Nitrogen dioxide
  integer             ::  i_NO       =   2  ! Nitric oxide
  integer             ::  i_O3       =   3  ! Ozone
  integer             ::  i_CO       =   4  ! Carbon monoxide
  integer             ::  i_FORM     =   5  ! Formaldehyde
  integer             ::  i_ALD      =   6  ! Aldehyde
  integer             ::  i_PAN      =   7  ! Peroxyacetyl nitrate
  integer             ::  i_MGLY     =   8  ! Methylglyoxal and other aromatic products
  integer             ::  i_PAR      =   9  ! Paraffin carbon bond (C-C)
  integer             ::  i_OLE      =  10  ! Terminal olefin carbon bond (R-C=C)
  integer             ::  i_ETH      =  11  ! Ethene
  integer             ::  i_TOL      =  12  ! Toluene and other monoalkyl aromatics
  integer             ::  i_CRES     =  13  ! Cresol and higher molecular weight phenols
  integer             ::  i_XYL      =  14  ! Xylene and other polyalkyl aromatics
  integer             ::  i_CO2      =  -1  ! Carbon dioxide
  integer             ::  i_SO4a_f   =  15  ! Sulfate aerosol fine mode
  integer             ::  i_SO2      =  16  ! Sulfur dioxide
  integer             ::  i_HNO2     =  17  ! Nitrous acid
  integer             ::  i_CH4      =  18  ! Methane
  integer             ::  i_NH3      =  19  ! Ammonia
  integer             ::  i_NO3      =  20  ! Nitrate radical
  integer             ::  i_OH       =  21  ! Hydroxyl radical
  integer             ::  i_HO2      =  22  ! Hydroperoxy radical
  integer             ::  i_N2O5     =  23  ! Dinitrogen pentoxide
  integer             ::  i_SO4a_c   =  24  ! Sulfate aerosol coarse mode
  integer             ::  i_NH4a_f   =  25  ! Ammonium aerosol fine mode
  integer             ::  i_NO3a_f   =  26  ! Nitrate aerosol fine mode
  integer             ::  i_NO3a_c   =  27  ! Nitrate aerosol coarse mode
  integer             ::  i_C2O3     =  28  ! Acetylperoxy radical
  integer             ::  i_XO2      =  29  ! NO to NO2 conversion from alkylperoxy (RO2) radical
  integer             ::  i_XO2N     =  30  ! NO to organic nitrate conversion from alkylperoxy (RO2) radical
  integer             ::  i_CRO      =  31  ! Methylphenoxy radical
  integer             ::  i_HNO3     =  32  ! Nitric acid
  integer             ::  i_H2O2     =  33  ! Hydrogen peroxide
  integer             ::  i_ISO      =  34  ! Isoprene
  integer             ::  i_ISPD     =  35  ! Isoprene product (lumped methacrolein/methyl vinyl/ketone etc.)
  integer             ::  i_TO2      =  36  ! Toluene-hydroxyl radical adduct
  integer             ::  i_OPEN     =  37  ! Aromatic ring opening product
  integer             ::  i_TERP     =  38  ! Terpene
  integer             ::  i_SF6      =  -2  ! Sulfur hexafluoride
  integer             ::  i_EC_f     =  39  ! elementary carbon aerosol fine mode
  integer             ::  i_EC_c     =  40  ! elementary carbon aerosol coarse mode
  integer             ::  i_POM_f    =  41  ! primary organic matter aerosol fine mode
  integer             ::  i_POM_c    =  42  ! primary organic matter aerosol coarse mode
  integer             ::  i_PPM_f    =  43  ! primary particulate matter fine mode
  integer             ::  i_PPM_c    =  44  ! primary particulate matter coarse mode
  integer             ::  i_PPM_1    =  -3  ! particulate matter ultra_fine mode
  integer             ::  i_PPM_1_25 =  -4  ! particulate matter ultra_fine to fine mode
  integer             ::  i_PPM_25_4 =  -5  ! particulate matter fine mode to medium mode
  integer             ::  i_PPM_4_10 =  -6  ! particulate matter medium mode to coarse mode
  integer             ::  i_EC_1     =  -7  ! Elemental carbon ultra_fine mode
  integer             ::  i_EC_1_25  =  -8  ! Elemental carbon ultra_fine mode to fine mode
  integer             ::  i_Na_ff    =  45  ! Sodium aerosol fine mode
  integer             ::  i_Na_f     =  46  ! Sodium aerosol fine mode
  integer             ::  i_Na_c     =  47  ! Sodium aerosol coarse mode
  integer             ::  i_Na_cc    =  48  ! Sodium aerosol coarse mode
  integer             ::  i_Na_ccc   =  49  ! Sodium aerosol coarse mode
  integer             ::  i_dust_ff  =  50  ! dust aerosol fine mode
  integer             ::  i_dust_f   =  51  ! dust aerosol fine mode
  integer             ::  i_dust_c   =  52  ! dust aerosol coarse mode
  integer             ::  i_dust_cc  =  53  ! dust aerosol coarse mode
  integer             ::  i_dust_ccc =  54  ! dust aerosol coarse mode
  integer             ::  i_pol_b    =  -9  ! birch pollen
  integer             ::  i_pol_g    = -10  ! grass pollen
  integer             ::  i_pol_o    = -11  ! olive pollen
  integer             ::  i_h2so4    = -12  ! sulphuric acid
  integer             ::  i_so4ns    = -13  ! sulphate aerosol nucle. mode soluble
  integer             ::  i_so4ks    = -14  ! sulphate aerosol aitken mode soluble
  integer             ::  i_so4as    = -15  ! sulphate aerosol accum. mode soluble
  integer             ::  i_so4cs    = -16  ! sulphate aerosol coarse mode soluble
  integer             ::  i_bcks     = -17  ! black carbon aerosol aitken mode soluble
  integer             ::  i_bcas     = -18  ! black carbon aerosol accum. mode soluble
  integer             ::  i_bccs     = -19  ! black carbon aerosol coarse mode soluble
  integer             ::  i_bcki     = -20  ! black carbon aerosol aitken mode insoluble
  integer             ::  i_ocks     = -21  ! organic carbon aerosol aitken mode soluble
  integer             ::  i_ocas     = -22  ! organic carbon aerosol accum. mode soluble
  integer             ::  i_occs     = -23  ! organic carbon aerosol coarse mode soluble
  integer             ::  i_ocki     = -24  ! organic carbon aerosol aitken mode insoluble
  integer             ::  i_ssas     = -25  ! sea salt aerosol accum. mode soluble
  integer             ::  i_sscs     = -26  ! sea salt aerosol coarse mode soluble
  integer             ::  i_duas     = -27  ! dust aerosol accum. mode soluble
  integer             ::  i_ducs     = -28  ! dust aerosol coarse mode soluble
  integer             ::  i_duai     = -29  ! dust aerosol accum. mode insoluble
  integer             ::  i_duci     = -30  ! dust aerosol coarse mode insoluble
  integer             ::  i_nnus     = -31  ! number concentration nucle. mode soluble
  integer             ::  i_nais     = -32  ! number concentration aitken mode soluble
  integer             ::  i_nacs     = -33  ! number concentration accum. mode soluble
  integer             ::  i_ncos     = -34  ! number concentration coarse mode soluble
  integer             ::  i_naii     = -35  ! number concentration aitken mode insoluble
  integer             ::  i_naci     = -36  ! number concentration accum. mode insoluble
  integer             ::  i_ncoi     = -37  ! number concentration coarse mode insoluble
  integer             ::  i_Ca_f     = -38  ! calcium aerosol fine mode
  integer             ::  i_Ca_c     = -39  ! calcium aerosol coarse mode
  integer             ::  i_K_f      = -40  ! potasium aerosol fine mode
  integer             ::  i_K_c      = -41  ! potasium aerosol coarse mode
  integer             ::  i_Mg_f     = -42  ! magnesium aerosol fine mode
  integer             ::  i_Mg_c     = -43  ! magnesium aerosol coarse mode
  integer             ::  i_vbs_cg1  = -44  ! vbs condensable gas 1
  integer             ::  i_vbs_cg2  = -45  ! vbs condensable gas 2
  integer             ::  i_vbs_cg3  = -46  ! vbs condensable gas 3
  integer             ::  i_vbs_cg4  = -47  ! vbs condensable gas 4
  integer             ::  i_vbs_cg5  = -48  ! vbs condensable gas 5
  integer             ::  i_vbs_cg6  = -49  ! vbs condensable gas 6
  integer             ::  i_vbs_cg7  = -50  ! vbs condensable gas 7
  integer             ::  i_vbs_cg8  = -51  ! vbs condensable gas 8
  integer             ::  i_vbs_cg9  = -52  ! vbs condensable gas 9
  integer             ::  i_vbs_soa1 = -53  ! vbs secondary organic aerosol 1
  integer             ::  i_vbs_soa2 = -54  ! vbs secondary organic aerosol 1
  integer             ::  i_vbs_soa3 = -55  ! vbs secondary organic aerosol 1
  integer             ::  i_vbs_soa4 = -56  ! vbs secondary organic aerosol 1
  integer             ::  i_vbs_soa5 = -57  ! vbs secondary organic aerosol 1
  integer             ::  i_vbs_soa6 = -58  ! vbs secondary organic aerosol 1
  integer             ::  i_vbs_soa7 = -59  ! vbs secondary organic aerosol 1
  integer             ::  i_vbs_soa8 = -60  ! vbs secondary organic aerosol 1
  integer             ::  i_vbs_soa9 = -61  ! vbs secondary organic aerosol 1
  integer             ::  i_Pb_f     = -62  ! lead aerosol fine mode
  integer             ::  i_Pb_c     = -63  ! lead aerosol coarse mode
  integer             ::  i_Cd_f     = -64  ! cadmium aerosol fine mode
  integer             ::  i_Cd_c     = -65  ! cadmium aerosol coarse mode
  integer             ::  i_O        = -66  ! Oxygen atom in the O3(P) electronic state
  integer             ::  i_O1D      = -67  ! Oxygen atom in the O1(D) electronic state
  integer             ::  i_PNA      = -68  ! Peroxynitric acid (HNO4)
  integer             ::  i_ALD2     = -69  ! Acetaldehyde
  integer             ::  i_ALDX     = -70  ! Propionaldehyde and higher aldehydes
  integer             ::  i_CXO3     = -71  ! C3 and higher acylperoxy radicals
  integer             ::  i_PANX     = -72  ! C3 and higher peroxyacyl nitrates
  integer             ::  i_NTR      = -73  ! Organic nitrate (RNO3)
  integer             ::  i_ETOH     = -74  ! Ethanol
  integer             ::  i_MEO2     = -75  ! Methylperoxy radical
  integer             ::  i_MEOH     = -76  ! Methanol
  integer             ::  i_MEPX     = -77  ! Methylhydroperoxide
  integer             ::  i_FACD     = -78  ! Formic acid
  integer             ::  i_ETHA     = -79  ! Ethane
  integer             ::  i_ROOH     = -80  ! Higher organic peroxide
  integer             ::  i_AACD     = -81  ! Acetic and higher carboxylic acids
  integer             ::  i_PACD     = -82  ! Peroxyacetic and higher peroxycarboxylic acids
  integer             ::  i_ROR      = -83  ! Secondary alkoxy radical
  integer             ::  i_IOLE     = -84  ! Internal olefin carbon bond (R-C=C-R)
  integer             ::  i_SULF     = -85  ! Sulfuric acid (gaseous)
  integer             ::  i_HCO3     = -86  ! Bicarbonate
  integer             ::  i_tpm10    =  55  ! total particulate matter 0-10um
  integer             ::  i_tpm25    =  56  ! total particulate matter 0-2.5um
  integer             ::  i_tss      =  57  ! total seasalt aerosol
  integer             ::  i_tdust    =  58  ! total dust aerosol
  integer             ::  i_tc       =  59  ! total carbon aerosol
  integer             ::  i_tcg      =  60  ! total concensible gasses
  integer             ::  i_tsoa     =  61  ! total secondary organic aerosol
  integer             ::  i_tnmvoc   =  62  ! total nmvoc
  integer             ::  i_SOxa     =  63  ! SOx aerosol
  integer             ::  i_NOya     =  64  ! NOy aerosol
  integer             ::  i_NHxa     =  65  ! NHx aerosol
  integer             ::  i_NO2_obs  =  66  ! Nitrogen dioxide reported by observation devices
  integer             ::  i_tpm10_biascorr =  67  ! total particulate matter 0-10um biascorrected
  integer             ::  i_tpm25_biascorr =  68  ! total particulate matter 0-2.5um biascorrected
  integer             ::  i_o3_biascorr =  69  ! ozone biascorrected

  ! tracer names:
  character(len=*), parameter  ::  specname(nspec_all) = (/ &
          'no2           ', &    ! NO2
          'no            ', &    ! NO
          'o3            ', &    ! O3
          'co            ', &    ! CO
          'form          ', &    ! FORM
          'ald           ', &    ! ALD
          'pan           ', &    ! PAN
          'mgly          ', &    ! MGLY
          'par           ', &    ! PAR
          'ole           ', &    ! OLE
          'eth           ', &    ! ETH
          'tol           ', &    ! TOL
          'cres          ', &    ! CRES
          'xyl           ', &    ! XYL
          'so4a_f        ', &    ! SO4a_f
          'so2           ', &    ! SO2
          'hno2          ', &    ! HNO2
          'ch4           ', &    ! CH4
          'nh3           ', &    ! NH3
          'no3           ', &    ! NO3
          'oh            ', &    ! OH
          'ho2           ', &    ! HO2
          'n2o5          ', &    ! N2O5
          'so4a_c        ', &    ! SO4a_c
          'nh4a_f        ', &    ! NH4a_f
          'no3a_f        ', &    ! NO3a_f
          'no3a_c        ', &    ! NO3a_c
          'c2o3          ', &    ! C2O3
          'xo2           ', &    ! XO2
          'xo2n          ', &    ! XO2N
          'cro           ', &    ! CRO
          'hno3          ', &    ! HNO3
          'h2o2          ', &    ! H2O2
          'iso           ', &    ! ISO
          'ispd          ', &    ! ISPD
          'to2           ', &    ! TO2
          'open          ', &    ! OPEN
          'terp          ', &    ! TERP
          'ec_f          ', &    ! EC_f
          'ec_c          ', &    ! EC_c
          'pom_f         ', &    ! POM_f
          'pom_c         ', &    ! POM_c
          'ppm_f         ', &    ! PPM_f
          'ppm_c         ', &    ! PPM_c
          'na_ff         ', &    ! Na_ff
          'na_f          ', &    ! Na_f
          'na_c          ', &    ! Na_c
          'na_cc         ', &    ! Na_cc
          'na_ccc        ', &    ! Na_ccc
          'dust_ff       ', &    ! dust_ff
          'dust_f        ', &    ! dust_f
          'dust_c        ', &    ! dust_c
          'dust_cc       ', &    ! dust_cc
          'dust_ccc      ', &    ! dust_ccc
          'tpm10         ', &    ! tpm10
          'tpm25         ', &    ! tpm25
          'tss           ', &    ! tss
          'tdust         ', &    ! tdust
          'tc            ', &    ! tc
          'tcg           ', &    ! tcg
          'tsoa          ', &    ! tsoa
          'tnmvoc        ', &    ! tnmvoc
          'soxa          ', &    ! SOxa
          'noya          ', &    ! NOya
          'nhxa          ', &    ! NHxa
          'no2_obs       ', &    ! NO2_obs
          'tpm10_biascorr', &    ! tpm10_biascorr
          'tpm25_biascorr', &    ! tpm25_biascorr
          'o3_biascorr   ' /)    ! o3_biascorr

  ! tracer units:
  character(len=*), parameter  ::  specunit(nspec_all) = (/ &
          'ppb    ', &    ! NO2
          'ppb    ', &    ! NO
          'ppb    ', &    ! O3
          'ppb    ', &    ! CO
          'ppb    ', &    ! FORM
          'ppb    ', &    ! ALD
          'ppb    ', &    ! PAN
          'ppb    ', &    ! MGLY
          'ppb    ', &    ! PAR
          'ppb    ', &    ! OLE
          'ppb    ', &    ! ETH
          'ppb    ', &    ! TOL
          'ppb    ', &    ! CRES
          'ppb    ', &    ! XYL
          'ug/m3  ', &    ! SO4a_f
          'ppb    ', &    ! SO2
          'ppb    ', &    ! HNO2
          'ppb    ', &    ! CH4
          'ppb    ', &    ! NH3
          'ppb    ', &    ! NO3
          'ppb    ', &    ! OH
          'ppb    ', &    ! HO2
          'ppb    ', &    ! N2O5
          'ug/m3  ', &    ! SO4a_c
          'ug/m3  ', &    ! NH4a_f
          'ug/m3  ', &    ! NO3a_f
          'ug/m3  ', &    ! NO3a_c
          'ppb    ', &    ! C2O3
          'ppb    ', &    ! XO2
          'ppb    ', &    ! XO2N
          'ppb    ', &    ! CRO
          'ppb    ', &    ! HNO3
          'ppb    ', &    ! H2O2
          'ppb    ', &    ! ISO
          'ppb    ', &    ! ISPD
          'ppb    ', &    ! TO2
          'ppb    ', &    ! OPEN
          'ppb    ', &    ! TERP
          'ug/m3  ', &    ! EC_f
          'ug/m3  ', &    ! EC_c
          'ug/m3  ', &    ! POM_f
          'ug/m3  ', &    ! POM_c
          'ug/m3  ', &    ! PPM_f
          'ug/m3  ', &    ! PPM_c
          'ug/m3  ', &    ! Na_ff
          'ug/m3  ', &    ! Na_f
          'ug/m3  ', &    ! Na_c
          'ug/m3  ', &    ! Na_cc
          'ug/m3  ', &    ! Na_ccc
          'ug/m3  ', &    ! dust_ff
          'ug/m3  ', &    ! dust_f
          'ug/m3  ', &    ! dust_c
          'ug/m3  ', &    ! dust_cc
          'ug/m3  ', &    ! dust_ccc
          'ug/m3  ', &    ! tpm10
          'ug/m3  ', &    ! tpm25
          'ug/m3  ', &    ! tss
          'ug/m3  ', &    ! tdust
          'ug/m3  ', &    ! tc
          'ppb    ', &    ! tcg
          'ug/m3  ', &    ! tsoa
          'ug/m3  ', &    ! tnmvoc
          'ug/m3  ', &    ! SOxa
          'ug/m3  ', &    ! NOya
          'ug/m3  ', &    ! NHxa
          'ppb    ', &    ! NO2_obs
          'ug/m3  ', &    ! tpm10_biascorr
          'ug/m3  ', &    ! tpm25_biascorr
          'ppb    ' /)    ! o3_biascorr

  ! tracer mole mass:
  real, parameter  ::  specmolm(nspec_all) = (/ &
       xm_O * 2 + xm_N, &     ! NO2
       xm_O + xm_N, &     ! NO
       xm_O * 3, &     ! O3
       xm_C + xm_O, &     ! CO
       xm_H * 2 + xm_C + xm_O, &     ! FORM
       xm_H * 3 + xm_C * 2 + xm_O, &     ! ALD
       xm_H * 3 + xm_C * 2 + xm_O * 5 + xm_N, &     ! PAN
       xm_H * 4 + xm_C * 3 + xm_O * 2, &     ! MGLY
       xm_H * 3 + xm_C, &     ! PAR
       xm_H * 3 + xm_C * 2, &     ! OLE
       xm_H * 4 + xm_C * 2, &     ! ETH
       xm_H * 8 + xm_C * 7, &     ! TOL
       xm_H * 8 + xm_C * 7 + xm_O, &     ! CRES
       xm_H * 10 + xm_C * 8, &     ! XYL
       xm_S + xm_O * 4, &     ! SO4a_f
       xm_S + xm_O * 2, &     ! SO2
       xm_H + xm_O * 2 + xm_N, &     ! HNO2
       xm_H * 4 + xm_C, &     ! CH4
       xm_H * 3 + xm_N, &     ! NH3
       xm_O * 3 + xm_N, &     ! NO3
       xm_H + xm_O, &     ! OH
       xm_H + xm_O * 2, &     ! HO2
       xm_O * 5 + xm_N * 2, &     ! N2O5
       xm_S + xm_O * 4, &     ! SO4a_c
       xm_H * 4 + xm_N, &     ! NH4a_f
       xm_O * 3 + xm_N, &     ! NO3a_f
       xm_O * 3 + xm_N, &     ! NO3a_c
       xm_C * 2 + xm_O * 3, &     ! C2O3
       xm_dummy, &     ! XO2
       xm_dummy, &     ! XO2N
       xm_dummy, &     ! CRO
       xm_H + xm_O * 3 + xm_N, &     ! HNO3
       xm_H * 2 + xm_O * 2, &     ! H2O2
       xm_H * 8 + xm_C * 5, &     ! ISO
       xm_dummy, &     ! ISPD
       xm_dummy, &     ! TO2
       xm_dummy, &     ! OPEN
       xm_H * 16 + xm_C * 10, &     ! TERP
       xm_dummy, &     ! EC_f
       xm_dummy, &     ! EC_c
       xm_dummy, &     ! POM_f
       xm_dummy, &     ! POM_c
       xm_dummy, &     ! PPM_f
       xm_dummy, &     ! PPM_c
       xm_Na, &     ! Na_ff
       xm_Na, &     ! Na_f
       xm_Na, &     ! Na_c
       xm_Na, &     ! Na_cc
       xm_Na, &     ! Na_ccc
       xm_dummy, &     ! dust_ff
       xm_dummy, &     ! dust_f
       xm_dummy, &     ! dust_c
       xm_dummy, &     ! dust_cc
       xm_dummy, &     ! dust_ccc
       xm_dummy, &     ! tpm10
       xm_dummy, &     ! tpm25
       xm_dummy, &     ! tss
       xm_dummy, &     ! tdust
       xm_dummy, &     ! tc
       xm_dummy, &     ! tcg
       xm_dummy, &     ! tsoa
       xm_dummy, &     ! tnmvoc
       xm_dummy, &     ! SOxa
       xm_dummy, &     ! NOya
       xm_dummy, &     ! NHxa
       xm_O * 2 + xm_N, &     ! NO2_obs
       xm_dummy, &     ! tpm10_biascorr
       xm_dummy, &     ! tpm25_biascorr
       xm_O * 3 /)     ! o3_biascorr

  ! aerosol densities:
  real, parameter  ::  aerdens(nspec_all) = (/ &
          -999.9e3, &    ! NO2
          -999.9e3, &    ! NO
          -999.9e3, &    ! O3
          -999.9e3, &    ! CO
          -999.9e3, &    ! FORM
          -999.9e3, &    ! ALD
          -999.9e3, &    ! PAN
          -999.9e3, &    ! MGLY
          -999.9e3, &    ! PAR
          -999.9e3, &    ! OLE
          -999.9e3, &    ! ETH
          -999.9e3, &    ! TOL
          -999.9e3, &    ! CRES
          -999.9e3, &    ! XYL
          1.77e3 , &    ! SO4a_f
          -999.9e3, &    ! SO2
          -999.9e3, &    ! HNO2
          -999.9e3, &    ! CH4
          -999.9e3, &    ! NH3
          -999.9e3, &    ! NO3
          -999.9e3, &    ! OH
          -999.9e3, &    ! HO2
          -999.9e3, &    ! N2O5
          1.77e3 , &    ! SO4a_c
          1.7e3  , &    ! NH4a_f
          1.5e3  , &    ! NO3a_f
          1.5e3  , &    ! NO3a_c
          -999.9e3, &    ! C2O3
          -999.9e3, &    ! XO2
          -999.9e3, &    ! XO2N
          -999.9e3, &    ! CRO
          -999.9e3, &    ! HNO3
          -999.9e3, &    ! H2O2
          -999.9e3, &    ! ISO
          -999.9e3, &    ! ISPD
          -999.9e3, &    ! TO2
          -999.9e3, &    ! OPEN
          -999.9e3, &    ! TERP
          1.0e3  , &    ! EC_f
          1.0e3  , &    ! EC_c
          1.9e3  , &    ! POM_f
          1.9e3  , &    ! POM_c
          1.9e3  , &    ! PPM_f
          1.9e3  , &    ! PPM_c
          2.165e3, &    ! Na_ff
          2.165e3, &    ! Na_f
          2.165e3, &    ! Na_c
          2.165e3, &    ! Na_cc
          2.165e3, &    ! Na_ccc
          2.65e3 , &    ! dust_ff
          2.65e3 , &    ! dust_f
          2.65e3 , &    ! dust_c
          2.65e3 , &    ! dust_cc
          2.65e3 , &    ! dust_ccc
          -999.9e3, &    ! tpm10
          -999.9e3, &    ! tpm25
          -999.9e3, &    ! tss
          -999.9e3, &    ! tdust
          -999.9e3, &    ! tc
          -999.9e3, &    ! tcg
          -999.9e3, &    ! tsoa
          -999.9e3, &    ! tnmvoc
          -999.9e3, &    ! SOxa
          -999.9e3, &    ! NOya
          -999.9e3, &    ! NHxa
          -999.9e3, &    ! NO2_obs
          -999.9e3, &    ! tpm10_biascorr
          -999.9e3, &    ! tpm25_biascorr
          -999.9e3 /)    ! o3_biascorr

  ! aerosol hygroscopicities:
  real, parameter  ::  aerhygro(nspec_all) = (/ &
          -999.9 , &    ! NO2
          -999.9 , &    ! NO
          -999.9 , &    ! O3
          -999.9 , &    ! CO
          -999.9 , &    ! FORM
          -999.9 , &    ! ALD
          -999.9 , &    ! PAN
          -999.9 , &    ! MGLY
          -999.9 , &    ! PAR
          -999.9 , &    ! OLE
          -999.9 , &    ! ETH
          -999.9 , &    ! TOL
          -999.9 , &    ! CRES
          -999.9 , &    ! XYL
          0.8    , &    ! SO4a_f
          -999.9 , &    ! SO2
          -999.9 , &    ! HNO2
          -999.9 , &    ! CH4
          -999.9 , &    ! NH3
          -999.9 , &    ! NO3
          -999.9 , &    ! OH
          -999.9 , &    ! HO2
          -999.9 , &    ! N2O5
          0.8    , &    ! SO4a_c
          0.8    , &    ! NH4a_f
          0.7    , &    ! NO3a_f
          0.7    , &    ! NO3a_c
          -999.9 , &    ! C2O3
          -999.9 , &    ! XO2
          -999.9 , &    ! XO2N
          -999.9 , &    ! CRO
          -999.9 , &    ! HNO3
          -999.9 , &    ! H2O2
          -999.9 , &    ! ISO
          -999.9 , &    ! ISPD
          -999.9 , &    ! TO2
          -999.9 , &    ! OPEN
          -999.9 , &    ! TERP
          0.0    , &    ! EC_f
          0.0    , &    ! EC_c
          0.1    , &    ! POM_f
          0.1    , &    ! POM_c
          0.1    , &    ! PPM_f
          0.1    , &    ! PPM_c
          1.0    , &    ! Na_ff
          1.0    , &    ! Na_f
          1.0    , &    ! Na_c
          1.0    , &    ! Na_cc
          1.0    , &    ! Na_ccc
          0.0    , &    ! dust_ff
          0.0    , &    ! dust_f
          0.0    , &    ! dust_c
          0.0    , &    ! dust_cc
          0.0    , &    ! dust_ccc
          -999.9 , &    ! tpm10
          -999.9 , &    ! tpm25
          -999.9 , &    ! tss
          -999.9 , &    ! tdust
          -999.9 , &    ! tc
          -999.9 , &    ! tcg
          -999.9 , &    ! tsoa
          -999.9 , &    ! tnmvoc
          -999.9 , &    ! SOxa
          -999.9 , &    ! NOya
          -999.9 , &    ! NHxa
          -999.9 , &    ! NO2_obs
          -999.9 , &    ! tpm10_biascorr
          -999.9 , &    ! tpm25_biascorr
          -999.9  /)    ! o3_biascorr

  ! aerosol modes:
  integer, parameter              ::  NO_AEROSOL_MODE     = 0
  integer, parameter              ::  AEROSOL_NUCL_MODE   = 1
  integer, parameter              ::  AEROSOL_AITKEN_MODE = 2
  integer, parameter              ::  AEROSOL_ACCUM_MODE  = 3
   integer, parameter              ::  AEROSOL_FF_MODES    = 4
  integer, parameter              ::  AEROSOL_FINE_MODES  = 5
  integer, parameter              ::  AEROSOL_CCC_MODE    = 6
  integer, parameter              ::  AEROSOL_CC_MODE     = 7
  integer, parameter              ::  AEROSOL_COARSE_MODE = 8
  integer, parameter              ::  AEROSOL_ALL_MODES   = 9
  integer, parameter              ::  AEROSOL_POLLEN_MODE = 10
  integer, parameter              ::  AEROSOL_ULTRA_FINE_MODE      = 11
  integer, parameter              ::  AEROSOL_ULTRA_FINE_FINE_MODE = 12
  integer, parameter              ::  AEROSOL_FINE_MEDIUM_MODE     = 13
  integer, parameter              ::  AEROSOL_MEDIUM_COARSE_MODE   = 14
  integer, parameter              ::  N_AEROSOL_MODES     = 14
  ! per tracer:
  integer, parameter  ::  specmode(nspec_all) = (/ &
      NO_AEROSOL_MODE , &    ! NO2
      NO_AEROSOL_MODE , &    ! NO
      NO_AEROSOL_MODE , &    ! O3
      NO_AEROSOL_MODE , &    ! CO
      NO_AEROSOL_MODE , &    ! FORM
      NO_AEROSOL_MODE , &    ! ALD
      NO_AEROSOL_MODE , &    ! PAN
      NO_AEROSOL_MODE , &    ! MGLY
      NO_AEROSOL_MODE , &    ! PAR
      NO_AEROSOL_MODE , &    ! OLE
      NO_AEROSOL_MODE , &    ! ETH
      NO_AEROSOL_MODE , &    ! TOL
      NO_AEROSOL_MODE , &    ! CRES
      NO_AEROSOL_MODE , &    ! XYL
      AEROSOL_FINE_MODES , &    ! SO4a_f
      NO_AEROSOL_MODE , &    ! SO2
      NO_AEROSOL_MODE , &    ! HNO2
      NO_AEROSOL_MODE , &    ! CH4
      NO_AEROSOL_MODE , &    ! NH3
      NO_AEROSOL_MODE , &    ! NO3
      NO_AEROSOL_MODE , &    ! OH
      NO_AEROSOL_MODE , &    ! HO2
      NO_AEROSOL_MODE , &    ! N2O5
      AEROSOL_COARSE_MODE, &    ! SO4a_c
      AEROSOL_FINE_MODES , &    ! NH4a_f
      AEROSOL_FINE_MODES , &    ! NO3a_f
      AEROSOL_COARSE_MODE, &    ! NO3a_c
      NO_AEROSOL_MODE , &    ! C2O3
      NO_AEROSOL_MODE , &    ! XO2
      NO_AEROSOL_MODE , &    ! XO2N
      NO_AEROSOL_MODE , &    ! CRO
      NO_AEROSOL_MODE , &    ! HNO3
      NO_AEROSOL_MODE , &    ! H2O2
      NO_AEROSOL_MODE , &    ! ISO
      NO_AEROSOL_MODE , &    ! ISPD
      NO_AEROSOL_MODE , &    ! TO2
      NO_AEROSOL_MODE , &    ! OPEN
      NO_AEROSOL_MODE , &    ! TERP
      AEROSOL_FINE_MODES , &    ! EC_f
      AEROSOL_COARSE_MODE, &    ! EC_c
      AEROSOL_FINE_MODES , &    ! POM_f
      AEROSOL_COARSE_MODE, &    ! POM_c
      AEROSOL_FINE_MODES , &    ! PPM_f
      AEROSOL_COARSE_MODE, &    ! PPM_c
      AEROSOL_FF_MODES , &    ! Na_ff
      AEROSOL_FINE_MODES , &    ! Na_f
      AEROSOL_COARSE_MODE, &    ! Na_c
      AEROSOL_CC_MODE, &    ! Na_cc
      AEROSOL_ccc_MODE, &    ! Na_ccc
      AEROSOL_FF_MODES , &    ! dust_ff
      AEROSOL_FINE_MODES , &    ! dust_f
      AEROSOL_COARSE_MODE, &    ! dust_c
      AEROSOL_CC_MODE, &    ! dust_cc
      AEROSOL_ccc_MODE, &    ! dust_ccc
      AEROSOL_All_MODES  , &    ! tpm10
      AEROSOL_FINE_MODES , &    ! tpm25
      AEROSOL_All_MODES  , &    ! tss
      AEROSOL_All_MODES  , &    ! tdust
      AEROSOL_All_MODES  , &    ! tc
      NO_AEROSOL_MODE , &    ! tcg
      AEROSOL_All_MODES  , &    ! tsoa
      NO_AEROSOL_MODE , &    ! tnmvoc
      AEROSOL_All_MODES  , &    ! SOxa
      AEROSOL_All_MODES  , &    ! NOya
      AEROSOL_All_MODES  , &    ! NHxa
      NO_AEROSOL_MODE , &    ! NO2_obs
      AEROSOL_All_MODES  , &    ! tpm10_biascorr
      AEROSOL_FINE_MODES , &    ! tpm25_biascorr
      NO_AEROSOL_MODE  /)    ! o3_biascorr

  ! number of advected tracers:
  integer, parameter  ::  n_advected = 46
  ! list of tracer indices:
  integer, parameter  ::  ispecs_advected(n_advected) = (/ &
      ispec_NO2     , &   ! NO2
      ispec_NO      , &   ! NO
      ispec_O3      , &   ! O3
      ispec_CO      , &   ! CO
      ispec_FORM    , &   ! FORM
      ispec_ALD     , &   ! ALD
      ispec_PAN     , &   ! PAN
      ispec_MGLY    , &   ! MGLY
      ispec_PAR     , &   ! PAR
      ispec_OLE     , &   ! OLE
      ispec_ETH     , &   ! ETH
      ispec_TOL     , &   ! TOL
      ispec_CRES    , &   ! CRES
      ispec_XYL     , &   ! XYL
      ispec_SO4a_f  , &   ! SO4a_f
      ispec_SO2     , &   ! SO2
      ispec_HNO2    , &   ! HNO2
      ispec_CH4     , &   ! CH4
      ispec_NH3     , &   ! NH3
      ispec_SO4a_c  , &   ! SO4a_c
      ispec_NH4a_f  , &   ! NH4a_f
      ispec_NO3a_f  , &   ! NO3a_f
      ispec_NO3a_c  , &   ! NO3a_c
      ispec_HNO3    , &   ! HNO3
      ispec_H2O2    , &   ! H2O2
      ispec_ISO     , &   ! ISO
      ispec_ISPD    , &   ! ISPD
      ispec_TO2     , &   ! TO2
      ispec_OPEN    , &   ! OPEN
      ispec_TERP    , &   ! TERP
      ispec_EC_f    , &   ! EC_f
      ispec_EC_c    , &   ! EC_c
      ispec_POM_f   , &   ! POM_f
      ispec_POM_c   , &   ! POM_c
      ispec_PPM_f   , &   ! PPM_f
      ispec_PPM_c   , &   ! PPM_c
      ispec_Na_ff   , &   ! Na_ff
      ispec_Na_f    , &   ! Na_f
      ispec_Na_c    , &   ! Na_c
      ispec_Na_cc   , &   ! Na_cc
      ispec_Na_ccc  , &   ! Na_ccc
      ispec_dust_ff , &   ! dust_ff
      ispec_dust_f  , &   ! dust_f
      ispec_dust_c  , &   ! dust_c
      ispec_dust_cc , &   ! dust_cc
      ispec_dust_ccc /)   ! dust_ccc

  ! number of aerosol tracers:
  integer, parameter  ::  n_aerosol = 21
  ! list of tracer indices:
  integer, parameter  ::  ispecs_aerosol(n_aerosol) = (/ &
      ispec_SO4a_f  , &   ! SO4a_f
      ispec_SO4a_c  , &   ! SO4a_c
      ispec_NH4a_f  , &   ! NH4a_f
      ispec_NO3a_f  , &   ! NO3a_f
      ispec_NO3a_c  , &   ! NO3a_c
      ispec_EC_f    , &   ! EC_f
      ispec_EC_c    , &   ! EC_c
      ispec_POM_f   , &   ! POM_f
      ispec_POM_c   , &   ! POM_c
      ispec_PPM_f   , &   ! PPM_f
      ispec_PPM_c   , &   ! PPM_c
      ispec_Na_ff   , &   ! Na_ff
      ispec_Na_f    , &   ! Na_f
      ispec_Na_c    , &   ! Na_c
      ispec_Na_cc   , &   ! Na_cc
      ispec_Na_ccc  , &   ! Na_ccc
      ispec_dust_ff , &   ! dust_ff
      ispec_dust_f  , &   ! dust_f
      ispec_dust_c  , &   ! dust_c
      ispec_dust_cc , &   ! dust_cc
      ispec_dust_ccc /)   ! dust_ccc

  ! number of pollen tracers:
  integer, parameter  ::  n_pollen = 0
  ! dummy list of tracer indices:
  integer, parameter  ::  ispecs_pollen(1) = (/ -1 /)

  ! number of sia tracers:
  integer, parameter  ::  n_sia = 5
  ! list of tracer indices:
  integer, parameter  ::  ispecs_sia(n_sia) = (/ &
      ispec_SO4a_f  , &   ! SO4a_f
      ispec_SO4a_c  , &   ! SO4a_c
      ispec_NH4a_f  , &   ! NH4a_f
      ispec_NO3a_f  , &   ! NO3a_f
      ispec_NO3a_c   /)   ! NO3a_c

  ! number of dust tracers:
  integer, parameter  ::  n_dust = 5
  ! list of tracer indices:
  integer, parameter  ::  ispecs_dust(n_dust) = (/ &
      ispec_dust_ff , &   ! dust_ff
      ispec_dust_f  , &   ! dust_f
      ispec_dust_c  , &   ! dust_c
      ispec_dust_cc , &   ! dust_cc
      ispec_dust_ccc /)   ! dust_ccc

  ! number of seasalt tracers:
  integer, parameter  ::  n_seasalt = 5
  ! list of tracer indices:
  integer, parameter  ::  ispecs_seasalt(n_seasalt) = (/ &
      ispec_Na_ff   , &   ! Na_ff
      ispec_Na_f    , &   ! Na_f
      ispec_Na_c    , &   ! Na_c
      ispec_Na_cc   , &   ! Na_cc
      ispec_Na_ccc   /)   ! Na_ccc

  ! number of m7 tracers:
  integer, parameter  ::  n_m7 = 0
  ! dummy list of tracer indices:
  integer, parameter  ::  ispecs_m7(1) = (/ -1 /)

  ! number of basecation tracers:
  integer, parameter  ::  n_basecation = 5
  ! list of tracer indices:
  integer, parameter  ::  ispecs_basecation(n_basecation) = (/ &
      ispec_Na_ff   , &   ! Na_ff
      ispec_Na_f    , &   ! Na_f
      ispec_Na_c    , &   ! Na_c
      ispec_Na_cc   , &   ! Na_cc
      ispec_Na_ccc   /)   ! Na_ccc

  ! number of hm tracers:
  integer, parameter  ::  n_hm = 0
  ! dummy list of tracer indices:
  integer, parameter  ::  ispecs_hm(1) = (/ -1 /)

  ! number of vbs tracers:
  integer, parameter  ::  n_vbs = 0
  ! dummy list of tracer indices:
  integer, parameter  ::  ispecs_vbs(1) = (/ -1 /)

  ! number of vbs_cg tracers:
  integer, parameter  ::  n_vbs_cg = 0
  ! dummy list of tracer indices:
  integer, parameter  ::  ispecs_vbs_cg(1) = (/ -1 /)

  ! number of vbs_soa tracers:
  integer, parameter  ::  n_vbs_soa = 0
  ! dummy list of tracer indices:
  integer, parameter  ::  ispecs_vbs_soa(1) = (/ -1 /)

  ! number of soa_prec tracers:
  integer, parameter  ::  n_soa_prec = 6
  ! list of tracer indices:
  integer, parameter  ::  ispecs_soa_prec(n_soa_prec) = (/ &
      ispec_PAR     , &   ! PAR
      ispec_OLE     , &   ! OLE
      ispec_TOL     , &   ! TOL
      ispec_XYL     , &   ! XYL
      ispec_ISO     , &   ! ISO
      ispec_TERP     /)   ! TERP

  ! number of nmvoc tracers:
  integer, parameter  ::  n_nmvoc = 11
  ! list of tracer indices:
  integer, parameter  ::  ispecs_nmvoc(n_nmvoc) = (/ &
      ispec_FORM    , &   ! FORM
      ispec_ALD     , &   ! ALD
      ispec_MGLY    , &   ! MGLY
      ispec_PAR     , &   ! PAR
      ispec_OLE     , &   ! OLE
      ispec_ETH     , &   ! ETH
      ispec_TOL     , &   ! TOL
      ispec_CRES    , &   ! CRES
      ispec_XYL     , &   ! XYL
      ispec_ISO     , &   ! ISO
      ispec_TERP     /)   ! TERP

  ! number of biascorr tracers:
  integer, parameter  ::  n_biascorr = 0
  ! dummy list of tracer indices:
  integer, parameter  ::  ispecs_biascorr(1) = (/ -1 /)

  ! number of pops tracers:
  integer, parameter  ::  n_pops = 0
  ! dummy list of tracer indices:
  integer, parameter  ::  ispecs_pops(1) = (/ -1 /)

  ! number of snox tracers:
  integer, parameter  ::  n_snox = 8
  ! list of tracer indices:
  integer, parameter  ::  ispecs_snox(n_snox) = (/ &
      ispec_NO2     , &   ! NO2
      ispec_PAN     , &   ! PAN
      ispec_NH3     , &   ! NH3
      ispec_NO3     , &   ! NO3
      ispec_NH4a_f  , &   ! NH4a_f
      ispec_NO3a_f  , &   ! NO3a_f
      ispec_NO3a_c  , &   ! NO3a_c
      ispec_HNO3     /)   ! HNO3

  ! number of pom tracers:
  integer, parameter  ::  n_pom = 2
  ! list of tracer indices:
  integer, parameter  ::  ispecs_pom(n_pom) = (/ &
      ispec_POM_f   , &   ! POM_f
      ispec_POM_c    /)   ! POM_c

  ! flags to check if tracer has "aerosol" property:
  logical, parameter  ::  tracer_is_aerosol(nspec_all) = (/ &
      .false.  , &   ! NO2
      .false.  , &   ! NO
      .false.  , &   ! O3
      .false.  , &   ! CO
      .false.  , &   ! FORM
      .false.  , &   ! ALD
      .false.  , &   ! PAN
      .false.  , &   ! MGLY
      .false.  , &   ! PAR
      .false.  , &   ! OLE
      .false.  , &   ! ETH
      .false.  , &   ! TOL
      .false.  , &   ! CRES
      .false.  , &   ! XYL
      .true.   , &   ! SO4a_f
      .false.  , &   ! SO2
      .false.  , &   ! HNO2
      .false.  , &   ! CH4
      .false.  , &   ! NH3
      .false.  , &   ! NO3
      .false.  , &   ! OH
      .false.  , &   ! HO2
      .false.  , &   ! N2O5
      .true.   , &   ! SO4a_c
      .true.   , &   ! NH4a_f
      .true.   , &   ! NO3a_f
      .true.   , &   ! NO3a_c
      .false.  , &   ! C2O3
      .false.  , &   ! XO2
      .false.  , &   ! XO2N
      .false.  , &   ! CRO
      .false.  , &   ! HNO3
      .false.  , &   ! H2O2
      .false.  , &   ! ISO
      .false.  , &   ! ISPD
      .false.  , &   ! TO2
      .false.  , &   ! OPEN
      .false.  , &   ! TERP
      .true.   , &   ! EC_f
      .true.   , &   ! EC_c
      .true.   , &   ! POM_f
      .true.   , &   ! POM_c
      .true.   , &   ! PPM_f
      .true.   , &   ! PPM_c
      .true.   , &   ! Na_ff
      .true.   , &   ! Na_f
      .true.   , &   ! Na_c
      .true.   , &   ! Na_cc
      .true.   , &   ! Na_ccc
      .true.   , &   ! dust_ff
      .true.   , &   ! dust_f
      .true.   , &   ! dust_c
      .true.   , &   ! dust_cc
      .true.   , &   ! dust_ccc
      .true.   , &   ! tpm10
      .true.   , &   ! tpm25
      .true.   , &   ! tss
      .true.   , &   ! tdust
      .true.   , &   ! tc
      .false.  , &   ! tcg
      .true.   , &   ! tsoa
      .false.  , &   ! tnmvoc
      .true.   , &   ! SOxa
      .true.   , &   ! NOya
      .true.   , &   ! NHxa
      .false.  , &   ! NO2_obs
      .true.   , &   ! tpm10_biascorr
      .true.   , &   ! tpm25_biascorr
      .false.   /)   ! o3_biascorr

  ! index of ispec in "ispecs_aerosol" list:
  integer, parameter  ::  ispec2aerosol(nspec) = (/ &
        -999 , &   ! NO2
        -999 , &   ! NO
        -999 , &   ! O3
        -999 , &   ! CO
        -999 , &   ! FORM
        -999 , &   ! ALD
        -999 , &   ! PAN
        -999 , &   ! MGLY
        -999 , &   ! PAR
        -999 , &   ! OLE
        -999 , &   ! ETH
        -999 , &   ! TOL
        -999 , &   ! CRES
        -999 , &   ! XYL
           1 , &   ! SO4a_f
        -999 , &   ! SO2
        -999 , &   ! HNO2
        -999 , &   ! CH4
        -999 , &   ! NH3
        -999 , &   ! NO3
        -999 , &   ! OH
        -999 , &   ! HO2
        -999 , &   ! N2O5
           2 , &   ! SO4a_c
           3 , &   ! NH4a_f
           4 , &   ! NO3a_f
           5 , &   ! NO3a_c
        -999 , &   ! C2O3
        -999 , &   ! XO2
        -999 , &   ! XO2N
        -999 , &   ! CRO
        -999 , &   ! HNO3
        -999 , &   ! H2O2
        -999 , &   ! ISO
        -999 , &   ! ISPD
        -999 , &   ! TO2
        -999 , &   ! OPEN
        -999 , &   ! TERP
           6 , &   ! EC_f
           7 , &   ! EC_c
           8 , &   ! POM_f
           9 , &   ! POM_c
          10 , &   ! PPM_f
          11 , &   ! PPM_c
          12 , &   ! Na_ff
          13 , &   ! Na_f
          14 , &   ! Na_c
          15 , &   ! Na_cc
          16 , &   ! Na_ccc
          17 , &   ! dust_ff
          18 , &   ! dust_f
          19 , &   ! dust_c
          20 , &   ! dust_cc
          21  /)   ! dust_ccc

  ! flags to check if tracer has "m7" property:
  logical, parameter  ::  tracer_is_m7(nspec_all) = (/ &
      .false.  , &   ! NO2
      .false.  , &   ! NO
      .false.  , &   ! O3
      .false.  , &   ! CO
      .false.  , &   ! FORM
      .false.  , &   ! ALD
      .false.  , &   ! PAN
      .false.  , &   ! MGLY
      .false.  , &   ! PAR
      .false.  , &   ! OLE
      .false.  , &   ! ETH
      .false.  , &   ! TOL
      .false.  , &   ! CRES
      .false.  , &   ! XYL
      .false.  , &   ! SO4a_f
      .false.  , &   ! SO2
      .false.  , &   ! HNO2
      .false.  , &   ! CH4
      .false.  , &   ! NH3
      .false.  , &   ! NO3
      .false.  , &   ! OH
      .false.  , &   ! HO2
      .false.  , &   ! N2O5
      .false.  , &   ! SO4a_c
      .false.  , &   ! NH4a_f
      .false.  , &   ! NO3a_f
      .false.  , &   ! NO3a_c
      .false.  , &   ! C2O3
      .false.  , &   ! XO2
      .false.  , &   ! XO2N
      .false.  , &   ! CRO
      .false.  , &   ! HNO3
      .false.  , &   ! H2O2
      .false.  , &   ! ISO
      .false.  , &   ! ISPD
      .false.  , &   ! TO2
      .false.  , &   ! OPEN
      .false.  , &   ! TERP
      .false.  , &   ! EC_f
      .false.  , &   ! EC_c
      .false.  , &   ! POM_f
      .false.  , &   ! POM_c
      .false.  , &   ! PPM_f
      .false.  , &   ! PPM_c
      .false.  , &   ! Na_ff
      .false.  , &   ! Na_f
      .false.  , &   ! Na_c
      .false.  , &   ! Na_cc
      .false.  , &   ! Na_ccc
      .false.  , &   ! dust_ff
      .false.  , &   ! dust_f
      .false.  , &   ! dust_c
      .false.  , &   ! dust_cc
      .false.  , &   ! dust_ccc
      .false.  , &   ! tpm10
      .false.  , &   ! tpm25
      .false.  , &   ! tss
      .false.  , &   ! tdust
      .false.  , &   ! tc
      .false.  , &   ! tcg
      .false.  , &   ! tsoa
      .false.  , &   ! tnmvoc
      .false.  , &   ! SOxa
      .false.  , &   ! NOya
      .false.  , &   ! NHxa
      .false.  , &   ! NO2_obs
      .false.  , &   ! tpm10_biascorr
      .false.  , &   ! tpm25_biascorr
      .false.   /)   ! o3_biascorr

  ! index of ispec in "ispecs_m7" list:
  integer, parameter  ::  ispec2m7(nspec) = (/ &
        -999 , &   ! NO2
        -999 , &   ! NO
        -999 , &   ! O3
        -999 , &   ! CO
        -999 , &   ! FORM
        -999 , &   ! ALD
        -999 , &   ! PAN
        -999 , &   ! MGLY
        -999 , &   ! PAR
        -999 , &   ! OLE
        -999 , &   ! ETH
        -999 , &   ! TOL
        -999 , &   ! CRES
        -999 , &   ! XYL
        -999 , &   ! SO4a_f
        -999 , &   ! SO2
        -999 , &   ! HNO2
        -999 , &   ! CH4
        -999 , &   ! NH3
        -999 , &   ! NO3
        -999 , &   ! OH
        -999 , &   ! HO2
        -999 , &   ! N2O5
        -999 , &   ! SO4a_c
        -999 , &   ! NH4a_f
        -999 , &   ! NO3a_f
        -999 , &   ! NO3a_c
        -999 , &   ! C2O3
        -999 , &   ! XO2
        -999 , &   ! XO2N
        -999 , &   ! CRO
        -999 , &   ! HNO3
        -999 , &   ! H2O2
        -999 , &   ! ISO
        -999 , &   ! ISPD
        -999 , &   ! TO2
        -999 , &   ! OPEN
        -999 , &   ! TERP
        -999 , &   ! EC_f
        -999 , &   ! EC_c
        -999 , &   ! POM_f
        -999 , &   ! POM_c
        -999 , &   ! PPM_f
        -999 , &   ! PPM_c
        -999 , &   ! Na_ff
        -999 , &   ! Na_f
        -999 , &   ! Na_c
        -999 , &   ! Na_cc
        -999 , &   ! Na_ccc
        -999 , &   ! dust_ff
        -999 , &   ! dust_f
        -999 , &   ! dust_c
        -999 , &   ! dust_cc
        -999  /)   ! dust_ccc

  ! flags to check if tracer has "soluble" property:
  logical, parameter  ::  tracer_is_soluble(nspec_all) = (/ &
      .false.  , &   ! NO2
      .false.  , &   ! NO
      .false.  , &   ! O3
      .false.  , &   ! CO
      .false.  , &   ! FORM
      .false.  , &   ! ALD
      .false.  , &   ! PAN
      .false.  , &   ! MGLY
      .false.  , &   ! PAR
      .false.  , &   ! OLE
      .false.  , &   ! ETH
      .false.  , &   ! TOL
      .false.  , &   ! CRES
      .false.  , &   ! XYL
      .false.  , &   ! SO4a_f
      .false.  , &   ! SO2
      .false.  , &   ! HNO2
      .false.  , &   ! CH4
      .false.  , &   ! NH3
      .false.  , &   ! NO3
      .false.  , &   ! OH
      .false.  , &   ! HO2
      .false.  , &   ! N2O5
      .false.  , &   ! SO4a_c
      .false.  , &   ! NH4a_f
      .false.  , &   ! NO3a_f
      .false.  , &   ! NO3a_c
      .false.  , &   ! C2O3
      .false.  , &   ! XO2
      .false.  , &   ! XO2N
      .false.  , &   ! CRO
      .false.  , &   ! HNO3
      .false.  , &   ! H2O2
      .false.  , &   ! ISO
      .false.  , &   ! ISPD
      .false.  , &   ! TO2
      .false.  , &   ! OPEN
      .false.  , &   ! TERP
      .false.  , &   ! EC_f
      .false.  , &   ! EC_c
      .false.  , &   ! POM_f
      .false.  , &   ! POM_c
      .false.  , &   ! PPM_f
      .false.  , &   ! PPM_c
      .false.  , &   ! Na_ff
      .false.  , &   ! Na_f
      .false.  , &   ! Na_c
      .false.  , &   ! Na_cc
      .false.  , &   ! Na_ccc
      .false.  , &   ! dust_ff
      .false.  , &   ! dust_f
      .false.  , &   ! dust_c
      .false.  , &   ! dust_cc
      .false.  , &   ! dust_ccc
      .false.  , &   ! tpm10
      .false.  , &   ! tpm25
      .false.  , &   ! tss
      .false.  , &   ! tdust
      .false.  , &   ! tc
      .false.  , &   ! tcg
      .false.  , &   ! tsoa
      .false.  , &   ! tnmvoc
      .false.  , &   ! SOxa
      .false.  , &   ! NOya
      .false.  , &   ! NHxa
      .false.  , &   ! NO2_obs
      .false.  , &   ! tpm10_biascorr
      .false.  , &   ! tpm25_biascorr
      .false.   /)   ! o3_biascorr

  ! index of ispec in "ispecs_soluble" list:
  integer, parameter  ::  ispec2soluble(nspec) = (/ &
        -999 , &   ! NO2
        -999 , &   ! NO
        -999 , &   ! O3
        -999 , &   ! CO
        -999 , &   ! FORM
        -999 , &   ! ALD
        -999 , &   ! PAN
        -999 , &   ! MGLY
        -999 , &   ! PAR
        -999 , &   ! OLE
        -999 , &   ! ETH
        -999 , &   ! TOL
        -999 , &   ! CRES
        -999 , &   ! XYL
        -999 , &   ! SO4a_f
        -999 , &   ! SO2
        -999 , &   ! HNO2
        -999 , &   ! CH4
        -999 , &   ! NH3
        -999 , &   ! NO3
        -999 , &   ! OH
        -999 , &   ! HO2
        -999 , &   ! N2O5
        -999 , &   ! SO4a_c
        -999 , &   ! NH4a_f
        -999 , &   ! NO3a_f
        -999 , &   ! NO3a_c
        -999 , &   ! C2O3
        -999 , &   ! XO2
        -999 , &   ! XO2N
        -999 , &   ! CRO
        -999 , &   ! HNO3
        -999 , &   ! H2O2
        -999 , &   ! ISO
        -999 , &   ! ISPD
        -999 , &   ! TO2
        -999 , &   ! OPEN
        -999 , &   ! TERP
        -999 , &   ! EC_f
        -999 , &   ! EC_c
        -999 , &   ! POM_f
        -999 , &   ! POM_c
        -999 , &   ! PPM_f
        -999 , &   ! PPM_c
        -999 , &   ! Na_ff
        -999 , &   ! Na_f
        -999 , &   ! Na_c
        -999 , &   ! Na_cc
        -999 , &   ! Na_ccc
        -999 , &   ! dust_ff
        -999 , &   ! dust_f
        -999 , &   ! dust_c
        -999 , &   ! dust_cc
        -999  /)   ! dust_ccc

  ! flags to check if tracer has "numberconc" property:
  logical, parameter  ::  tracer_is_numberconc(nspec_all) = (/ &
      .false.  , &   ! NO2
      .false.  , &   ! NO
      .false.  , &   ! O3
      .false.  , &   ! CO
      .false.  , &   ! FORM
      .false.  , &   ! ALD
      .false.  , &   ! PAN
      .false.  , &   ! MGLY
      .false.  , &   ! PAR
      .false.  , &   ! OLE
      .false.  , &   ! ETH
      .false.  , &   ! TOL
      .false.  , &   ! CRES
      .false.  , &   ! XYL
      .false.  , &   ! SO4a_f
      .false.  , &   ! SO2
      .false.  , &   ! HNO2
      .false.  , &   ! CH4
      .false.  , &   ! NH3
      .false.  , &   ! NO3
      .false.  , &   ! OH
      .false.  , &   ! HO2
      .false.  , &   ! N2O5
      .false.  , &   ! SO4a_c
      .false.  , &   ! NH4a_f
      .false.  , &   ! NO3a_f
      .false.  , &   ! NO3a_c
      .false.  , &   ! C2O3
      .false.  , &   ! XO2
      .false.  , &   ! XO2N
      .false.  , &   ! CRO
      .false.  , &   ! HNO3
      .false.  , &   ! H2O2
      .false.  , &   ! ISO
      .false.  , &   ! ISPD
      .false.  , &   ! TO2
      .false.  , &   ! OPEN
      .false.  , &   ! TERP
      .false.  , &   ! EC_f
      .false.  , &   ! EC_c
      .false.  , &   ! POM_f
      .false.  , &   ! POM_c
      .false.  , &   ! PPM_f
      .false.  , &   ! PPM_c
      .false.  , &   ! Na_ff
      .false.  , &   ! Na_f
      .false.  , &   ! Na_c
      .false.  , &   ! Na_cc
      .false.  , &   ! Na_ccc
      .false.  , &   ! dust_ff
      .false.  , &   ! dust_f
      .false.  , &   ! dust_c
      .false.  , &   ! dust_cc
      .false.  , &   ! dust_ccc
      .false.  , &   ! tpm10
      .false.  , &   ! tpm25
      .false.  , &   ! tss
      .false.  , &   ! tdust
      .false.  , &   ! tc
      .false.  , &   ! tcg
      .false.  , &   ! tsoa
      .false.  , &   ! tnmvoc
      .false.  , &   ! SOxa
      .false.  , &   ! NOya
      .false.  , &   ! NHxa
      .false.  , &   ! NO2_obs
      .false.  , &   ! tpm10_biascorr
      .false.  , &   ! tpm25_biascorr
      .false.   /)   ! o3_biascorr

  ! index of ispec in "ispecs_numberconc" list:
  integer, parameter  ::  ispec2numberconc(nspec) = (/ &
        -999 , &   ! NO2
        -999 , &   ! NO
        -999 , &   ! O3
        -999 , &   ! CO
        -999 , &   ! FORM
        -999 , &   ! ALD
        -999 , &   ! PAN
        -999 , &   ! MGLY
        -999 , &   ! PAR
        -999 , &   ! OLE
        -999 , &   ! ETH
        -999 , &   ! TOL
        -999 , &   ! CRES
        -999 , &   ! XYL
        -999 , &   ! SO4a_f
        -999 , &   ! SO2
        -999 , &   ! HNO2
        -999 , &   ! CH4
        -999 , &   ! NH3
        -999 , &   ! NO3
        -999 , &   ! OH
        -999 , &   ! HO2
        -999 , &   ! N2O5
        -999 , &   ! SO4a_c
        -999 , &   ! NH4a_f
        -999 , &   ! NO3a_f
        -999 , &   ! NO3a_c
        -999 , &   ! C2O3
        -999 , &   ! XO2
        -999 , &   ! XO2N
        -999 , &   ! CRO
        -999 , &   ! HNO3
        -999 , &   ! H2O2
        -999 , &   ! ISO
        -999 , &   ! ISPD
        -999 , &   ! TO2
        -999 , &   ! OPEN
        -999 , &   ! TERP
        -999 , &   ! EC_f
        -999 , &   ! EC_c
        -999 , &   ! POM_f
        -999 , &   ! POM_c
        -999 , &   ! PPM_f
        -999 , &   ! PPM_c
        -999 , &   ! Na_ff
        -999 , &   ! Na_f
        -999 , &   ! Na_c
        -999 , &   ! Na_cc
        -999 , &   ! Na_ccc
        -999 , &   ! dust_ff
        -999 , &   ! dust_f
        -999 , &   ! dust_c
        -999 , &   ! dust_cc
        -999  /)   ! dust_ccc

  ! flags to check if tracer has "pollen" property:
  logical, parameter  ::  tracer_is_pollen(nspec_all) = (/ &
      .false.  , &   ! NO2
      .false.  , &   ! NO
      .false.  , &   ! O3
      .false.  , &   ! CO
      .false.  , &   ! FORM
      .false.  , &   ! ALD
      .false.  , &   ! PAN
      .false.  , &   ! MGLY
      .false.  , &   ! PAR
      .false.  , &   ! OLE
      .false.  , &   ! ETH
      .false.  , &   ! TOL
      .false.  , &   ! CRES
      .false.  , &   ! XYL
      .false.  , &   ! SO4a_f
      .false.  , &   ! SO2
      .false.  , &   ! HNO2
      .false.  , &   ! CH4
      .false.  , &   ! NH3
      .false.  , &   ! NO3
      .false.  , &   ! OH
      .false.  , &   ! HO2
      .false.  , &   ! N2O5
      .false.  , &   ! SO4a_c
      .false.  , &   ! NH4a_f
      .false.  , &   ! NO3a_f
      .false.  , &   ! NO3a_c
      .false.  , &   ! C2O3
      .false.  , &   ! XO2
      .false.  , &   ! XO2N
      .false.  , &   ! CRO
      .false.  , &   ! HNO3
      .false.  , &   ! H2O2
      .false.  , &   ! ISO
      .false.  , &   ! ISPD
      .false.  , &   ! TO2
      .false.  , &   ! OPEN
      .false.  , &   ! TERP
      .false.  , &   ! EC_f
      .false.  , &   ! EC_c
      .false.  , &   ! POM_f
      .false.  , &   ! POM_c
      .false.  , &   ! PPM_f
      .false.  , &   ! PPM_c
      .false.  , &   ! Na_ff
      .false.  , &   ! Na_f
      .false.  , &   ! Na_c
      .false.  , &   ! Na_cc
      .false.  , &   ! Na_ccc
      .false.  , &   ! dust_ff
      .false.  , &   ! dust_f
      .false.  , &   ! dust_c
      .false.  , &   ! dust_cc
      .false.  , &   ! dust_ccc
      .false.  , &   ! tpm10
      .false.  , &   ! tpm25
      .false.  , &   ! tss
      .false.  , &   ! tdust
      .false.  , &   ! tc
      .false.  , &   ! tcg
      .false.  , &   ! tsoa
      .false.  , &   ! tnmvoc
      .false.  , &   ! SOxa
      .false.  , &   ! NOya
      .false.  , &   ! NHxa
      .false.  , &   ! NO2_obs
      .false.  , &   ! tpm10_biascorr
      .false.  , &   ! tpm25_biascorr
      .false.   /)   ! o3_biascorr

  ! index of ispec in "ispecs_pollen" list:
  integer, parameter  ::  ispec2pollen(nspec) = (/ &
        -999 , &   ! NO2
        -999 , &   ! NO
        -999 , &   ! O3
        -999 , &   ! CO
        -999 , &   ! FORM
        -999 , &   ! ALD
        -999 , &   ! PAN
        -999 , &   ! MGLY
        -999 , &   ! PAR
        -999 , &   ! OLE
        -999 , &   ! ETH
        -999 , &   ! TOL
        -999 , &   ! CRES
        -999 , &   ! XYL
        -999 , &   ! SO4a_f
        -999 , &   ! SO2
        -999 , &   ! HNO2
        -999 , &   ! CH4
        -999 , &   ! NH3
        -999 , &   ! NO3
        -999 , &   ! OH
        -999 , &   ! HO2
        -999 , &   ! N2O5
        -999 , &   ! SO4a_c
        -999 , &   ! NH4a_f
        -999 , &   ! NO3a_f
        -999 , &   ! NO3a_c
        -999 , &   ! C2O3
        -999 , &   ! XO2
        -999 , &   ! XO2N
        -999 , &   ! CRO
        -999 , &   ! HNO3
        -999 , &   ! H2O2
        -999 , &   ! ISO
        -999 , &   ! ISPD
        -999 , &   ! TO2
        -999 , &   ! OPEN
        -999 , &   ! TERP
        -999 , &   ! EC_f
        -999 , &   ! EC_c
        -999 , &   ! POM_f
        -999 , &   ! POM_c
        -999 , &   ! PPM_f
        -999 , &   ! PPM_c
        -999 , &   ! Na_ff
        -999 , &   ! Na_f
        -999 , &   ! Na_c
        -999 , &   ! Na_cc
        -999 , &   ! Na_ccc
        -999 , &   ! dust_ff
        -999 , &   ! dust_f
        -999 , &   ! dust_c
        -999 , &   ! dust_cc
        -999  /)   ! dust_ccc

  ! flags to check if tracer has "seasalt" property:
  logical, parameter  ::  tracer_is_seasalt(nspec_all) = (/ &
      .false.  , &   ! NO2
      .false.  , &   ! NO
      .false.  , &   ! O3
      .false.  , &   ! CO
      .false.  , &   ! FORM
      .false.  , &   ! ALD
      .false.  , &   ! PAN
      .false.  , &   ! MGLY
      .false.  , &   ! PAR
      .false.  , &   ! OLE
      .false.  , &   ! ETH
      .false.  , &   ! TOL
      .false.  , &   ! CRES
      .false.  , &   ! XYL
      .false.  , &   ! SO4a_f
      .false.  , &   ! SO2
      .false.  , &   ! HNO2
      .false.  , &   ! CH4
      .false.  , &   ! NH3
      .false.  , &   ! NO3
      .false.  , &   ! OH
      .false.  , &   ! HO2
      .false.  , &   ! N2O5
      .false.  , &   ! SO4a_c
      .false.  , &   ! NH4a_f
      .false.  , &   ! NO3a_f
      .false.  , &   ! NO3a_c
      .false.  , &   ! C2O3
      .false.  , &   ! XO2
      .false.  , &   ! XO2N
      .false.  , &   ! CRO
      .false.  , &   ! HNO3
      .false.  , &   ! H2O2
      .false.  , &   ! ISO
      .false.  , &   ! ISPD
      .false.  , &   ! TO2
      .false.  , &   ! OPEN
      .false.  , &   ! TERP
      .false.  , &   ! EC_f
      .false.  , &   ! EC_c
      .false.  , &   ! POM_f
      .false.  , &   ! POM_c
      .false.  , &   ! PPM_f
      .false.  , &   ! PPM_c
      .true.   , &   ! Na_ff
      .true.   , &   ! Na_f
      .true.   , &   ! Na_c
      .true.   , &   ! Na_cc
      .true.   , &   ! Na_ccc
      .false.  , &   ! dust_ff
      .false.  , &   ! dust_f
      .false.  , &   ! dust_c
      .false.  , &   ! dust_cc
      .false.  , &   ! dust_ccc
      .false.  , &   ! tpm10
      .false.  , &   ! tpm25
      .false.  , &   ! tss
      .false.  , &   ! tdust
      .false.  , &   ! tc
      .false.  , &   ! tcg
      .false.  , &   ! tsoa
      .false.  , &   ! tnmvoc
      .false.  , &   ! SOxa
      .false.  , &   ! NOya
      .false.  , &   ! NHxa
      .false.  , &   ! NO2_obs
      .false.  , &   ! tpm10_biascorr
      .false.  , &   ! tpm25_biascorr
      .false.   /)   ! o3_biascorr

  ! index of ispec in "ispecs_seasalt" list:
  integer, parameter  ::  ispec2seasalt(nspec) = (/ &
        -999 , &   ! NO2
        -999 , &   ! NO
        -999 , &   ! O3
        -999 , &   ! CO
        -999 , &   ! FORM
        -999 , &   ! ALD
        -999 , &   ! PAN
        -999 , &   ! MGLY
        -999 , &   ! PAR
        -999 , &   ! OLE
        -999 , &   ! ETH
        -999 , &   ! TOL
        -999 , &   ! CRES
        -999 , &   ! XYL
        -999 , &   ! SO4a_f
        -999 , &   ! SO2
        -999 , &   ! HNO2
        -999 , &   ! CH4
        -999 , &   ! NH3
        -999 , &   ! NO3
        -999 , &   ! OH
        -999 , &   ! HO2
        -999 , &   ! N2O5
        -999 , &   ! SO4a_c
        -999 , &   ! NH4a_f
        -999 , &   ! NO3a_f
        -999 , &   ! NO3a_c
        -999 , &   ! C2O3
        -999 , &   ! XO2
        -999 , &   ! XO2N
        -999 , &   ! CRO
        -999 , &   ! HNO3
        -999 , &   ! H2O2
        -999 , &   ! ISO
        -999 , &   ! ISPD
        -999 , &   ! TO2
        -999 , &   ! OPEN
        -999 , &   ! TERP
        -999 , &   ! EC_f
        -999 , &   ! EC_c
        -999 , &   ! POM_f
        -999 , &   ! POM_c
        -999 , &   ! PPM_f
        -999 , &   ! PPM_c
           1 , &   ! Na_ff
           2 , &   ! Na_f
           3 , &   ! Na_c
           4 , &   ! Na_cc
           5 , &   ! Na_ccc
        -999 , &   ! dust_ff
        -999 , &   ! dust_f
        -999 , &   ! dust_c
        -999 , &   ! dust_cc
        -999  /)   ! dust_ccc

  ! flags to check if tracer has "dust" property:
  logical, parameter  ::  tracer_is_dust(nspec_all) = (/ &
      .false.  , &   ! NO2
      .false.  , &   ! NO
      .false.  , &   ! O3
      .false.  , &   ! CO
      .false.  , &   ! FORM
      .false.  , &   ! ALD
      .false.  , &   ! PAN
      .false.  , &   ! MGLY
      .false.  , &   ! PAR
      .false.  , &   ! OLE
      .false.  , &   ! ETH
      .false.  , &   ! TOL
      .false.  , &   ! CRES
      .false.  , &   ! XYL
      .false.  , &   ! SO4a_f
      .false.  , &   ! SO2
      .false.  , &   ! HNO2
      .false.  , &   ! CH4
      .false.  , &   ! NH3
      .false.  , &   ! NO3
      .false.  , &   ! OH
      .false.  , &   ! HO2
      .false.  , &   ! N2O5
      .false.  , &   ! SO4a_c
      .false.  , &   ! NH4a_f
      .false.  , &   ! NO3a_f
      .false.  , &   ! NO3a_c
      .false.  , &   ! C2O3
      .false.  , &   ! XO2
      .false.  , &   ! XO2N
      .false.  , &   ! CRO
      .false.  , &   ! HNO3
      .false.  , &   ! H2O2
      .false.  , &   ! ISO
      .false.  , &   ! ISPD
      .false.  , &   ! TO2
      .false.  , &   ! OPEN
      .false.  , &   ! TERP
      .false.  , &   ! EC_f
      .false.  , &   ! EC_c
      .false.  , &   ! POM_f
      .false.  , &   ! POM_c
      .false.  , &   ! PPM_f
      .false.  , &   ! PPM_c
      .false.  , &   ! Na_ff
      .false.  , &   ! Na_f
      .false.  , &   ! Na_c
      .false.  , &   ! Na_cc
      .false.  , &   ! Na_ccc
      .true.   , &   ! dust_ff
      .true.   , &   ! dust_f
      .true.   , &   ! dust_c
      .true.   , &   ! dust_cc
      .true.   , &   ! dust_ccc
      .false.  , &   ! tpm10
      .false.  , &   ! tpm25
      .false.  , &   ! tss
      .false.  , &   ! tdust
      .false.  , &   ! tc
      .false.  , &   ! tcg
      .false.  , &   ! tsoa
      .false.  , &   ! tnmvoc
      .false.  , &   ! SOxa
      .false.  , &   ! NOya
      .false.  , &   ! NHxa
      .false.  , &   ! NO2_obs
      .false.  , &   ! tpm10_biascorr
      .false.  , &   ! tpm25_biascorr
      .false.   /)   ! o3_biascorr

  ! index of ispec in "ispecs_dust" list:
  integer, parameter  ::  ispec2dust(nspec) = (/ &
        -999 , &   ! NO2
        -999 , &   ! NO
        -999 , &   ! O3
        -999 , &   ! CO
        -999 , &   ! FORM
        -999 , &   ! ALD
        -999 , &   ! PAN
        -999 , &   ! MGLY
        -999 , &   ! PAR
        -999 , &   ! OLE
        -999 , &   ! ETH
        -999 , &   ! TOL
        -999 , &   ! CRES
        -999 , &   ! XYL
        -999 , &   ! SO4a_f
        -999 , &   ! SO2
        -999 , &   ! HNO2
        -999 , &   ! CH4
        -999 , &   ! NH3
        -999 , &   ! NO3
        -999 , &   ! OH
        -999 , &   ! HO2
        -999 , &   ! N2O5
        -999 , &   ! SO4a_c
        -999 , &   ! NH4a_f
        -999 , &   ! NO3a_f
        -999 , &   ! NO3a_c
        -999 , &   ! C2O3
        -999 , &   ! XO2
        -999 , &   ! XO2N
        -999 , &   ! CRO
        -999 , &   ! HNO3
        -999 , &   ! H2O2
        -999 , &   ! ISO
        -999 , &   ! ISPD
        -999 , &   ! TO2
        -999 , &   ! OPEN
        -999 , &   ! TERP
        -999 , &   ! EC_f
        -999 , &   ! EC_c
        -999 , &   ! POM_f
        -999 , &   ! POM_c
        -999 , &   ! PPM_f
        -999 , &   ! PPM_c
        -999 , &   ! Na_ff
        -999 , &   ! Na_f
        -999 , &   ! Na_c
        -999 , &   ! Na_cc
        -999 , &   ! Na_ccc
           1 , &   ! dust_ff
           2 , &   ! dust_f
           3 , &   ! dust_c
           4 , &   ! dust_cc
           5  /)   ! dust_ccc

