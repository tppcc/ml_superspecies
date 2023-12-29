Strip complete code to get only modules for Open-source version

  Functions to skip:
    o Labeling
    o M7
    o Pollen
    o Pops
    o OPS-emissions
    o Mars-Modified aerosol chemistry
    o Radiation property calculations
    o Sulphur/Methane only settings
    o MODIS/OMI/GOME output functions
    o Overall clean-up of all rc-files  
    
    o New AOD calculations, Polder???
    o RACMO meteo, WRF meteo, Cosmo meteo??
    o Plume in Grid ??
    
  Labeling/M7/Pollen
    - use script bin/remove_labeling_m7_pollen.py  ----> First change destination name in script
    - clean up labeling parts in rc-files (manually)
    - clean up pollen parts in rc-files
    - clean up m7 parts in rc-files
    
    remove pollen/m7 tracers in input file
      data/tracers.csv    ???
      data/reactions.csv  ???    
    
        
  Pops    
    - remove specific POP routines
        src/le_pop_chem.F90
        src/le_pop_depodry_kz.F90
        src/le_pop_module.F90
        src/le_pop_proces.F90
        src/le_pop_soil.F90
        src/le_pop_update.F90
        src/le_pop_water.F90
    - remove POP parts from wet deposition
        src/le_wetdepos_emep.F90
    - remove POP parts from emission
        src/le_emis.F90
    - remove POP parts from driver
        src/le_driver.F90
    - remove POP parts from boundary conditions
        src/le_bound_clim_const.F90
    - remove POP indices and tracers
        src/le_indices.F90              ?
        data/tracers.csv                ?
    - clean up pop parts in rc-files
   
  OPS-emissions
    - remove specific routine for OPS-emissions
        src/le_emis_ops.F90
        src/le_emis_ops_ec_fractions.F90
        src/le_emis_ops_output.F90
        src/le_emis_ops_pm_composition.F90
    - remove OPS parts in emission routine
        src/le_emis.F90
    - remove ops PM tracers and indices
        src/le_indices.F90              ?
        data/tracers.csv                ?
    - clean up ops-emis parts in rc-file
   
  MARS-Modified aerosol chemistry
    - remove specific mars routine
        src/le_sia_mars_modified.F90
    - remove mars from chemistry part
        src/le_chem.F90
    - clean mars-modified option in rc-files
        
  Radiation
    - remove specific radiation routine
        src/le_radiation.F90
    - remove radiation from driver parts
        src/le_driver.F90
    - remove radiation parts from output
        src/le_output_dat.F90
    - clean up radiation parts in rc-files
   
  Sulphur-only/Methane-only
    - remove specific routine 
        src/le_chem_oh.F90
    - remove options in driver
        src/le_driver.F90
    - remove options in dims
        src/le_dims.F90
    - clean up methane/sulphur only in rc-files
        
  Modis/GOME/OMI output
    - remove specific output routines
        src/le_output_omi_trc.F90  ( gome use the same routine)
        src/le_output_modis.F90
        src/modis.F90
    - remove OMI parts from output routine
        src/le_output.F90
    - clean up satellite output in rc-files
    
  RACMO meteo
    - remove specific RACMO routines (from v2.0.000, not compatible anymore, only for inspiration
        src/le_meteo_racmo_caldat.F90
        src/le_meteo_racmo.F90
        src/le_meteo_racmo_get.F90
        src/le_meteo_racmo_julday.F90
        src/le_meteo_racmo_nrtype.F90
        src/le_meteo_racmo_nrutil.F90
    - clean up racmo part from rc-files
    
  WRF meteo (from v2.0.000 not compatible anymore
    -clean up parts in rc-file ?
    
  Overall clean-up of rc-files
    - remove all extra emission properties
        OPS, Pollen, co2op, macc, macc_ii, parest, parest2
        hm, hm-natural, PN, sibcasa??, point
    
