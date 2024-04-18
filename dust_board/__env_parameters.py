import datetime


class EnvParameters:
    r"""
    Class of dictionary of environmental parameters to run the download script
    """

    def __init__(self):
        # Initialise datetime.datetime object and transform to format matching DWD server
        self.model_init_time = datetime.datetime.now().replace(hour=0, minute=0).isoformat(
            timespec='minutes')
        # Local destination path for different model file (Will be automatically created if not exist)
        self.local_directory = {
            'dwd_art': '/Users/corneliustai/prj/dwd_art',
            'dwd_art_eu': '/Users/corneliustai/prj/dwd_art_eu',
            'dwd_nwp': '/Users/corneliustai/prj/dwd_nwp',
            'dwd_nwp_eu': '/Users/corneliustai/prj/dwd_nwp_eu'
        }
        # URL to the variable name page of the models
        self.model_url = {
            'dwd_art': 'https://opendata.dwd.de/weather/nwp/icon-art/v1/p/',
            'dwd_art_eu': 'https://opendata.dwd.de/weather/nwp/icon-art-eu/v1/p/',
            'dwd_nwp': 'https://opendata.dwd.de/weather/nwp/icon/grib/00/',
            'dwd_nwp_eu': 'https://opendata.dwd.de/weather/nwp/icon-eu/grib/00/'
        }
        # relative path from vairable name page to list of model variable files
        self.dwd_art_variable = {
            'taod_dust': 'TAOD_DUST/lvt/10_10/lv/0.0/r/%s/s/' % (self.model_init_time),
            'dust_total_mc': 'DUST_TOTAL_MC/lvt/150_GENV/lv/120.0/r/%s/s/' % (self.model_init_time),
            't_2m': 'T_2M/lvt/103_HG/lv/2.0/r/%s/s/' % (self.model_init_time),
            'asob_s': 'ASOB_S/lvt/1_G/lv/0.0/r/%s/s/' % (self.model_init_time),
            'aswdifd_s': 'ASWDIFD_S/lvt/1_G/lv/0.0/r/%s/s/' % (self.model_init_time)
        }
        # relative path from vairable name page to list of model variable files
        self.dwd_art_eu_variable = {
            'taod_dust': 'TAOD_DUST/lvt/10_10/lv/0.0/r/%s/s/' % (self.model_init_time),
            'dust_total_mc': 'DUST_TOTAL_MC/lvt/150_GENV/lv/74.0/r/%s/s/',
            't_2m': 'T_2M/lvt/103_HG/lv/2.0/r/%s/s/' % (self.model_init_time),
            'asob_s': 'ASOB_S/lvt/1_G/lv/0.0/r/%s/s/' % (self.model_init_time),
            'aswdifd_s': 'ASWDIFD_S/lvt/1_G/lv/0.0/r/%s/s/' % (self.model_init_time)
        }
        # relative path from vairable name page to list of model variable files
        self.dwd_nwp_variable = {
            'asob_s': 'asob_s/',
            'aswdifd_s': 'aswdifd_s/',
            't_2m': 't_2m/'
        }
        # relative path from vairable name page to list of model variable files
        self.dwd_nwp_eu_variable = {
            'asob_s': 'asob_s/',
            'aswdifd_s': 'aswdifd_s/',
            't_2m': 't_2m/'
        }
        # Long names of the variables, used in Plotting routine
        self.long_names = {
            'taod_dust': 'Column integrated Atmospheric Optical Depth',
            'dust_total_mc': 'Total Dust',
            't_2m': '2M Surface Temperature',
            'asob_s': 'Net surface direct solar radiation',
            'aswdifd_s': 'Surface downwards diffused solar radiation',
        }
        # Path to source file grid (ICON), can be obtained in MPI server. ICON-NWP: R3B6
        self.source_grid = {
            'dwd_art': '/Users/corneliustai/prj/icon_grid_0005_R03B06_R.nc',
            'dwd_art_eu': '/path/to/grid',
            'dwd_nwp': '/path/to/grid',
            'dwd_nwp_eu': '/path/to/grid'
        }
        # Path to destination grid, see cdo documentary for the format of the file used in remapnn
        self.target_grid = {
            'dwd_art': '/path/to/dwd_nwp.txt',
            'dwd_art_eu': '/path/to/dwd_nwp_eu.txt',
            'dwd_nwp': '/Users/corneliustai/PycharmProjects/ml_superspecies/dust_board/dwd_nwp.txt',
            'dwd_nwp_eu': '/path/to/dwd_nwp_eu.txt'
        }
        # Meteogram line colour
        self.meteogram_colour = {
            'dwd_art': 'red',
            'dwd_art_eu': 'blue',
            'dwd_nwp': 'green',
            'dwd_nwp_eu': 'orange'
        }
        # Long Name of the model, used in Plotting routine
        self.model_long_names = {
            'dwd_art': 'ICON-ART',
            'dwd_art_eu': 'ICON-ART-EU',
            'dwd_nwp': 'ICON-NWP',
            'dwd_nwp_eu': 'ICON-NWP-EU'
        }
        # Enable Multi-processing (NOT YET IMPLEMENTED)
        self.multi_process = {
            'switch': False,
            'n_process': 1
        }
