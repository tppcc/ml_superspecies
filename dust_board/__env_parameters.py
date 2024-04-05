import datetime
import json
import os


class EnvParameters:
    r"""
    Class of dictionary of environmental parameters to run the download script
    """
    def __init__(self):
        self.model_init_time = datetime.datetime.now().replace(hour=0, minute=0).isoformat(
            timespec='minutes')
        self.local_directory = {
            'dwd_art': '/prj/dwd_art',
            'dwd_art_eu': '/prj/dwd_art_eu',
            'dwd_nwp': '/prj/dwd_nwp',
            'dwd_nwp_eu': 'prj/dwd_nwp_eu'
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
            'taod_dust': 'TAOD_DUST/lvt/10_10/lv/0.0/r/%s/s' % (self.model_init_time),
            'dust_total_mc': 'DUST_TOTAL_MC/lvt/150_GENV/lv/120.0/r/%s/s' % (self.model_init_time),
            't_2m': 'T_2M/lvt/103_HG/lv/2.0/r/%s/s' % (self.model_init_time),
            'asob_s': 'ASOB_S/lvt/1_G/lv/0.0/r/%s/s/' % (self.model_init_time),
            'aswdifd_s': 'ASWDIFD_S/lvt/1_G/lv/0.0/r/%s/s/' % (self.model_init_time)
        }
        self.dwd_art_eu_variable = {
            'taod_dust': 'TAOD_DUST/lvt/10_10/lv/0.0/r/%s/s' % (self.model_init_time),
            'DUST_TOTAL_MC': 'DUST_TOTAL_MC/lvt/150_GENV/lv/74.0/r/%s/s',
            't_2m': 'T_2M/lvt/103_HG/lv/2.0/r/%s/s' % (self.model_init_time),
            'asob_s': 'ASOB_S/lvt/1_G/lv/0.0/r/%s/s/' % (self.model_init_time),
            'aswdifd_s': 'ASWDIFD_S/lvt/1_G/lv/0.0/r/%s/s/' % (self.model_init_time)
        }
        self.dwd_nwp_variable = {
            'asob_s': 'asob_s/',
            'aswdifd_s/': 'aswdifd_s/',
            't_2m': 't_2m/'
        }
        self.dwd_nwp_eu_variable = {
            'asob_s': 'asob_s/',
            'aswdifd_s/': 'aswdifd_s/',
            't_2m': 't_2m/'
        }
        self.long_names = {
            'taod_dust': 'Column integrated Atmospheric Optical Depth',
            'dust_total_mc': 'Total Dust',
            't_2m': '2M Surface Temperature',
            'asob_s': 'Net surface direct solar radiation',
            'aswdifd_s': 'Surface downwards diffused solar radiation',
        }
        self.icon_grid = {
            'dwd_nwp': 'icon_0005_R03B06_R.nc'
        }