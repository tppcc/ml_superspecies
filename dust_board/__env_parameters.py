import datetime
import json
import os


class EnvParameters:
    r"""
    Class of dictionary of environmental parameters to run the download script
    Change the parameters to suit your need
    """
    def __init__(self):
        self.model_init_time = datetime.datetime.now().replace(hour=0, minute=0).isoformat(
            timespec='minutes')
        self.local_directory = {
            'dwd_nwp': '/Users/corneliustai/prj/dwd_nwp',
        }
        # URL to the variable name page of the models
        self.model_url = {
            'dwd_nwp': 'https://opendata.dwd.de/weather/nwp/icon/grib/00/',
        }
        # relative path from vairable name page to list of model variable files
        self.dwd_art_variable = {
            't_2m': 'T_2M/lvt/103_HG/lv/2.0/r/%s/s/' % (self.model_init_time),
        }
        self.dwd_art_eu_variable = {
            't_2m': 'T_2M/lvt/103_HG/lv/2.0/r/%s/s/' % (self.model_init_time),
        }
        self.dwd_nwp_variable = {
            't_2m': 't_2m/'
        }
        self.dwd_nwp_eu_variable = {
            't_2m': 't_2m/'
        }
        self.long_names = {
            'taod_dust': 'Column integrated Atmospheric Optical Depth',
            'dust_total_mc': 'Total Dust',
            't_2m': '2M Surface Temperature',
            'asob_s': 'Net surface direct solar radiation',
            'aswdifd_s': 'Surface downwards diffused solar radiation',
        }
        self.source_grid = {
            'dwd_nwp': '/Users/corneliustai/prj/icon_grid_0006_R03B07_G.nc'
        }
        self.target_grid = {
            'dwd_art': '/Users/corneliustai/PycharmProjects/ml_superspecies/dust_board/dwd_nwp.txt',
            'dwd_art_eu': '/path/to/dwd_nwp_eu.txt',
            'dwd_nwp': '/Users/corneliustai/PycharmProjects/ml_superspecies/dust_board/dwd_nwp.txt',
            'dwd_nwp_eu': '/path/to/dwd_nwp_eu.txt'
        }
        self.meteogram_colour = {
            'dwd_art': 'red',
            'dwd_art_eu': 'blue',
            'dwd_nwp': 'green',
            'dwd_nwp_eu': 'orange'
        }
        self.model_long_names = {
            'dwd_art': 'ICON-ART',
            'dwd_art_eu': 'ICON-ART-EU',
            'dwd_nwp': 'ICON-NWP',
            'dwd_nwp_eu': 'ICON-NWP-EU'
        }
        self.icon_name = {
            't_2m': '2t'
        }