import xarray as xr
import os
import matplotlib as mpl
import matplotlib.pyplot as plt

def Grib2nc(fpath, grid_file_directory):
    os.system('module load cdo')
    # Assign grid to
    os.system('cdo -f nc4 -remapnn, icon_pollen_description.txt f{fpath} f{path}.nc')

def LoadArray(path, ):
    r"""Load the downloaded file, load as xarray array, concat along time and return as object
    :param path:
    :return:
    """
    xr.load_dataset()