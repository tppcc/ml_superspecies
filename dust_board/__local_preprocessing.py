import xarray as xr
import os
import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np

def Grib2nc(fpath, source_grid, target_grid):
    os.system('module load cdo')
    # Assign grid to
    os.system(f'cdo -f nc4 -remapnn,f{target_grid} -setgrid,f{source_grid} icon_pollen_description.txt f{fpath} f{fpath}.nc')

    fpath_nc = fpath + '.nc'

    return fpath_nc

def DataProcessing(model, parameters_dict):
    r"""
    Load the downloaded file, load as xarray array, concat along time and return as object
    :param model: Model name
    :param parameters_dict: class of dictionary of Environmental Parameters
    :return:
    :rtype: list(xr.DataArray)
    """

    env_dict = parameters_dict
    source_grid = env_dict.source_grid[model]
    target_grid = env_dict.target_grid[model]

    vnames = [x for x in env_dict.__getattribute__((model + '_variable'))]
    for vname in vnames:
        fdirs = os.path.join(env_dict.local_directory[model], vname)
        fdirs = [x for x in os.listdir(fdirs) if '.grib2' in x]

        da_list = []
        for fpath in fdirs:
            fpath_nc = Grib2nc(fpath, source_grid, target_grid)
            da = xr.load_dataset(fpath_nc, engine='netcdf4')
            varkey = [x for x in da.keys()]
            da_list.append(da.__getattr__(varkey[0]))

        locals()[vname] = xr.concat(da_list, dim='time')

        Plotting(locals()[vname], vname, model, env_dict)

    return locals()[vname]

def Plotting(da, vname, model, parameters_dict):
    r"""
    Plot the data array and pass it to <local_directory>/plots/
    :param da:
    :param model:
    :param parameters_dict:
    :return:
    """
    env_dict = parameters_dict

    plot_dir = os.path.join(env_dict.local_directory[model], 'plots')
    if not os.path.exists(plot_dir):
        os.makedirs(plot_dir)

    time = da.time

    for i in range(time):
        plot_time = np.datetime_as_string(da.time.values[i], unit='m')
        plot_data = da.loc[dict(time=plot_time)]

        # Define the levels to be 20, between the max and min value with 20 % margin
        levels = np.arange(plot_data.min() - (plot_data.min() * 0.2), plot_data.max() + (plot_data.max() * 0.2), 20)

        fig = plt.plot(figsize=[20, 10])
        plt.rc('font', size=18)
        plt.contourf(plot_data.lon, plot_data.lat, plot_data, levels=levels, cmap='RdBu_r')
        plt.title(env_dict.long_names[vname], loc='center')
        plt.title('Init. time: ' + env_dict.model_init_time, loc='left', size=14)
        plt.title('Current: ' + plot_time, loc='right', size=14)


        plt.savefig(os.path.join(plot_dir, (vname + '_' + plot_time + '.jpg')), bbox_inches='tight')