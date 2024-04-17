import os

import matplotlib.pyplot as plt
import numpy as np
import pytz
import xarray as xr
import datetime


def LocalTime(time_obj):
    r"""
    Convert DateTime object to <DOW>, <DD> <MM> <YY> <HH><Z>
    :param time_obj:
    :return:
    """
    # Convert UTC to CEST (Central European Summer Time)
    cest = pytz.timezone('Europe/Berlin')
    dt_cest = time_obj.astimezone(cest)

    # Format the datetime object
    return dt_cest.strftime("%a, %d %b. %Y %H%Z")


def Grib2nc(fpath, source_grid, target_grid):
    os.system('module load cdo')
    # Assign grid to
    os.system(
        # f'cdo -f nc4 -remapnn,f{target_grid} -setgrid,f{source_grid} icon_pollen_description.txt f{fpath} f{fpath}.nc')
        f'cdo -f nc4 -remapnn,{target_grid} -setgrid,{source_grid} {fpath} {fpath}.nc')

    fpath_nc = fpath + '.nc'

    return fpath_nc


def Plotting(da, vname, model, parameters_dict):
    r"""
    Plot the data array and pass it to <local_directory>/plots/
    :param da:
    :param model:
    :param parameters_dict:
    :return:
    """
    env_dict = parameters_dict

    plot_dir = os.path.join(env_dict.local_directory[model], vname, 'plots')
    if not os.path.exists(plot_dir):
        os.makedirs(plot_dir)

    time = da.time

    for i in range(len(time)):
        plot_time = np.datetime_as_string(da.time.values[i], unit='m')
        plot_data = da.loc[dict(time=plot_time)].squeeze()

        # Convert K to C
        if vname == 't_2m':
            plot_data = plot_data - 273.15

        title_init_time = LocalTime(datetime.datetime.fromisoformat(env_dict.model_init_time))
        ctime = da.time.values[i].astype('datetime64[s]').tolist()
        title_current_time = LocalTime(ctime)

        # Define the levels to be 20, between the max and min value with 20 % margin
        levels = np.arange(plot_data.min() - (plot_data.min() * 0.2),
                           plot_data.max() + (plot_data.max() * 0.2), 20)

        fig = plt.plot(figsize=[20, 10])
        plt.rc('font', size=18)
        plt.contourf(plot_data.lon, plot_data.lat, plot_data, levels=levels, cmap='RdBu_r')
        plt.title(env_dict.long_names[vname], loc='center')
        plt.title('Init. time: ' + title_init_time, loc='left', size=14)
        plt.title('Current: ' + title_current_time, loc='right', size=14)
        plt.ylabel('Latitude [$^oN$]')
        plt.ylabel('Longitude [$^oE$]')

        plt.savefig(os.path.join(plot_dir, (vname + '_' + plot_time + '.jpg')), bbox_inches='tight')


def DataProcessing(model, parameters_dict):
    r"""
    Entry Point 2 for dust_board routine
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
        fnames = [x for x in os.listdir(fdirs) if '.grib2' in x and '.nc' not in x]
        fnames.sort()
        fnames = [os.path.join(fdirs, x) for x in fnames]

        da_list = []
        for fpath in fnames:
            # print(fpath)
            fpath_nc = Grib2nc(fpath, source_grid, target_grid)
            da = xr.load_dataset(fpath_nc, engine='netcdf4')
            # Fetch dataset keys (variable names) and access the first variable in the dataset,
            # pass to list for plotting. Note: Only one variable should be contained in the file.
            varkey = [x for x in da.keys()]
            da_list.append(da.__getattr__(varkey[0]))

        locals()[vname] = xr.concat(da_list, dim='time')

        Plotting(locals()[vname], vname, model, env_dict)

    return {x: locals()[x] for x in vnames}


def PreprocessingMeteogram(da, target_lon, target_lat):
    r"""
    Interpolate input field into a point based product
    :param da: Data Array to be interpolated
    :type: xr.DataArray
    :param target_lon:
    :type: float
    :param target_lat:
    :type: float
    :return: interpolated_da xr.DataArray
    :rtype: xr.DataArray
    """
    return da.interp({'lat': target_lat, 'lon': target_lon})


def Meteogram(plot_dir, target_lon, target_lat, t_2m_dict, asob_s_dict, aswdifd_s_dict,
              parameters_dict):
    r"""
    Entry Point 3 for dust_board routine
    Produce a point based product Meteogram. Currently:
    1. 2-M Temperature
    2. Surface Net Radiation
    3. Surface Diffused Downards Radiation
    :param t2m_dict: Dictionary of Data Array, key: Model Name
    :type: dict(da)
    :param vname:
    :param parameters_dict:
    :return:
    """
    env_dict = parameters_dict
    models = env_dict.model_url

    vnames = ['t_2m', 'asob_s', 'aswdifd_s']
    plot_y_label = ['Temperature [$^oK$]', 'Net Surface Radiation [W m$^{-2}$]',
                    'Downward Surface Diffused Radiation [W m$^{-2}$]']
    title_init_time = LocalTime(datetime.datetime.fromisoformat(env_dict.model_init_time))

    fig, axes = plt.subplots(3, 1, figsize=[10, 20])
    plt.rc('font', size=20)

    for i in range(len(vnames)):
        vname = vnames[i]
        for model in models:
            da = locals()[vname + '_dict'][model]
            interpolated_da = PreprocessingMeteogram(da, target_lon, target_lat)
            # Convert from K to C if var = t_2m
            if i == 0:
                interpolated_da = interpolated_da - 273.15
            axes[i].plot(interpolated_da.time, interpolated_da,
                         color=env_dict.meteogram_colour[model], linewidth=2,
                         label=env_dict.model_long_names[model])
            axes[i].set_title(env_dict.long_names[vname], loc='center')
            axes[i].set_title('Init. time: ' + title_init_time, loc='left', size=14)
            axes[i].set_ylabel(plot_y_label[i])
            axes[i].set_xlabel('Time [h]')
            axes[i].legend()
    plt.savefig(plot_dir, bbox_inches='tight')
