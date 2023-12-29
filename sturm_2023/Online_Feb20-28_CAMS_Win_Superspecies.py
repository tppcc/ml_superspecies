#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Aug 23 02:18:18 2021

@author: sturmpo
"""

# Numpy and ML modules
import numpy as np

# netCDF4 data
import netCDF4
import xarray as xr

# Time module
from datetime import date, datetime, timedelta 


# Plotting
import imageio
from matplotlib.pyplot import close,plot,subplots
from go_plot import * # note: keep go_plot.py in folder
import matplotlib.pyplot as plt

# Operating system
import os
import glob # for example glob.glob("data/*.nc")

def get_data(start_date = None,stop_date = None,folder = None,file_prefix = None):
    # This function accesses sequential days worth of netCDF data.
    # To inquire about data access, contact Obin Sturm (posturm@ucdavis.edu)
    # Inputs: 
        # start_date: tuple of form (YYYY (int), MM (int), DD (int))
        # stop_date: tuple of form (YYYY (int), MM (int), DD (int)) where the stop date is NOT included
        # folder: string of path to files
        # file_prefix: string of the first name of the file
    # Outputs: 
        # nc, a netCDF-like data structure with the days concatenated 
    
    # Assign default folder and file prefix -- done this way for argument readability
    if start_date == None: start_date = (2018, 2, 20)
    if stop_date == None: stop_date = (2018, 3, 1)
    if folder == None: folder = "/Scratch/sturmpo/projects/LOTOS-EUROS-CONTROL/feb/output/" 
    if file_prefix == None: file_prefix = "LE_feb_conc-sfc_"
    
    days = (date(*stop_date)-date(*start_date)).days 
    filenames = [None]*(days)
    for d in range(0,days):
        filenames[d] = folder + file_prefix + (date(*start_date)+timedelta(d)).strftime("%Y%m%d") + ".nc"
    print(filenames)
    nc = xr.open_mfdataset(filenames,concat_dim = "time", combine="by_coords")
    return nc

def plot_vbs(reshaped_vbs, time, lati_idx, long_idx, title = "VBS Partitioning", bincolor = "g"):
    # Print the concentration distribution across the volatility bins for a given basis set, location, and time.   
    number_bins = int(reshaped_vbs.shape[0]/2)

    aer = reshaped_vbs[0:number_bins,time ,0,lati_idx,long_idx]    
    gas = reshaped_vbs[number_bins:,time ,0,lati_idx,long_idx]
    COA = np.sum(aer)
    COG = np.sum(gas)
    X = np.arange(number_bins) 
    width = 0.15 # plus width/2
    fig, ax = plt.subplots(1,1)
    ax.bar(X + width/2, aer, color = bincolor, edgecolor = "black",label = "$C_{OA} = $" + "{:.3e}".format(COA) + " $ug \: m^{-3}$")
    ax.bar(X + width/2, gas, color = 'none', bottom = aer, edgecolor = "black", label = "$C_{gas} = $" + "{:.3e}".format(COG) + " $ug \: m^{-3}$")
    ax.set_xticks(X+width)
    
    ax.set_xticklabels(["{:.0e}".format(1*10**b) for b in range(-2,number_bins-2)])
    ax.set_xlabel("Saturation vapor pressure, $C^*$  [$ug \: m^{-3}$]")
    ax.set_ylabel("Organic mass [$ug \: m^{-3}$]")
    ax.set_title(title)
    ax.legend()
    fig.show()
    return fig


def convert_ppb(gas_ppb,mw_cg,tk,pk):
    R = 8.31446261815324 # J / (mol K)
    gas_ugm3 = (gas_ppb*1e-9) * pk * (mw_cg*1e9) / (tk * R)
    return gas_ugm3

def vbs_week_singlephase(vbs_class,dataset,meteo,days,phase = "aer"):
    # This function accesses the data from dataset for the given vbs class and phase over a certain amount of days.
    print("accessing data from the following VBS:  " + vbs_class)
    vbslist = ["POA", "siSOA", "aVOC", "bVOC"]
    namelist = ["poa", "sisoa", "asoa", "bsoa"] if phase =="aer" else ["pog", "sisog", "asog", "bsog"]
    binlist = [9, 8, 6, 6]
    
    name = namelist[vbslist.index(vbs_class)]
    bins = binlist[vbslist.index(vbs_class)]
    
    
    temper = np.zeros([1,(days*24)*1*len(lats)*len(lons)])
    psurf  = np.zeros([1,(days*24)*1*len(lats)*len(lons)])
    temper[:] = np.reshape(meteo.variables["temper"].data,[1,(days*24)*len(lats)*len(lons)])
    psurf[:]  = np.reshape(meteo.variables["psurf"].data,[1,(days*24)*len(lats)*len(lons)])
    
    
    # C is aerosol and gas phase concentrations, reshaped to be (tracers by observations)
    C = np.zeros([bins,(days*24)*1*len(lats)*len(lons)]) 
    for i in range(0,bins):
        C[i,:] = np.reshape(dataset.variables['vbs_' + name + str(i+1)].data,
                               [1,(days*24)*1*len(lats)*len(lons)])
        if phase == "gas":
            mw_cg = np.float64(dataset["vbs_" + name + str(i+1)].molemass)
            C[i,:] = convert_ppb(C[i,:],mw_cg,temper,psurf)
    return C

def NMF_reconstruct(vbs,d=2):
    # This function reconstructs test data with NMF, and also outputs W and H.
    # Inputs: 
        # vbs: test data, rows are tracers, each column is a new data point
        # d (optional): the number of latent dimensions (superspecies) to compress to. 2 is the default
    # Outputs:
        # vbsNMF: the reconstruction of vbs from the product WH.
        # W: optimized matrix mapping superspecies to tracers
        # H: representation of vbs in the lower dimensional latent space
        # P: the Moore-Penrose left pseudoinverse of W
    model = NMF(n_components=d, init='random', random_state=1)
    W = model.fit_transform(vbs)
    H = model.components_
    vbsNMF = W@H   #model.inverse_transform(W)
    
    # Calculate pseudoinverse
    P = np.linalg.inv(W.T@W)@W.T
    return vbsNMF,W,H,P


def plot_TOA(ave_TOA,ave_TOA_predict, date = ""):
    # This function plots total organic aerosol (TOA), TOA predicted from NMF, and the relative error between them.
    figLE = QuickMap( ave_TOA[0,0,:,:], xx = lons, yy = lats,
                         bmp=dict(countries = 'True', resolution = 'i'), 
                         cbar=dict( label='Average TOA (ug/m3)'),
                         vmin=0, vmax=np.max([np.max(ave_TOA),np.max(ave_TOA_predict)]),
                         figsize=(6,5), title = 'LOTOS-EUROS Control' + date
                   )
    figNMF = QuickMap( (ave_TOA_predict[0,0,:,:]),  domain = [minlon, maxlon, minlat, maxlat],
                         bmp=dict(countries = 'True', resolution = 'i'), 
                         cbar=dict( label='Average TOA (ug/m3)'),
                         vmin=0, vmax=np.max([np.max(ave_TOA),np.max(ave_TOA_predict)]),
                         figsize=(6,5), title = 'LOTOS-EUROS with online ML superspecies' +date
                      )
    
    fig_diff = QuickMap( 100*(ave_TOA_predict[0,0,:,:] - ave_TOA[0,0,:,:] )/ave_TOA[0,0,:,:] ,
                         bmp=dict(countries = 'True', resolution = 'i'), xx = lons, yy = lats,
                         cbar=dict( label='Relative bias'),
                         vmin=-100, vmax=100, cmap=dict( colors=['blue', 'white','red'], ncolor=500 ),
                         figsize=(6,5), title = 'Relative bias of average TOA predictions'
                      )
    return
    

    
#timeseries time
def plot_timeseries(lati,long,site, TOA, TOA_NMF, TOA_test, TOA_NMF_test, timerange= ""):
    # This function plots timeseries given latitude, longitude, and the name of a site.
    time_train = np.arange(0,5,1/24)
    time_test = np.arange(5,9,1/24)
    fig, timeseries = subplots()
    
    # Train days
    timeseries.plot(time_train,TOA[:,0,lati,long],color='black',label='LOTOS-EUROS')
    timeseries.scatter(time_train,TOA_NMF[:,0,lati,long],marker='.',color='green',label='NMF on training data')
    
    # Dividing line between train and test days
    timeseries.axvline(x=5, label = "Test days begin", color = 'red', linestyle = 'dashed' )
    
    # Test days
    timeseries.plot(time_test,TOA_test[:,0,lati,long],color='black')
    timeseries.scatter(time_test,TOA_NMF_test[:,0,lati,long],marker='.',color='red',label='NMF on testing data')
    
    timeseries.set_title(site + ', ' + timerange )
    timeseries.set_ylabel('Total organic aerosol [ug/m^3]')
    timeseries.set_xlabel('Time [days]')
    timeseries.legend()
    return timeseries


def plot_online_timeseries(lati,long, TOA, TOA_NMF, site ="", timerange= ""):
    # This function plots timeseries given latitude, longitude, and the name of a site.
    time = np.arange(0,int(len(TOA)/24),1/24)
    fig, timeseries = subplots()
    
    # Train days
    timeseries.plot(time,TOA[:,0,lati,long],color='green',label='Control Run')
    timeseries.scatter(time,TOA_ML[:,0,lati,long],marker='.',color='maroon',label='Superspecies Run')
    
    timeseries.set_title(site + ', ' + timerange )
    timeseries.set_ylabel('Total organic aerosol [ug/m^3]')
    timeseries.set_xlabel('Time [days]')
    timeseries.legend()
    return timeseries


def plot_multiple_vbs(reshaped_vbs, time, lati_idx, long_idx, titles, bincolors):
    # Print the concentration distribution across the volatility bins for a given basis set, location, and time.   
    fig, ax = plt.subplots(len(reshaped_vbs),1,figsize=(6,8))
    for i in range(0,len(reshaped_vbs)):
    
        number_bins = int(reshaped_vbs[i].shape[0]/2)
    
        aer = reshaped_vbs[i][0:number_bins,time ,0,lati_idx,long_idx]    
        gas = reshaped_vbs[i][number_bins:,time ,0,lati_idx,long_idx]
        COA = np.sum(aer)
        COG = np.sum(gas)
        X = np.arange(number_bins) 
        width = 0.15 # plus width/2
        
        ax[i].bar(X + width/2, aer, color = bincolors[i], edgecolor = "black",label = "$C_{OA}$")
        if (i == 0): 
            labelgas = "$C_{gas}$"
        else:
            labelgas = None
        ax[i].bar(X + width/2, gas, color = 'none', bottom = aer, edgecolor = "black", label = labelgas)
        ax[i].set_xticks(X+width)
        
        ax[i].set_xticklabels(["{:.0e}".format(1*10**b) for b in range(-2,number_bins-2)])
        ax[i].set_xlabel("Saturation vapor pressure, $C^*$  [$ug \: m^{-3}$]")
        ax[i].set_ylabel("Organic mass [$ug \: m^{-3}$]")
        ax[i].set_title(titles[i])
        ax[i].legend(loc = 'upper left')
    fig.tight_layout()

    return fig

def plot_multiple_vbs_withconc(reshaped_vbs, time, lati_idx, long_idx, titles, bincolors, ymax = None):
    # Print the concentration distribution across the volatility bins for a given basis set, location, and time.   
    fig, ax = plt.subplots(len(reshaped_vbs),1,figsize=(4,8))
    for i in range(0,len(reshaped_vbs)):
    
        number_bins = int(reshaped_vbs[i].shape[0]/2)
    
        aer = reshaped_vbs[i][0:number_bins,time ,0,lati_idx,long_idx]    
        gas = reshaped_vbs[i][number_bins:,time ,0,lati_idx,long_idx]
        COA = np.sum(aer)
        COG = np.sum(gas)
        X = np.arange(number_bins) 
        width = 0.15 # plus width/2
        
        ax[i].bar(X + width/2, aer, color = bincolors[i], edgecolor = "black",
                  label = "$C_{OA} = $" + "{:.3e}".format(COA) + " $ug \: m^{-3}$")
        ax[i].bar(X + width/2, gas, color = 'none', bottom = aer, edgecolor = "black", 
                  label = "$C_{gas} = $" + "{:.3e}".format(COG) + " $ug \: m^{-3}$")
        ax[i].set_xticks(X+width)
        
        ax[i].set_xticklabels(["{:.0e}".format(1*10**b) for b in range(-2,number_bins-2)])
        ax[i].set_xlabel("Saturation vapor pressure, $C^*$  [$ug \: m^{-3}$]")
        ax[i].set_ylabel("Organic mass [$ug \: m^{-3}$]")
        ax[i].set_title(titles[i])
        ax[i].legend(loc = 'upper right')
        
        if ymax is not None:
            ax[i].set_ylim([0, ymax])
            
    fig.tight_layout()

    return fig



def reshape_vbs(vbs,total_hours=96):
    # This function reshapes a 2 dimensional vbs of form [tracer,reshaped_dims]
    # to a 5 dimensional array of form [tracer,total_hours,altitude,latitude,longitude]
    # Inputs:
        # vbs, a two dimensional array
        # hours, how many hours in the reshaped array, second dimension  (dim 1) of the output
    # Output:
        # reshaped_vbs, for easier and consistent visualization
    num_tracers = vbs.shape[0]
    reshaped_vbs = np.zeros([num_tracers,total_hours,1,len(lats),len(lons)])
    for i in range(0,num_tracers):
        reshaped_vbs[i,:,:,:,:] = np.reshape(vbs[i,:],[total_hours,1,len(lats),len(lons)])
    
    return reshaped_vbs


#%%
# Winter Data

print("loading and concatenating netCDF4 files")
start = (2018, 2, 20)
stop = (2018, 3, 1)
days = 9


nc = get_data(start_date = start, stop_date = stop,
              folder = "/Scratch/sturmpo/projects/LOTOS-EUROS-CONTROL/feb_CAMS/output/" )
nc_met = get_data(start_date = start, stop_date = stop,
                  folder = "/Scratch/sturmpo/projects/LOTOS-EUROS-CONTROL/feb_CAMS/output/", 
                  file_prefix = "/LE_feb_meteo-sfc_")

# Winter optimized superspecies
# nc_ML = get_data(start_date = start, stop_date =  stop,
#                   folder = "/Scratch/sturmpo/projects/LOTOS-EUROS-ML/feb/output",
#                   file_prefix = "/LE_feb_conc-sfc_")

# nc_met_ML = get_data(start_date = start, stop_date = stop,
#                         folder = "/Scratch/sturmpo/projects/LOTOS-EUROS-ML/feb/output",
#                         file_prefix = "/LE_feb_meteo-sfc_")

# Summer optimized superspecies
nc_ML = get_data(start_date = start, stop_date =  stop,
                  folder = "/Scratch/sturmpo/projects/LOTOS-EUROS-ML/feb_CAMS/output",
                  file_prefix = "/LE_feb_conc-sfc_")
nc_met_ML = get_data(start_date = start, stop_date = stop,
                        folder = "/Scratch/sturmpo/projects/LOTOS-EUROS-ML/feb_CAMS/output",
                        file_prefix = "/LE_feb_meteo-sfc_")



lons = nc.variables['longitude']#[:].values
lats = nc.variables['latitude']#[:].values


minlon = lons.values.min()
maxlon = lons.values.max()
minlat = lats.values.min()
maxlat = lats.values.max()


mace_long = abs(nc.variables["longitude"][:]-[-9.9]).argmin()
mace_lati = abs(nc.variables["latitude"][:]-[53.333333]).argmin()
caba_long = abs(nc.variables["longitude"][:]-[4.923611]).argmin()
caba_lati = abs(nc.variables["latitude"][:]-[51.974444]).argmin()
barc_long = abs(nc.variables["longitude"][:]-[2.154007]).argmin()
barc_lati = abs(nc.variables["latitude"][:]-[41.390205]).argmin()
melp_long = abs(nc.variables["longitude"][:]-[12.93386]).argmin()
melp_lati = abs(nc.variables["latitude"][:]-[51.53014]).argmin()
hels_long = abs(nc.variables["longitude"][:]-[24.9384]).argmin()
hels_lati = abs(nc.variables["latitude"][:]-[60.1699]).argmin()

# tuebingen latitude longitude
tubi_long = abs(nc.variables["longitude"][:]-[9.057645]).argmin()
tubi_lati = abs(nc.variables["latitude"][:]-[48.521637]).argmin()

# getting control data
print("Control run: get days")
print("  Get aerosol data")
A_aer = vbs_week_singlephase("aVOC",nc,nc_met, days,phase = "aer")
B_aer = vbs_week_singlephase("bVOC",nc,nc_met,days,phase = "aer")
SI_aer =  vbs_week_singlephase("siSOA",nc,nc_met,days,phase = "aer")
P_aer = vbs_week_singlephase("POA",nc,nc_met,days,phase = "aer")

print("  Get gas data")
# Get gas control data
A_gas = vbs_week_singlephase("aVOC",nc,nc_met, days,phase = "gas")
B_gas = vbs_week_singlephase("bVOC",nc,nc_met,days,phase = "gas")
SI_gas =  vbs_week_singlephase("siSOA",nc,nc_met,days,phase = "gas")
P_gas = vbs_week_singlephase("POA",nc,nc_met,days,phase = "gas")

# Concatenate aerosol and gas control data
A = np.vstack((A_aer,A_gas))
B = np.vstack((B_aer,B_gas))
SI = np.vstack((SI_aer,SI_gas))
P = np.vstack((P_aer,P_gas))
TOA = np.sum(A[0:6,:],axis = 0) + np.sum(B[0:6,:],axis = 0) + np.sum(SI[0:8,:],axis = 0) + np.sum(P[0:9,:],axis = 0)
TOA = np.reshape(TOA,[days*24,1,len(lats),len(lons)])
average_TOA = np.expand_dims(np.mean(TOA, axis=0),axis=0)

# getting ML data
print("ML run: get days")
print("  Get aerosol data")
# Get aerosol ML data
A_aer_ML = vbs_week_singlephase("aVOC",nc_ML,nc_met_ML, days,phase = "aer")
B_aer_ML = vbs_week_singlephase("bVOC",nc_ML,nc_met_ML,days,phase = "aer")
SI_aer_ML =  vbs_week_singlephase("siSOA",nc_ML,nc_met_ML,days,phase = "aer")
P_aer_ML = vbs_week_singlephase("POA",nc_ML,nc_met_ML,days,phase = "aer")

print("  Get gas data")
# Get gas ML data
A_gas_ML = vbs_week_singlephase("aVOC",nc_ML,nc_met_ML, days,phase = "gas")
B_gas_ML = vbs_week_singlephase("bVOC",nc_ML,nc_met_ML,days,phase = "gas")
SI_gas_ML =  vbs_week_singlephase("siSOA",nc_ML,nc_met_ML,days,phase = "gas")
P_gas_ML = vbs_week_singlephase("POA",nc_ML,nc_met_ML,days,phase = "gas")

# Concatenate aerosol and gas ML data
A_ML = np.vstack((A_aer_ML,A_gas_ML))
B_ML = np.vstack((B_aer_ML,B_gas_ML))
SI_ML = np.vstack((SI_aer_ML,SI_gas_ML))
P_ML = np.vstack((P_aer_ML,P_gas_ML))
TOA_ML = np.sum(A_ML[0:6,:],axis = 0) + np.sum(B_ML[0:6,:],axis = 0) + np.sum(SI_ML[0:8,:],axis = 0) + np.sum(P_ML[0:9,:],axis = 0)
TOA_ML = np.reshape(TOA_ML,[days*24,1,len(lats),len(lons)])
average_TOA_ML = np.expand_dims(np.mean(TOA_ML, axis=0),axis=0)

plot_TOA(average_TOA,average_TOA_ML, date = ", February 25-28 2018")

RMSE_A = np.mean((A-A_ML)**2)**0.5
RMSE_B = np.mean((B-B_ML)**2)**0.5
RMSE_SI = np.mean((SI-SI_ML)**2)**0.5
RMSE_P = np.mean((P-P_ML)**2)**0.5
RMSE_TOA = np.mean((TOA-TOA_ML)**2)**0.5
RMSE_TOA = np.mean((TOA-TOA_ML)**2)**0.5
bias_TOA = np.mean(TOA_ML-TOA)

# Where is maximum TOA overestimation? Is it representative?
index_max_bias = np.unravel_index(abs(TOA_ML-TOA).argmax(), TOA_ML.shape)
percentile95 = np.percentile((TOA_ML-TOA),99)
percentile99 = np.percentile((TOA_ML-TOA),95)
plot_online_timeseries(index_max_bias[2],index_max_bias[3],TOA,TOA_ML,
                       site="Gridcell with highest single TOA overestimate", timerange="Feb 25 - 28")
plt.savefig('figs/gridcell_highest_bias.pdf', format='pdf')


A_reshaped = reshape_vbs(A,total_hours=days*24)
A_ML_reshaped = reshape_vbs(A_ML,total_hours=days*24)
SI_reshaped = reshape_vbs(SI,total_hours=days*24)
SI_ML_reshaped = reshape_vbs(SI_ML,total_hours=days*24)
B_reshaped = reshape_vbs(B,total_hours=days*24)
B_ML_reshaped = reshape_vbs(B_ML,total_hours=days*24)
P_reshaped = reshape_vbs(P,total_hours=days*24)
P_ML_reshaped = reshape_vbs(P_ML,total_hours=days*24)


date = ", February 25-28 2018"
figLE = QuickMap( average_TOA[0,0,:,:], xx = lons, yy = lats,
                         bmp=dict(countries = 'True', resolution = 'i'), 
                         cbar=dict( label='Average TOA (ug/m3)'),
                         vmin=0, vmax=np.percentile(average_TOA_ML,99.85), #7.5 #np.max([np.max(average_TOA),np.max(average_TOA_ML)]),
                         figsize=(6,5), title = 'LE CAMS Control Run' + date
                   )
plt.savefig('figs/highres_online_cams_winter_control.png', format='png', dpi=300)

figNMF = QuickMap( (average_TOA_ML[0,0,:,:]),  domain = [minlon, maxlon, minlat, maxlat],
                         bmp=dict(countries = 'True', resolution = 'i'), 
                         cbar=dict( label='Average TOA (ug/m3)'),
                         vmin=0, vmax=np.percentile(average_TOA_ML,99.85), #7.5,#np.max([np.max(average_TOA),np.max(average_TOA_ML)]),
                         figsize=(6,5), title = 'LE February CAMS Superspecies run' +date
                      )
plt.savefig('figs/highres_online_cams_winter_superspecies.png', format='png', dpi=300)
    
figbias = QuickMap( average_TOA_ML[0,0,:,:] - average_TOA[0,0,:,:],
                  domain = [minlon, maxlon, minlat, maxlat],
                         bmp=dict(countries = 'True', resolution = 'i'), 
                         cbar=dict( label='Bias (ug/m3)'),
                         cmap=dict( colors=['blue', 'cyan','white','yellow','red']),
                         vmin=-np.percentile(abs(average_TOA_ML[0,0,:,:] - average_TOA[0,0,:,:]),99.2), 
                         vmax=np.percentile(abs(average_TOA_ML[0,0,:,:] - average_TOA[0,0,:,:]),99.2), 
                         figsize=(6,5), title = "LE February CAMS Superspecies Run, Bias"
                      )
plt.savefig('figs/highres_online_cams_winter_winsup_bias.png', format='png', dpi=300)


figrelbias = QuickMap( 100*(average_TOA_ML[0,0,:,:] - average_TOA[0,0,:,:] )/average_TOA[0,0,:,:] ,
                         bmp=dict(countries = 'True', resolution = 'i'), xx = lons, yy = lats,
                         cbar=dict( label='Relative bias'),
                         vmin=-100, vmax=100, cmap=dict( colors=['blue', 'white','red'], ncolor=500 ),
                         figsize=(6,5), title = 'LE February CAMS Superspecies Run, Normalized Bias'
                      )
plt.savefig('figs/highres_online_cams_winter_winsup_relbias.png', format='png', dpi=300)



plot_online_timeseries(caba_lati, caba_long, TOA, TOA_ML,
                       site = "Cabauw CAMS Grid", timerange = "Feb 20-28")
plt.savefig('figs/cabauw_cams.pdf', format='pdf')
plot_online_timeseries(mace_lati, mace_long, TOA, TOA_ML, 
                       site ="Mace Head CAMS Grid", timerange = "Feb 20-28")
plt.savefig('figs/macehead_cams.pdf', format='pdf')

# take a look at error
plt.plot(np.linspace(0,9,216),np.mean(np.mean(TOA_ML-TOA,axis=3),axis=2))