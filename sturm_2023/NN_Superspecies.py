#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Jun 15 15:09:08 2021

@author: sturmpo
"""


# Numpy and ML modules
import numpy as np
from sklearn.decomposition import NMF, non_negative_factorization
from sklearn.metrics import mean_squared_error
from scipy.optimize import nnls
import random

# Tensorflow and Keras
import tensorflow as tf
from tensorflow.keras.layers import Input, Dense, Dropout
from tensorflow.keras import Sequential
from tensorflow.keras.constraints import Constraint
import tensorflow.keras.backend as K
from tensorflow.keras.callbacks import EarlyStopping

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

seed_val = 42
os.environ['PYTHONHASHSEED'] = str(seed_val)
np.random.seed(seed_val)
tf.random.set_seed(seed_val)

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



def convert_ppb(gas_ppb,mw_cg,tk,pk):
    R = 8.31446261815324 # J / (mol K)
    gas_ugm3 = (gas_ppb*1e-9) * pk * (mw_cg*1e9) / (tk * R)
    return gas_ugm3

def vbs_week(vbs_class,dataset,meteo,days):
    # This function accesses the data from dataset for the given vbs over a certain amount of days.
    print("accessing data from the following VBS:  " + vbs_class)
    vbslist = ["POA", "siSOA", "aVOC", "bVOC"]
    namelist = ["po", "siso", "aso", "bso"]
    binlist = [9, 8, 6, 6]
    
    name = namelist[vbslist.index(vbs_class)]
    bins = binlist[vbslist.index(vbs_class)]
    
    mw_cg = np.float64(dataset["vbs_" + name + "g1"].molemass)
    temper = np.zeros([1,(days*24)*1*len(lats)*len(lons)])
    psurf  = np.zeros([1,(days*24)*1*len(lats)*len(lons)])
    temper[:] = np.reshape(meteo.variables["temper"].data,[1,(days*24)*len(lats)*len(lons)])
    psurf[:]  = np.reshape(meteo.variables["psurf"].data,[1,(days*24)*len(lats)*len(lons)])
    
    
    # C is aerosol and gas phase concentrations, reshaped to be (tracers by observations)
    C = np.zeros([2*bins,(days*24)*1*len(lats)*len(lons)]) 
    for i in range(0,bins):
        C[i,:] = np.reshape(dataset.variables['vbs_' + name + 'a'+str(i+1)].data,
                               [1,(days*24)*1*len(lats)*len(lons)])
    
        C[i+bins,:] = np.reshape(dataset.variables['vbs_' + name + 'g'+str(i+1)].data,
                               [1,(days*24)*1*len(lats)*len(lons)])
        C[i+bins,:] = convert_ppb(C[i+bins,:],mw_cg,temper,psurf)
    return C


def plot_TOA(ave_TOA,ave_TOA_predict):
    # This function plots total organic aerosol (TOA), TOA predicted from NMF, and the relative error between them.
    figLE = QuickMap( ave_TOA[0,0,:,:], domain =[-24.9, 44.9, 30.1, 71.9], 
                         bmp=dict(countries = 'True', resolution = 'i'), 
                         cbar=dict( label='Average TOA (ug/m3)'),
                         vmin=0, vmax=np.max(ave_TOA),
                         figsize=(6,5), title = 'LOTOS-EUROS'
                   )
    figNMF = QuickMap( (ave_TOA_predict[0,0,:,:]), domain =[-24.9, 44.9, 30.1, 71.9], 
                         bmp=dict(countries = 'True', resolution = 'i'), 
                         cbar=dict( label='Average TOA (ug/m3)'),
                         vmin=0, vmax=np.max(ave_TOA_predict),
                         figsize=(6,5), title = 'Non-negative Matrix Factorization'
                      )
    
    fig_diff = QuickMap( 100*abs(ave_TOA[0,0,:,:] - ave_TOA_predict[0,0,:,:] )/ave_TOA[0,0,:,:] , domain =[-24.9, 44.9, 30.1, 71.9], 
                         bmp=dict(countries = 'True', resolution = 'i'), 
                         cbar=dict( label='Relative absolute error'),
                         vmin=0, vmax=100, cmap=dict( colors=['white','red'], ncolor=500 ),
                         figsize=(6,5), title = 'Relative absolute error of average TOA predictions'
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

def plot_W(W,vbs=""):
    # This function plots the different columns of W, which represent the compositions of each superspecies.
    # Inputs:
        # W, a previously optimized matrix mapping superspecies to original tracers
        # vbs, optional string input to prepend to the figure suptitle (e.g. "siSOA")
    # Outputs: 
        # returns NoneType object
    number_bins = int(W.shape[0]/2)
    feats = W.shape[1]
    fig, ax = subplots(3,1)
    bincolor = "red"
    for f in range(0,feats):
        title = "Normalized direction of W[:," + str(f) + "]"
        X = np.arange(number_bins) # plus width/2
        width = 0.15 
        aer = W[0:number_bins,f]/np.sum(W[:,f])
        gas = W[number_bins:,f]/np.sum(W[:,f])
        XOA = np.sum(aer)
        XOG = np.sum(gas)
        ax[f].bar(X + width/2, aer, color = bincolor, edgecolor = "black",label = "$X_{OA} = $" + "{:0.3f}".format(XOA))
        ax[f].bar(X + width/2, gas, color = 'none', bottom = aer, edgecolor = "black", label = "$X_{gas} = $" + "{:0.3f}".format(XOG))
        ax[f].set_xticks(X+width)
        
        ax[f].set_xticklabels(["{:.0e}".format(1*10**b) for b in range(-2,number_bins-2)])
    
        ax[f].set_title(title)
        ax[f].legend()
    ax[1].set_ylabel("Proportion of tracers in superspecies")
    ax[2].set_xlabel("Saturation vapor pressure, $C^*$  [$ug \: m^{-3}$]")
    fig.tight_layout()
    fig.suptitle(vbs + " vbs: composition of superspecies")
    fig.show() 
    return 



#Summer Data
# nc = get_data(start_date = (2018, 7, 24), stop_date = (2018, 7, 29),
#               folder = "/Scratch/sturmpo/projects/LOTOS-EUROS-CONTROL/jul/output",
#               file_prefix = "/LE_jul_conc-sfc_")
# nc_test = get_data(start_date = (2018, 7, 29), stop_date = (2018, 8, 3),
#                    folder = "/Scratch/sturmpo/projects/LOTOS-EUROS-CONTROL/jul/output",
#                    file_prefix = "/LE_jul_conc-sfc_")

# nc_met = get_data(start_date = (2018, 7, 24), stop_date = (2018, 7, 29),
#                   folder = "/Scratch/sturmpo/projects/LOTOS-EUROS-CONTROL/jul/output",
#                   file_prefix = "/LE_jul_meteo-sfc_")
# nc_met_test = get_data(start_date = (2018, 7, 29), stop_date = (2018, 8, 3),
#                        folder = "/Scratch/sturmpo/projects/LOTOS-EUROS-CONTROL/jul/output",
#                        file_prefix = "/LE_jul_meteo-sfc_")
    
# Winter Data
nc = get_data(start_date = (2018, 2, 20), stop_date = (2018, 2, 25))
nc_test = get_data(start_date = (2018, 2, 25), stop_date = (2018, 3, 1))
nc_met = get_data(start_date = (2018, 2, 20), stop_date = (2018, 2, 25),
                  file_prefix = "/LE_feb_meteo-sfc_")
nc_met_test = get_data(start_date = (2018, 2, 25), stop_date = (2018, 3, 1),
                       file_prefix = "/LE_feb_meteo-sfc_")

lons = nc.variables['longitude'][:]
lats = nc.variables['latitude'][:]


minlon = lons.min()
maxlon = lons.max()
minlat = lats.min()
maxlat = lats.max()

# getting train data: first five days
print("getting training data")
A = vbs_week("aVOC",nc,nc_met, 5)
B = vbs_week("bVOC",nc,nc_met,5)
SI =  vbs_week("siSOA",nc,nc_met,5)
P = vbs_week("POA",nc,nc_met,5)

# getting test data: days 5 until the end of 8
print("getting test data")
A_test = vbs_week("aVOC",nc_test,nc_met_test,4)
B_test = vbs_week("bVOC",nc_test,nc_met_test,4)
SI_test =  vbs_week("siSOA",nc_test,nc_met_test,4)
P_test = vbs_week("POA",nc_test,nc_met_test,4)

TOM_test =  np.sum(P_test,axis = 0) + np.sum(SI_test,
            axis=0) + np.sum(A_test,axis=0) + np.sum(B_test,axis=0)
TOA_test =  np.sum(P_test[0:9,:],axis = 0) + np.sum(SI_test[0:8,:],
            axis=0) + np.sum(A_test[0:6,:],axis=0) + np.sum(B_test[0:6,:],axis=0)

# parameters that NN training regimes share
num_epochs = 100
es = EarlyStopping(monitor='val_loss', min_delta = 1e-5, verbose=1,patience =10)

print("training the poa autoencoder")
P_autoencoder = Sequential()
P_autoencoder.add(Input(shape=(18,)))
P_autoencoder.add(Dropout(0.1))
P_autoencoder.add(Dense(10,activation='tanh'))
P_autoencoder.add(Dropout(0.1))
P_autoencoder.add(Dense(3,activation='relu'))
P_autoencoder.add(Dense(10,activation='tanh'))
P_autoencoder.add(Dropout(0.1))
P_autoencoder.add(Dense(18,activation ='relu'))
P_autoencoder.compile( loss = 'mse', optimizer = 'adam', 
                    metrics = ['mean_absolute_error', tf.keras.metrics.RootMeanSquaredError()])
P_autoencoder.summary()
P_autoencoder.fit(P.T,P.T, epochs=num_epochs, callbacks = [es], validation_split = 0.1)

print("training the sisoa autoencoder")
S_autoencoder = Sequential()
S_autoencoder.add(Input(shape=(16,)))
S_autoencoder.add(Dropout(0.1))
S_autoencoder.add(Dense(10,activation='tanh'))
S_autoencoder.add(Dropout(0.1))
S_autoencoder.add(Dense(3,activation='relu'))
S_autoencoder.add(Dense(10,activation='tanh'))
S_autoencoder.add(Dropout(0.1))
S_autoencoder.add(Dense(16,activation ='relu'))
S_autoencoder.compile( loss = 'mse', optimizer = 'adam',
                    metrics = ['mean_absolute_error', tf.keras.metrics.RootMeanSquaredError()])
S_autoencoder.summary()
S_autoencoder.fit(SI.T,SI.T, epochs=num_epochs, callbacks = [es], validation_split = 0.1)

A_autoencoder = Sequential()
A_autoencoder.add(Input(shape=(12,)))
A_autoencoder.add(Dropout(0.1))
A_autoencoder.add(Dense(10,activation='tanh'))
A_autoencoder.add(Dropout(0.1))
A_autoencoder.add(Dense(3,activation='relu'))
A_autoencoder.add(Dense(10,activation='tanh'))
A_autoencoder.add(Dropout(0.1))
A_autoencoder.add(Dense(12,activation ='relu'))
A_autoencoder.compile( loss = 'mse', optimizer = 'adam', 
                    metrics = ['mean_absolute_error', tf.keras.metrics.RootMeanSquaredError()])
A_autoencoder.summary()
A_autoencoder.fit(A.T,A.T, epochs=num_epochs, callbacks = [es], validation_split = 0.1)

B_autoencoder = Sequential()
B_autoencoder.add(Input(shape=(12,)))
B_autoencoder.add(Dropout(0.1))
B_autoencoder.add(Dense(10,activation='tanh'))  
B_autoencoder.add(Dropout(0.1))
B_autoencoder.add(Dense(3,activation='relu'))
B_autoencoder.add(Dense(10,activation='tanh'))
B_autoencoder.add(Dropout(0.1))
B_autoencoder.add(Dense(12,activation ='relu'))
B_autoencoder.compile( loss = 'mse', optimizer = 'adam',
                    metrics = ['mean_absolute_error', tf.keras.metrics.RootMeanSquaredError()])
B_autoencoder.summary()
B_autoencoder.fit(B.T,B.T, epochs=num_epochs, callbacks = [es], validation_split = 0.1)

P_NN_test = P_autoencoder.predict(P_test.T)
S_NN_test = S_autoencoder.predict(SI_test.T)
A_NN_test = A_autoencoder.predict(A_test.T)
B_NN_test = B_autoencoder.predict(B_test.T)

TOM_NN_test = np.sum(P_NN_test,axis = 1) +  np.sum(S_NN_test,
            axis=1) + np.sum(A_NN_test,axis=1) + np.sum(B_NN_test,axis=1)

TOA_NN_test =  np.sum(P_NN_test[:,0:9],axis = 1) +  np.sum(S_NN_test[:,0:8],
            axis=1) + np.sum(A_NN_test[:,0:6],axis=1) + np.sum(B_NN_test[:,0:6],axis=1)

RMSE_P = np.mean((P_NN_test - P_test.T)**2)**0.5
RMSE_S = np.mean((S_NN_test - SI_test.T)**2)**0.5
RMSE_A = np.mean((A_NN_test - A_test.T)**2)**0.5
RMSE_B = np.mean((B_NN_test - B_test.T)**2)**0.5
RMSE_TOM = np.mean((TOM_test - TOM_NN_test)**2)**0.5
RMSE_TOA = np.mean((TOA_test - TOA_NN_test)**2)**0.5

bias_P = np.mean(P_NN_test - P_test.T)
bias_S = np.mean(S_NN_test - SI_test.T)
bias_A = np.mean(A_NN_test - A_test.T)
bias_B = np.mean(B_NN_test - B_test.T)
bias_TOM = np.mean(TOM_NN_test - TOM_test)
bias_TOA = np.mean(TOA_NN_test - TOA_test)

TOA_test = np.reshape(TOA_test,[4*24,1,len(lats),len(lons)])
TOA_NN_test = np.reshape(TOA_NN_test,[4*24,1,len(lats),len(lons)])
average_TOA_test = np.expand_dims(np.mean(TOA_test, axis=0),axis=0)
average_TOA_NN_test = np.expand_dims(np.mean(TOA_NN_test, axis=0),axis=0)

fig_diff = QuickMap( 100*(average_TOA_NN_test[0,0,:,:] -average_TOA_test[0,0,:,:] )/average_TOA_test[0,0,:,:] ,
                    xx = lons, yy = lats, 
                    bmp=dict(countries = 'True', resolution = 'i'), 
                    cbar=dict( label='Normalized Bias of average TOA'),
                    vmin=-100, vmax=100,cmap=dict( colors=['blue', 'white','red'], ncolor=500 ),
                    figsize=(6,5), title = "Autoencoder with 3 superspecies"
                    )                         
plt.savefig('figs/autoencoder_relbias.pdf', format='pdf')
                    