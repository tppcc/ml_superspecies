# -*- coding: utf-8 -*-

# Numpy and ML modules
import numpy as np
from sklearn.decomposition import NMF, non_negative_factorization
from sklearn.metrics import mean_squared_error
from scipy.optimize import nnls
from archetypes import *

# Tensorflow and Keras
# import tensorflow as tf
# from tensorflow.keras.layers import Input, Dense
# from tensorflow.keras import Sequential
# #from tensorflow_probability.python.layers.weight_norm import WeightNorm
# from tensorflow.keras.constraints import Constraint
# import tensorflow.keras.backend as K

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
    model = NMF(n_components=d, init='random', random_state=1, alpha = 0.1)
    W = model.fit_transform(vbs)
    H = model.components_
    vbsNMF = W@H   #model.inverse_transform(W)
    
    # Calculate pseudoinverse
    P = np.linalg.inv(W.T@W)@W.T
    return vbsNMF,W,H,P

def Pseudoinverse_predict(X,W,H=None,P=None):
    # This function takes a new test data X and a pre-optimized W from a training dataset
    # The data points in the reduced dimension, H, are calculated using a left Moore-Penrose inverse of W
    #
    H_NMF = np.linalg.inv(W.transpose()@W)@W.transpose()@X
    X_NMF = W@H_NMF
    return X_NMF, H_NMF

def plot_TOA(ave_TOA,ave_TOA_predict, date = ''):
    # This function plots total organic aerosol (TOA), TOA predicted from NMF, and the relative error between them.
    figLE = QuickMap( ave_TOA[0,0,:,:], xx = lons, yy = lats,
                         bmp=dict(countries = 'True', resolution = 'i'), 
                         cbar=dict( label='Average TOA (ug/m3)'),
                         vmin=0, vmax=np.max([np.max(ave_TOA),np.max(ave_TOA_predict)]),
                         figsize=(6,5), title = 'LOTOS-EUROS Control' + date
                   )
    figNMF = QuickMap( (ave_TOA_predict[0,0,:,:]),  xx = lons, yy = lats,
                         bmp=dict(countries = 'True', resolution = 'i'), 
                         cbar=dict( label='Average TOA (ug/m3)'),
                         vmin=0, vmax=np.max([np.max(ave_TOA),np.max(ave_TOA_predict)]),
                         figsize=(6,5), title = 'LOTOS-EUROS with online ML superspecies' + date
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
    timeseries.plot(time_train,TOA[:,0,lati,long],color='green',label='LOTOS-EUROS')
    timeseries.scatter(time_train,TOA_NMF[:,0,lati,long],marker='.',color='blue',label='NMF on training data')
    
    # Dividing line between train and test days
    timeseries.axvline(x=5, label = "Test days begin", color = 'maroon', linestyle = 'dashed' )
    
    # Test days
    timeseries.plot(time_test,TOA_test[:,0,lati,long],color='green')
    timeseries.scatter(time_test,TOA_NMF_test[:,0,lati,long],marker='.',color='maroon',label='NMF on testing data')
    
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
    ax.bar(X + width/2, gas, color = 'none', bottom = None, edgecolor = "black", label = "$C_{gas} = $" + "{:.3e}".format(COG) + " $ug \: m^{-3}$")
    ax.set_xticks(X+width)
    
    ax.set_xticklabels(["{:.0e}".format(1*10**b) for b in range(-2,number_bins-2)])
    ax.set_xlabel("Saturation vapor pressure, $C^*$  [$ug \: m^{-3}$]")
    ax.set_ylabel("Organic mass [$ug \: m^{-3}$]")
    ax.set_title(title)
    ax.legend()
    fig.show()
    return fig

def plot_POA_emissions(title = "Emission Fractions", bincolor = "red"):
    # Print the concentration distribution across the volatility bins for a given basis set, location, and time.   
    number_bins = 9
    emission_factor = [0.1, 0.2, 0.3, 0.4, 0.1, 0.2, 0.3, 0.4, 0.5]

    X = np.arange(number_bins) 
    width = 0.15 # plus width/2
    fig, ax = plt.subplots(1,1)
    ax.bar(X + width/2, emission_factor, color = bincolor, edgecolor = "black",label = "total mass fraction per bin")
    ax.set_xticks(X+width)
    
    ax.set_xticklabels(["{:.0e}".format(1*10**b) for b in range(-2,number_bins-2)])
    ax.set_xlabel("Saturation vapor pressure, $C^*$  [$ug \: m^{-3}$]")
    ax.set_ylabel("Mass fraction of POM")
    ax.set_title(title)
    ax.legend()
    fig.show()
    return fig

def plot_neg_vbs(reshaped_vbs, time, lati_idx, long_idx, title = None, bincolor = "g"):
    # Print the concentration distribution across the volatility bins for a given basis set, location, and time.   
    number_bins = int(reshaped_vbs.shape[0]/2)

    aer = reshaped_vbs[0:number_bins,time ,0,lati_idx,long_idx]    
    gas = reshaped_vbs[number_bins:,time ,0,lati_idx,long_idx]
    COA = np.sum(aer)
    COGpos = np.sum(gas[4:])
    COGneg = np.sum(gas[0:4])
    X = np.arange(number_bins) 
    width = 0.15 # plus width/2
    fig, ax = plt.subplots(1,1)
    ax.bar(X + width/2, aer, color = bincolor, edgecolor = "black",label = "$C_{OA} = $" + "{:3.1f}".format(COA) + " $ug \: m^{-3}$")
    ax.bar(X[0:4] + width/2, gas[0:4], color = 'grey', bottom = None, edgecolor = "black", label = "net (-) $C_{gas} = $" + "{:2.1f}".format(COGneg) + " $ug \: m^{-3}$")
    ax.bar(X[4:] + width/2, gas[4:], color = 'none', bottom = aer[4:], edgecolor = "black", label = "net (+) $C_{gas} = $" + "{:3.1f}".format(COGpos) + " $ug \: m^{-3}$")
    ax.set_xticks(X+width)
    
    ax.set_xticklabels(["{:.0e}".format(1*10**b) for b in range(-2,number_bins-2)])
    ax.set_xlabel("Saturation vapor pressure, $C^*$  [$ug \: m^{-3}$]")
    ax.set_ylabel("Organic mass [$ug \: m^{-3}$]")
    ax.set_title(title)
    ax.legend()
    fig.show()
    return fig


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


def plot_multiple_vbs_withconc(reshaped_vbs, time, lati_idx, long_idx, titles, bincolors):
    # Print the concentration distribution across the volatility bins for a given basis set, location, and time.   
    fig, ax = plt.subplots(len(reshaped_vbs),1,figsize=(6,10))
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

def plot_W(W,vbs="",bincolor=None):
    # This function plots the different columns of W, which represent the compositions of each superspecies.
    # Inputs:
        # W, a previously optimized matrix mapping superspecies to original tracers
        # vbs, optional string input to prepend to the figure suptitle (e.g. "siSOA")
    # Outputs: 
        # returns NoneType object
    number_bins = int(W.shape[0]/2)
    if bincolor is None: color = "red"
    feats = W.shape[1]
    fig, ax = subplots(3,1,figsize=(6,8))
    #bincolor = "red"
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
    # fig.suptitle(vbs + " vbs: composition of superspecies")
    fig.show() 
    return 

class EvaluateCompression():
     # This class runs a data pipeline for creating decompression and compression matrices
    # For mixed-phase superspecies
    # This all happens in the __init__ method
    # High level scope:
        # 1. Generate compression and decompression matrices using train_func and training data
        # 2. Use matrices to compress and decompress test data
        # 3. Compute total organic aerosol and total organic mass (TOA and TOM)
        # 4. Compute error metrics on training and test data
        # 5. Reshape data for plotting on domain   
    def __init__(self,num_supspec = 3, train_func = NMF_reconstruct, test_func = Pseudoinverse_predict):
    #     return
        
    # def compress(self):  
        print("Data pipeline with " + str(num_supspec) + " superspecies")
        print("Step 1 of 5: Training data")
        self.A_NMF, self.W_A, self.H_A, self.P_A = train_func(A,d=num_supspec)
        self.B_NMF, self.W_B, self.H_B, self.P_B = train_func(B,d=num_supspec)
        self.SI_NMF, self.W_SI, self.H_SI, self.P_SI = train_func(SI,d=num_supspec)
        self.P_NMF, self.W_P, self.H_P, self.P_P = train_func(P,d=num_supspec)
        
        print("Step 2 of 5: Testing data")
        self.A_NMF_test, self.H_A_NMF_test = test_func(A_test, self.W_A, self.H_A, self.P_A)
        self.B_NMF_test, self.H_B_NMF_test = test_func(B_test, self.W_B, self.H_B, self.P_B)
        self.SI_NMF_test, self.H_SI_NMF_test = test_func(SI_test, self.W_SI, self.H_SI, self.P_SI)
        self.P_NMF_test, self.H_P_NMF_test = test_func(P_test, self.W_P, self.H_P, self.P_P)
        
        print("Step 3 of 5: Calculating TOA and average TOA")
        # Calculate total organic aerosol, TOA
        self.TOA = np.sum(A[0:6,:],axis=0) + np.sum(B[0:6,:],axis=0) + np.sum(SI[0:8,:],axis=0) + np.sum(P[0:9,:],axis=0)
        self.TOA_NMF = np.sum(self.A_NMF[0:6,:],axis=0) + np.sum(self.B_NMF[0:6,:],
                                axis=0) + np.sum(self.SI_NMF[0:8,:],axis=0) + np.sum(self.P_NMF[0:9,:],axis=0)
        
        # Calculate TOA for the test days
        self.TOA_test = np.sum(A_test[0:6,:],axis=0) + np.sum(B_test[0:6,:],axis=0) + np.sum(SI_test[0:8,:],axis=0) + np.sum(P_test[0:9,:],axis=0)
        self.TOA_NMF_test = np.sum(self.A_NMF_test[0:6,:],axis=0) + np.sum(self.B_NMF_test[0:6,:],
                              axis=0) + np.sum(self.SI_NMF_test[0:8,:],axis=0) + np.sum(self.P_NMF_test[0:9,:],axis=0)
        
        
        # Calculate total organic concentration, TOM
        self.TOM = np.sum(A,axis=0) + np.sum(B,axis=0) + np.sum(SI,axis=0) + np.sum(P,axis=0)
        self.TOM_NMF = np.sum(self.A_NMF,axis=0) + np.sum(self.B_NMF,axis=0) + np.sum(self.SI_NMF,axis=0) + np.sum(self.P_NMF,axis=0)
        # Calculate total organic concentration, TOM for test days
        self.TOM_test = np.sum(A_test,axis=0) + np.sum(B_test,axis=0) + np.sum(SI_test,axis=0) + np.sum(P_test,axis=0)
        self.TOM_NMF_test = np.sum(self.A_NMF_test,axis=0) + np.sum(self.B_NMF_test,axis=0) + np.sum(self.SI_NMF_test,axis=0) + np.sum(self.P_NMF_test,axis=0)
        
        # Put TOA on grid and full time scale
        self.TOA = np.reshape(self.TOA,[5*24,1,len(lats),len(lons)])
        self.TOA_NMF = np.reshape(self.TOA_NMF,[5*24,1,len(lats),len(lons)])
        self.TOA_test = np.reshape(self.TOA_test,[4*24,1,len(lats),len(lons)])
        self.TOA_NMF_test = np.reshape(self.TOA_NMF_test,[4*24,1,len(lats),len(lons)])
        
        # Put TOM on grid and full time scale
        self.TOM = np.reshape(self.TOM,[5*24,1,len(lats),len(lons)])
        self.TOM_NMF = np.reshape(self.TOM_NMF,[5*24,1,len(lats),len(lons)])
        self.TOM_test = np.reshape(self.TOM_test,[4*24,1,len(lats),len(lons)])
        self.TOM_NMF_test = np.reshape(self.TOM_NMF_test,[4*24,1,len(lats),len(lons)])
        
        # Average TOA training
        self.average_TOA = np.expand_dims(np.mean(self.TOA, axis=0),axis=0)
        self.average_TOA_NMF = np.expand_dims(np.mean(self.TOA_NMF, axis=0),axis=0)
        
        # Average TOA testing
        self.average_TOA_test = np.expand_dims(np.mean(self.TOA_test, axis=0),axis=0)
        self.average_TOA_NMF_test = np.expand_dims(np.mean(self.TOA_NMF_test, axis=0),axis=0)
    
        print("Step 4 of 5: Calculating RMSE and bias")
        # Calculate RMSE for all VBS and also TOA
        self.RMSE_A = np.mean(((A_test-self.A_NMF_test))**2)**0.5
        self.RMSE_B = np.mean(((B_test-self.B_NMF_test))**2)**0.5
        self.RMSE_SI = np.mean(((SI_test-self.SI_NMF_test))**2)**0.5
        self.RMSE_P = np.mean(((P_test-self.P_NMF_test))**2)**0.5
        self.RMSE_T = np.mean(((self.TOA_test-self.TOA_NMF_test))**2)**0.5
        self.RMSE_TC = np.mean(((self.TOM_test-self.TOM_NMF_test))**2)**0.5
        
        # Calculate bias for all VBS and also TOA
        self.bias_A = np.mean(self.A_NMF_test - A_test)
        self.bias_B = np.mean(self.B_NMF_test - B_test)
        self.bias_P = np.mean(self.P_NMF_test - P_test)
        self.bias_SI = np.mean(self.SI_NMF_test - SI_test)
        self.bias_T = np.mean(self.TOA_NMF_test - self.TOA_test)
        self.bias_TC = np.mean(self.TOM_NMF_test - self.TOM_test)
        
        
        print("Step 5 of 5: Reshaping test data for plotting on domain")
        # Reshape test data for plotting on domain
        self.A_test_reshaped = reshape_vbs(A_test,total_hours=96)
        self.B_test_reshaped = reshape_vbs(B_test,total_hours=96)
        self.P_test_reshaped = reshape_vbs(P_test,total_hours=96)
        self.SI_test_reshaped = reshape_vbs(SI_test,total_hours=96)
        
        # Reshape reconstructed NMF test data for plotting on domain
        self.A_NMF_test_reshaped = reshape_vbs(self.A_NMF_test,total_hours=96)
        self.B_NMF_test_reshaped = reshape_vbs(self.B_NMF_test,total_hours=96)
        self.P_NMF_test_reshaped = reshape_vbs(self.P_NMF_test,total_hours=96)
        self.SI_NMF_test_reshaped = reshape_vbs(self.SI_NMF_test,total_hours=96)
        return
        

def compression_extent():
    # Evaluate different amounts of superspecies
    num_features = [1, 2, 3, 4, 5, 6]
    RMSE_A = np.zeros(len(num_features))
    RMSE_B = np.zeros(len(num_features))
    RMSE_P = np.zeros(len(num_features))
    RMSE_SI = np.zeros(len(num_features))
    RMSE_T = np.zeros(len(num_features))
    Bias_A = np.zeros(len(num_features))
    Bias_B = np.zeros(len(num_features))
    Bias_P = np.zeros(len(num_features))
    Bias_SI = np.zeros(len(num_features))
    Bias_T = np.zeros(len(num_features))

    for n in range(0,len(num_features)):
        metrics = EvaluateCompression(num_supspec =num_features[n], train_func = NMF_reconstruct, test_func = Pseudoinverse_predict)
        
        RMSE_A[n] = np.mean(((A_test-metrics.A_NMF_test))**2)**0.5
        RMSE_B[n] = np.mean(((B_test-metrics.B_NMF_test))**2)**0.5
        RMSE_SI[n] = np.mean(((SI_test-metrics.SI_NMF_test))**2)**0.5
        RMSE_P[n] = np.mean(((P_test-metrics.P_NMF_test))**2)**0.5
        RMSE_T[n] = np.mean(((metrics.TOA_test-metrics.TOA_NMF_test))**2)**0.5
        
        
        Bias_A[n] = np.mean(metrics.A_NMF_test - A_test)
        Bias_B[n] = np.mean(metrics.B_NMF_test - B_test)
        Bias_P[n] = np.mean(metrics.P_NMF_test - P_test)
        Bias_SI[n] = np.mean(metrics.SI_NMF_test - SI_test)
        Bias_T[n] = np.mean(metrics.TOA_NMF_test - metrics.TOA_test)
    fig, (r_ax,b_ax) = subplots(1,2)
    r_ax.plot(num_features,RMSE_A,marker='.',color='turquoise',label = "aVOC VBS")
    r_ax.plot(num_features,RMSE_B,marker='o',color='olivedrab',label = "bVOC VBS")
    r_ax.plot(num_features,RMSE_P,marker='^',color='sienna', label = "POA VBS")
    r_ax.plot(num_features,RMSE_SI,marker='v',color='darkorange', label = "siSOA VBS")
    r_ax.plot(num_features,RMSE_T,marker='*',color='black', label = "TOA")
    r_ax.legend()
    r_ax.set_xlabel("Number of superspecies")
    r_ax.set_ylabel("RMSE [$ug \: m^{-3}$]")
    
    b_ax.plot(num_features,Bias_A,marker='.',color='turquoise',label = "aVOC VBS")
    b_ax.plot(num_features,Bias_B,marker='o',color='olivedrab',label = "bVOC VBS")
    b_ax.plot(num_features,Bias_P,marker='^',color='sienna', label = "POA VBS")
    b_ax.plot(num_features,Bias_SI,marker='v',color='darkorange',label = "siSOA VBS")
    b_ax.plot(num_features,Bias_T,marker='*',color='black',label = "TOA")
    b_ax.legend()
    b_ax.set_xlabel("Number of superspecies")
    b_ax.set_ylabel("Bias [$ug \: m^{-3}$]")
    fig.tight_layout()
    return fig

def Pseudoinverse_predict_TOM_conserved(X,W, H = None, P = None):
    # This function takes a new test data X and a pre-optimized W from a training dataset
    # The data points in the reduced dimension, H, are calculated using a left Moore-Penrose inverse of W
    # It conserves total concentration, such that no material is added or removed during compression/decompression
    
    # Create Moore-Penrose pseudoinverse
    P = np.linalg.inv(W.T@W)@W.T
    
    # Sum up total organic compounds (TOM) for each data point
    TOM = np.sum(X,axis=0)
    
    # Project into superspecies latent space
    H_NMF = P@X
    
    # Rescale the total concentration of superspecies to be equal to TOM
    H_NMF = (TOM/(np.sum(H_NMF,axis=0)+1e-10))*H_NMF
    
    # Transform back into original tracer space
    X_NMF = W@H_NMF
    
    # Make sure that sum of tracers in X_NMF is equal to sum of superspecies concentrations
    X_NMF = (np.sum(H_NMF,axis=0)/(np.sum(X_NMF,axis=0)+1e-10))*X_NMF
    
    return X_NMF, H_NMF

   

def Pseudoinverse_predict_TOA_conserved(X, W, H = None, P = None):
    # This function takes a new test data X and a pre-optimized W from a training dataset
    # The data points in the reduced dimension, H, are calculated using a left Moore-Penrose inverse of W
    # It conserves total aerosol mass during compression/decompression
    
    # Create Moore-Penrose pseudoinverse
    P = np.linalg.inv(W.T@W)@W.T
    
    # Calculate number of aerosol tracers, assuming that they are the first half of the tracers
    num_aer = int(X.shape[0]/2)
    
    # Sum up total organic (TOA) for each data point
    TOA = np.sum(X[0:num_aer,:],axis=0)
    
    # Project into superspecies latent space
    H_NMF = P@X
    
    # Rescale the total concentration of superspecies to be equal to TOA
    H_NMF = (TOA/(np.sum(H_NMF,axis=0)+1e-10))*H_NMF
    
    # Transform back into original tracer space
    X_NMF = W@H_NMF
    
    # Make sure that sum of tracers in X_NMF is equal to sum of superspecies concentrations
    X_NMF = (np.sum(H_NMF,axis=0)/(np.sum(X_NMF[0:num_aer,:],axis=0)+1e-10))*X_NMF
    
    return X_NMF, H_NMF

def NMF_NonNeg_predict_conserve_TOA(X, W, H = None, P = None):
    # Take data X and latent space representation H, as well as a preoptimized W.
    
    # Project into superspecies latent space
    H_pos = P@X
        
    # Calculate number of aerosol tracers, assuming that they are the first half of the tracers
    num_aer = int(X.shape[0]/2)
    
    # Sum up total organic (TOA) for each data point
    TOA = np.sum(X[0:num_aer,:],axis=0)
    
    # Rescale the total concentration of superspecies to be equal to TOA
    H_pos = (TOA/(np.sum(H_pos,axis=0)+1e-20))*H_pos
    
    # Transform into original tracer space
    X_pos = W@H_pos
    
    # Rescale tracers such that sum of decompressed aerosol tracers to be equal to sum of superspecies concentrations
    X_pos = (np.sum(H_pos,axis=0)/(np.sum(X_pos[0:num_aer,:],axis=0)+1e-20))*X_pos

    
    return X_pos,H_pos     
    
def NMF_NonNeg_predict(X, W, H = None, P = None):
    # This function takes a new test data X and a pre-optimized W from a training dataset
    # The data points in the reduced dimension, H, are calculated using a left Moore-Penrose inverse of W
    # It conserves total aerosol mass during compression/decompression

    # Project into superspecies latent space
    H_pos = P@X
        
    # Transform back into original tracer space
    X_pos = W@H_pos

    return X_pos, H_pos

def NMF_NonNeg_predict(X, W, H = None, P = None):
    # Take data X and latent space representation H, as well as a preoptimized W.

    H_pos = P@X
    X_pos = W@H_pos
    return X_pos,H_pos



def NMF_NonNeg_predict_conserve_TOM(X, W, H = None, P = None):
    # Take data X and latent space representation H, as well as a preoptimized W.
    
    # Project into superspecies latent space
    H_pos = P@X
        
    # Sum up total organic compounds (TOM) for each data point
    TOM = np.sum(X,axis=0)
    
    # Rescale the total concentration of superspecies to be equal to TOM
    H_pos = (TOM/(np.sum(H_pos,axis=0)+1e-20))*H_pos
    
    # Transform into original tracer space
    X_pos = W@H_pos
    
    # Rescale sum of decompressed tracers to be equal to TOM
    X_pos = (np.sum(H_pos,axis=0)/(np.sum(X_pos,axis=0)+1e-20))*X_pos
    
    return X_pos,H_pos



def NMF_NonNeg_train(vbs,d=2):
    # This function reconstructs test data with NMF, and also outputs W and H.
    # Inputs: 
        # vbs: test data, rows are tracers, each column is a new data point
        # d (optional): the number of latent dimensions (superspecies) to compress to. 2 is the default
    # Outputs:
        # vbsNMF: the reconstruction of vbs from the product WH.
        # W: optimized decompression matrix mapping superspecies to tracers
        # H: representation of vbs in the lower dimensional latent space
        # P: optimized compression matrix (non-negative) mapping tracers to superspecies
    model = NMF(n_components=d, init='random', random_state=1, alpha = 0.1)
    W = model.fit_transform(vbs)
    H = model.components_
    vbsNMF = W@H   #model.inverse_transform(W)

    
    
    # P in this case is not a pseudoinverse but a non-negative matrix optimized to compress.
    # Solve the non_negative_factorization problem H_pos = P@X, finding optimal W_pos.
    # P is the matrix that best compresses the original data.
    # W is the matrix that best decompresses the data.
    P, X, n_iter = non_negative_factorization(H, n_components = W.shape[0], alpha = 0.1, 
                                              init = 'custom', random_state=1,
                                              update_H = False, H = vbs)
    

    return vbsNMF,W,H,P

def NMF_NonNeg_train_normalize_columns(vbs,d=2):
    # This function reconstructs test data with NMF, and also outputs W and H.
    # Inputs: 
        # vbs: test data, rows are tracers, each column is a new data point
        # d (optional): the number of latent dimensions (superspecies) to compress to. 2 is the default
    # Outputs:
        # vbsNMF: the reconstruction of vbs from the product WH.
        # W: optimized decompression matrix mapping superspecies to tracers
        # H: representation of vbs in the lower dimensional latent space
        # P: optimized compression matrix (non-negative) mapping tracers to superspecies
    model = NMF(n_components=d, init='random', random_state=1, alpha = 0.1)
    W = model.fit_transform(vbs)
    H = model.components_
    vbsNMF = W@H   #model.inverse_transform(W)
    
    
    P = W.T
    
    W = W/np.sum(W,axis=0)
    P = P/np.sum(P,axis=0)
    
    return vbsNMF,W,H,P



def NMF_NonNeg_train_conserve_TOM(vbs,d=2):
    # This function reconstructs test data with NMF, and also outputs W and H.
    # Inputs: 
        # vbs: test data, rows are tracers, each column is a new data point
        # d (optional): the number of latent dimensions (superspecies) to compress to. 2 is the default
    # Outputs:
        # vbsNMF: the reconstruction of vbs from the product WH.
        # W: optimized matrix mapping superspecies to tracers
        # H: representation of vbs in the lower dimensional latent space
    model = NMF(n_components=d, init='random', random_state=1,alpha = 0.1)
    W = model.fit_transform(vbs)
    H = model.components_
    vbsNMF = W@H   #model.inverse_transform(W)
    
    # Normalize H so that the sum of superspecies is equal to sum of o.g. tracers
    H = H/(np.sum(vbs,axis=0)+1e-20)
    
    # P in this case is not a pseudoinverse but a non-negative matrix optimized to compress.
    # Solve the non_negative_factorization problem H_pos = P@X, finding optimal W_pos.
    # P is the matrix that best compresses the original data.
    # W is the matrix that best decompresses the data.
    P, X, n_iter = non_negative_factorization(H, n_components = W.shape[0],alpha = 0.1, 
                                              init = 'custom', random_state=1,
                                              update_H = False, H = vbs)
    

    return vbsNMF,W,H,P

def NMF_NonNeg_train_conserve_TOA(vbs,d=2):
    # This function reconstructs test data with NMF, and also outputs W and H.
    # Inputs: 
        # vbs: test data, rows are tracers, each column is a new data point
        # d (optional): the number of latent dimensions (superspecies) to compress to. 2 is the default
    # Outputs:
        # vbsNMF: the reconstruction of vbs from the product WH.
        # W: optimized matrix mapping superspecies to tracers
        # H: representation of vbs in the lower dimensional latent space
    model = NMF(n_components=d, init='random', random_state=1,alpha = 0.1)
    W = model.fit_transform(vbs)
    H = model.components_
    vbsNMF = W@H   #model.inverse_transform(W)
            
    # Calculate number of aerosol tracers, assuming that they are the first half of the tracers
    num_aer = int(vbs.shape[0]/2)
    
    # Normalize H so that the sum of superspecies is equal to sum of o.g. tracers
    H = H/(np.sum(vbs[0:num_aer,:],axis=0)+1e-20)
    
    # P in this case is not a pseudoinverse but a non-negative matrix optimized to compress.
    # Solve the non_negative_factorization problem H_pos = P@X, finding optimal W_pos.
    # P is the matrix that best compresses the original data.
    # W is the matrix that best decompresses the data.
    P, X, n_iter = non_negative_factorization(H, n_components = W.shape[0], 
                                              init = 'custom', random_state=1,alpha = 0.1,
                                              update_H = False, H = vbs)


    return vbsNMF,W,H,P

#%%
#----------------------- Begin main code, global scope ----------------------#
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



#%%
# NMF with 1 single feature
print("NMF/Pseudoinverse with 1 single feature")
metrics1 = EvaluateCompression(num_supspec=1)


# Plot siSOA distribution at midnight, Feb 27 (test data) at Cabauw, for LE and NMF
long_index = abs(nc.variables["longitude"][:]-[4.923611]).argmin()
lati_index = abs(nc.variables["latitude"][:]-[51.974444]).argmin()   
ax1 = plot_vbs(metrics1.SI_test_reshaped,72,lati_index,long_index,title="Cabauw siSOA, February 28, 00:00, LOTOS EUROS")
ax2 = plot_vbs(metrics1.SI_NMF_test_reshaped,72,lati_index,long_index,title="Cabauw siSOA, February 28, 00:00, 1 feature NMF",bincolor = 'maroon')

# check: does TOA match the sums of COA from all VBS?
t = 72
check = np.isclose(metrics1.TOA_NMF_test[t,0,lati_index,long_index],
                   np.sum(metrics1.A_NMF_test_reshaped[0:6,t,0,lati_index,long_index],axis=0)+
                   np.sum(metrics1.B_NMF_test_reshaped[0:6,t,0,lati_index,long_index],axis=0)+
                   np.sum(metrics1.P_NMF_test_reshaped[0:9,t,0,lati_index,long_index],axis=0)+
                   np.sum(metrics1.SI_NMF_test_reshaped[0:8,t,0,lati_index,long_index],axis=0))


# Plot test data on domain
figLE = QuickMap( metrics1.average_TOA_test[0,0,:,:], xx = lons, yy = lats, 
                         bmp=dict(countries = 'True', resolution = 'i'), 
                         cbar=dict( label='Average TOA (ug/m3)'),
                         vmin=0, vmax=np.max(metrics1.average_TOA_test),
                         figsize=(6,5), title = 'LOTOS-EUROS February 25-28, 2018'
                   )
plt.savefig('figs/LE_winter_test.pdf', format='pdf')

# Plot reconstructed test data on domain after compression from 1 superspecies
figNMF = QuickMap( (metrics1.average_TOA_NMF_test[0,0,:,:]), xx = lons, yy = lats, 
                         bmp=dict(countries = 'True', resolution = 'i'), 
                         cbar=dict( label='Average TOA (ug/m3)'),
                         vmin=0, vmax=np.max(metrics1.average_TOA_NMF_test),
                         figsize=(6,5), title = 'NMF/Pseudoinverse with one superspecies'
                      )
plt.savefig('figs/nmfpseudo_1sup.pdf', format='pdf')

# Plot relative bias between the two
fig_diff = QuickMap( 100*(metrics1.average_TOA_NMF_test[0,0,:,:] - metrics1.average_TOA_test[0,0,:,:] )/metrics1.average_TOA_test[0,0,:,:] ,
                    xx = lons, yy = lats, 
                    bmp=dict(countries = 'True', resolution = 'i'), 
                         cbar=dict( label='Normalized Bias of average TOA'),
                         vmin=-100, vmax=100,cmap=dict( colors=['blue', 'white','red'], ncolor=500 ),
                         figsize=(6,5), title = "Normalized bias compared to LE"
                      )
plt.savefig('figs/nmfpseudo_relbias_1sup.pdf', format='pdf')

# Timeseries of Cabauw
Cabauw = plot_timeseries(caba_lati,caba_long,"Cabauw",
                         metrics1.TOA, metrics1.TOA_NMF, 
                         metrics1.TOA_test, metrics1.TOA_NMF_test,
                         timerange = "February 20-28")  
plt.savefig('figs/cabauw_1sup.pdf', format='pdf')

# Timeseries of Mace Head
MaceHead = plot_timeseries(mace_lati,mace_long,"Mace Head",
                         metrics1.TOA, metrics1.TOA_NMF, 
                         metrics1.TOA_test, metrics1.TOA_NMF_test,
                         timerange = "February 20-28") 
plt.savefig('figs/macehead_1sup.pdf', format='pdf')

coords = (caba_lati,caba_long) 
vbses = (metrics1.P_test_reshaped,
         metrics1.P_NMF_test_reshaped)
titles = ("Cabauw POA, Feb 26, 20:00, LOTOS EUROS", "Cabauw NMF/Pseudoinverse, one superspecies")
ax1 = plot_multiple_vbs_withconc(vbses,44,coords[0],coords[1],titles,("g","maroon"))
plt.savefig('figs/cabauw_vbs_1sup.pdf', format='pdf')

coords = (mace_lati,mace_long) 
vbses = (metrics1.P_test_reshaped,
         metrics1.P_NMF_test_reshaped)
titles = ("Mace Head POA, Feb 26, 20:00, LOTOS EUROS", "Mace Head NMF/Pseudoinverse, one superspecies")
ax1 = plot_multiple_vbs_withconc(vbses,44,coords[0],coords[1],titles,("g","maroon"))
plt.savefig('figs/macehead_vbs_1sup.pdf', format='pdf')

plot_POA_emissions()
plt.savefig('figs/poa_emission_fractions.pdf', format='pdf')
#%%
# NMF with 3 features
print("NMF/Pseudoinverse with 3 latent features")
metrics3 = EvaluateCompression(num_supspec = 3, train_func = NMF_reconstruct, test_func = Pseudoinverse_predict)

# Plot test total organic aerosol
# Plot test total organic aerosol

# Plot Cabauw

Cabauw = plot_timeseries(caba_lati,caba_long,"Cabauw, 3 superspecies",
                           metrics3.TOA, metrics3.TOA_NMF, metrics3.TOA_test, metrics3.TOA_NMF_test,
                           timerange = "February 20-28")
plt.savefig('figs/cabauw_3sup.pdf', format='pdf')                     

#Plot Mace Head

MaceHead = plot_timeseries(mace_lati,mace_long,"Mace Head, 3 superspecies",
                           metrics3.TOA, metrics3.TOA_NMF, metrics3.TOA_test, metrics3.TOA_NMF_test,
                           timerange = "February 20-28")
plt.savefig('figs/macehead_3sup.pdf', format='pdf')

plot_W(metrics3.W_SI,vbs="siSOA",bincolor="darkorange")
plt.savefig('figs/plotW_siSOA.pdf', format='pdf')
plot_W(metrics3.W_P,vbs="POA",bincolor="sienna")
plt.savefig('figs/plotW_POA.pdf', format='pdf')


# Investigate negative concentrations          
neg_indices = np.unravel_index(metrics3.P_NMF_test_reshaped[:,:,0,:,:].argmin(), metrics3.P_NMF_test_reshaped.shape)
neg_lati = lats[neg_indices[3]]
neg_long = lons[neg_indices[4]]
neg_time = nc_test["time"][neg_indices[1]]
fig_neg = plot_neg_vbs(metrics3.P_NMF_test_reshaped,neg_indices[1],neg_indices[3],neg_indices[4],
                       title = "Decompressed VBS tracers of POA partitioning",bincolor = "maroon")
plt.savefig('figs/neg_poa.pdf', format='pdf')

num_neg_P = np.sum(metrics3.P_NMF_test < -1e-8)
num_biasneg_P = np.sum(metrics3.P_NMF_test < -abs(metrics3.bias_P))
size_P = metrics3.P_NMF_test.size

num_neg_TOA = np.sum(metrics3.TOA_NMF_test < -abs(metrics3.bias_T))
size_TOA = size_P/18


#%%
# Investigate extent of compression and accuracy
comp_extent =  compression_extent()
#plt.savefig('figs/compression_extent.pdf', format='pdf')
plt.savefig('figsPNG/compression_extent.png', format='png')
#%%
# Positive Forward Compression
print("Non-negative factorization: positive compression and decompression")
nmf_nonneg_metrics = EvaluateCompression(num_supspec = 3, 
                                                  train_func = NMF_NonNeg_train,
                                                  test_func = NMF_NonNeg_predict)
long_index = abs(nc.variables["longitude"][:]-[4.923611]).argmin()
lati_index = abs(nc.variables["latitude"][:]-[51.974444]).argmin()
Cabauw = plot_timeseries(lati_index,long_index,"Cabauw",
                         nmf_nonneg_metrics.TOA, nmf_nonneg_metrics.TOA_NMF, 
                         nmf_nonneg_metrics.TOA_test, nmf_nonneg_metrics.TOA_NMF_test)

fig_diff = QuickMap( 100*(nmf_nonneg_metrics.average_TOA_NMF_test[0,0,:,:] - nmf_nonneg_metrics.average_TOA_test[0,0,:,:] )/nmf_nonneg_metrics.average_TOA_test[0,0,:,:] ,
                     xx = lons, yy = lats, 
                    bmp=dict(countries = 'True', resolution = 'i'), 
                         cbar=dict( label='Normalized Bias of average TOA'),
                         vmin=-100, vmax=100,cmap=dict( colors=['blue', 'white','red'], ncolor=500 ),
                         figsize=(6,5), title = "Non-negative compression with 3 superspecies"
                      )
plt.savefig('figs/nonneg_relbias.pdf', format='pdf')
# Mass Conservation Experiments

print("Mass Conservation Experiment 1: NMF/Non-neg with total superspecies concentration equal to TOM")
strategy1 = EvaluateCompression(num_supspec = 3, 
                                                  train_func = NMF_NonNeg_train,
                                                  test_func = NMF_NonNeg_predict_conserve_TOM)

print("Mass Conservation Experiment 2: NMF/Non-neg with total superspecies concentration equal to TOA")
strategy2 = EvaluateCompression(num_supspec = 3, 
                                                  train_func = NMF_NonNeg_train,
                                                  test_func = NMF_NonNeg_predict_conserve_TOA)
#  Pseudoinverse_predict_TOA_conserved

print("Mass Conservation Experiment 3: NMF, Constructing composition matrices via column normalization")
strategy3 = EvaluateCompression(num_supspec = 3, 
                                                  train_func = NMF_NonNeg_train_normalize_columns,
                                                  test_func = NMF_NonNeg_predict)



# Get distribution at a snapshot in time

coords = (mace_lati,mace_long) 
vbses = (strategy1.P_test_reshaped,
         strategy1.P_NMF_test_reshaped,
         strategy2.P_NMF_test_reshaped,
         strategy3.P_NMF_test_reshaped)
titles = ("Mace Head POA, Feb 26, 20:00, LOTOS EUROS", "Strategy 1: Conserve TOM",
          "Strategy 2: Conserve TOA","Strategy 3: Composition Matrices")
ax1 = plot_multiple_vbs_withconc(vbses,44,coords[0],coords[1],titles,("g","maroon","orange","red"))
plt.savefig('figs/macehead_nonneg_pVBS_feb26.pdf', format='pdf')

coords = (caba_lati,caba_long) 
vbses = (strategy1.P_test_reshaped,
         strategy1.P_NMF_test_reshaped,
         strategy2.P_NMF_test_reshaped,
         strategy3.P_NMF_test_reshaped)
titles = ("Cabauw POA, Feb 26, 20:00, LOTOS EUROS", "Strategy 1: Conserve TOM",
          "Strategy 2: Conserve TOA","Strategy 3: Composition Matrices")
ax1 = plot_multiple_vbs_withconc(vbses,44,coords[0],coords[1],titles,("g","maroon","orange","red"))
plt.savefig('figs/cabauw_nonneg_pVBS_feb26.pdf', format='pdf')

coords = (caba_lati,caba_long) 
vbses = (strategy1.A_test_reshaped,
         strategy1.A_NMF_test_reshaped,
         strategy2.A_NMF_test_reshaped,
         strategy3.A_NMF_test_reshaped)
titles = ("Cabauw siOA, Feb 26, 20:00, LOTOS EUROS", "Strategy 1: Conserve TOM",
          "Strategy 2: Conserve TOA","Strategy 3: Composition Matrices")
ax1 = plot_multiple_vbs_withconc(vbses,44,coords[0],coords[1],titles,("g","maroon","orange","red"))




#----------------------- End working code -----------------------------------#

#%%

#numpy.savetxt("W_A.csv", W_A, delimiter=",")

#nc.close()


def make_gif(nc_control,nc_superspecies,gifname='movie'):
    base_date = datetime(2018, 1, 1) 
    files = []
    for i in range(0,24):
        
        d = base_date + timedelta(seconds=(nc["time"][i]+0)) 
        
        fig = QuickMap( (RMSE[i,0,:,:]), domain =[minlon,maxlon,minlat,maxlat], 
                         bmp=dict(countries = 'True', resolution = 'i'), 
                         cbar=dict( label=label),
                         vmin=0, vmax=np.max(RMSE),
                         figsize=(6,5), title = d.strftime("%Y-%m-%d %H:%M:%S")
                         )
    
        #matplotlib.pyplot.text(0, 1, s)
        picpath = 'gifs/gifpics/' + f'{i:02d}' + '_20180220.png'
        files.append(picpath)   
        fig.Export(picpath)  
        close()
    
    # make images from the file list "files"
    images = [imageio.imread(file) for file in files]
    imageio.mimwrite('gifs/' + filename + '.gif', images, fps=2)
#make_gif()



