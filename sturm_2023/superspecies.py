#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Aug 26 17:06:15 2021

@author: sturmpo
"""
# This library contains functions used in the thesis Advecting Superspecies
# For questions contact Obin Sturm (posturm@ucdavis.edu)

# Numpy and ML modules
import numpy as np
from sklearn.decomposition import NMF, non_negative_factorization

# netCDF4 data
# import netCDF4
import xarray as xr

# Time module
from datetime import date, timedelta #, datetime

# Plotting
# import imageio
from matplotlib.pyplot import close,plot,subplots
from go_plot import * # note: keep go_plot.py in folder if not in TNO system
import matplotlib.pyplot as plt

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
    # This function converts gas mixing ratio in ppb to concentration in ug m^-3
    # Inputs:
        # gas_ppb: mixing ratio in parts per billion (ppb)
        # mw_cg: molecular weight of the gas in kg/mol
        # tk: temperature in Kelvin (K)
        # pk: pressure in Pascals (Pa)
    # Output:
        # gas_ugm3: gas concentration in micrograms per cubic meter
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
    fig, ax = plt.subplots(len(reshaped_vbs),1,figsize=(6,8))
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


def NMF_NonNeg_predict_conserve_TOM(X, W, H = None, P = None):
    # This function evaluates mass-conserving strategy 1 on test data
    # Inputs: 
        # X: VBS data 
        # W: Decompression matrix
        # H: Optimized latent space representation of training data (not used, included for compatibility)
        # P: Compression matrix
    # Outputs:
        # X_pos: Reconstructed VBS test data after compression
        # H_pos: Superspecies representation of test data
    
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


def NMF_NonNeg_predict(X, W, H = None, P = None):
    # This function evaluates non-negative compression and decompression on test data
    # Inputs: 
        # X: VBS data 
        # W: Decompression matrix
        # H: Optimized latent space representation of training data (not used, included for compatibility)
        # P: Compression matrix
    # Outputs:
        # X_pos: Reconstructed VBS test data after compression
        # H_pos: Superspecies representation of test data
        
    # Project into superspecies latent space
    H_pos = P@X    
    
    # Transform into original tracer space
    X_pos = W@H_pos

    return X_pos,H_pos


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


class EvaluateCompression_SinglePhase():
    # This class runs a data pipeline for creating decompression and compression matrices
    # For phase-specific superspecies
    # This all happens in the __init__ method
    # High level scope:
        # 1. Generate compression and decompression matrices using train_func and training data
        # 2. Use matrices to compress and decompress test data
        # 3. Compute total organic aerosol and total organic mass (TOA and TOM)
        # 4. Compute error metrics on training and test data
        # 5. Reshape data for plotting on domain
    
    def __init__(self,num_supspec = 2, train_func = NMF_NonNeg_train, test_func = NMF_NonNeg_predict_conserve_TOM):
    #     return
        
    # def compress(self):  
        print("Data pipeline with " + str(num_supspec) + " superspecies")
        print("Step 1 of 5: Training data")
        # Compression on gas training data
        self.A_gas_NMF, self.W_A_gas, self.H_A_gas, self.P_A_gas = train_func(A_gas,d=num_supspec)
        self.B_gas_NMF, self.W_B_gas, self.H_B_gas, self.P_B_gas = train_func(B_gas,d=num_supspec)
        self.SI_gas_NMF, self.W_SI_gas, self.H_SI_gas, self.P_SI_gas = train_func(SI_gas,d=num_supspec)
        self.P_gas_NMF, self.W_P_gas, self.H_P_gas, self.P_P_gas = train_func(P_gas,d=num_supspec)
        
         # Compression on aerosol training data
        self.A_aer_NMF, self.W_A_aer, self.H_A_aer, self.P_A_aer = train_func(A_aer,d=num_supspec)
        self.B_aer_NMF, self.W_B_aer, self.H_B_aer, self.P_B_aer = train_func(B_aer,d=num_supspec)
        self.SI_aer_NMF, self.W_SI_aer, self.H_SI_aer, self.P_SI_aer = train_func(SI_aer,d=num_supspec)
        self.P_aer_NMF, self.W_P_aer, self.H_P_aer, self.P_P_aer = train_func(P_aer,d=num_supspec)       
        
        # Concatenate training aerosol and gas 
        self.A_NMF = np.vstack((self.A_aer_NMF,self.A_gas_NMF))
        self.B_NMF = np.vstack((self.B_aer_NMF,self.B_gas_NMF))
        self.SI_NMF = np.vstack((self.SI_aer_NMF,self.SI_gas_NMF))
        self.P_NMF = np.vstack((self.P_aer_NMF,self.P_gas_NMF))
        
        print("Step 2 of 5: Testing data")
        # Compression on gas testing data
        self.A_gas_NMF_test, self.H_A_gas_NMF_test = test_func(A_gas_test, self.W_A_gas, self.H_A_gas, self.P_A_gas)
        self.B_gas_NMF_test, self.H_B_gas_NMF_test = test_func(B_gas_test, self.W_B_gas, self.H_B_gas, self.P_B_gas)
        self.SI_gas_NMF_test, self.H_SI_gas_NMF_test = test_func(SI_gas_test, self.W_SI_gas, self.H_SI_gas, self.P_SI_gas)
        self.P_gas_NMF_test, self.H_P_gas_NMF_test = test_func(P_gas_test, self.W_P_gas, self.H_P_gas, self.P_P_gas)
        
        # Compression on aerosol testing data
        self.A_aer_NMF_test, self.H_A_aer_NMF_test = test_func(A_aer_test, self.W_A_aer, self.H_A_aer, self.P_A_aer)
        self.B_aer_NMF_test, self.H_B_aer_NMF_test = test_func(B_aer_test, self.W_B_aer, self.H_B_aer, self.P_B_aer)
        self.SI_aer_NMF_test, self.H_SI_aer_NMF_test = test_func(SI_aer_test, self.W_SI_aer, self.H_SI_aer, self.P_SI_aer)
        self.P_aer_NMF_test, self.H_P_aer_NMF_test = test_func(P_aer_test, self.W_P_aer, self.H_P_aer, self.P_P_aer)
        
        # Concatenate testing aerosol and gas 
        self.A_NMF_test = np.vstack((self.A_aer_NMF_test,self.A_gas_NMF_test))
        self.B_NMF_test = np.vstack((self.B_aer_NMF_test,self.B_gas_NMF_test))
        self.SI_NMF_test = np.vstack((self.SI_aer_NMF_test,self.SI_gas_NMF_test))
        self.P_NMF_test = np.vstack((self.P_aer_NMF_test,self.P_gas_NMF_test))
                
        print("Step 3 of 5: Calculating TOA and average TOA")
        # Calculate total organic aerosol, TOA
        self.TOA = np.sum(A[0:6,:],axis=0) + np.sum(B[0:6,:],axis=0) + np.sum(SI[0:8,:],axis=0) + np.sum(P[0:9,:],axis=0)
        self.TOA_NMF = np.sum(self.A_NMF[0:6,:],axis=0) + np.sum(self.B_NMF[0:6,:],
                                axis=0) + np.sum(self.SI_NMF[0:8,:],axis=0) + np.sum(self.P_NMF[0:9,:],axis=0)
        
        # Calculate TOA for the test days
        self.TOA_test = np.sum(A_test[0:6,:],axis=0) + np.sum(B_test[0:6,:],axis=0) + np.sum(SI_test[0:8,:],axis=0) + np.sum(P_test[0:9,:],axis=0)
        self.TOA_NMF_test = np.sum(self.A_NMF_test[0:6,:],axis=0) + np.sum(self.B_NMF_test[0:6,:],
                              axis=0) + np.sum(self.SI_NMF_test[0:8,:],axis=0) + np.sum(self.P_NMF_test[0:9,:],axis=0)
        
        
        # Calculate total organic mass, TOM
        self.TOM = np.sum(A,axis=0) + np.sum(B,axis=0) + np.sum(SI,axis=0) + np.sum(P,axis=0)
        self.TOM_NMF = np.sum(self.A_NMF,axis=0) + np.sum(self.B_NMF,axis=0) + np.sum(self.SI_NMF,axis=0) + np.sum(self.P_NMF,axis=0)
        # Calculate total organic mass, TOM for test days
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
        # Calculate RMSE for all VBS and also TOA/TOM
        self.RMSE_A = np.mean(((A_test-self.A_NMF_test))**2)**0.5
        self.RMSE_B = np.mean(((B_test-self.B_NMF_test))**2)**0.5
        self.RMSE_SI = np.mean(((SI_test-self.SI_NMF_test))**2)**0.5
        self.RMSE_P = np.mean(((P_test-self.P_NMF_test))**2)**0.5
        self.RMSE_T = np.mean(((self.TOA_test-self.TOA_NMF_test))**2)**0.5
        self.RMSE_TC = np.mean(((self.TOM_test-self.TOM_NMF_test))**2)**0.5
        
        # Calculate bias for all VBS and also TOA/TOM
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
        
