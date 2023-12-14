import numpy as np
import xarray as xr
import torchnmf as nmf
import torch
import os

################
# class nmf_object:         This class defines the nmf object and perform checks on the initialisation
#                           of weight and inheretences. We follow the definition in torchnmf, where
#                           the projection matrix to latent space is termed H instead of W, while the
#                           latent represented data matrix is termed W, such that
#                           argmin|| V(N,C) - H(N,R) * W(C,R)^T || is performed with beta divergence
#                           minimisation routine
################


class nmf_object:
    def __init__(self, rank=None, inheretence=False, H_init=None, W_init=None, kernel_size=(1,1,1)):
        self.inheretence = inheretence
        self.W = W_init
        self.H = H_init
        self.inheretence_check()
        self.rank = rank
        self.dimensionality_check()
        self.kernel_size=kernel_size

    def inheretence_check(self):
        if self.inheretence == False:
            if (self.W != None) & (self.H != None):
                raise Exception("W_init & H_init exist when inheretence is set to False!")

    def dimensionality_check(self):
        if self.H.shape[1] != self.rank:
            raise Exception("H_init shape is incorrect, dimension 1 should match number of rank")
        if self.W.shape[1] != self.rank:
            raise Exception("W_init shape is incorrect, dimension 1 should match number of rank")


    def nmf(self):
        #initialise NMF class
        if self.inheretence == True:
            # can be changed to NMF3D H (N,R, x, y, z)
            self.instance = nmf.nmf.NMF(H=self.H, W=self.W, rank=self.rank)
        elif self.inheretence == False:
            self.instance = nmf.nmf.NMF(rank=self.rank)

    def trainer(self, V, beta=1, tol=0.0001, max_iter=200, verbose=False, alpha=0, l1_ratio=0):
        self.instance.fit(V, beta=beta, tol=tol, max_iter=max_iter, verbose=verbose, alpha=alpha, l1_ratio=l1_ratio)