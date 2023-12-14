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
#                           minimisation
################


class nmf_object:
    def __init__(self, V, rank=None, inheretence=False, H_init=None, W_init=None):
        self.V = V
        self.inheretence = inheretence
        self.W = W_init
        self.H = H_init
        self.inheretence_check()
        self.rank = rank

    def inheretence_check(self):
        if self.inheretence == False:
            if self.W != None & self.H != None:
                raise Exception("W_init & H_init exist when inheretence is set to False!")

    def nmf(self):
        #initialise NMF class
        if self.inheretence == True:
            self.instance = nmf.nmf.NMF(H=self.H, W=self.W, rank=self.rank)
        elif self.inheretence == False:
            self.instance = nmf.nmf.NMF(rank=self.rank)


instance = nmf.nmf.NMF()