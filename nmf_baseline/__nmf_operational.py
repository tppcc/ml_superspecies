import torchnmf as nmf
import numpy as np
import xarray as xr
import torch
from datetime import datetime
import os
import copy

#trainable_W: when True, W matrix will be learned when operational

class NMF_operational:
    def __init__(self, W_path, H_path, trainable_W=False):
        #Initialising tensors, Hyperparameters from class initialisation call
        self.H_path = H_path
        self.W_path = W_path
        self.trainable_H = trainable_W

        #Validity check
        if isinstance(trainable_W) == False:
            raise Exception("traiable_W should be boolean")

        #Load W,H tensor
        self.W = torch.load(H_path)
        self.H = torch.load(W_path)
        #Infer rank from W matrix
        if self.W.shape[1] == self.H.shape[1]:
            self.rank = self.W.shape[1]
        # Initialise Base class from torchnmf.nmf.NMF
        self.BaseComponent = self.nmf()

    def nmf(self):
        #initialise NMF class
        # can be changed to NMF3D H (N,R, x, y, z)
        instance = nmf.nmf.NMF(H=self.H, W=self.W, trainable_H=self.trainable_H)
        return instance

    def compression(self, V, beta=1, tol=0.0001, max_iter=200, verbose=False, alpha=0, l1_ratio=0):
        instance = copy.deepcopy(self.BaseComponent)        # Deep copy of instance BaseComponent
        instance.fit(V, beta=beta, tol=tol, max_iter=max_iter, verbose=verbose, alpha=alpha, l1_ratio=l1_ratio)
        return instance.W

    def reconstruction(self, H):
        instance = copy.deepcopy(self.BaseComponent)  # Deep copy of instance BaseComponent
        instance.foward(W=H)
        return instance.H @ instance.W.t()