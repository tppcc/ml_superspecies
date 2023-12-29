import numpy as np
import xarray as xr
import torchnmf as nmf
import torch
from datetime import datetime
import os

################
# class nmf_object:         This class defines the nmf object and perform checks on the initialisation
#                           of weight and inheritances. We follow the definition in torchnmf, where
#                           the projection matrix to latent space is termed H instead of W, while the
#                           latent represented data matrix is termed W (Line 22-23, 63-64), such that
#                           argmin|| V(N,C) - H(N,R) * W(C,R)^T || is performed with beta divergence
#                           minimisation routine
################


class NMF:
    def __init__(self, rank=None, kernel_size=(1,1,1)):     # inheritance=False, H_init=None, W_init=None,
        self.cwd = os.getcwd()
        #self.inheritance = inheritance_check()
        #self.W = H_init
        #self.H = W_init
        #self.inheretence_check()
        self.rank = rank
        #self.dimensionality_check()
        self.kernel_size=kernel_size


    def inheretence_check(self):
        if self.inheretence == False:
            if (self.W != None) & (self.H != None):
                raise Exception("W_init & H_init exist when inheritance is set to False!")

    def dimensionality_check(self):
        if self.W.shape[1] != self.rank:
            raise Exception("H_init shape is incorrect, dimension 1 should match number of rank")
        if self.H.shape[1] != self.rank:
            raise Exception("W_init shape is incorrect, dimension 1 should match number of rank")


    def nmf(self, shape, rank, **kwargs):
        #initialise NMF class
        #instance = nmf.nmf.NMF(H=self.H, W=self.W, rank=rank)
        instance = nmf.nmf.NMF(shape, rank=rank)
        return instance

    def trainer(self, V, beta=1, tol=0.0001, max_iter=200, verbose=False, alpha=0, l1_ratio=0):
        # Training Projection Matrix
        self.Projection_BaseComponent = self.nmf([V.shape[0], V.shape[1]], rank=self.rank)
        Reconstruction_BaseComponent = self.nmf([self.rank, V.shape[1]], rank=V.shape[0])
        Reconstruction_B = Reconstruction_BaseComponent.H
        print("Training start, V has %s time component" %(V.shape[2]))
        for i in np.arange(V.shape[2]):
            self.Projection_BaseComponent.fit(V[:,:,i], beta=beta, tol=tol, max_iter=max_iter, verbose=verbose, alpha=alpha, l1_ratio=l1_ratio)
            Reconstruction = nmf.nmf.NMF(H=Reconstruction_B, W=V[:,:,i], rank=V.shape[0], trainable_W = False)
            Reconstruction.fit(self.Projection_BaseComponent.W, beta=beta, tol=tol, max_iter=max_iter, verbose=verbose, alpha=alpha, l1_ratio=l1_ratio)
            Reconstruction_B = Reconstruction.H
            if np.mod((i + 1), 5):
                print("%s iterations completed, %s time component remaining" %((i + 1), (V.shape[2] + 1)))
        print("Training completed")
        with torch.no_grad():
            dt = datetime.now().strftime("%Y%m%d_%H%M%S")
            torch.save(self.BaseComponent.H, os.path.join(self.cwd, "trained_W_%s.pth" %(dt)))
            torch.save(Reconstruction_B, os.path.join(self.cwd, "trained_B_%s.pth" % (dt)))
            print("Trained weight saved at %s" %(self.cwd))
