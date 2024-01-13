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
    def __init__(self, m_size = None, n_size = None, rank=None):     # inheritance=False, H_init=None, W_init=None,
        self.cwd = os.getcwd()
        #self.inheritance = inheritance_check()
        #self.W = H_init
        #self.H = W_init
        #self.inheretence_check()
        self.n_size = n_size
        self.rank = rank
        #self.dimensionality_check()
        self.Projection_BaseComponent = self.nmf([m_size, n_size], rank=self.rank, **kwargs)
        self.Reconstruction_BaseComponent = self.nmf([self.rank, n_size], rank=m_size, **kwargs)
        self.B = self.Reconstruction_BaseComponent.H
        self.i = 0

    def directory_check(self, directory):
        # Check if directory eixst, if == False create direcotry
        if os.path.exists(directory) == False:
            os.makedirs(directory)

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
        instance = nmf.nmf.NMF(shape, rank=rank, **kwargs)
        return instance

    def trainer(self, V, beta=1, tol=0.0001, max_iter=200, verbose=False, alpha=0, l1_ratio=0, storage=True, intermediate_stop=50, **kwargs):
        print("Training start, V has %s time component" %(V.shape[2]))
        self.Projection_BaseComponent.fit(V, beta=beta, tol=tol, max_iter=max_iter, verbose=verbose, alpha=alpha, l1_ratio=l1_ratio)
        self.W = self.Projection_BaseComponent.W
        self.Reconstruction_BaseComponent = nmf.nmf.NMF(H=self.B, W=V, rank=V.shape[0], trainable_W = False)
        self.Reconstruction_BaseComponent.fit(self.W, beta=beta, tol=tol, max_iter=max_iter, verbose=verbose, alpha=alpha, l1_ratio=l1_ratio)
        self.B = self.Reconstruction_BaseComponent.H
        if np.mod((self.i + 1), 5) == 0:
            print("%s iterations completed, %s time component remaining" %((self.i + 1), (V.shape[2] - self.i + 1)))
        # Intermediate stop storage to avoid Overfitting
        if storage == True:
            self.directory_check(os.path.join(self.cwd, "trained_matrix_backup"))
            if np.mod((self.i+1), intermediate_stop) == 0:
                with torch.no_grad():
                    dt = datetime.now().strftime("%Y%m%d_%H%M%S")
                    torch.save(self.Projection_BaseComponent.H,
                               os.path.join(self.cwd, "backup_W_%s_%s.pth" % (self.i, dt)))
                    torch.save(self.B, os.path.join(self.cwd, "backup_B_%s_%s.pth" % (self.i, dt)))
                    print("Intermediate weight saved at iteration" % (self.i + 1))
        self.i += 1
        print("Training completed")
        with torch.no_grad():
            dt = datetime.now().strftime("%Y%m%d_%H%M%S")
            torch.save(self.Projection_BaseComponent.H, os.path.join(self.cwd, "trained_W_%s.pth" %(dt)))
            torch.save(self.B, os.path.join(self.cwd, "trained_B_%s.pth" % (dt)))
            print("Trained weight saved at %s" %(self.cwd))
