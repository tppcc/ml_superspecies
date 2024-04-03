import os
from datetime import datetime

import numpy as np
import torch
import torchnmf as nmf

r"""
 class nmf_object
 This class defines the nmf object and perform checks on the initialisation of weight and inheritances. 
 We follow the definition in torchnmf, where the projection matrix to latent space is termed H instead of W, 
 while the latent represented data matrix is termed W (Line 22-23, 63-64), such that argmin|| V(N,C) - H(N,R) * W(C,R)^T || 
 is performed with beta divergence minimisation routine
"""


class NMF:
    def __init__(self, n_size=None, m_size=None, rank=None):
        r"""
        Initializes an instance of the NMF class.

        Args:
            m_size (int): Size of each batch of input.
            n_size (int): Size of input channels.
            rank (int): Size of channel in latent space.
        """
        self.cwd = os.getcwd()  # Getting the current working directory
        self.n_size = n_size  # Assigning n_size attribute
        self.m_size = m_size
        self.rank = rank  # Assigning rank attribute

        # Initializing the Projection_BaseComponent and Reconstruction_BaseComponent using nmf function
        # and assigning to corresponding attributes
        self.Projection_BaseComponent = self.__nmf([self.rank, self.m_size], rank=self.n_size)
        self.Reconstruction_BaseComponent = self.__nmf([self.n_size, self.m_size], rank=self.rank)

        # Setting B attribute to the H attribute of Reconstruction_BaseComponent
        self.B = self.Projection_BaseComponent.H

        self.i = 0  # Initializing i attribute to 0

    def __directory_check(self, directory):
        r"""
        Checks if a directory exists. If not, creates the directory.

        Args:
            directory (str): The directory path to check/create.
        Returns:
            None
        """
        if not os.path.exists(directory):  # Check if directory exists
            os.makedirs(directory)  # Create directory if it doesn't exist

    def __nmf(self, shape, rank, **kwargs):
        r"""
        Initializes an instance of the NMF class.

        Args:
            shape (list): The shape of the matrix.
            rank (int): Size of channel in latent space.
            **kwargs: Additional keyword arguments.

        Returns:
            instance (NMF): An instance of the NMF class.
        """
        # Initialise NMF class instance
        # instance = nmf.nmf.NMF(H=self.H, W=self.W, rank=rank)
        instance = nmf.nmf.NMF(shape, rank=rank, **kwargs)
        return instance

    def fit(self, V, beta=1, tol=0.0001, max_iter=200, verbose=False, alpha=0, l1_ratio=0,
            storage=True, intermediate_stop=500, **kwargs):
        r"""
        Fits the NMF model to the input data.

        Args:
            V (Tensor): Input data.
            beta (float): Parameter for regularization.
            tol (float): Tolerance value for convergence.
            max_iter (int): Maximum number of iterations.
            verbose (bool): Whether to print progress messages.
            alpha (float): Parameter for L1 regularization.
            l1_ratio (float): Parameter for L1 regularization.
            storage (bool): Whether to store intermediate results.
            intermediate_stop (int): Interval for storing intermediate results.
            **kwargs: Additional keyword arguments.
        Returns:
            None
        """
        # Training start message
        # print("Training start, V has %s time component" %(V.shape[2]))

        # Fitting Projection_BaseComponent
        self.Reconstruction_BaseComponent.fit(V, beta=beta, tol=tol, max_iter=max_iter, verbose=verbose,
                                          alpha=alpha, l1_ratio=l1_ratio)

        # Getting W and H matrices from Projection_BaseComponent
        self.W = self.Reconstruction_BaseComponent.W
        self.H = self.Reconstruction_BaseComponent.H

        # Fitting Reconstruction_BaseComponent
        self.Projection_BaseComponent = nmf.nmf.NMF(H=self.B, W=V.T, rank=self.n_size,
                                                        trainable_W=False)
        self.Projection_BaseComponent.fit(self.W.T, beta=beta, tol=tol, max_iter=max_iter,
                                              verbose=verbose, alpha=alpha, l1_ratio=l1_ratio)
        self.B = self.Projection_BaseComponent.H

        # Intermediate stop storage to avoid Overfitting
        if storage == True:
            self.__directory_check(os.path.join(self.cwd, "bkp"))
            if np.mod((self.i + 1), intermediate_stop) == 0:
                with torch.no_grad():
                    dt = datetime.now().strftime("%Y%m%d_%H%M%S")
                    torch.save(self.Reconstruction_BaseComponent.H,
                               os.path.join(self.cwd, "bkp", "backup_W_%s_%s.pth" % (self.i, dt)))
                    torch.save(self.B,
                               os.path.join(self.cwd, "bkp", "backup_B_%s_%s.pth" % (self.i, dt)))
                    print("Intermediate weight saved at iteration %s" % (self.i + 1))
        self.i += 1  # Incrementing iteration counter
