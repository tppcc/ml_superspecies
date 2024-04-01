import time

import numpy as np
import torch

import nmf_baseline
from .__utils import RootMeanSquare, RelativeRootMeanSquare

r""" Wrapper of nmf_baseline

 This class gives a one-stop solution of training the projection matrix of NMF based on Slurm et al. (2023)
 and torchnmf module. Note that memory constrain is often reached before CPU bottle-neck is
 reached.
"""


class NonNegTrainer:
    def __init__(self, output_dir, n_size, m_size, rank, n_process=1):
        r"""initialise matrix
        Args:
            output_dir    (char): Direcotry for W, B matrix
            n_size         (int): Size of input channels
            m_size         (int): Size of each batch of input
            rank           (int): Size of channel in latent space
            n_process      (int): (default: 1) number of parallel processes to be initiated for training, performance critical,
                                    keep in mind of the memory restriction of the host system
        """
        # Assert type of input values
        assert ((type(n_size) == int) & (type(m_size) == int) & (
                type(rank) == int)), "dtype of (n_size, m_size, rank) must be int"
        self.__n_size = n_size
        self.__m_size = m_size
        self.__rank = rank
        self.__n_process = n_process
        self.output_dir = output_dir

    def __stack(self, input_array):
        r""" Flatten each input species into 1-D ndarray(q,)
        Args:
            input_array (ndarray): Numpy Array of each species (jc, jk, jc) when coupled to ICON (ComIn)
        Return:
            (ndarray(q,)): Ravelled 1-D array
        """

        # Perform data check, input must be numpy array
        assert type(input_array) == np.ndarray, "Input must be a numpy array"
        return input_array.ravel()

    def __species_preprocessing(self, data):
        r"""Preprocess each data
        Args:
            data (list): list of species array, dimension must be the same
        Return:
            data (ndarray(n_size, q)): pre-processed 2-D ndarray ready for training
        """

        # Assert if number of species matches n_size, dtype of elements in the list are numpy array and size of all array matches
        assert len(
            data) == self.__n_size, "Number of speices does not match with value of n_size (%s) initialised in this instnace" % (
            self.__n_size)
        assert all([(type(x) == np.ndarray) for x in
                    data]), "Some elements in the input list is not np.ndarray"
        assert all([(x.size == data[0].size) for x in
                    data]), "Size of array is not the same for all arrays"

        # Preprocessing: flatten each array and concat as a ndarray with dimnension (n_size, q) [q: size of sample space]
        data = [self.__stack(x) for x in data]
        data = np.vstack(data)

        return data

    def __batching(self, data):
        r"""Perform batching to input array if the size of dim-1 of da is larger than m_size
        Args:
            data (ndarray(n_size, q)): 2-D ndarray ready for training
        Return:
            (list(ndarray(n_size, m_size))): list of 2-D ndarray matching the dimension of n_size, m_size
                                                while initiaising this class instance
        """

        assert data.shape[
                   1] >= self.__m_size, "dimension 1 of the flattened array has a smaller size than the selected m_size in this class instance"

        if data.shape[1] != self.__m_size:
            self.batching_flag = True
            batch_number = data.shape[1] // self.__m_size
            return np.array_split(data, batch_number, axis=1)

        else:
            # Return a one element list containing the data
            return [data]

    def __benchmarking(self, x_train, B, W):
        r"""
        Benchmarking for the current batch using RMSE and RRMSE (See __utils)
        Args:
            x_train (ndarray):Array for training
            B (ndarray or Tensor): Projection Tensor/Matrix to latent space
            W (ndarray or Tensor): Reconstruction Tensor/Matrix to source space
        Return:
            rmse (float): Root Mean Square Error
            rrmse (float): Relative Root Mean Square Error
        """

        x_predict = W @ (B @ x_train)
        rmse = RootMeanSquare(x_train, x_predict)
        rrmse = RelativeRootMeanSquare(x_train, x_predict, rmse)
        return rmse, rrmse

    def

    def __training(self, x_train):
        r"""Training backend of the class self.__preprocess_and_fit
        Args:
            x_train (ndarray(n_size, m_size)): training data for each thread for further processing
        """

        # Initialise nmf_baseline class instance if it does not already already, i.e. fit is run already
        if not hasattr(self, 'nmf_instance'):
            self.nmf_instance = nmf_baseline.NMF(self.__n_size, self.__m_size, rank=self.__rank)

        self.nmf_instance.fit(torch.tensor(x_train), **self.hyperparameters)
        B, W = self.nmf_instance.Projection_BaseComponent.H, self.nmf_instance.Reconstruction_BaseComponent.H

        rmse, rrmse = self.__benchmarking(x_train, B, W)

        return rmse, rrmse

    def __compute(self, training_data):
        r"""Backend for self.fit() This function preprocess the training_data (list(ndarray)),
        perform the necessary consistency check and ravel into ndarray(n_size, q). Then
        batching is performed such that the training data is split along axis-1 with ( q // m_size).
        Args:
            training_data (list(ndarray)): list of species array, length of training_data must be the same as n_size
        """

        rmse_total = []
        rrmse_total = []

        # Preprocess and batching of the input training data
        data = self.__species_preprocessing(training_data)
        data = self.__batching(data)

        # timer for training
        start_time = time.time()

        for x_train in data:
            rmse, rrmse = self.__training(x_train)

        print(
            ' Training for the current step completed, time_elapsed=%s \n Benchmark: \n RMSE: %s, RRMSE: %s' % (
            time.time() - start_time, rmse, rrmse))

        rmse_total.append(rmse)
        rrmse_total.append(rrmse)

        return rmse_total, rrmse_total

    def fit(self, training_data, beta=1, tol=0.0001, max_iter=200, verbose=False, alpha=0,
            l1_ratio=0, storage=True, intermediate_stop=500):
        r"""processing the input data and fit to train nmf_baseline.NMF object
        Args:
            training_data (list(ndarray)): list of species array, dimension must be the same
            beta and beyond: see torchnmf.nmf.NMF documentation, these arguments are passed directly to torchnmnf backend
        Return:
            B (Tensor): Trained projection matrix for the given
        """

        # Pass hyperparameters as dictionary
        self.hyperparameters = {'beta': beta,
                                'tol': tol,
                                'max_iter': max_iter,
                                'verbose': verbose,
                                'alpha': alpha,
                                'l1_ratio': l1_ratio,
                                'storage': storage,
                                'intermediate_stop': intermediate_stop}

        # Perform data preprocessing and fitting serially with protected class property
        rmse, rrmse = self.__compute(training_data)

        # Return matrix to and from latent projection
        return self.nmf_instance.Projection_BaseComponent.H, self.nmf_instance.Reconstruction_BaseComponent.H
