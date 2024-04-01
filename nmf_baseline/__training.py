ptf = '/work/bb1070/b382290/ml_superspecies_icon_idealised/icon_r02b07_dust/DUST_NO_SOURCE_SINK/out/comin_output/advection_before/'

import numpy as np
import os
import torch
import nmf_baseline
import multiprocessing
from concurrent.futures import ThreadPoolExecutor
import time


####################################################
# Wrapper of nmf_baseline
#
# This class gives a one-stop solution of training the projection matrix of NMF based on Slurm et al. (2023)
# and torchnmf module. This module is multi-treading enabled, such that the efficiency largely depends on
# the number of available threads. Note that memory constrain is often reached before CPU bottle-neck is
# reached.

class NonNegTrainer:
    def __init__(self, n_size, m_size, rank, n_process=1):
        # initialisation:
        #                       n_size (int): Size of input channels
        #                       m_size (int): Size of each batch of input
        #                       rank   (int): Size of channel in latent space
        #                       n_process      (int): (default: 1) number of parallel processes to be initiated for training, performance critical,
        #                                              keep in mind of the memory restriction of the host system

        # Assert type of input values
        assert ((type(n_size) == int) & (type(m_size) == int) & (
                    type(rank) == int)), "dtype of (n_size, m_size, rank) must be int"
        self.__n_size = n_size
        self.__m_size = m_size
        self.__rank = rank
        self.__n_process = n_process

        # Initialise class instance from nmf_baseline
        self.nmf_instance = nmf_baseline.NMF(self.__n_size, self.__m_size, rank=self.__rank)

    def __stack(self, input_array):
        # Flatten each input species into 1-D ndarray(q,)
        #                       input_array (ndarray): Numpy Array of each species (jc, jk, jc) when coupled to ICON (ComIn)
        # Return value:
        #                       (ndarray(q,)): Ravelled 1-D array

        # Perform data check, input must be numpy array
        assert type(input_array) == np.ndarray, "Input must be a numpy array"
        return input_array.ravel()

    def __species_preprocessing(self, data):
        # Preprocess each data:
        #                       data (list): list of species array, dimension must be the same
        # Return value:
        #                       data (ndarray(n_size, q)): pre-processed 2-D ndarray ready for training

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
        # Perform batching to input array if the size of dim-1 of da is larger than m_size
        #                       data (ndarray(n_size, q)): 2-D ndarray ready for training
        # Return value:
        #                       (list(ndarray(n_size, m_size))): list of 2-D ndarray matching the dimension of n_size, m_size
        #                                                        while initiaising this class instance

        assert data.shape[
                   1] >= self.__m_size, "dimension 1 of the flattened array has a smaller size than the selected m_size in this class instance"

        if data.shape[1] != self.__m_size:
            self.batching_flag = True
            batch_number = data.shape[1] // self.__m_size
            return np.array_split(data, batch_number, axis=1)

        else:
            # Return a one element list containing the data
            return [data]

    """

        def __training(data_subset):
            st = time.time()
            for k in range(data_subset.shape[1] // 10000):
                nmf_instance = nmf_baseline.NMF(6, 10000,
                                                rank=2)  # nmf_baseline.NMF(6, 10000, rank=2)
                nmf_instance.fit(torch.tensor(data_subset[:, 0 + (10000 * k): 10000 + (10000 * k)]),
                                 storage=False)
            print("complete at thread, time elapsed: %s s" % (time.time() - st))
            return nmf_instance.B, nmf_instance.H


    """

    def __training(self, x_train):
        # Training backend of the class self.__preprocess_and_fit
        #                       x_train (ndarray(n_size, m_size)): training data for each thread for further processing

        nmf_instance = nmf_baseline.NMF(self.__n_size, self.__m_size, rank=self.__rank)
        nmf_instance.fit(torch.tensor(x_train), **self.hyperparameters)

        return nmf_instance.B, nmf_instance.H

    def __preprocess_and_fit(self, training_data):
        # Backend for self.fit() This function preprocess the training_data (list(ndarray)),
        # perform the necessary consistency check and ravel into ndarray(n_size, q). Then
        # batching is performed such that the training data is split along axis-1 with ( q //
        # m_size).
        # Training is performed using multiprocessing in Python
        #                       training_data (list(ndarray)): list of species array, length of training_data must be the same as n_size

        data = self.__species_preprocessing(training_data)
        data = self.__batching(data)

        # timer for training
        start_time = time.time()

        with multiprocessing.Pool(processes=self.__n_process) as pool:  # num_processes
            # Perform NMF training on each chunk of data
            results = pool.map(self.__training, data)

        trained_B_matrices, trained_H_matrices = zip(*results)

        print(
            'Parallelised training for the current step completed, n_processes=%s, time_elapsed=%s' % (
                num_processes, (time.time() - start_time)))

        # Collect results from multiprocessing

    def fit(self, training_data, beta=1, tol=0.0001, max_iter=200, verbose=False, alpha=0,
            l1_ratio=0, storage=True, intermediate_stop=500):
        # processing the input data and fit to train nmf_baseline.NMF object
        #                       training_data (list(ndarray)): list of species array, dimension must be the same
        #                       beta and beyond: see torchnmf.nmf.NMF documentation, these arguments are passed directly to torchnmnf backend

        # Pass hyperparameters as dictionary
        self.hyperparameters = {'beta': beta,
                                'tol': tol,
                                'max_iter': max_iter,
                                'verbose': verbose,
                                'alpha': alpha,
                                'l1_ratio': l1_ratio,
                                'storage': storage,
                                'intermediate_stop': intermediate_stop}

        self.__preprocess_and_fit(training_data)

        return True





# Function to load array and hstack from file
def load_and_stack(file_paths):
    arrays = [np.load(file_path).squeeze().ravel() for file_path in file_paths]
    return np.hstack(arrays)


# Function to perform NMF training on a subset of data
def train_nmf(data_subset):
    st = time.time()
    for k in range(data_subset.shape[1] // 10000):
        nmf_instance = nmf_baseline.NMF(6, 10000, rank=2)  # nmf_baseline.NMF(6, 10000, rank=2)
        nmf_instance.fit(torch.tensor(data_subset[:, 0 + (10000 * k): 10000 + (10000 * k)]),
                         storage=False)
    print("complete at thread, time elapsed: %s s" % (time.time() - st))
    return nmf_instance.B, nmf_instance.H


def run(fnames_time_dusta, fnames_time_dustb, fnames_time_dustc, fnames_time_numb_dusta,
        fnames_time_numb_dustb, fnames_time_numb_dustc):
    start_time = time.time()

    # Set the number of processes to the number of CPU cores
    num_processes = multiprocessing.cpu_count()

    with ThreadPoolExecutor(max_workers=num_processes) as executor:
        # Load and stack data concurrently
        future_dusta = executor.submit(load_and_stack,
                                       [os.path.join(ptf, fname) for fname in fnames_time_dusta])
        future_dustb = executor.submit(load_and_stack,
                                       [os.path.join(ptf, fname) for fname in fnames_time_dustb])
        future_dustc = executor.submit(load_and_stack,
                                       [os.path.join(ptf, fname) for fname in fnames_time_dustc])
        future_numb_dusta = executor.submit(load_and_stack, [os.path.join(ptf, fname) for fname in
                                                             fnames_time_numb_dusta])
        future_numb_dustb = executor.submit(load_and_stack, [os.path.join(ptf, fname) for fname in
                                                             fnames_time_numb_dustb])
        future_numb_dustc = executor.submit(load_and_stack, [os.path.join(ptf, fname) for fname in
                                                             fnames_time_numb_dustc])

        # Wait for all futures to complete and get the stacked data
        dusta = future_dusta.result()
        dustb = future_dustb.result()
        dustc = future_dustc.result()
        numb_dusta = future_numb_dusta.result()
        numb_dustb = future_numb_dustb.result()
        numb_dustc = future_numb_dustc.result()

    print('Distributive IO complete, num_processes=%s, time_elapsed=%s' % (
        num_processes, (time.time() - start_time)))

    # Combine the loaded data
    test_data = np.vstack([dusta, dustb, dustc, numb_dusta, numb_dustb, numb_dustc])

    print("stacking complete")

    # Split the combined data into chunks for parallel training
    thread_data = np.array_split(test_data, num_processes, axis=1)

    start_time = time.time()

    # Perform parallel training using multiprocessing
    with multiprocessing.Pool(processes=10) as pool:  # num_processes
        # Perform NMF training on each chunk of data
        results = pool.map(train_nmf, thread_data)

    print('Distributive computing complete, num_processes=%s, time_elapsed=%s' % (
        num_processes, (time.time() - start_time)))

    # Collect results from multiprocessing
    trained_B_matrices, trained_H_matrices = zip(*results)

    trained_B_arrays = np.array([x.detach().numpy() for x in list(trained_B_matrices)])
    trained_H_arrays = np.array([x.detach().numpy() for x in list(trained_H_matrices)])

    mean_B = np.mean(trained_B_arrays, axis=0)
    mean_H = np.mean(trained_H_arrays, axis=0)

    return mean_B, mean_H


# Set the number of processes to the number of CPU cores
num_processes = multiprocessing.cpu_count()

fnames = os.listdir(ptf)
fnames.sort()
fnames_dusta = [x for x in fnames if ('.npy' in x) and ('dusta' in x) and ('numb' not in x)]
fnames_dustb = [x for x in fnames if ('.npy' in x) and ('dustb' in x) and ('numb' not in x)]
fnames_dustc = [x for x in fnames if ('.npy' in x) and ('dustc' in x) and ('numb' not in x)]
fnames_numb_dusta = [x for x in fnames if ('.npy' in x) and ('numb_dusta' in x)]
fnames_numb_dustb = [x for x in fnames if ('.npy' in x) and ('numb_dustb' in x)]
fnames_numb_dustc = [x for x in fnames if ('.npy' in x) and ('numb_dustc' in x)]

print('fnames initialised')

# Parallelised array loading

# nmf_baseline traning

test_nmf = nmf_baseline.NMF(6, 10000, rank=2)

fnames_time_dusta = []
fnames_time_dustb = []
fnames_time_dustc = []
fnames_time_numb_dusta = []
fnames_time_numb_dustb = []
fnames_time_numb_dustc = []

B = []
W = []

for i in np.arange(20, 50, 1):
    training_data = []

    fnames_time_dusta = []
    fnames_time_dustb = []
    fnames_time_dustc = []
    fnames_time_numb_dusta = []
    fnames_time_numb_dustb = []
    fnames_time_numb_dustc = []
    # List of fnames matching the desired variable name and time step
    fnames_time_dusta = fnames_time_dusta + [x for x in fnames_dusta if '_%s_' % (i) in x]
    fnames_time_dustb = fnames_time_dustb + [x for x in fnames_dustb if '_%s_' % (i) in x]
    fnames_time_dustc = fnames_time_dustc + [x for x in fnames_dustc if '_%s_' % (i) in x]
    fnames_time_numb_dusta = fnames_time_numb_dusta + [x for x in fnames_numb_dusta if
                                                       '_%s_' % (i) in x]
    fnames_time_numb_dustb = fnames_time_numb_dustb + [x for x in fnames_numb_dustb if
                                                       '_%s_' % (i) in x]
    fnames_time_numb_dustc = fnames_time_numb_dustc + [x for x in fnames_numb_dustc if
                                                       '_%s_' % (i) in x]

    mean_B, mean_H = run(fnames_time_dusta, fnames_time_dustb, fnames_time_dustc,
                         fnames_time_numb_dusta, fnames_time_numb_dustb, fnames_time_numb_dustc)

    print(mean_B.shape)
    print(mean_H.shape)

    B.append(mean_B)
    W.append(mean_H)

print('attachment of list complete')

B = np.mean(np.dstack(B), axis=2)
W = np.mean(np.dstack(W), axis=2)

print(B.shape)
print(W.shape)

np.save('final_B.npy', B)
np.save('final_W.npy', W)
