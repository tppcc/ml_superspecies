import copy

import torchnmf as nmf
from .__utils import *
import multiprocessing
import torch
import time


# trainable_W: when True, W matrix will be learned when operational

class NonNegOperational:
    def __init__(self, B, W, m_size, n_processes=1):
        r"""Class object for operatinoal NMF coupled to ICON-Model
        Args:
            B (Tensor): Trained projection matrix for the given training data
            W (Tensor): trained reconstruction matrix for the given training data
            m_size (int): Number of sample to be processed at once
        """
        self.B = B
        self.W = W
        self.__n_process = n_processes
        self.__m_size = m_size

        self.Projection_BaseComponent = nmf.nmf.NMF(H=self.B, W=torch.tensor(
            np.ones([m_size, self.B.shape[1]])), trainable_H=False)
        self.Reconstruction_BaseComponent = nmf.nmf.NMF(H=self.W, W=torch.tensor(
            np.ones([m_size, self.W.shape[1]])), trainable_H=False)

        # Initialise Projection and Reconstruction matrix

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
            dump_column = data.shape[1] % self.__m_size
            if dump_column != 0:
                print(
                    "The array size is not divisible by selected m_size, will dump some elements in amount of the remainder in dimension-1")
            return np.array_split(data[:, :-dump_column], batch_number, axis=1)

        else:
            # Return a one element list containing the data
            return [data]

    def __species_preprocessing(self, data):
        r"""Preprocess each data
        Args:
            data (list): list of species array, dimension must be the same
        Return:
            data (ndarray(n_size, q)): pre-processed 2-D ndarray ready for training
        """

        # Preprocessing: flatten each array and concat as a ndarray with dimnension (n_size, q) [q: size of sample space]
        with multiprocessing.Pool(processes=self.__n_process) as pool:
            data = pool.map(Stack, data)

        data = np.vstack(data)

        return data

    def testing(self, V, beta=1, tol=0.0001, max_iter=200, verbose=False, alpha=0, l1_ratio=0):
        V = self.__species_preprocessing(V)
        V = self.__batching(V)
        V1 = [(self.B, self.W, x) for x in V]
        #V2 = [(x, self.Projection_BaseComponent, self.Reconstruction_BaseComponent) for x in V]
        import copy

        import torchnmf as nmf
        from .__utils import *
        import multiprocessing
        import torch
        import time

        # trainable_W: when True, W matrix will be learned when operational

        class NonNegOperational:
            def __init__(self, B, W, m_size, n_processes=1):
                r"""Class object for operatinoal NMF coupled to ICON-Model
                Args:
                    B (Tensor): Trained projection matrix for the given training data
                    W (Tensor): trained reconstruction matrix for the given training data
                    m_size (int): Number of sample to be processed at once
                """
                self.B = B
                self.W = W
                self.__n_process = n_processes
                self.__m_size = m_size

                self.Projection_BaseComponent = nmf.nmf.NMF(H=self.B, W=torch.tensor(
                    np.ones([m_size, self.B.shape[1]])), trainable_H=False)
                self.Reconstruction_BaseComponent = nmf.nmf.NMF(H=self.W, W=torch.tensor(
                    np.ones([m_size, self.W.shape[1]])), trainable_H=False)

                # Initialise Projection and Reconstruction matrix

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
                    dump_column = data.shape[1] % self.__m_size
                    if dump_column != 0:
                        print(
                            "The array size is not divisible by selected m_size, will dump some elements in amount of the remainder in dimension-1")
                    return np.array_split(data[:, :-dump_column], batch_number, axis=1)

                else:
                    # Return a one element list containing the data
                    return [data]

            def __species_preprocessing(self, data):
                r"""Preprocess each data
                Args:
                    data (list): list of species array, dimension must be the same
                Return:
                    data (ndarray(n_size, q)): pre-processed 2-D ndarray ready for training
                """

                # Preprocessing: flatten each array and concat as a ndarray with dimnension (n_size, q) [q: size of sample space]
                with multiprocessing.Pool(processes=self.__n_process) as pool:
                    data = pool.map(Stack, data)

                data = np.vstack(data)

                return data

            def testing(self, V, beta=1, tol=0.0001, max_iter=200, verbose=False, alpha=0,
                        l1_ratio=0):
                V = self.__species_preprocessing(V)
                V = self.__batching(V)
                V1 = [(self.B, self.W, x) for x in V]
                # V2 = [(x, self.Projection_BaseComponent, self.Reconstruction_BaseComponent) for x in V]

                with multiprocessing.Pool(processes=self.__n_process) as pool:
                    # rmse_nmf, rrmse_nmf = pool.map(benchmarking_nmf, V2)
                    results = pool.map(benchmarking, V1)

                    rmse_matmul = [x[0] for x in results]
                    rrmse_matmul = [x[1] for x in results]

                return rmse_matmul, rrmse_matmul

        def benchmarking(input_data):
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

            start_time = time.time()

            B, W, x_train = input_data

            B = B.detach().numpy()
            W = W.detach().numpy()

            x_predict = W @ (B @ x_train)
            rmse = RootMeanSquare(x_train, x_predict)
            rrmse = RelativeRootMeanSquare(x_train, x_predict, rmse)

            print(
                ' Training for the current step completed, time_elapsed=%s \n Benchmark: \n RMSE: %s, RRMSE: %s' % (
                    time.time() - start_time, rmse, rrmse))

            return rmse, rrmse

        def benchmarking_nmf(input_dict):
            start_time = time.time()

            V, Projection_BaseComponent, Reconstruction_BaseComponent = input_dict
            beta = 1
            tol = 0.0001
            max_iter = 200
            verbose = False
            alpha = 0
            l1_ratio = 0
            Projection_BaseComponent.fit(torch.tensor(V), beta=beta, tol=tol, max_iter=max_iter,
                                         verbose=verbose, alpha=alpha, l1_ratio=l1_ratio)
            compressed = Projection_BaseComponent.W
            Reconstruction_BaseComponent.fit(compressed, beta=beta, tol=tol, max_iter=max_iter,
                                             verbose=verbose, alpha=alpha, l1_ratio=l1_ratio)
            V_reconstructed = Reconstruction_BaseComponent.W

            rmse = RootMeanSquare(V, V_reconstructed)
            rrmse = RelativeRootMeanSquare(V, V_reconstructed, rmse)

            print(
                ' Training for the current step completed, time_elapsed=%s \n Benchmark: \n RMSE: %s, RRMSE: %s' % (
                    time.time() - start_time, rmse, rrmse))
            return rmse, rrmse
        with multiprocessing.Pool(processes=self.__n_process) as pool:
            # rmse_nmf, rrmse_nmf = pool.map(benchmarking_nmf, V2)
            results = pool.map(benchmarking, V1)

            rmse_matmul = [x[0] for x in results]
            rrmse_matmul = [x[1] for x in results]

        return rmse_matmul, rrmse_matmul


def benchmarking(input_data):
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

    start_time = time.time()

    B, W, x_train = input_data

    B = B.detach().numpy()
    W = W.detach().numpy()

    x_predict = W @ (B @ x_train)
    rmse = RootMeanSquare(x_train, x_predict)
    rrmse = RelativeRootMeanSquare(x_train, x_predict, rmse)

    print(
        ' Training for the current step completed, time_elapsed=%s \n Benchmark: \n RMSE: %s, RRMSE: %s' % (
        time.time() - start_time, rmse, rrmse))

    return rmse, rrmse


def benchmarking_nmf(input_dict):
    start_time = time.time()

    V, Projection_BaseComponent, Reconstruction_BaseComponent = input_dict
    beta = 1
    tol = 0.0001
    max_iter = 200
    verbose = False
    alpha = 0
    l1_ratio = 0
    Projection_BaseComponent.fit(torch.tensor(V), beta=beta, tol=tol, max_iter=max_iter,
                                 verbose=verbose, alpha=alpha, l1_ratio=l1_ratio)
    compressed = Projection_BaseComponent.W
    Reconstruction_BaseComponent.fit(compressed, beta=beta, tol=tol, max_iter=max_iter,
                                     verbose=verbose, alpha=alpha, l1_ratio=l1_ratio)
    V_reconstructed = Reconstruction_BaseComponent.W

    rmse = RootMeanSquare(V, V_reconstructed)
    rrmse = RelativeRootMeanSquare(V, V_reconstructed, rmse)

    print(
        ' Training for the current step completed, time_elapsed=%s \n Benchmark: \n RMSE: %s, RRMSE: %s' % (
        time.time() - start_time, rmse, rrmse))
    return rmse, rrmse