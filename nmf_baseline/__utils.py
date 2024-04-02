import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt
import os
import datetime
import torch
from concurrent.futures import ThreadPoolExecutor

r"""
Some utilities to support the main functions
"""

def RootMeanSquare(y, y_predict):
    r"""Calculates the Root Mean Square Error
    Args:
        y (ndarray): Array of the True state
        y_predict (ndarray); Array of the Predicted state
    Return:
        rmse (float): computed RMSE for the current array
    """

    assert y.shape == y_predict.shape, "The input y and y_predict is not the same"
    return np.sum(np.sqrt((1 / y.size) * np.sum(np.square(y_predict - y))))

def RelativeRootMeanSquare(y, y_predict, rmse=None):
    r"""Calculates the Relative (Normalised) Root Mean Square Error
    Args:
        y (ndarray): Array of the True state
        y_predict (ndarray): Array of the Predicted state
        rmse (float, optional): computed RMSE, if this is provided, will skip the full computation
                                and fall back to rmse / np.sum(y) to save computation resources
    Return:
        rrmse (float): computed Relative RMSE for the current array
    """

    assert y.shape == y_predict.shape, "The input y and y_predict is not the same"
    if rmse != None:
        assert type(rmse) == float, "incorrect data type for rmse (float)"
        return rmse / np.sum(y)
    elif rmse == None:
        return np.sum(np.sqrt((1 / y.size) * np.sum(np.square(y_predict - y))) / np.sum(y))
    else:
        raise Exception("incorrect data type for rmse (float)")

def ErrorPlot(rmse, rrmse, output_dir):
    r"""Plots RMSE and RRMSE for error estimation.

    Args:
        rmse (ndarray): Array of root mean square error.
        rrmse (ndarray): Array of relative root mean square error.

    Returns:
        None
    """

    fig, axs = plt.subplots(1, 2, figsize=(8, 4))
    axs[0].plot(rmse, color='red', label='RMSE')
    axs[1].plot(rrmse, color='red', label='RRMSE')
    axs[0].set_title("RMSE of training")
    axs[0].set_title("Relative RMSE of training")

    t_now = datetime.now()
    plot_out = os.path.join(output_dir, 'training_error_%s.jpg') %(t_now.strftime("%y%m%d%H%M%S"))
    plt.savefig(plot_out)

    print("Error estimation plot saved at %s" %(plot_out))

def __np_loading(fname):
    r"""Routine to read NP arrays, used only for parallelization
    Args:
        fname (str): Absolute or relative path to the file to be read by numpy.load routine
    Return:
        da (np.darray): read np.darrau
    """

    return np.load(fname)

def MultipleFileLoad(self, fpaths, n_processes):
    r"""Routine to read multiple files in parallelized manner. This script utilises the concurrent package of Python to parallel
    Args:
        fpaths (list): Sorted list of absolute or relative path to the files for reading
    Returns:
        data (list): list of np.darrays returned from loading the arrays
    """

    n_processes = n_processes
    with ThreadPoolExecutor(max_workers=n_processes) as executor:
        futures_execution = executor.submit(__np_loading, [fname for fname in fpaths])

        dusta = futures_execution.result()

def SaveModel(self, B, W, output_dir):
    r"""
    Saves the trained model weights.

    Prints a message indicating completion and saves the model weights to files. File saved is named
    trained_W_%Y%m%d_%H%M%S.pth (Tensor) & trained_B%Y%m%d_%H%M%S.pth (Tensor)
    Args:
        B (Tensor): Projection matrix for output
        W (Tensor): Reconstruction matrix for output
        output_dir (str): Directory of output
    Returns:
        None
    """
    with torch.no_grad():
        dt = datetime.now().strftime("%Y%m%d_%H%M%S")  # Getting current date and time
        # Saving Projection_BaseComponent.H and B to files
        torch.save(W, os.path.join(output_dir, "trained_W_%s.pth" % (dt)))
        torch.save(B, os.path.join(output_dir, "trained_B_%s.pth" % (dt)))
        # Printing the directory where weights are saved
        print("Trained weight saved at %s" % (output_dir))