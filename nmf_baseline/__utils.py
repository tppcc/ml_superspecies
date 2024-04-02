import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt
import os
import datetime

r"""
Some utilities to support the main functions
"""

def RootMeanSquare(self, y, y_predict):
    r"""Calculates the Root Mean Square Error
    Args:
        y (ndarray): Array of the True state
        y_predict (ndarray); Array of the Predicted state
    Return:
        rmse (float): computed RMSE for the current array
    """

    assert y.shape == y_predict.shape, "The input y and y_predict is not the same"
    return np.sum(np.sqrt((1 / y.size) * np.sum(np.square(y_predict - y))))

def RelativeRootMeanSquare(self, y, y_predict, rmse=None):
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
