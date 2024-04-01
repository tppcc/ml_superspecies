import numpy as np
import os

class utils:
    # Utilities for
    def __init__(self):
        self.pwd = os.getcwd()

    def __rmse(self, y, y_predict):
        r"""Calculates the Root Mean Square Error
        Args:
            y (ndarray): Array of the True state
            y_predict (ndarray); Array of the Predicted state
        """

        assert y.shape == y_predict.shape, "The input y and y_predict is not the same"
        return np.sqrt((1 / y.size) * np.sum(np.square(y_predict - y)))