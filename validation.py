import icon_coupler
from nmf_baseline import *
import torch
import torchnmf
import numpy as np
import xarray as xr
import os
from icon_coupler import *

# Define Random matrix
V = torch.rand(20, 1000, 1000)  # A 20 species, flattened 1000 space and 1000 time tensor

# Training example
instance = NMF(rank=5)  # 1:4 compression ratio
# Training start
instance.trainer(V)

# Operational
W_path = "w_trained.pth"
H_path = "h_trained.pth"
instance = NMF_operational(W_path, H_path, trainable_W=False)

for i in range(1000):
    #Each loop corresponds to one slow physics timestep
    art_coupler = art_input()
    V = art_coupler.input_compression
    H_compressed = instance.compression(V, beta=1, tol=0.0001, max_iter=200, verbose=False, alpha=0, l1_ratio=0)
    H_compressed_advected = art_coupler.advection(H_compressed)
    V_reconstructed = instance.reconstruction(H_compressed_advected)
    art_coupler.input_decompression = V_reconstructed