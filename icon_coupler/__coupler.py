from nmf_baseline import *
import os


instance = NMF_operational(W_path, H_path, trainable_W=False)

art_coupler = art_input()
V = art_coupler.input_compression
H_compressed = instance.compression(V, beta=1, tol=0.0001, max_iter=200, verbose=False, alpha=0,
                                    l1_ratio=0)
H_compressed_advected = art_coupler.advection(H_compressed)
V_reconstructed = instance.reconstruction(H_compressed_advected)
art_coupler.input_decompression = V_reconstructed