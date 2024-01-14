from nmf_baseline import *
from autoencoder import *
import os
import numpy as np
import numexpr as ne
import comin.plugins.python_adapter.comin as comin

@comin.register_callback(comin.EP_SECONDARY_CONSTRUCTOR)
def SC_initialisation():
    global nmf, model
    nmf = NMF_operational('W.pth', 'H.pth')
    autoencoder = compute('model')
    model = nmf

def TracerConstructor():
    global tracer
    EP = [comin.EP_ATM_ADVECTION_BEFORE, comin.EP_ATM_ADVECTION_AFTER]
    tracer = comin.var_get(EP, ("tracer", comin.DEFAULT_DOMAIN_ID))

class validation_model():
    def __init__(self):
        self.test = 0
    def compression(self, tracer):
        import time
        time.sleep(0.1)
        return tracer
    def decompression(self, tracer):
        import time
        time.sleep(0.1)
        return tracer

tracer = np.random.randn(100, 5000)
model = validation_model()

@comin.register_callback(comin.EP_ATM_ADVECTION_BEFORE)
    # Required: tracer (array, real), model (class, require [compression, decompression] method)
    # Return: tracer_ADVECTION_BEFORE (array, pointer)
def DimensionalityReduction():
    # Fetch Tracer as Numpy Array
    tracer_ADVECTION_BEFORE = np.asarray(tracer)
    tracer_sum_source = np.sum(tracer_ADVECTION_BEFORE)
    # Compress using model
    tracer_ADVECTION_BEFORE = model.compression(tracer_ADVECTION_BEFORE)
    tracer_sum_latent = np.sum(tracer_ADVECTION_BEFORE)
    # Mass Conservation (Using Numexpr as it provides faster elementwise array evaluation)
    s_com =  tracer_sum_source / tracer_sum_latent
    # Evaluate Mass Conservation constrain
    tracer_MC_ADVECTION_BEFORE = ne.evaluate("s_com*tracer_ADVECTION_BEFORE")
    # return compressed array to tracer pointer
    tracer_ADVECTION_BEFORE = tracer_MC_ADVECTION_BEFORE


@comin.register_callback(comin.EP_ATM_ADVECTION_AFTER)
def DimensionalityReconstruction():
    tracer_ADVECTION_AFTER = np.asarray(tracer)
    tracer_sum_latent = np.sum(tracer_ADVECTION_AFTER)
    # Compress using model
    tracer_ADVECTION_AFTER = model.decompression(tracer_ADVECTION_AFTER)
    tracer_sum_source = np.sum(tracer_ADVECTION_AFTER)
    # Mass Conservation (Using Numexpr as it provides faster elementwise array evaluation)
    s_com =  tracer_sum_latent / tracer_sum_source
    # Evaluate Mass Conservation constrain
    tracer_MC_ADVECTION_AFTER = ne.evaluate("s_com*tracer_ADVECTION_AFTER")
    # return decompressed array to tracer pointer
    tracer_ADVECTION_AFTER = tracer_MC_ADVECTION_AFTER