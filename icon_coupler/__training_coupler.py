from nmf_baseline import *
from autoencoder import *
import os
import numpy as np
import comin.plugins.python_adapter.comin as comin

@comin.register_callback(comin.EP_SECONDARY_CONSTRUCTOR)
def SC_initialisation():
    global nmf, autoencoder, model
    nmf = NMF(rank=40)
    autoencoder = train()
    model = nmf

def TracerConstructor():
    global tracer
    EP = [comin.EP_ATM_ADVECTION_BEFORE, comin.EP_ATM_ADVECTION_AFTER]
    tracer = comin.var_get([comin.EP_ATM_ADVECTION_BEFORE], ("tracer", comin.DEFAULT_DOMAIN_ID))

@comin.register_callback(comin.EP_ATM_ADVECTION_BEFORE)
def DimensionalityReduction():
    tracer_ADVECTION_BEFORE = np.asarray(tracer)
    tracer_ADVECTION_BEFORE = model.trainer(tracer_ADVECTION_BEFORE)


@comin.register_callback(comin.EP_ATM_ADVECTION_AFTER)
def DimensionalityReconstruction():
    tracer_ADVECVTION_AFTER = np.asarray(tracer)
    tracer_ADVECVTION_AFTER = model.reconstruction(tracer_ADVECVTION_AFTER)