from nmf_baseline import *
from autoencoder import *
import os
import numpy as np
import comin.plugins.python_adapter.comin as comin

@comin.register_callback(comin.EP_SECONDARY_CONSTRUCTOR)
def SC_initialisation():
    global nmf, autoencoder, model, rank, tracer
    rank = 40
    nmf = NMF(rank=rank)
    autoencoder = train(rank, num_epochs = 50, lr=(0.01, 1e-3), batch_size=10)
    model = nmf
    EP = [comin.EP_ATM_ADVECTION_BEFORE, comin.EP_ATM_ADVECTION_AFTER]
    tracer = comin.var_get(EP, ("tracer", comin.DEFAULT_DOMAIN_ID))

@comin.register_callback(comin.EP_ATM_ADVECTION_BEFORE)
def DimensionalityReduction():
    #Tracer as Numpy Array
    tracer_ADVECTION_BEFORE = np.asarray(tracer)
    #Fit to model, obtain
    model.fit(tracer_ADVECTION_BEFORE)
    tracer_ADVECTION_BEFORE = model.W


@comin.register_callback(comin.EP_ATM_ADVECTION_AFTER)
def DimensionalityReconstruction():
    tracer_ADVECVTION_AFTER = np.asarray(tracer)
    tracer_ADVECVTION_AFTER = model.reconstruction(tracer_ADVECVTION_AFTER)