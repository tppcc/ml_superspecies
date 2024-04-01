from nmf_baseline import *
from autoencoder import *
import os
import sys
import numpy as np
import comin.plugins.python_adapter.comin as comin

comin.var_request_add(("test_species", -1), False)
comin.metadata_set(("test_species", -1), tracer=True, tracer_turb=True, tracer_conv=True)
msgrank = 0
def message(message_string, rank):
    """Short helper function to print a message on one PE"""
    if (comin.parallel_get_host_mpi_rank() == rank):
        print(f"ComIn point_source.py: {message_string}", file=sys.stderr)

@comin.register_callback(comin.EP_SECONDARY_CONSTRUCTOR)
def SC_initialisation():
    global tracer, test_species
    EP = [comin.EP_ATM_ADVECTION_BEFORE, comin.EP_ATM_ADVECTION_AFTER, comin.EP_FINISH]
    test_species = comin.var_get(EP, ("test_species", comin.DEFAULT_DOMAIN_ID))
    tracer = comin.var_get(EP, ("tracer", comin.DEFAULT_DOMAIN_ID))


@comin.register_callback(comin.EP_ATM_ADVECTION_BEFORE)
def DimensionalityReduction():
    message(comin.metadata_get("tracer"), msgrank)
    #Tracer as Numpy Array
    tracer_ADVECTION_BEFORE = np.asarray(tracer)
    message(tracer_ADVECTION_BEFORE.shape, msgrank)
    f = open('/work/b/bb1070/b382290/ml_superspecies_icon_idealised/ep_advection_before.txt')
    f.write(tracer_ADVECTION_BEFORE.shape)
    np.asarray(test_species)[:] = 5



@comin.register_callback(comin.EP_ATM_ADVECTION_AFTER)
def DimensionalityReconstruction():
    np.asarray(test_species)[:] = 10

@comin.register_callback(comin.EP_FINISH)
def finish_output():
    sp = np.asarray(test_species)
    tc =  np.asarray(tracer)
    np.save('/work/b/bb1070/b382290/ml_superspecies_icon_idealised/test_species.npy', sp)
    np.save('/work/b/bb1070/b382290/ml_superspecies_icon_idealised/tracer_finish.npy', tc)

@comin.register_callback(comin.EP_DESTRUCTOR)
def pntsrc_destructor():
    message("pntsrc_destructor called!", msgrank)