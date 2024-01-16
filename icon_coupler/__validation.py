import os
import numpy as np
import numexpr as ne
import time

# Define some parameter
tracer = np.random.lognormal(1e-4, 1e-3, (120, 144000))
model_wait_time = 0.001
time_step = 100
s_com = 1.004


# Simple model (wait for 0.0001, then return original array)
class validation_model():
    def __init__(self):
        self.test = 0

    def compression(self, tracer):
        time.sleep(model_wait_time)
        return tracer

    def decompression(self, tracer):
        time.sleep(model_wait_time)
        return tracer


def compression_performance():
    def DimensionalityReduction():
        # Fetch Tracer as Numpy Array
        tracer_ADVECTION_BEFORE = np.asarray(tracer)
        tracer_sum_source = np.sum(tracer_ADVECTION_BEFORE)
        # Compress using model
        tracer_ADVECTION_BEFORE = model.compression(tracer_ADVECTION_BEFORE)
        tracer_sum_latent = np.sum(tracer_ADVECTION_BEFORE)
        # Mass Conservation (Using Numexpr as it provides faster elementwise array evaluation)
        s_com = tracer_sum_source / tracer_sum_latent
        # Evaluate Mass Conservation constrain
        tracer_MC_ADVECTION_BEFORE = ne.evaluate("s_com*tracer_ADVECTION_BEFORE")
        # return compressed array to tracer pointer
        tracer_ADVECTION_BEFORE = tracer_MC_ADVECTION_BEFORE

    model = validation_model()

    start_time = time.time()
    for t in range(time_step):
        DimensionalityReduction()
    print("--- NUMEXPR %s seconds ---" % (time.time() - start_time))


def sum_operator():
    global ne_time_elapse, np_time_elapse
    start_time = time.time()
    for t in range(time_step):
        tracer_sum_source = np.sum(tracer)
    np_time_elapse = time.time() - start_time
    print("--- NUMPY SUM %s seconds ---" % (np_time_elapse))
    del tracer_sum_source
    start_time = time.time()
    for t in range(time_step):
        tracer_sum_source = ne.evaluate("sum(tracer)")
    ne_time_elapse = time.time() - start_time
    print("--- NUMEXPR SUM %s seconds ---" % (ne_time_elapse))

    print("NUMEXPR evaluation takes %s percent more time than numpy.sum method on a %s array" % (
    100 * (ne_time_elapse - np_time_elapse) / np_time_elapse, tracer.shape))

def multiply_operator():
    global ne_time_elapse, np_time_elapse
    start_time = time.time()
    for t in range(time_step):
        tracer_sum_source = s_com * tracer
    np_time_elapse = time.time() - start_time
    print("--- NUMPY Multiply %s seconds ---" % (np_time_elapse))
    del tracer_sum_source
    start_time = time.time()
    for t in range(time_step):
        tracer_sum_source = ne.evaluate("s_com*tracer")
    ne_time_elapse = time.time() - start_time
    print("--- NUMEXPR Multiply %s seconds ---" % (ne_time_elapse))

    print(
        "NUMEXPR evaluation takes %s percent more time than s_com * tracer method on a %s array" % (
        100 * (ne_time_elapse - np_time_elapse) / np_time_elapse, tracer.shape))


if __name__ == "__main__":
    sum_operator()
    multiply_operator()