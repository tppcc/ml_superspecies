import os
import numpy as np
import numexpr as ne
import time

class validation_model():
    def __init__(self):
        self.test = 0
    def compression(self, tracer):
        import time
        time.sleep(0.0001)
        return tracer
    def decompression(self, tracer):
        import time
        time.sleep(0.0001)
        return tracer

tracer = np.random.randn(100, 5000)
model = validation_model()

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

time_step = 241920
start_time = time.time()
for time in range(time_step):
    DimensionalityReduction()
print("--- NUMEXPR %s seconds ---" % (time.time() - start_time - 0.0001 * time_step))