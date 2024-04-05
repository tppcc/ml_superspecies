import datetime
from dust_board import *
import os
import json

# Initialise Environmental Dictionary to be passed to the model_load function
environmental_dict = EnvParameters()

############# Fully adaptive, only need to change EnvParameters in .__env_parameters.py

# Fetch nwp variable that exists in all models.
for model in environmental_dict.model_url:
    ModelLoad(model, environmental_dict)
    locals()['da_' + model] = DataProcessing(model, environmental_dict)

############# End of adaptive script

# Plotting Meteogram

