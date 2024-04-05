import datetime
from dust_board import *
import os
import json

# Initialise Environmental Dictionary to be passed to the model_load function
environmental_dict = EnvParameters()

# Fetch nwp variable that exists in all models
for model in environmental_dict.model_url:
    model_load(model, environmental_dict)

