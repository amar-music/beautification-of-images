from tensorflow.keras.models import Model
import assessors.aestheticsnet
from assessors.aestheticsnet import AestheticsNet
from PIL import Image
import pickle
#import matplotlib.pyplot as plt
import numpy as np
import os
import tensorflow as tf
import assessors

import inspect


# Load test image
image_path = 'output/biggan__biggan256/aestheticsnet/OneDirection-None/fd76206/' \
             'jsPsych-version_alpha_0.0025_truncation_1.0_iteration_400000__iterative/'\
             'cat0_seed0_img31.jpg'
test_image = Image.open(image_path)

# Load AestheticsNet as base_model
base_model = AestheticsNet()
#base_model = getattr(assessors, "aestheticsnet")

for layer in base_model.aestheticsnet_fn():
    print(layer)

# Load AestheticsNet weights
#base_model.get_weights(weights_path='assessors/aestheticsnet_state_dict.p')
#base_model.get_mu(image_path)
#base_model.aestheticsnet_preprocess(test_image)
#base_model.channels_first(test_image)
#base_model.aestheticsnet_fn(test_image)

#base_model.get_mu(image_path)
#base_model.channels_first(test_image)
#base_model.aestheticsnet_preprocess()




#activation_model = Model(inputs=base_model.get_mu, outputs=base_model.aestheticsnet_fn())

