import os
import numpy as np
from PIL import Image, ImageFilter
import pandas as pd

#### Functions
# Edge detection scoring
def edge_score(img):
    image = Image.open(img)
    image = image.convert("L")
    edges = image.filter(ImageFilter.FIND_EDGES)
    return np.mean(edges)

# Remove first n characters from string
def remove_chars(a_string, number_to_remove):
    return a_string[number_to_remove:]


# Load all images in folder
path = "../../jsPsych_ImageRating/stimuli/"
images = list(os.listdir(path))


# Create rows for dataframe
rows = []


# Edge scores for all images
for image in images:
    img_str = image.split("_")
    cat = remove_chars(img_str[0], 3)
    img = remove_chars(img_str[2], 3)
    score = edge_score(path + image)

    print("cat_" + str(cat) + " img_" + str(img) + " Score: " + str(score))


#### Convert to grayscale
#img = img.convert("L")


#### Edge detection
#img_edg = img.filter(ImageFilter.FIND_EDGES)
#img_edg.show()

#edges = np.array(img_edg)
#print(edges)
#print(np.mean(edges))
#Image.show(img_edg)