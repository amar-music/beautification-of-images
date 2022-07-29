import os
import numpy as np
from PIL import Image, ImageFilter, ImageStat, ImageColor
import pandas as pd

RUN = True

#### Functions
# Edge detection scoring
def edge_score(img):
    image = Image.open(img)
    image = image.convert("L")
    edges = image.filter(ImageFilter.FIND_EDGES) # 3x3 Laplacian filter
    return np.mean(edges)

# Color distribution scoring
#def hue_score(img):
#    image = Image.open(img)
#    hsv_image = image.convert("HSV")
#    hsv = np.array(hsv_image)
#    hsv = hsv[:, :, 1]

# Saturation score
def sat_score(img):
    image = Image.open(img)
    hsv_image = image.convert("HSV")
    hsv = np.array(hsv_image)
    saturations = hsv[:, :, 1]
    return np.mean(saturations)



# Blur scoring
#def blur_score(img):

## Low-level features
# Contrast scoring
def contrast_score(img):
    image = Image.open(img)
    image = image.convert("L")
    stats = ImageStat.Stat(image)
    return stats.stddev[0]


# Brightness scoring
def bright_score(img):
    image = Image.open(img)
    image = image.convert("L")
    stats = ImageStat.Stat(image)
    return stats.mean[0]


# Remove first n characters from string
def remove_chars(a_string, number_to_remove):
    return a_string[number_to_remove:]


# Load all images in folder
path = "../../jsPsych_ImageRating/stimuli/"
images = list(os.listdir(path))


# Create rows for dataframe
rows = []


# Edge scores for all images
if RUN:

    # Loop over all images and extract features
    for i in images:

        # Store image category and id
        img_str = i.split("_")
        cat = remove_chars(img_str[0], 3)
        img = remove_chars(img_str[2][:-4], 3)

        # Calculate all score
        edge = edge_score(path + i)
        contrast = contrast_score(path + i)
        brightness = bright_score(path + i)
        saturation = sat_score(path + i)

        # Calculate
        rows.append([cat, img, edge, contrast, brightness, saturation])
        print("cat_" + str(cat) + " img_" + str(img) +
              " Edge: " + str(edge) +
              " Contrast: " + str(contrast) +
              " Brightness: " + str(brightness) +
              " Saturation: " + str(saturation))

    # Export dataframe
    df = pd.DataFrame(rows, columns=["cat", "img", "edge_score", "contrast_score", "brightness_score", "saturation_score"])
    df.to_csv("../../Data Analysis/extraction/output.csv", index=False)

else:
    print("Please enable the run variable")
