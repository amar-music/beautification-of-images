import os
import numpy as np
from PIL import Image, ImageFilter, ImageStat, ImageColor
import pandas as pd
import re


def col_dist(img):
    image = Image.open(img)
    rgb_image = image.convert("RGB")
    rgb = np.array(rgb_image)
    red = rgb[:, :, 0]
    green = rgb[:, :, 1]
    blue = rgb[:, :, 2]
    bands = [red, green, blue]
    image_distribution = []
    for i in bands:
        image_distribution.append(i.reshape(-1))
    return image_distribution


# Natural sorting
_nsre = re.compile("([0-9]+)")
def natural_sort_key(s):
    return [int(text) if text.isdigit() else text.lower()
            for text in re.split(_nsre, s)]



# Load all images in folder
path = "../../jsPsych_ImageRating/stimuli/"
images = list(os.listdir(path))


# Select category
cat_no = 100
cat = list(filter(lambda x: "cat" + str(cat_no) + "_" in x, images))
cat.sort(key=natural_sort_key)


alphas = [-0.25, -0.2, -0.15, -0.1, -0.05, -0.015, -0.0025, 0, 0.0025, 0.015, 0.05, 0.1, 0.15, 0.2, 0.25]
iter = 0

for i in cat:
    current_img = i
    alpha = alphas[iter]
    iter += 1
    image = col_dist(path + current_img)
    col_dict = {"red": image[0], "green": image[1], "blue": image[2], "cat":cat_no, "alpha":alpha}
    pd.DataFrame(col_dict).to_csv("../extraction/color_distributions/colors_cat" + str(cat_no) + "_" + str(current_img[13:-4]) + ".csv", index=False)
