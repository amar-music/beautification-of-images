import os
import numpy as np
from PIL import Image, ImageFilter, ImageStat, ImageColor
import pandas as pd
import re


RUN = False

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

#os.chdir(path)

# Select subset of images depending on alpha value
img0 = list(filter(lambda x: "img0" in x, images))
img100 = list(filter(lambda x: "img100" in x, images))
img200 = list(filter(lambda x: "img200" in x, images))

cat_no = 100
cat = list(filter(lambda x: "cat" + str(cat_no) + "_" in x, images))
cat.sort(key=natural_sort_key)

#image_colors = []
all_red = []
all_green = []
all_blue = []

#colors = col_dist(images)

#images = []
#for image in cat1:
#    img = (path + image)
#    colors = col_dist(img)
#    images.append([colors, image[11:-4]])


#all_red = np.concatenate(all_red)
#all_green = np.concatenate(all_green)
#all_blue = np.concatenate(all_blue)
alphas = [-0.25, -0.2, -0.15, -0.1, -0.05, -0.015, -0.0025, 0, 0.0025, 0.015, 0.05, 0.1, 0.15, 0.2, 0.25]
iter = 0

for i in cat:
    current_img = i
    alpha = alphas[iter]
    iter += 1
    image = col_dist(path + current_img)
    col_dict = {"red": image[0], "green": image[1], "blue": image[2], "cat":cat_no, "alpha":alpha}
    pd.DataFrame(col_dict).to_csv("image_distributions/colors_cat" + str(cat_no) + "_" + str(current_img[11:-4]) + ".csv", index=False)


#col_dict = {"red": images[0][0][0], "green": images[0][0][1], "blue": images[0][0][2]}
#pd.DataFrame(col_dict).to_csv("cat1_colors.csv", index=False)


#### Create average image for seed
if RUN:
    # Get dimensions of first image
    w,h=Image.open(img0[0]).size
    N=len(img0)

    # Create a numpy array of floats to store the average (assume RGB images)
    arr = np.zeros((h,w,3),np.float)

    # Build up average pixel intensities, casting each image as an array of floats
    for im in img100:
        imarr = np.array(Image.open(im), dtype = np.float)
        arr = arr+imarr/N

    # Round values in array and cast as 8-bit integer
    arr = np.array(np.round(arr),dtype=np.uint8)

    # Generate, save and preview final image
    out = Image.fromarray(arr, mode="RGB")
    out.save("Average_alpha100.png")
    out.show()
else:
    print("Please enable RUN")

#colors = col_dist(test_img)


