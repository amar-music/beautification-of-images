import pandas as pd
import pyaesthetics
import os

# Remove first n characters from string
def remove_chars(a_string, number_to_remove):
    return a_string[number_to_remove:]

# Load all images in folder
path = "jsPsych_ImageRating/stimuli/"
images = list(os.listdir(path))

# Create list for df rows
rows = []


# Loop over all images and extract features
for index, image in enumerate(images):

    # Store image category and id
    img_str = image.split("_")
    cat = remove_chars(img_str[0], 3)
    img = remove_chars(img_str[2][:-4], 3)

    # Analyze images
    results = pyaesthetics.analysis.analyzeImage(path + image, method="fast")

    # Print progress
    print(str(index+1) + "/" + str(len(images)))

    # Append results to rows
    rows.append([cat, img, results['brightness_BT709'], results['VC_quadTree'], results['Symmetry_QTD'], results['Colorfulness_RGB']])

# # Create dataframe
df = pd.DataFrame(rows, columns=['cat', 'img', 'brightness', 'visual_complexity', 'symmetry', 'colorfulness'])
df.to_csv("Data Analysis/extraction/mid-level_features.csv", index=False)