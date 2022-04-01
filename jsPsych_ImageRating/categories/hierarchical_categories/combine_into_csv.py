import json
import numpy as np
import pandas as pd

with open("C:/Users/amarm/PycharmProjects/ImageRating/hierarchical_categories/imagenet_class_index.json") as json_file:
    categories = json.load(json_file)


labels = pd.read_csv("C:/Users/amarm/PycharmProjects/ImageRating/hierarchical_categories/ImageNetLabels_flip.csv")
labels = labels.replace(np.nan, "", regex=True)
labels_dict = labels.to_dict("list")

combinations = []

i = 0
while i < len(categories):
    for key, value in labels_dict.items():
        if categories[str(i)][1] in value:
            print(categories[str(i)][0], categories[str(i)][1], key)
            combinations.append([categories[str(i)][0], categories[str(i)][1], key])
    i += 1


print(combinations)
df = pd.DataFrame(combinations)
df.to_csv("output_information.csv")
