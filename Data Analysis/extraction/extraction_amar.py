# -*- coding: utf-8 -*-
"""
Created on Thu Jun  3 11:46:48 2021

@author: u0072088
"""
import os
import json
import pandas as pd

root_folder = r'C:\Users\amarm\Documents\GitHub\beautification-of-images\Data Analysis\extraction'

data_filename = 'data_table.json'

    
# Parse the data file

# Open file and read JSON format
f = open(os.path.join(root_folder, data_filename))
data = json.load(f)
f.close()

df = []
subjectno = 0

for row in data:
    row_id = row[0]
    row_data = row[1]
    row_timestamp = row[2]
    
    json_data = json.loads(row_data)
    i = 1

    # Here you can extract data from the individual trials
    for trial in json_data:
        exp_trial = trial['trial_type'] == 'image-comparison'
        if exp_trial:
            df.append([
                i,
                trial['cat_no'],
                trial['wordnet_id'],
                trial['imagenet_label'],
                trial['label_1'],
                trial['label_2'],
                trial['seed'],
                trial['base_position'],
                trial['image_number'],
                float(trial['alpha']),
                trial['correct_key'],
                trial['user_key'],
                trial['eval'],
                trial['rt'],
                trial['time_elapsed'],
                trial['subject_id'],
                row_timestamp
            ]
            )
            dataframe = pd.DataFrame(
                df, columns=[
                    'trial_index',
                    'cat_no',
                    'wordnet_id',
                    'imagenet_label',
                    'label_1',
                    'label_2',
                    'seed',
                    'base_position',
                    'image_number',
                    'alpha',
                    'correct_key',
                    'user_key',
                    'eval',
                    'rt',
                    'time_elapsed',
                    'subject_id',
                    'date'
                    ])
            i += 1
        pass
    print(subjectno)
    subjectno += 1
dataframe.to_csv('test2.csv', index=False)
