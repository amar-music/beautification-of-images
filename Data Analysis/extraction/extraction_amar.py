# -*- coding: utf-8 -*-
"""
Created on Thu Jun  3 11:46:48 2021

@author: u0072088
"""
import os
import json
import pandas as pd

root_folder = r''

data_filename = 'data_table3.json'

    
# Parse the data file

# Open file and read JSON format
f = open(os.path.join(root_folder, data_filename))
data = json.load(f)
f.close()

subjectno = 0

for row in data:
    df = []
    row_id = row[0]
    row_data = row[1]
    row_timestamp = row[2]
    
    json_data = json.loads(row_data)
    aeq = json_data[0]['response']
    aeq_total = []
    for item in aeq:
        aeq_total.append(aeq[item])
    aeq_sum = 0
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
                aeq['emotional'],
                aeq['cultural'],
                aeq['perceptual'],
                aeq['understanding'],
                aeq['flow-proximal'],
                aeq['flow-experience'],
                aeq_sum,
                row_timestamp,

            ]
            )
            dataframe = pd.DataFrame(
                df)
                # columns=[
                #     'trial_index',
                #     'cat_no',
                #     'wordnet_id',
                #     'imagenet_label',
                #     'label_1',
                #     'label_2',
                #     'seed',
                #     'base_position',
                #     'image_number',
                #     'alpha',
                #     'correct_key',
                #     'user_key',
                #     'eval',
                #     'rt',
                #     'time_elapsed',
                #     'subject_id',
                #     'emotional',
                #     'cultural',
                #     'perceptual',
                #     'understanding',
                #     'flow-proximal',
                #     'flow-experience',
                #     'aeq_total',
                #     'date'
                # ])
            i += 1
        pass
    print(subjectno)
    subjectno += 1
    dataframe.to_csv('test3.csv', index=False, header=False,  mode='a')
