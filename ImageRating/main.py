from psychopy import visual, core, event
import numpy as np
import random
import pandas as pd
import time

# SET PARAMETERS
ALPHA = 0.0025
STEPS = 100             # one direction
NUM_CATEGORIES = 1000   # full: 1000
TOTAL = (STEPS*2)+1

# choose stimuli to include
with open("stimuli_list.txt", "r") as stimuli_txt:
    cat = [int(i) for i in stimuli_txt]
alpha_val = list(np.arange((-STEPS * ALPHA), ((STEPS * ALPHA)+0.000001), ALPHA))
# cat = range(0, NUM_CATEGORIES)
seed = range(0, 2)
img_full = range(0, TOTAL)
exclude_set = {STEPS}
# img = list(num for num in img_full if num not in exclude_set)     # use this for full range
img = [0, 20, 40, 60, 70, 80, 90, 94, 98, 99, 101, 102, 106, 110, 120, 130, 140, 160, 180, 200]     # testing 100 steps

# load imagenet labels
labels = pd.read_csv("hierarchical_categories/manual_general_categories.csv")

# information
subject = random.randint(1000000, 9999999)
date = time.strftime("%Y-%m-%d_%H%M%S")

# create output
output = []

# create window
my_win = visual.Window([1200, 900], monitor="testMonitor", units="deg")

# start experiment
while True:
    # define stimuli
    cat_no = random.choice(cat)
    seed_no = random.choice(seed)
    base_img = STEPS
    comp_img = random.choice(img)
    stimulus_base = cat_no, seed_no, base_img
    stimulus_comp = cat_no, seed_no, comp_img
    stimuli = [stimulus_base, stimulus_comp]
    random.shuffle(stimuli)
    alpha_left = format(alpha_val[stimuli[0][2]], ".4f")
    alpha_right = format(alpha_val[stimuli[1][2]], ".4f")
    delta_alpha = format(alpha_val[base_img] + alpha_val[comp_img], ".4f")
    difference = format(abs(alpha_val[base_img] + alpha_val[comp_img]), ".4f")
    if alpha_val[stimuli[0][2]] > alpha_val[stimuli[1][2]]:
        correct_key = "left"
    else:
        correct_key = "right"

    # stimuli
    left_image = str("stimuli/a" + str(ALPHA) + "/cat" + str(stimuli[0][0]) + "_seed" + str(stimuli[0][1]) +
                     "_img" + str(stimuli[0][2]) + " a=(" + str(alpha_left) + ").jpg")
    right_image = str("stimuli/a" + str(ALPHA) + "/cat" + str(stimuli[1][0]) + "_seed" + str(stimuli[1][1]) +
                      "_img" + str(stimuli[1][2]) + " a=(" + str(alpha_right) + ").jpg",)

    stimulus_left = visual.ImageStim(win=my_win, image=left_image, pos=[-8, 0])
    stimulus_right = visual.ImageStim(win=my_win, image=right_image, pos=[8, 0])

    # trial
    while True:
        timer = core.Clock()
        stimulus_left.draw()
        stimulus_right.draw()
        my_win.update()
        user_key = event.waitKeys(keyList=["left", "right", "escape"])
        if user_key == ["escape"]:
            df = pd.DataFrame(output, columns=["subj", "cat_no", "wordnet_id", "imagenet_category", "label_1",
                                               "label_2", "seed", "left_image", "right_image",
                                               "left_alpha", "right_alpha", "delta_alpha", "difference",
                                               "correct_key", "user_key", "eval", "time"])
            df.to_csv("data/" + str(date) + "_" + str(subject) + ".csv")
            my_win.close()
            core.quit()
        else:
            if user_key[0] == correct_key:
                evaluation = 1
            else:
                evaluation = 0
            output.append(["subj_" + str(subject), cat_no, labels.loc[cat_no, "wordnet_id"],
                           labels.loc[cat_no, "imagenet_category"], labels.loc[cat_no, "label_1"],
                           labels.loc[cat_no, "label_2"], seed_no, left_image, right_image,
                           str(alpha_left), str(alpha_right), str(delta_alpha), str(difference),
                           str(correct_key), str(user_key[0]), str(evaluation), str(format((timer.getTime()), ".3f"))])
            break

