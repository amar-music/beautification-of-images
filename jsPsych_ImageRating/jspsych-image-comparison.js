jsPsych.plugins["image-comparison"] = (function() {

  var plugin = {}

  plugin.info = {
    name: "image-comparison",
    parameters: {
      category: {
        type: jsPsych.plugins.parameterType.INT,
        default: undefined
      },
      seed: {
        type: jsPsych.plugins.parameterType.INT,
        default: undefined
      },
      image_number: {
        type: jsPsych.plugins.parameterType.INT,
        default: undefined
      },
      base_position: {
        type: jsPsych.plugins.parameterType.STRING,
        default: undefined
      },
      choices: {
        type: jsPsych.plugins.parameterType.KEY,
        array: true,
        default: jsPsych.ALL_KEYS,
      }
    }
  }

  plugin.trial = function(display_element, trial) {

    // data saving
    var trial_data = {
      cat_no: trial.category,
      wordnet_id: trial.wordnet_id,
      imagenet_label: trial.imagenet_category,
      label_1: trial.label_1,
      label_2: trial.label_2,
      seed: trial.seed,
      base_position: trial.base_position,
      image_number: trial.image_number,
      alpha: ((0.0025 * trial.image_number) - 0.25).toFixed(4)

    }
    
    if (trial.image_number < 100 && trial.base_position == 'l') {  // 100 <=> 50
      trial_data.correct_key = 'f'
    } else if (trial.image_number > 100 && trial.base_position == "r") {  // 150 <=> 100
      trial_data.correct_key = "f"
    } else {
      trial_data.correct_key = "j"
    }


    function show_stimuli(){
      let base_image = "<img src='stimuli/cat" + trial.category + "_seed" + trial.seed + "_img100.jpg'"
      let comp_image = "<img src='stimuli/cat" + trial.category + "_seed" + trial.seed + "_img" + trial.image_number + ".jpg'"
      let padding = "style='padding-right:250px'>"
      if (trial.base_position == 'l'){
        display_element.innerHTML = base_image + padding + comp_image + ">"
      } else {
        display_element.innerHTML = comp_image + padding + base_image + ">"
      }


      jsPsych.pluginAPI.getKeyboardResponse({
        callback_function: after_response,
        valid_responses: trial.choices,
        rt_method: 'performance',
        persist: false,
        allow_held_key: false
      })
    }

    function after_response(response_info){
      trial_data.user_key = response_info.key
      if (response_info.key == trial_data.correct_key){
          trial_data.eval = 1
      } else{
          trial_data.eval = 0
      }
      trial_data.rt = response_info.rt
      end_trial()
    }

    function end_trial(){
      jsPsych.finishTrial(trial_data)
    }

    show_stimuli()
  }

  return plugin
})()
