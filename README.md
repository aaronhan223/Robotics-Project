In this project, an intent recognition system is implemented to recognize the human‘s hand motion intent and a motion planner system is implemented to enable the robot to generate legible and predictable motion.

Data analysis conducted the study of the effect of motion type and intent recognition in human-robot collaboration senario.

RUNNING INSTRUCTIONS:

Make sure robot's power cord is plugged in <br />
Turn on robot's speaker <br />

# To start the arm
ssh vector@vector1 <br />
Welcome00 <br />
sudo su <br />
source .bashrc <br />
roslaunch kinova_bringup arm.launch <br />

# move arm to starting position
rosservice call /j2s7s300_driver/in/start_force_control <br />
move arm so that blue tape markers are aligned (first 2 joints) <br />
rosservice call /j2s7s300_driver/in/stop_force_control <br />

# launch perception
rostopic pub /tilt_controller/command std_msgs/Float64 "data: 0.4" <br />
roslaunch hlpr_segmentation pc_seg.launch <br />
Verify that there are 4 clean clusters for the cups <br />

# launch baseline
roscd sym_intent_rec/src/sym_intent_rec <br />
python world/extract_state.py ../../data/world_state/cup_positions.yaml <br />
python intent/object_intent.py <br />
python motion/playback.py ../../data/manip_seq/manip_seq_pred.pkl <br />

# launch intent recognition only
roscd sym_intent_rec/src/sym_intent_rec <br />
python world/extract_state.py ../../data/world_state/cup_positions.yaml <br />
python intent/hand_intent.py <br />
python motion/playback.py ../../data/manip_seq/manip_seq_pred.pkl <br />

# launch legible motion only
roscd sym_intent_rec/src/sym_intent_rec <br />
python world/extract_state.py ../../data/world_state/cup_positions.yaml <br />
python intent/object_intent.py <br />
python motion/playback.py ../../data/manip_seq/manip_seq_leg.pkl <br />

# launch both
roscd sym_intent_rec/src/sym_intent_rec <br />
python world/extract_state.py ../../data/world_state/cup_positions.yaml <br />
python intent/hand_intent.py <br />
python motion/playback.py ../../data/manip_seq/manip_seq_leg.pkl <br />

-----------------------------------------------
# To test playback
roscd sym_intent_rec/src/sym_intent_rec <br />
python motion/playback_test.py [../../data/manip_seq/manip_seq_leg.pkl | ../../data/manip_seq/manip_seq_pred.pkl] <br /> [right_objs | left_objs] [0 | 1] [left_bin | right_bin] <br />

# Variables to potentially tune
hand_intent.py <br />
    -d_thresh <br />

playback.py <br />
    -n <br />

# Audio commands in playback.py

