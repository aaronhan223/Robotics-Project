In this project, an intent recognition system is implemented to recognize the human‘s hand motion intent and a motion planner system is implemented to enable the robot to generate legible and predictable motion.

Data analysis conducted the study of the effect of motion type and intent recognition in human-robot collaboration senario.

RUNNING INSTRUCTIONS:

Make sure robot's power cord is plugged in
Turn on robot's speaker

# To start the arm
ssh vector@vector1
Welcome00
sudo su
source .bashrc
roslaunch kinova_bringup arm.launch

# move arm to starting position
rosservice call /j2s7s300_driver/in/start_force_control
move arm so that blue tape markers are aligned (first 2 joints)
rosservice call /j2s7s300_driver/in/stop_force_control

# launch perception
rostopic pub /tilt_controller/command std_msgs/Float64 "data: 0.4"
roslaunch hlpr_segmentation pc_seg.launch
Verify that there are 4 clean clusters for the cups

# launch baseline
roscd sym_intent_rec/src/sym_intent_rec
python world/extract_state.py ../../data/world_state/cup_positions.yaml
python intent/object_intent.py
python motion/playback.py ../../data/manip_seq/manip_seq_pred.pkl

# launch intent recognition only
roscd sym_intent_rec/src/sym_intent_rec
python world/extract_state.py ../../data/world_state/cup_positions.yaml
python intent/hand_intent.py
python motion/playback.py ../../data/manip_seq/manip_seq_pred.pkl

# launch legible motion only
roscd sym_intent_rec/src/sym_intent_rec
python world/extract_state.py ../../data/world_state/cup_positions.yaml
python intent/object_intent.py
python motion/playback.py ../../data/manip_seq/manip_seq_leg.pkl

# launch both
roscd sym_intent_rec/src/sym_intent_rec
python world/extract_state.py ../../data/world_state/cup_positions.yaml
python intent/hand_intent.py
python motion/playback.py ../../data/manip_seq/manip_seq_leg.pkl

-----------------------------------------------
# To test playback
roscd sym_intent_rec/src/sym_intent_rec
python motion/playback_test.py [../../data/manip_seq/manip_seq_leg.pkl | ../../data/manip_seq/manip_seq_pred.pkl] [right_objs | left_objs] [0 | 1] [left_bin | right_bin]

# Variables to potentially tune
hand_intent.py
    -d_thresh

playback.py
    -n 

# Audio commands in playback.py

