"""Arguments for the post-experiment processing in the VACC


"""
import os
import argparse

parser = argparse.ArgumentParser() 

parser.add_argument('-parent_dir', default= os.getcwd(), type = str,\
    help = "")   

parser.add_argument('-specific_batch', default = "False", type = str, help = "")

parser.add_argument('-specific_file', default = "False", type = str, help = "")

parser.add_argument('-last_modified', default= "False", type = str, help = "Only fix the most recent files. \
    Indicated fby the number of hours.")

parser.add_argument('-hour_window', default= 24, type = int, help = "Window to choose files from, will only edit files \
    created in the last X hours.")

args = vars(parser.parse_args())

args['specific_batch'] = args['specific_batch'] == "True"
args['specific_file'] = args['specific_file'] if args['specific_file'] != "False" else None
args['last_modified'] = args['last_modified'] == "True"

print("Use specific files from batch: {}".format(args['specific_batch']))
print("Use specific files from file: {}".format(args['specific_file']))