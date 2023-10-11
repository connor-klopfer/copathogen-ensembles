"""Main for executing the CI experiment for faster rewiring. 

The order of operation:

1. Import the Mal-Ed specific matrix. 
    - For the sake of efficiency, save the matrix type through R \
        then read in and load as a sparse numpy matrix. Then perform all the rewiring.
    - The experiment file containing the highe and low CI's can be done here, then 
        the saved to file. 
2. Rewire by choosing from a random distribution then swapping the rows, holding 
    the columns constant 
3. Generate the mean, STDEV, contstant, actual, and the Low/HIgh CI
"""
import datetime

import numpy as np

from myparser import args
from myimport import import_matrix
from rewire import build_ensemble
# from rewire_tools import build_ci_table, save_replicates
from myarchiver import archive_text_output

import cProfile, pstats
import io


def main():

    # Start time and filename
    params = add_exp_params()
    
    # Import the data from the datasets based on the passed arguments. 
    actual, params['path_names'] = import_matrix(args)

    # Perform build multiple replicates and rewire each 
    build_ensemble(actual, params, args)

    # TODO: These archiving results are getting moved to the rewiring 
    # functions. 
    # NOTE: This might need to be merged. 
    # Build a CI table to keep all the dataframe from the replicates
    # df = build_ci_table(reps, actual, path_names)

    # if args['se']:
    #     save_replicates(reps, path_names, args)

    print("UUID:", args['uuid'])

    # End of processing time. 
    params['end_time'] = datetime.datetime.today()
    # params['pathogens'] = path_names
    
    # Write text file containing experiment specs to file. 
    archive_text_output(args, params)


def add_exp_params():
    params = {}
    params['start_time'] = datetime.datetime.today() 

    params['todays_date'] = datetime.datetime.today().strftime('%d%b%y') 
    
    # If the title of the experiment is given, then assign that as the filename
    if args['t'] is None:
        params['filename'] = 'no_title_given_{}'.format(params['todays_date'])
    else:
        params['filename'] = '{}_{}'.format(args['t'], params['todays_date'])
    
    return params



if __name__ == "__main__":
    # CProfile to measure performance. I want to capture the whole 
    # things. 
    pr = cProfile.Profile()
    pr.enable()
    # Main program
    main()
    # Stop profiler
    pr.disable()
    # Sort results by cumulative time. 
    sortby = 'cumulative'
    # Set Filename and open stream
    filename = args['uuid'] if args['uuid'] != 'None_Given' else args['t']
    fout = open("run_diagnostics/{}.txt".format(filename), 'w')
    # Save Stats 
    ps = pstats.Stats(pr, stream= fout).sort_stats(sortby)
    ps.print_stats()
    fout.close()
