"""
For archiving all the results of the rewiring to file. Saves the experiment results from the rewiring experiment
and the text description file for all the parameters of the experiment. 
"""
import datetime
import uuid

def archive_text_output(args, params):
    """Write results to file. Creates a description text file with all the 
    descriptor fields and argument options for the experiment. Also writes 
    the dataframe containing the rewiring results to file. 
    
    Arguments:
        args {dict} -- contains all the parsed arguments from argparse to 
            write to file. 
        params {dict} -- contains all non argument parameters to write to 
            file. 
        df {pandas Dataframe} -- the results from the rewiring experiment. 
            contains mean value, actual value, and the confidence interval 
            for each pathogen pair. Does contain symmetic pairs.  
    """
    todays_date = datetime.datetime.today().strftime('%d%b%y') 
    
    # If the title of the experiment is given, then assign that as the filename
    if args['t'] is None:
        filename = 'no_title_given_{}'.format(todays_date)
    else:
        filename = '{}_{}'.format(args['t'], todays_date)

    # Assign the experiment a uuid? Prevents overwriting previous files. 
    # if args['u']:
    #     unique_id = uuid.uuid4()
    # else:
    #     unique_id = 'None Given'

    # File output
    fout = open('results/txt_output/{}.txt'.format(filename), 'w')

    # Each rewiring experiment generates a text description file. The fields 
    # for each are described below. Each line is put into a list then written to 
    # a file. The filename of this file is the title of the experiment with the 
    # date the experiment was run. 
    list_to_file = ['EXPERIMENT FILE: CONFIGURATION MODEL',
    'TITLE: {}'.format(args['t']),
    'EXP ID: {}'.format(args['uuid']),
    'VACC JOBID: {}'.format(args['jobid']),
    'SCRIPT FILENAME {}'.format(args['script_name']),
    'DATE RUN: {}'.format(todays_date),
    'TIME START: {}'.format(params['start_time'].strftime('%d%b%y %H:%M:%S')),
    'TIME FINISHED: {}'.format(params['end_time'].strftime('%d%b%y %H:%M:%S')),
    'STUDY: {}'.format(args['study'] if args['study'] is not None else 'both'),
    'STOOL: {}'.format(args['stool'] if args['stool'] is not None else 'both'),
    # Should binning be here? 
    'DATA FORMAT: {}'.format("POOLED" if args['partition_type'] == "simple" else "TIME_SERIES"),
    'PARTITIONED BY: {}'.format("N/A" if args['partition_type'] == "simple" else args['partition_type']),
    '# BINS: {}'.format(args['b'] if args['partition_type'] != 'simple' else 'N/A'),

    # '# BINS: {}'.format(args['b'] if args['type'] != 'simple' else 'N/A'),
    'SOURCE FILE: {}'.format(args['f']),
    'COMMENTS:\n{}'.format(args['c']),
    '=' * 60,
    'PATHOGENS:']

    # Write all the pathogens to the experiment description file. These are the 
    # pathogens used in the rewiring experiment
    list_to_file.extend(params['path_names'])

    # Append to a single string and write to file. 
    string_to_file = '\n'.join(list_to_file)
    fout.write(string_to_file)

    fout.close()

    # The dataframe containing the results (mean, ci, etc.) is written to file as a uuid. 
    # The corresponding uuid is written in the experiment description file. 
    # If the uuid option is not given, then save the filename as the title of the experiment, 
    # otherwise save as a uuid thats saved in the text description file. 
    # if args['uuid'] != 'None_Given':
    #     df.to_csv('results/datatables/EXP_{}.csv'.format(args['uuid']), index = False)
    # else:
    #     df.to_csv('results/datatables/{}.csv'.format(filename), index = False)