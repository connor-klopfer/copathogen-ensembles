""" Post processing for results from the configuration model. Goes throught and creates new data tables in a seperate 
 folder with the additional metrics

- this is done here to prevent  ensemble data that takes up a ot of storage on the local drive and run the anaysis 
    on the VACC itself. 
 """
import csv
import glob
import os
import time 

import pandas as pd
import numpy as np

from post_args import args
from selected_files import specific_files

def main(args):
    """Main function for post experiment processing. 
    """

    if args['specific_batch']:
        files = [os.path.join(args['parent_dir'], s) for s in specific_files]
    elif args['specific_file'] is not None:
        files = [os.path.join(args['parent_dir'], args['specific_file'])]
        assert args['specific_file'] in [os.path.split(f)[-1] for f in glob.glob(os.path.join(args['parent_dir'], 'EXP_*.csv'))], \
            "ERROR: Given file not in given parent directory: {}".format(args['parent_dir'])        
    else:
        files = [f for f in glob.glob(os.path.join(args['parent_dir'], 'EXP_*.csv'))]

    # TODO: Test with a supplied parent directory 02Jun21

    for f in files:
        # Read in the data table as a Pandas Dataframe

        # If the last modified aregument is True and the file is not within the window, then skip it. 
        if args["last_modified"] and not file_within_window(f, args):
            continue
        else:
            print("file being processed: {}".format(f))

        results = pd.read_csv(f.replace("ensembles", 'datatables'))
        
        ts = get_time_variable(results.columns)

        if ts is not None and results[ts].isnull().values.all():
            ts = None
        
        ensemble = read_in_ensemble(f, ts)

        appended_df = append_c_score_to_df(ensemble, results, ts)

        appended_df.to_csv(f.replace("ensembles", "appended_datatables"), index = False)

def check_if_ts(cols):
    for c in cols:
        if "time" in c or "bin" in c:
            return True
    return False

def read_in_ensemble(filename, ts):
    """Read in the the ensemble as a dictionary of lists. Recommeneded to run on the 
    VACC because the ensembles are large files.  

    Args:
        filename (str): Filename with the extension, with the filepath necessary. The filename points
        to a dataframe, where each column is a copathogen pair, and the rows are the cooccurences for 
        those pairs in each replicate, where every row is a replicate. Can also contain a timepoint column 
        to represent the timepoint (month, week, etc) that timpeoint represents.  
        ts (str or None): The column name of the variable, usually 12_bin or 24_bin, 
        which indicates if this is a time partition. If not a time partition, return None. 

    Returns:
        dict : A dictionary. Each key is a copathogen pair, which maps to a list. If a time controlled ensemble, 
        then the list is a 2D list, where each list at the 1st level is the replicate for that timepoint, 
        and the inner list is the co-occurences for that timepoint. The nuimber of lists is the number of time points and 
        the number of elements in each inner-list is the number of replicates in the ensemble.    
    """

    with open(filename) as fin:
        # Initialise files and read in 
        replicates = {}
        read_in = csv.DictReader(fin, delimiter = ",")
        
        # Read in the column names, initialise of the keys if the lists
        for k in read_in.fieldnames:
            # There's a timepoint list 
            if ts is not None:
                replicates['timepoint'] = []
            replicates[k] = []

        # If the provided variable is not in the dataframe, find an alternative 
        if ts not in read_in.fieldnames:
            ts_ensemble = get_time_variable(read_in.fieldnames)
        else:
            ts_ensemble = ts

        # Read in each rows. 
        for row in read_in:
            # For every key in the final dictionary. 
            for k in replicates.keys():
                if k == "timepoint":
                    # Append the timepoint variable to the dictionary
                    replicates[k].append(row[ts_ensemble])
                else:    
                    # For column in a row, which is a copathogen pair, append to a list of replicates
                    replicates[k].append(row[k])

        # If there is a timepoint, then convert to a doctionary for each timepoint. 
        if ts is not None:
            # Get the set of all levels
            ts_levels = set(replicates["timepoint"])
            # If the number of replicates is not a multiple of the number of levels, then the 
            #bin size is wrong. 
            if len(replicates['timepoint']) % len(ts_levels) != 0:
                print('ERROR: Bad bin size')
                exit()
            bin_size = len(replicates['timepoint']) // len(ts_levels)
            ts_replicates = {}

            # For all the copathogen pairs 
            for k in replicates.keys():
                if k == "timepoint":
                    continue
                ts_replicates[k] = []
                # Iterate throught the levels for that copathogen pair, partition into a 2D
                # list, windexed by the time window. (ie index: 0 = first timepoint)
                for t, timepoint in enumerate(ts_levels):
                    ts_replicates[k].append(replicates[k][t * bin_size : (t + 1) * bin_size])
            return ts_replicates
        else:
            return replicates

def get_c_score(path1, path2, rep, r, t = None):
    """Return the C-Score from STONE and Roberts 1990

    Args:
        path1 ([type]): [description]
        path2 ([type]): [description]
        rep ([type]): [description]
        r ([type]): [description]

    Returns:
        [type]: [description]
    """
    if t is None:
        c = float(rep['{}+{}'.format(path1, path2)][r])
        r1 = float(rep['{}+{}'.format(path1, path1)][r])
        r2 = float(rep['{}+{}'.format(path2, path2)][r])
    else:
        c = float(rep['{}+{}'.format(path1, path2)][t][r])
        r1 = float(rep['{}+{}'.format(path1, path1)][t][r])
        r2 = float(rep['{}+{}'.format(path2, path2)][t][r])

    if r1 == 0 or r2 == 0:
        return 0

    return ((r1 - c) * (r2 - c))/(r1 * r2)


def file_within_window(filename, args):
    # print("Entered here")
    # Check if the file is within window, and return a filename
    stat_obj = os.stat(filename)
    mod_time = stat_obj.st_mtime
    # print(mod_time)
    # print("{} : {}".format(filename, mod_time))
    time_diff = (time.time() - mod_time) / 60**2
    # print("Time difference: {}".format(time_diff))
    # print("Within Window: {}".format(time_diff < args['hour_window']))
    # exit()
    return time_diff < args['hour_window']


def calculate_across_ensemble(rep, results, ts):
    """Calculate metrics across the ensemble for both time controlled and non-controlled experiements. 

    For finding metrics that are ensemble-dependent like the c-score and percentiles. Calculates 
    across the ensemble and saves to a list to append to a dataframe later. 

    Args:
        rep (dict): The ensembles. Each key is a copathogen pair. If the ensemble is time controlled, 
        then each key maps to a 2D list, where each element is a timepoint that has a co-occurences 
        for all the replicates. IF not time controled, then each element in the list is the co-occurences
        ts (str): The column containing the timepoint. 

    Returns:
        [type]: [description]
    """
    # Get the list of all distinct pathogens 
    k = list(rep.keys())
    paths = []
    for p in k:
        paths.extend(p.split('+'))
    path_names = list(set(paths))

    # If time-controlled. 
    if ts is not None:
        # Get the number of replicates
        n_reps = len(rep[list(rep.keys())[0]][0])
        # Get the number of timepoints
        ts_length = len(rep[list(rep.keys())[0]])
    else:
        # Number of replicates when not time-controlled. 
        n_reps = len(rep[list(rep.keys())[0]])
    rep_aves = {}
    rep_stdev = {}
    percentiles = {}
    for path1 in path_names:
        for path2 in path_names:
            # Skip the timepoint variable
            if "time" in path1 or "time" in path2:
                continue
            path_key = '{}+{}'.format(path1, path2)
            if ts is not None:
                # Get a 2D Array of C scrores, with one list per time point 
                # First list, get the c_score for each replicate, second, for each timepoint 
                c_scores = [[get_c_score(path1, path2, rep, n, t) for n in range(n_reps)] for t in range(ts_length)]
                rep_aves[path_key] = [np.mean(c_list) for c_list in c_scores]
                rep_stdev[path_key] = [np.std(c_list) for c_list in c_scores]

                # For that timepoint, get the number of co-occurences that are below the observed number f
                # for that pathogen pair. 
                percentiles[path_key] = [calc_percentile(path1, path2, rep, results, ts, t) for t in range(ts_length)]
                
                # print([c_list for c_list in c_scores])
                # print(c_scores)
            else:
                rep_aves[path_key] = np.mean([get_c_score(path1, path2, rep, n) for n in range(n_reps)])
                rep_stdev[path_key] = np.std([get_c_score(path1, path2, rep, n) for n in range(n_reps)])
                
                percentiles[path_key] = calc_percentile(path1, path2, rep, results)
                
    return rep_aves, rep_stdev, percentiles


def calc_percentile(path1, path2, ensemble, results, ts = None, t = None):
    path_key = '{}+{}'.format(path1, path2)
    if t is None:
        converted_list = [float(i) for i in ensemble[path_key]]
        actual_co = results.loc[(results['path1'] == path1) & (results['path2'] == path2), 'actual'].values[0]
    else:
        t_df = results[ts].unique()[t]
        converted_list = [float(i) for i in ensemble[path_key][t]]
        if results.loc[(results['path1'] == path1) & (results['path2'] == path2) & 
        (results[ts] == t_df), 'actual'].shape[0] == 0:
            print(path1, path2, ts, t)
            print(type(t))
            print(results[ts].dtypes)
            print(results.loc[(results['path1'] == path1) & (results['path2'] == path2)])

        actual_co = results.loc[(results['path1'] == path1) & (results['path2'] == path2) & 
        (results[ts] == t_df), 'actual'].values[0]

    return sum(converted_list < actual_co)/len(converted_list)


def append_c_score_to_df(replicate, df, ts):
    """[summary]

    Args:
        replicate ([type]): [description]
        df ([type]): [description]
        ts ([type]): [description]

    Returns:
        [type]: [description]
    """

    rep_aves, rep_stdev, percentiles = calculate_across_ensemble(replicate, df, ts)

    time_var_dict = {
        "time_rep" : "time_rep", 
        "timepoint" : "timepoint",
        "bins" : "bins" 
    }

    #TODO: Continue here 05Feb21

    if ts is not None:
        time_var = time_var_dict[get_time_variable(df.columns)]
        if sum(pd.isna(df[time_var])) > 0:
            # df['c_mean'] = [rep_aves['{}+{}'.format(row['path1'], row['path2'])][0] for _, row in df.iterrows()]
            # df['c_stdev'] = [rep_stdev['{}+{}'.format(row['path1'], row['path2'])][0] for _, row in df.iterrows()]
            # df['c_mean'] = build_df_column(df, rep_aves)
            # df['c_stdev'] = build_df_column(df, rep_stdev)
            idx_adjustment = None
            
        elif 0 in set(df[time_var]):
            # df['c_mean'] = [rep_aves['{}+{}'.format(row['path1'], row['path2'])][int(row[time_var])] for _, row in df.iterrows()]
            # df['c_stdev'] = [rep_stdev['{}+{}'.format(row['path1'], row['path2'])][int(row[time_var])] for _, row in df.iterrows()]
            # df['c_mean'] = build_df_column(df, rep_aves, idx_adjustment= 0)
            # df['c_stdev'] = build_df_column(df, rep_stdev, idx_adjustment=0)
            idx_adjustment = 0
            
        else:
            # df['c_mean'] = [rep_aves['{}+{}'.format(row['path1'], row['path2'])][int(row[time_var] - 1)] for _, row in df.iterrows()]
            # df['c_stdev'] = [rep_stdev['{}+{}'.format(row['path1'], row['path2'])][int(row[time_var] - 1)] for _, row in df.iterrows()]
            idx_adjustment = 1

        df['c_mean'] = build_df_column(df, rep_aves, idx_adjustment= idx_adjustment, time_var = time_var)
        df['c_stdev'] = build_df_column(df, rep_stdev, idx_adjustment=idx_adjustment, time_var = time_var)
        df['percentile'] = build_df_column(df, percentiles, idx_adjustment=idx_adjustment, time_var = time_var)

    else:
        # df['c_mean'] = [rep_aves['{}+{}'.format(row['path1'], row['path2'])] for _, row in df.iterrows()]
        # df['c_stdev'] = [rep_stdev['{}+{}'.format(row['path1'], row['path2'])] for _, row in df.iterrows()]
        
        df['c_mean'] = build_df_column(df, rep_aves)
        df['c_stdev'] = build_df_column(df, rep_stdev)
        df['percentile'] = build_df_column(df, percentiles)

    return df    

def build_df_column(df, metrics, idx_adjustment = None, time_var = None):
    # If no adjustment, then the variable is an empty column (earlier versions), or 
    # there is no time-controlled component.
    if idx_adjustment is None:
        # Empty column
        if isinstance(metrics['{}+{}'.format(df['path1'][0], df['path2'][0])], list):
            return [metrics['{}+{}'.format(row['path1'], row['path2'])][0] for _, row in df.iterrows()]
        # No time control. 
        else:
            return [metrics['{}+{}'.format(row['path1'], row['path2'])] for _, row in df.iterrows()]
    # Adjustment allows of 1 or 0 indexing (R by defaut does 1 indexing, also easier to interpret)
    else:
        return [metrics['{}+{}'.format(row['path1'], row['path2'])][int(row[time_var] - idx_adjustment)] for _, row in df.iterrows()]

def get_time_variable(cols):
    for c in cols:
        if "time" in c or "bin" in c:
            return c
    return None

if __name__ == "__main__":


    main(args)

