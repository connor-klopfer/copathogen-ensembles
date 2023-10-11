"""Import the matrix to file. Takes in different types (Simple, Lists) and returns the matrix as a numpy 
array. I also neec to return the naming dictionary so I can rename later. 

To consider: 
    1.) Need to import the 
    2.) The time series componenet, this script needs to have a binning component
    3.) 

"""
import datetime
from collections import Counter
import math
from os import path, stat
import time 

import pandas as pd
import numpy as np

from myparser import clean_filters, check_files
from consts import seasons, timepoints


def break_apart_stool_samples(df, args):
    # Break apart stool samples ans convert to a 3 dimensional matrix. 
    # This should be a list, the sizes will have different shapes. 

    # NOTE: To compare date objects, we'll have to replcae the dates in the dataframe 
    # with the corrent date
    # TODO: This is seperating thhe observations by the time of year. 
    # if args['seasons'] or args['bymonth']:
    if args['partition_type'] == "seasons" or args['partition_type'] == "monthbycalender" :
        df['collection_date'] = pd.to_datetime(df['collection_date'], format = "%Y-%m-%d")
        df['fixed_date'] = pd.to_datetime(df['collection_date'].dt.month.astype(str)+ "-" + df['collection_date'].dt.day.astype(str) + "-" + "2020")
        df_subsets = []

    # Break Apart by Calender seasons 
    # if args['seasons']:
    if args['partition_type'] == "seasons": 
        for season in seasons:
            if season['name'] == 'shit':
                df_subsets.append(df[(season['start'] <= df['fixed_date']) | (df['fixed_date'] < season['end'])])
            else:
                df_subsets.append(df[(season['start'] <= df['fixed_date']) & (df['fixed_date'] < season['end'])])
    
    # Break apart by Calender month in which the stool was collected 
    # elif args['bymonth']:
    elif args['partition_type'] == "monthbycalender":
        for month in range(1, 13):
            df_subsets.append(df[df['collection_date'].dt.month == month])
    
    else:
        if args['partition_type'] == "specific":
            # print(df.columns)
            final = partition_into_specific_timepoints(df, args)
        else:
            binsize = get_binsize(args['b'])
            df['temp_timewindow'] = (df['age'] // binsize).astype(int)
            df_subsets = [df[df['temp_timewindow'] == i] for i in range(args['b'])]
            final = [np.array(subset.drop(['temp_timewindow', 'age'], axis = 1), dtype='uint16') for subset in df_subsets]
    
    # if args['seasons'] or args['bymonth']:
    # if args['partition_type'] == "specific":
    if args['partition_type'] in ["seasons", "monthbycalender"]:
        final = [np.array(subset.drop(['collection_date', 'fixed_date'], axis = 1), dtype='uint16') for subset in df_subsets]
    
    print("Timepoint Shapes:")
    for f in final:
        print(f.shape)
    return final


def partition_into_specific_timepoints(df, args):
    """Partition the timeseries into timepoints defined specifically. 

    Due to study design, the timepoints for meaningful partitions don't fall into neatly placed 
    bins. This function uses specific timepoints and windows defined in 'const.py' dictionary 
    'timepoints' to sepcifcy those specific partitions. Highly sensitive to the structure of 
    this distionary, so be careful when updating. 

    If no specific study or stool type is provided, 

    Args:
        df (Pandas DataFrame): Filtered dataframe, filtered by the appropiate study and stool type. 
            Columns should include all the appropiate pathogens and the 'age' and 'study' columns. 
        args (dict): Disctionary of the arguments passed to the main program. Necessary to choose 
            the right values in the timepoint disctionary.

    Returns:
        [type]: [description]
    """
    if args['stool'] is not None:
        timepoints_specific = timepoints[args['stool']]
    else:
        timepoints_specific = timepoints
    
    # TODO: Implement both stool types. This is going to require some serious branching (CK: 01Mar21)
    #   -  Will need the stool type column in here as well, then drop it later. 
    assert args['stool'] == "Diarrhea" or args['stool'] == "Asymptomatic", \
        "ERROR: Specific time series partitions for specific stool types not implemented yet," +\
            "results will be innaccurate." 

    data_frame_partitions = []

    if args['study'] is None:
        for study in timepoints_specific:
            for tp_label, tp  in timepoints_specific['study']['timepoints'].items():
                data_frame_partitions.append(df.loc[(df['study'] == study) & 
                    (df['age'] <= (tp + timepoints_specific[study]['upper_bound'])) & 
                    (df['age'] >= (tp + timepoints_specific[study]['lower_bound'])),:])
    else:
        for tp_label, tp  in timepoints_specific[args['study']]['timepoints'].items():
            dataframe_subset = df.loc[(df['study'] == args['study']) & 
                (df['age'] <= (tp + timepoints_specific[args["study"]]['upper_bound'])) & 
                (df['age'] >= (tp - timepoints_specific[args["study"]]['lower_bound'])),:]
            
            # print(dataframe_subset['age'])
            # print("Before Condesing: {}".format(dataframe_subset.shape))
            # print(sorted(dataframe_subset['age'].tolist()))
            dataframe_subset = combine_like_participants(dataframe_subset, ['participant', 'age', 'study'])
            # print("After Condesing: {}".format(dataframe_subset.shape))
            data_frame_partitions.append(np.array(dataframe_subset.drop(['participant', 'age', 'study'], axis = 1), dtype = "uint16"))

    # multi_dim_array = np.empty((len(data_frame_partitions), data_frame))
    return data_frame_partitions


def combine_like_participants(df, id_cols):
    """[summary]

    Args:
        df ([type]): [description]
        id_cols ([type]): [description]

    Returns:
        [type]: [description]
    """
    all_rows = []
    # only one or 0 rows, return dataset. 
    for p in df['participant'].unique():
        all_rows.append(combine_like_timepoints(df.loc[df['participant'] == p, :], id_cols))
    # If there are no rows that fit criteria, then pandas contaenate will not work, so we need 
    # to add an empty dataframe. 
    if len(all_rows) == 0:
        # NOTE: This only works if participants cannot take multiple simultaneous values, which is impossible 
        all_rows.append(
            combine_like_timepoints(df.loc[(df['participant'] == "Impossible") &  (df['participant'] == "participant"), :], id_cols))

    return pd.concat(all_rows, axis = 0, sort = False)
    
    
def combine_like_timepoints(df, id_cols): 
    # only one or 0 rows, return dataset. 
    if df.shape[0] < 2:
        return df
    pathogens_only = df.copy().loc[:, df.columns[~df.columns.isin(id_cols)]]
    condensed_row = pd.DataFrame(pathogens_only.sum(axis = 0) > 0).T * 1

    condensed_row = pd.concat([condensed_row,
        pd.DataFrame([df[id_cols].iloc[0]], index = condensed_row.index, columns = id_cols)
        ], axis = 1)

    condensed_cols = condensed_row.columns.tolist()[-3:] + condensed_row.columns.tolist()[:-3] 
    condensed_row = condensed_row.loc[:, condensed_cols]
    return condensed_row

def read_in_data(args):
    check_files(args)
    all_data = pd.read_csv('data/{}'.format(args['f']), na_values='NA')
    return all_data


def get_pathogens(include_opv):
    """Get the names of the pathogens to filter/rename the dataset with the 
    appropiate pathogen names. 

    NOTE: As of 13Jul20, I included code that would remove crypto sub groups (hominus, parvum), 
        giardia from the pathogen groups. 

    Args:
        include_opv (bool): Boolean provided in the commands line arguments, 
        indicates wheather to include the OPV vaccines 

    Returns:
        [Pandas Dataframe]: Dataframe containing the study specific (PROVIDE/MAL-ED)
            names for the pathogens, and the general names (actual) for the pathgoens. 
    """
    # Inlcuding removed pathogens, might move this out to a parameter, but for now we'll leave 
    # is as hard-coded into the script
    removed_pathogens = ['giardia', 'crypto_lib hominis', 'crypto_lib parvum', 'c.jejuni/coli']

    # Edit 24May21: Add in EPEC, this is renamed from typical epec. 
    added_pathogens = ['epec']
    if include_opv:
        # names = pd.read_csv('data/name_key.csv', header = 0)
        names = pd.read_csv('data/name_key2.csv', header = 0)
    else:
        # names = pd.read_csv('data/name_key_noopv.csv', header = 0)
        names = pd.read_csv('data/name_key_noopv2.csv', header = 0)
    names['actual'] = names['actual'].str.rstrip()
    names = names[~names['actual'].isin(removed_pathogens)]
    # print(names.columns)
    # names.extend(added_pathogens)
    return names


def get_binsize(n_bins):
    # The max number of days, should be set to 372 for the first year of life. 
    # First 53 weeks of life 
    max_val = 372
    # Get the bin size
    return max_val / n_bins


def convert_to_matrix(df):
    return np.array(df)


def filter_df(df, args):
    if args['study'] is not None and args['stool'] is not None:
        return df.loc[(df['stool_type'] == args['stool']) & (df['study'] == args['study']), :]
    elif args['study'] is not None and args['stool'] is None:
        return df.loc[df['study'] == args['study'], :]
    elif args['study'] is None and args['stool'] is not None:
        return df.loc[df['stool_type'] == args['stool'], :]
    else:
        return df


def filter_country(df, args):
    if args['country'] is None:
        return df
    else:
        assert 'country' in df.columns, "Country Variable is not in the dataset"
        return df.loc[df['country'] == args['country']]


def import_matrix(args):
    """Main function for reading in the dataframe from file and comverting 
    to a numpy matrix to use in the rewiring experiment. 
    NOTE: This does not do the datacleaning from the raw files. This assumes that 
    that all the data cleaning was done with R and saved to file. 

    TODO: Add in description of the dataframes
    
    Arguments:
        args {dict} -- The arguments passed to the script as parameters for the rewiring 
            experiment. 
    
    Returns:
        NumPy array -- A numpy array of the kids and pathogens. Each row is a child or 
            observation and each column is a pathogen. Also contains a column for the age
        path_names -- A list of filenames, in the same order as the columns in the matrix.
            Used for renaming the columns later since the Numpy arrays cannot have column 
            names. 
    """
    # Read in the data as a pandas dataframe. 
    data = read_in_data(args)

    # Get the name of the pathogen columns from the key file. 
    path_names = get_pathogens(args['opv'])
    
    # Clean arguments. 
    args['study'], args['stool'] = clean_filters(args['study'], args['stool'])
    
    # Apply filters. 
    reduced = filter_df(data, args)

    # Apply Country filter 
    reduced = filter_country(reduced, args)

    # Get only pathogen columns
    reduced.columns = reduced.columns.str.rstrip()
    # if args['type'] == "simple":
    if args['partition_type'] == "simple":
        # NOTE: I need to remove the OPV readings. 
        column_selection = path_names['actual'].loc[(path_names['ispathogen'] > 0)]
    # elif args['seasons'] or args['bymonth']:
    elif args['partition_type'] == "seasons" or args['partition_type'] == "bycalendermonth":
        column_selection = path_names['actual'].loc[(path_names['ispathogen'] > 0) | (path_names['actual'] == 'collection_date')
            ]
    else:
        column_selection = path_names['actual'].loc[(path_names['ispathogen'] > 0) | (path_names['actual'] == 'age')\
            # | (path_names['actual'] == 'participant')
            ]

    column_selection = list(column_selection)
    # Need the study since the timepoints are specific
    if args['partition_type'] == "specific":
        column_selection.extend(['participant', 'study'])

    # pathogens = reduced
    pathogens = reduced.loc[:, reduced.columns.isin(column_selection)]
    path_names = pathogens.columns
    print(path_names)
    #Sanity Check. Should be 41, 40 pathogens and 1 age column
    print('Shape of Dataframe before conversion:', pathogens.shape)
    
    # Apply data tranformation here. 
    # if args['type'] == 'simple':
    if args['partition_type'] == "simple":
        # IF the pathogen type is simple, then the age column is not needed
        # pathogens = pathogens.drop(columns = ['age'])
        pathogen_m = np.array(pathogens, dtype = 'uint16')
    else:
        if not args['bystool']:
            # Convert the dataframe into a time series, 3 dimensional matrix by age. 
            # This segments within observations in a 
            pathogen_m = timeseries_transformation(pathogens,args)
        else:
            # Breaking apart stool samples, returns a list of matrices. 
            pathogen_m = break_apart_stool_samples(pathogens.copy(), args)
        # if args['seasons'] or args['bymonth']:
        if args['partition_type'] == "seasons" or args['partition_type'] == "bycalendermonth":
            path_names = path_names.drop('collection_date')
        else:
            if args['partition_type'] == "specific":
                path_names = path_names.drop(['participant', 'age', "study"])
            else:
                path_names = path_names.drop('age')
    # Only the pathogen names are needed. 

    # print("Pathogen Length", len(pathogen_m))
    return pathogen_m, path_names


def merge_observation(current, additive, idx = None, flag = None):
    # Merge the observation with the current obervation. 
    # If the value current value is one, then leave it, if the current value is 
    # zero or none, and the additive is one, and None if both are None. 
    final = []
    try:
        for c, a in zip(current, additive):
            if idx is not None:
                if isinstance(a, float):
                    if np.isnan(a):
                        a = None
                else:
                    a = float(a[idx]) if a[idx] != 'NA' else None 
            else:
                a = float(a)

            if np.isnan(c) or c is None:
                if np.isnan(a):
                    final.append(None)
                elif a == 0 or a == 1:
                    final.append(a)
            elif c == 1:
                final.append(c)
            elif c == 0:
                if a == 1:
                    # if flag == "1209":
                    #     print("Flag Entered")
                    final.append(a)
                else:
                    final.append(c)
    except ValueError:
        print('Value Error')
        print(current)
        print(additive)
        print(a)
        exit()
    except TypeError:
        print('Type Error')
        print(a)
        print(np.isnan(a))
        print(isinstance(a, float))
        exit()
    return final
            
        
    

def filter_func(row, n_bins, test_list, above_list, below_list, n_obs, participant_list):
        # Each apply function will build a layer of the final Numpy array, 
        # with the number of columns equal to the number of pathogens, and the 
        # depth equal to the max length. 

        # print(row)
        age_var = 'age'
        # Nrows: Number of pathogens, 
        # NCols: Number of depth
        # Create the layer, of the final array, each layer for each individual subject
        # The row of each lawer is equal to the number of pathogens, and the number 
        # of columns is equal to the number of bins in the time series.  
        
        # Initialize the layer
        if 'participant' in row.keys():
            final = np.zeros((len(row) - 2, n_bins))
        else:
            final = np.zeros((len(row) - 1, n_bins))
        
        binsize = get_binsize(n_bins)

        # Each cell from the original is a string representing a list, where the 
        #individual items are seperated by a '|'. For each cell, split the list and 
        # set as the value for that variable. Ro is an index (dict) and row.iteritems()
        # returns the variable and the element as a tuple. 
        # name <- variable name
        # element <- value for that variable in that row. 
        for name, element in row.iteritems():
            if isinstance(element, str):
                if '|' in str(element):
                    row[name] = element.split('|')

        # For MAL-ED, some if the rows are not a list, so the follwing is performed for
        #  entries that are a list
        if isinstance(row[age_var], list):
            # GEt the list of ages. 
            timepoints = list(row[age_var])

            # Remove the age variable, we'll use the timepoints variable
            row = row.drop(age_var)

            n_obs.append(len([int(np.floor(int(point)//binsize)) for point in timepoints if int(np.floor(int(point)//binsize)) == 19]))
            # For every age in the list of ages for that participant. 
            for idx, point in enumerate(timepoints):
                # What bin does the age fall into 
                spot_location = int(np.floor(int(point)//binsize))
                # if spot_location == 8:
                #     test_list.append(point)
                #     # participant_list.append(row['participant'])
                #     # print(row['participant'])
                # elif spot_location == 7:
                #     below_list.append(point)
                # elif spot_location == 9:
                #     above_list.append(point)

                # TODO: This needs to be fixed, it should add a one if zero, and nothing if one, and remain as None
                final[:, spot_location] = merge_observation(final[:, spot_location], row, idx = idx)

        else:
            try:
                age_index = math.floor(int(row[age_var])//binsize)
            except ValueError:
                print(row)
                exit()
            row = row.drop(age_var)
    
            final[:, age_index] = merge_observation(final[:, age_index], row)
    
        return final 

def timeseries_transformation(df, args):
    """Transform the dataframe made for a timeseries into a 3-dimensional matrix with each slice 
    representing a time bin. 
    
    Arguments:
        df {pandas Dataframe} -- The dataframe cleaned in R. Needs an age column which will bin 
            the observations into the respective slice. 
        args {dict} -- the arguments passed, processed with argParse which determine the number 
            of bins, or slices. 
    
    Returns:
        NumPy array -- The 3 dimensional Numpy array, with the number of rows equal to the number
            of observations, columns equal to the number of pathogens. Slices are equal to the 
            number of slices or time bins. 
    """
    # Apply the time series tranformation here, this can easily be done if the export
    # from R is done correctly. Mainly, there has to be a character that seperates the
    # different timepoints correctly. 
    n_bins = args['b']
    if 'participant' in df.columns:
        final_df = np.empty((n_bins, df.shape[0], df.shape[1] - 2))
    else:
        final_df = np.empty((n_bins, df.shape[0], df.shape[1] - 1))

    # 1. For every value, split string into a list
    test_list = []
    person_list = []
    below_list = []
    n_obs = []
    above_list = []
    returned_df = df.apply(filter_func, axis = 1, args = [n_bins, test_list, above_list, below_list, n_obs, person_list])

    for idx, x in enumerate(returned_df):
        final_df[:, idx, :] = x.T.astype(int) 
    return final_df


if __name__ == "__main__":
    from myparser import args    

    """
    For testing the script consider this imput into the command line: 

    python main.py -study provide -n 10000 -r 15000 -t "Provide_bystool_asym_byseason" -u True -c "Initial testing, this is 1.) using unsigned integers to reduce memory usage, and 2.) increased I/O to free up more memory. This is by observation, and the timeseries subsetting is by season rather than strictly by time." -f simple_non_condensed_provide_select_15Jun20.csv -stool asym -type simple_list -se True -opv False -multip True -bystool True -b 12 -jobid ${PBS_JOBID} -seasons T

    """
    # print(args['opv'])
    # print("Seasons", args['seasons'])
    # print("Stool", args['bystool'])
    # print("By Month", args['bymonth'])

    # print(args)
    # path_names = get_pathogens(args['opv'])
    # print("Pathogen Names:")
    # print(path_names)

    sample_matrix,_  = import_matrix(args)

    # print(sample_matrix.shape)
    # for s in sample_matrix:
    #     print(s.shape)
    # print(type(sample_matrix))