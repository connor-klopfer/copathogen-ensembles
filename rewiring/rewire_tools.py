# All the tools needed for rewiring the matix
from math import sqrt

from scipy.stats import norm 

import numpy as np
import pandas as pd

from consts import MISSING_VALUE


# # NOTE: This might be removed, I think the 
# def build_ci_table(reps, actual, names):
#     """Main archiving function called by the main() master function. 
#     Takes the replicates and the actual co-occurance matrix to create 
#     a datatable in tidy format. 

#     Args:
#         reps ([list or Numpy Array]): A list of Arrays or a Numpy Array of 
#             contianing the replicates for the configuration models. 
#         actual (Numpy Array): Numpy array of the individuals as the rows and 
#             the columns as the pathogens of interest. 
#         names (list): List of the pathogen names to rename the matrix. 

#     Returns:
#         Pandas Dataframe: Dataset of all the pathogen pairs with the actual, mean, 
#             standard deviation of all the pathogen pairs across the ensemble.
#     """
#     # Axis of Zero is across all the arrays. 
#     if isinstance(reps, list):
#         #if instance of a list, need to build a dataframe with each replicates version
#         all_dfs = []
#         for idx, rep in enumerate(reps):
#             temp_df = build_ci_df(rep, actual[idx], names)
#             temp_df['bins'] = idx + 1
#             all_dfs.append(temp_df)
#         final = pd.concat(all_dfs)
#     else:
#         final = build_ci_df(reps, actual, names)
#     return final


def build_ci_df(reps, actual, params, args, timepoint = None):
    """Build a dataframe containing the results of the configuration model. 
    This makes the actual dataframe. 

    Args:
        reps ([type]): [description]
        actual ([type]): [description]
        names ([type]): [description]

    Returns:
        [type]: [description]
    """
    # Across Ensemble
    # NOTE: How do I want to handle missing data? 
    if MISSING_VALUE in reps:
        print("PROBLEM: Missing value in replicates")
    mean = np.mean(reps, axis = 0)
    stdev = np.std(reps, axis = 0)
    mean_df = convert_to_dataframe(mean, params['path_names'], 'mean')
    stdev_df = convert_to_dataframe(stdev, params['path_names'], 'stdev')
    # Edit from the 08Jun20 edits to change the matrix representation to an integer preresentation. 
    # actual[np.isnan(actual)] = 0
    actual[actual == MISSING_VALUE] = 0
    actual_df = convert_to_dataframe(np.dot(actual.T, actual), params['path_names'], 'actual')
    
    # Replicate information with information from the actual replicates
    final = actual_df.merge(mean_df, on = ['path1', 'path2'])
    final = final.merge(stdev_df, on = ['path1', 'path2'])
    # This might not be needed, I'm getting the bounds in r. 
    # NOTE: Consider removing. 
    final['low'] = final.apply(get_bounds, \
        axis = 1,
        args = (reps.shape[0], 'low'))
    
    final['high'] = final.apply(get_bounds, \
        axis = 1,
        args = (reps.shape[0], 'high'))

    final['timepoint'] = timepoint

    # NOTE: Archive the datatable here. 
    
    if args['uuid'] != 'None_Given':
        final.to_csv('results/datatables/EXP_{}.csv'.format(args['uuid']), 
        mode = 'a' if (timepoint is not None and timepoint > 0) else 'w', index = False, 
        header = False if (timepoint is not None and timepoint > 0) else True)
    else:
        final.to_csv('results/datatables/{}.csv'.format(params['filename']),
        mode = 'a' if (timepoint is not None and timepoint > 0) else 'w', index = False, 
        header = False if (timepoint is not None and timepoint > 0) else True)
    
    # return final



    #TODO: Generate the confidence intervals, and convert to a 
    # matrix to the fold out to a dataframe. 

def get_bounds(r, n, direction):
    if n == 0:
        return None
    try:
        CI = norm.ppf(.95)
        # TODO: Update this, the wilson score is for approvimations, not for wholw values counts, 
        # you will need the standard divation to do this.

        # Removed as of 05Apr20
        # limit = CI * r['stdev']/sqrt(n)

        limit = CI * r['stdev']
        #  means_m[r, c] - CI * stdev_col[i]/sqrt(rep_dim[3])

        if direction == 'low':
            # bound = (term_a - term_b)/denominator
            bound = r['mean'] - limit
        elif direction == 'high':
            # bound = (term_a + term_b)/denominator
            bound = r['mean'] + limit
        else: 
            print('ERROR: incrorect dimension command')
        return bound
    except ValueError:
        print('n: {}, \np: {}'.format(n, r['mean']))
        exit()


def convert_df_to_matirx(df):
    df_names = df.columns
    final = np.array(df)
    return final, df_names


def convert_to_dataframe(m, names, col_name):
    df = pd.DataFrame(m, columns = names)
    df['path1'] = names
    final = pd.melt(df, id_vars= 'path1', 
    value_vars = df.columns.difference(['path1']), 
    var_name = 'path2', value_name=col_name)
    return final


# def run_rewire_experiment(df, n_replicates, n_rewires):
#     co_matrix, path_names = convert_df_to_matirx(df)
#     replicates = np.empty(
#         shape = (co_matrix.shape[0], co_matrix.shape[1], n_rewires))
    
#     for r in range(n_replicates):
#         replicates[:, :, r] = rewire_matrix(co_matrix, n_rewires)
#     # Once we generates the replacaites, butild the ci table. \
#     table = build_ci_table(replicates, path_names)


# def rewire_matrix(co_matix, n_rewires):
    
#     for r in range(n_rewires):
#         pass


def save_replicates(reps, params, args, timepoint = None):
    """Save the ensemble of configuration models to file. 

    Args:
        reps (3D Numpy Array): Each slice is a replicate of a 
            re-wired configuration model. 
        names (list): List of pathogen names to use as names for the 
            matrix. 
        args ([type]): Arguments containing replicate counts, rewires, 
            and other options. 

    Returns:
        None: Save option only. 
    """
    converted_dict = {}

    converted_dict['timepoint'] = [timepoint] * args['n']

    for idx1, name1 in enumerate(params['path_names']):
        for idx2, name2 in enumerate(params['path_names']):
            # if converted_dict.get('{}+{}'.format(name1, name2)) is None:
            converted_dict['{}+{}'.format(name1, name2)] = reps[:, idx1, idx2]

    final  = pd.DataFrame(converted_dict)
    
    if args['partition_type'] != 'simple':

        if args['uuid'] != "None_Given":
            final.to_csv('results/ensembles/EXP_{}.csv'.format(args['uuid']), 
            mode = 'a' if timepoint > 0 else 'w', index = False, 
            header = False if timepoint > 0 else True)
        else:
            final.to_csv('results/ensembles/EXP_{}.csv'.format(args['t']), 
            mode = 'a' if timepoint > 0 else 'w', index = False,
            header = False if timepoint > 0 else True)

    else:
    
        if args['uuid'] != "None_Given":
            final.to_csv('results/ensembles/EXP_{}.csv'.format(args['uuid']), index = False)
        else:
            final.to_csv('results/ensembles/EXP_{}.csv'.format(args['t']), index = False)


if __name__ == '__main__':
    from scipy.stats import norm
    import scipy.stats as st
    from myimport import import_matrix
    from myparser import args

    
    # actual, p_names = import_matrix(args)

    # reps = np.random.randint(0, 2, size = (args['n'], actual.shape[1], \
    #     actual.shape[1]))

    # actual_co = np.dot(actual.T, actual)

    # ci_table = build_ci_table(reps, actual_co, p_names)

    # print(ci_table.head())

    sample_dist = [322, 341, 331, 335, 341, 325, 334, 325, 334, 331, 334, 338, 335, 325, 324, 334, 334, 337, 330, 326, 322, 329, 327, 326, 339, 335, 322, 327, 325, 330, 337, 335, 327, 334,
  328, 326, 334, 332, 330, 328, 337, 339, 329, 336, 330, 331, 332, 331, 322, 325, 327, 324, 328, 333, 323, 330, 338, 327, 332, 324, 330, 337, 336, 329, 328, 330, 328, 331,
  338, 327, 330, 324, 328, 325, 328, 330, 326, 333, 338, 331, 331, 333, 329, 330, 329, 329, 327, 328, 335, 342, 324, 331, 334, 323, 332, 322, 331, 331, 332, 336, 335, 334,
   338, 331, 329, 329, 333, 330, 336, 332, 329, 338, 327, 327, 331, 325, 324, 328, 331, 340, 327, 326, 328, 335, 334, 329, 334, 328, 330, 335, 333, 333, 338, 331, 332, 332,
   330, 331, 331, 325, 331, 327, 331, 327, 335, 331, 334, 334, 333, 329, 334, 327, 331, 327, 336, 323, 337, 325, 327, 335, 329, 332, 324, 342, 333, 334, 326, 337, 337, 335,
   327, 324, 331, 331, 328, 334, 326, 336, 330, 332, 327, 320, 329, 325, 326, 322, 323, 327, 331, 334, 327, 329, 330, 333, 335, 335, 323, 329, 333, 336, 340, 328, 333, 329,
   330, 325, 336, 331, 324, 334, 327, 327, 318, 335, 332, 330, 333, 323, 328, 337, 332, 334, 332, 325, 326, 328, 333, 331, 330, 329, 325, 322, 327, 327, 326, 326, 336, 327,
   327, 331, 325, 330, 329, 332, 334, 329, 325, 333, 325, 325, 326, 330, 328, 334, 330, 326, 325, 329, 323, 333, 326, 331, 326, 326, 330, 317, 338, 337, 328, 335, 326, 332,
   333, 322, 328, 336, 329, 329, 322, 327, 324, 334, 326, 324, 336, 326, 322, 324, 326, 330, 331, 330, 323, 322, 333, 334, 331, 328, 327, 329, 325, 328, 335, 335, 335, 333,
   328, 327, 326, 331, 323, 336, 331, 324, 327, 332, 339, 335, 335, 335, 323, 330, 330, 326, 330, 331, 330, 335, 324, 333, 322, 331, 336, 328, 327, 342, 328, 332, 328, 331,
   327, 332, 330, 326, 336, 331, 323, 327, 327, 331, 332, 329, 328, 328, 329, 336, 328, 329, 326, 330, 328, 336, 329, 329, 326, 332, 337, 333, 333, 332, 326, 332, 333, 324,
   331, 332, 319, 326, 331, 327, 330, 322, 332, 326, 324, 325, 323, 332, 336, 329, 332, 338, 335, 331, 317, 331, 347, 325, 331, 326, 336, 329, 329, 322, 327, 332, 328, 332,
   338, 322, 327, 329, 330, 328, 327, 331, 333, 340, 328, 328, 326, 327, 332, 326, 328, 329, 335, 329, 333, 332, 335, 334, 332, 334, 335, 317, 327, 333, 335, 322, 331, 332,
   326, 333, 335, 331, 332, 325, 333, 328, 327, 332, 337, 331, 325, 336, 325, 335, 323, 334, 329, 331, 327, 327, 328, 331, 327, 334, 329, 334, 338, 332, 331, 320, 329, 331,
   334, 332, 327, 326, 329, 333, 327, 319, 321, 331, 341, 323, 320, 340, 326, 324, 328, 323, 329, 326, 336, 334, 333, 330, 332, 321, 335, 325, 328, 323, 333, 337, 330, 329,
   329, 329, 331, 335, 325, 321, 329, 326, 328, 327, 335, 324, 330, 328, 332, 330, 325, 329, 339, 323, 328, 330, 331, 327, 329, 339, 331, 322, 333, 324, 324, 330, 331, 322,
   332, 326, 325, 337, 325, 328, 336, 332, 327, 322, 327, 328, 333, 336, 336, 328, 332, 330, 333, 332, 326, 325, 337, 329, 332, 328, 327, 333, 330, 326, 330, 323, 332, 326,
   336, 323, 328, 327, 335, 332, 327, 322, 337, 328, 332, 327, 329, 327, 328, 339, 326, 330, 331, 332, 335, 330, 332, 335, 322, 326, 323, 324, 337, 332, 325, 331, 331, 329,
   332, 321, 335, 330, 328, 331, 328, 332, 329, 327, 333, 331, 317, 335, 323, 331, 325, 324, 332, 333, 332, 327, 325, 324, 331, 323, 338, 329, 328, 328, 332, 338, 330, 331,
   336, 331, 331, 333, 340, 330, 334, 330, 327, 329, 327, 332, 327, 331, 318, 327, 325, 321, 320, 326, 319, 326, 333, 331, 330, 325, 333, 338, 329, 332, 338, 335, 336, 319,
   334, 333, 330, 326, 330, 331, 317, 331, 326, 336, 332, 337, 328, 337, 326, 329, 335, 329, 336, 326, 335, 333, 334, 336, 332, 326, 328, 325, 340, 325, 332, 336, 337, 326,
   333, 331, 331, 336, 333, 334, 333, 330, 336, 334, 322, 326, 329, 324, 331, 338, 324, 328, 328, 331, 320, 325, 329, 331, 324, 339, 336, 330, 330, 330, 327, 330, 329, 326,
   340, 335, 325, 331, 323, 338, 329, 331, 326, 332, 332, 329, 330, 333, 331, 339, 333, 325, 331, 337, 334, 324, 337, 325, 326, 321, 327, 330, 322, 325, 327, 333, 325, 327,
   328, 328, 318, 330, 326, 324, 328, 329, 334, 339, 327, 339, 332, 332, 324, 327, 332, 337, 324, 330, 334, 332, 333, 336, 335, 330, 337, 332, 333, 332, 328, 329, 325, 321,
   322, 329, 333, 324, 331, 335, 326, 331, 327, 330, 337, 332, 329, 325, 329, 324, 330, 325, 327, 323, 328, 330, 326, 333, 329, 327, 332, 333, 331, 333, 337, 331, 332, 322,
   332, 331, 327, 325, 338, 319, 327, 331, 327, 324, 335, 329, 332, 332, 336, 329, 323, 333, 327, 324, 330, 336, 326, 330, 334, 324, 331, 338, 327, 339, 330, 340, 327, 333,
   335, 334, 325, 336, 335, 328, 325, 327, 336, 334, 326, 331, 331, 325, 338, 319, 332, 328, 327, 336, 337, 326, 331, 330, 331, 331, 330, 335, 336, 332, 329, 333, 335, 331,
   331, 333, 331, 329, 330, 327, 331, 339, 332, 325, 325, 330, 331, 329, 336, 338, 332, 337, 325, 331, 337, 332, 329, 340, 335, 329, 334, 334, 328, 328, 330, 332, 333, 326,
   328, 324, 339, 332, 343, 327, 328, 328, 325, 334, 321, 324, 327, 330, 331, 331, 335, 332, 326, 327, 334, 324, 333, 331, 330, 322, 335, 331, 334, 332, 332, 336, 331, 324,
   334, 334, 336, 332, 330, 325, 330, 334, 331, 329, 333, 332, 332, 329]
    # print(sample_dist)
    print(st.norm.interval(.95, loc = np.mean(sample_dist), scale = np.std(sample_dist)))

    print(np.std(sample_dist))
    print(np.mean(sample_dist))
    


