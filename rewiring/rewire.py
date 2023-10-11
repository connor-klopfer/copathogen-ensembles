"""Rewire the numpy matrix, choosing uniformly from the columns and rows in the matrix and the 
"""
from collections import Counter
import numpy as np
from numba import jit
import datetime
import multiprocessing as mp
import tqdm

from consts import MISSING_VALUE
from rewire_tools import build_ci_df, save_replicates



# NOTE: The program is still crashing, and I want to minimize my resource usage. 
# I think I would me more efficient if I write the ensemble fater each time replicate. This 
# might need to be a new branch? I'm not sure. Perhaps an option to compare. 

def build_ensemble(m, path_names, args):
    # NOTE: Do I break these apart into different functions 
    #  A timeseries, breaking apart a by-subject timeseries 
    if isinstance(m, list) or len(m.shape) > 2:
        perform_timeseries_configuration(m, path_names, args)
    else:
        perform_simple_configuration(m, path_names, args)


def perform_simple_configuration(m, params, args):
    """Simple configuration with no timeseries component. Configuration on
    one matrix. Returns a 3D matrix with all replicates. 

    Arguments:\n
        m {2D Numpy Array} -- Matrix representing the study with rows equal 
            to the number of participants if stool specific, and columns 
            equal to the number of pathogens. \n
        args {dict} -- A dictionary containing options from the command line 
            aarguments, including the number or rewires, the number of 
            replicates, and whether to incoportate multiprocessing. For a full
            list of options see mypaser.py

    Returns:\n
        3D Numpy Array -- Array conatining all the replicates, with each slice 
            along the axis 0 representing a replicate, with the number of rows 
            and columns wqual to the number of pathogens. Replicates of the one 
            mode projection. 
    """
    replicates = np.empty((args['n'], m.shape[1], m.shape[1]))
    if not args['multip']:
        for n in range(args['n']):
            # temp = rewire_matrix_complete_random(m, n_rewires, verbose)
            temp = rewire_matrix_directed(m, args['r'])
            # Edit for exapnding replicates 08Jun2020
            # temp[np.isnan(temp)] = 0
            temp[temp == MISSING_VALUE] = 0
            replicates[n, :, :] = np.dot(temp.T, temp)
    else:
        # Attempting to parallelise
        # NOTE: There is little to no imporvement in performance. This could be because the program has to 
        # reinitialize for every worker. What if I split ahaed of time.? 
        # NOTE: It could be that apply is only evaluating one member at a time, perhaps .map would be faster? 
        #   I should try this before any subsetting. 
        pool = mp.Pool(mp.cpu_count())
        # replicate_slices = [pool.apply(rewire_directed_parallel, args = (m, args['r'])) for n in range(args['n'])]
        replicate_slices = list(pool.starmap(rewire_directed_parallel, [(m, args['r']) for n in range(args['n'])]))
        replicates = np.stack(replicate_slices)
        pool.close()
    
    build_ci_df(replicates, m, params, args)

    if args['se']:
        save_replicates(replicates, params, args)
    

def configuration_in_parallel():
    # Split the list into multiple threads, run a smaller configuration on each thread, then conatenate them together. 
    pass




def perform_timeseries_configuration(m, params, args):
    """Perform the configuration model on multiple networks, for each timepoint, 
    either participant-centri or observation-centric. 

    Edit 04Jun20: To allow for flexibility wihout a large re-write, the function 
    hanldes bith observation centric and stool centric observations. 

    Arguments:
        m {Numpy Array or list} -- A 3D Numpy array if participant centric or a 
            list of arrays if observation sentic. For each matrix in the 0 axis 
            or element in the list, performs a configuration model. 
        args {dict} -- A dictionary from the argument parser, with command-line
            arguments for the number of replicates and the number of rewires, as well 
            as an indicator wheter the rewiring is done verbose (printing progress) or 
            not. 

    Returns:
        list -- a list of matrices, of length for the number of replicates, with each 
            element or slice along axis 0 representing a network fter a number of rewires. 
    """
    # replicate_list = []
    by_participant = not isinstance(m, list)
    n_timepoints = len(m) if not by_participant else m.shape[0]
    
    # For each timepoint
    for b in range(n_timepoints):
        # all_attempts = []
        # For each timepoint, make a 3d matrix of replicates 
        # If by paricipant, then the timeseries, is a 3 dimensional matrix, 
        # and all the slices have the same number of rows and columns, otherwise
        # the sizes are irregular and needs to be reshaped at each point. 
        if by_participant:
            replicates = np.empty((args['n'], m.shape[2], m.shape[2]))
        else:
            replicates = np.empty((args['n'], m[b].shape[1], m[b].shape[1]))
        if args['v']:
            print('Timestep {} start. Total Connections: {} Time: {}'.format(b + 1, \
                np.nansum(m[b, :, :]), datetime.datetime.today().strftime("%H:%M:%S")))
        # #### REVISIT HERE: THE MISSING VALUES ARE SET TO ZERO ###
        # mzeroed = m[b, :, :].copy()
        # mzeroed[np.isnan(mzeroed)] = 0
        # # Validate edge list here. 
        # if isinstance(m, list) and non_valid_edges(m[b]):
            # replicates = np.zeros(replucate)
        if (not isinstance(m, list) and non_valid_edges(m[b, :, :])) or \
            (isinstance(m, list) and non_valid_edges(m[b])):
            # No valid edges for rewiring. 
            # np.empty((args['n'], m.shape[2], m.shape[2]))
            # replicates = np.zeros((args['n'], m.shape[2], m.shape[2]))
            replicates = np.zeros(replicates.shape)
        # Everything is good, proceed
        else:
            if not args['multip']:
                for n in range(args['n']):
                    # temp, n_attempts = rewire_matrix_complete_random(m[b, :, :], n_rewires, False)
                    if isinstance(m, list):
                        temp = rewire_matrix_directed(m[b], args['r'])
                    else:
                        temp = rewire_matrix_directed(m[b, :, :], args['r'])
                    # temp, n_attempts = rewire_matrix_directed(mzeroed, n_rewires)
                    # all_attempts.append(n_attempts)

    
                    # Edit done 08Jun20: Changing the invalid values to 9999, if there is a 
                    # possibility this could become a valie value, this needs to change. 
                    # temp[np.isnan(temp)] = 0
                    temp[temp == MISSING_VALUE] = 0
                    replicates[n, :, :] = np.dot(
                        temp.T, temp)
            else:
                # Attempting to parallelise
                pool = mp.Pool(mp.cpu_count())
                # replicate_slices = [pool.apply(rewire_directed_parallel, args = (m, args['r'])) for n in range(args['n'])]
                if isinstance(m, list):
                    replicate_slices = list(pool.starmap(rewire_directed_parallel, tqdm.tqdm([(m[b], args['r']) for n in range(args['n'])], total=args['n'])))
                else:
                    replicate_slices = list(pool.starmap(rewire_directed_parallel, tqdm.tqdm([(m[b, :, :], args['r']) for n in range(args['n'])], total=args['n'])))
                replicates = np.stack(replicate_slices)
                pool.close()
        # list of replicates for each time bin 
        # NOTE: This is where we'll archive the ensemble and the results. 

        build_ci_df(replicates, m[b] if isinstance(m, list) else m[b, :, :], params, args, timepoint = b)

        if args['se']:
            save_replicates(replicates, params, args, timepoint = b)

        print("Rewiring complete for time bin {} and results archived.".format(b))
        del replicates

        # replicate_list.append(replicates)
    # return replicate_list


def non_valid_edges(m):
    m_temp = m.copy()
    # Edit 08Jun20
    # m_temp[np.isnan(m)] = 0
    m_temp[m_temp == MISSING_VALUE] = 0
    edge_list = np.array(np.where(m > 0)).T
    if np.sum(m) == 0:
        print('ERROR: No Edges')
        return True
    if np.all(edge_list[:, 0] == edge_list[0, 0]) or np.all(edge_list[:, 1] == edge_list[0, 1]):
        print('ERROR: This time step does not have valid rewirings')
        return True
    return False
            

@jit(nopython = True)
def get_degrees(m):
    row_k = np.zeros(m.shape[0])
    col_k = np.zeros(m.shape[1])
    # NOTE: It seems that the np.isnan() function is not supported by numba
    # m_copy[np.isnan(m)] = 0
    # NOTE: The axis arguments for NUMba is not up and running as 
    # of 09Mar2020, so I have to use some for-loops to get around this. 
    for row in range(m.shape[0]):
        temp_row = 0
        for col in range(m.shape[1]):
            # Edit 08Jun20
            # if not np.isnan(m[row, col]):
            if m[row, col] != MISSING_VALUE:
                temp_row += m[row, col]
                col_k[col] += m[row, col]
        row_k[row] = temp_row

    # NOTE: It seems that the axis argument is not currently supported by numba. 
    # row_k = np.nansum(m, axis = 0)
    # col_k = np.nansum(m, axis = 1)
    col_k.sort()
    row_k.sort()
    return row_k, col_k



def get_degrees_nonumba(m):
    """No Numba version of the get matching degrees functions. 
    
    Arguments:
        m {[type]} -- [description]
    
    Returns:
        [type] -- [description]
    """
    row_k = np.zeros(m.shape[0])
    col_k = np.zeros(m.shape[1])
    # m_copy = m.copy()
    # NOTE: It seems that the np.isnan() function is not supported by numba
    # m_copy[np.isnan(m)] = 0
    # NOTE: The axis arguments for NUMba is not up and running as 
    # of 09Mar2020, so I have to use some for-loops to get around this. 
    for row in range(m.shape[0]):
        temp_row = 0
        for col in range(m.shape[1]):
            # Edit: 08Jun20 
            # if not np.isnan(m[row, col]):
            if m[row, col] != MISSING_VALUE:
                temp_row += m[row, col]
                col_k[col] += m[row, col]
        row_k[row] = temp_row
            

    #     col_k[col] = np.sum(m_copy[:, col])
    # for row in range(m.shape[0]):
    #     row_k[row] = np.sum(m_copy[row, :])

    # NOTE: It seems that the axis argument is not currently supported by numba. 
    # row_k = np.nansum(m, axis = 0)
    # col_k = np.nansum(m, axis = 1)
    col_k.sort()
    row_k.sort()
    return row_k, col_k


def rewire_directed_parallel(m, n_rewires):
        # temp = rewire_matrix_complete_random(m, n_rewires, verbose)
        # Normally returns the number of attempts, but that's not needed at this time. 
        temp = rewire_matrix_directed(m, n_rewires)
        # Edited for 08Jun20 changing the matrix type. 
        # temp[np.isnan(temp)] = 0
        temp[temp == MISSING_VALUE] = 0
        final = np.dot(temp.T, temp)
        return final
    
# TODO: Implement this for numba support
@jit(nopython = True)
def rewire_matrix_directed(m_original, n_rewires):
    """This is the 'directed' version of rewiring, and by directed I mean it 
    finds all of the endges and randomises those, rather than pick a spot at
     random, determine if that's an edge, then find another one.  

    Arguments:
        m_original {Numpy Array} -- The original matrix of presence absence data
        n_rewires {int} -- the number of rewires to perform. 

    Returns:
        [type] -- [description]
    """
    m = m_original.copy()

    original_row_k, original_col_k = get_degrees(m_original)
    n_attempts = 0
    for r in range(n_rewires):
        # Create an edge list, returns a tuple of np.arrays, the first 
        # element is the roww coordinates and the second is the column 
        # coordinates that have a value > 0. 
        where_valid = np.where((m > 0) & (m != MISSING_VALUE))
        has_edge = np.empty((len(where_valid[0]), 2), dtype= np.int32)
        # For every coordinate with an edge
        for idx in range(len(where_valid[0])):
            # Make an edgelist
            has_edge[idx, 0] = int(where_valid[0][idx])
            has_edge[idx, 1] = int(where_valid[1][idx])
        
        if has_edge.shape[0] < 1:
            print('ERROR: No valid edges to rewire')

        rewire_successful = False

        # print(np.nansum(m))
        local_attempts = 0
        while not rewire_successful:
            n_attempts += 1
            local_attempts += 1
            if local_attempts > 2000000 and local_attempts < 2000002:
                print('Greater than 2mil attempts, consider refactoring')
                temp = np.empty(m.shape).astype(np.uint16)
                # temp[:] = np.nan
                # Edit implemented 08Jun20
                temp[:] = MISSING_VALUE
                return temp


            # Choose two edges. Choose two rows of the edge list randomly 
            choice1 = np.random.randint(has_edge.shape[0])
            choice2 = np.random.randint(has_edge.shape[0])

            # If you choose the same edge, choose again
            while choice1 == choice2:
                choice1 = np.random.randint(has_edge.shape[0])
                choice2 = np.random.randint(has_edge.shape[0])

            # Rewiring works by swapping the rows. If the row of the second 
            # choice with the column of the first choice is already and edge,
            #  then you can't add another 
            if m[has_edge[choice1, 0], has_edge[choice2, 1]] != 0 or m[has_edge[choice2, 0], has_edge[choice1, 1]] != 0:
                continue

            # Remove the old edge 
            m[has_edge[choice1, 0], has_edge[choice1, 1]] -= 1
            m[has_edge[choice2, 0], has_edge[choice2, 1]] -= 1

            # Add in the new edge 
            m[has_edge[choice1, 0], has_edge[choice2, 1]] += 1
            m[has_edge[choice2, 0], has_edge[choice1, 1]] += 1

            rewire_successful = True

    new_row_k, new_col_k = get_degrees(m)

    if not np.array_equal(new_row_k, original_row_k) or not np.array_equal(new_col_k, original_col_k):
        print('ERROR: Non-mathcing degree distributions')

    return m
    

# NOTE: I think this is proving to be a time bottleneck for very sparse matrices, I think that a more directed 
@jit(nopython = True)
def rewire_matrix_complete_random(m_original, n_rewires, verbose, update_steps = 10):
    m = m_original.copy()
    n_successes = 0
    n_attempts = 0
    update_steps = n_rewires // update_steps
    original_row_k, original_col_k = get_degrees(m_original)
    for r in range(n_rewires):
        rewire_successful = False
        local_attempts = 0
        while not rewire_successful:
            n_attempts += 1
            local_attempts += 1
            # if verbose and local_attempts == 1000001:
            #     print('WARNING: > 1mil local attempts, possible inifinte loop')
            # For the large number of bins and the larger study like maled, there might 
            # be an infinite loop where there are no valid rewiring partners. 
            if local_attempts > 1000000 and verbose:
                print('ERROR: > 1mil local attempts')
                # print('N Attempts:', local_attempts)
                # break
            rows_1 = np.random.randint(m_original.shape[0])
            rows_2 = np.random.randint(m_original.shape[0])
            cols_1 = np.random.randint(m_original.shape[1])
            cols_2 = np.random.randint(m_original.shape[1])
            
            # The row or column selection is the same
            if rows_1 == rows_2 or cols_1 == cols_2:
                continue

            # The source coordinates do not have an edge
            if m[rows_1, cols_1] != 1 or m[rows_2, cols_2] != 1:
                continue
            
            # The target already has an edge, skip
            if m[rows_2, cols_1] != 0 or m[rows_1, cols_2] != 0:
                continue

            m[rows_1, cols_1] -= 1
            m[rows_2, cols_2] -= 1

            m[rows_2, cols_1] += 1
            m[rows_1, cols_2] += 1

            rewire_successful = True
            n_successes += 1
        
        # if verbose:
        #     if r % update_steps == 0:
        #         # percent_var = format((r/n_rewires) * 100, '.1f')
        #         print('Rewiring % Complete:', (r/n_rewires) * 100)

    new_row_k, new_col_k = get_degrees(m)

    if not np.array_equal(new_row_k, original_row_k) or not np.array_equal(new_col_k, original_col_k):
        print('ERROR: Non-mathcing degree distributions')

    return m, n_attempts


if __name__ == "__main__":
    pass
    # sample_1 = np.random.randint(0, 2, size = (5, 5))

    # # TODO: Write the testing functions to get the row and column degrees for the 
    # # matrix in a form that can use the numba functions 

    # # TODO: Write both the numba version and non-numba version (optimized for efficiency) to
    # # compare speed later. Does using the numba toolbax actually improve performance? 
    # sample_1 = np.random.randint(0, 2, size = (10, 10))
    
    # print(sample_1)
    # np_row_k = np.nansum(sample_1, axis = 1)
    # np_col_k = np.nansum(sample_1, axis = 0)
    # np_row_k.sort()
    # np_col_k.sort()
    # print('Row Sum:', np_row_k)
    # print('Col Sum:', np_col_k)

    # r_df, attempts = rewire_matrix_directed(sample_1, 10)

    # row_k, col_k = get_degrees(r_df)

    # print('Row(custom):', row_k)
    # print('Col(custom):', col_k)

    
