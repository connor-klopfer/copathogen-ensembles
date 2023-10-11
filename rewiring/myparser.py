"""Script for taking all the arguments and parsing them to file. 


Arguments: 
    -study: The type of study either provide or maled
    -stool_type: The type of stool type to restrict the analysis to 

    - r the number of rewires

    - n the number of iterations. 

"""
import argparse
import uuid

parser = argparse.ArgumentParser() 

parser.add_argument('-study', default="none", type = str,\
    help = "The study to filter the dataset by, the options are maled or provide. \
        If neither are given or misspelled, then the default is set to None and no \
            filtering is done.")

parser.add_argument('-stool', default = "none", type = str, \
    help = 'The stool type to filter the dataset by, takes sym or asym, if neither \
        of those are given, then will set either to None and no filtering will be done.')

parser.add_argument('-n', default = 1000, type = int, \
    help = 'The number of configuration model replicates for the experiment. Enter as an \
        interger, defaults to 1000.')

parser.add_argument('-r', default = 1000, type = int, \
    help = 'Number of rewires in the configuration model. The number of edge swithces within \
        one replicate of the model.')

parser.add_argument('-t', default = 'No Title given', type = str, \
    help = 'Title for the experiment, will use this title to write the file name for the \
        saved csv and parameter file.')

parser.add_argument('-c', default = '', type = str, \
    help = 'Additional comments in the experiment text file.')

# NOTE: Updated 01Mar21
parser.add_argument('-f', default = 'simple_noncondensed_60m_10Sep20.csv', type = str, \
    help = 'The name of the source file to use, must be a csv file with the file extension and \
        located in the "data" folder')

# parser.add_argument('-type', default = 'simple', type = str, \
#     help = 'The data type of the source dataframe, will comvert the datatype to the appropiate version, \
#         then apply the rewiring model to each version. The avaialble options are "simple", "simple_list", \
#             and "and qpcr_list". Defaults to simple list.')

parser.add_argument('-b', default = 4, type = int, \
    help = 'The default number of bins to bin the timeseries into. For example, the value of 4 will take \
        the timeseries and break it into 4 chunks, and combine elements witin each chunk')

parser.add_argument('-u', default = False, type = bool, \
    help = 'Whether or not to egenerate a unique ID for the experiment file. The Unique ID is generated \
        and recorded to the records file and saved as the filename in the CI dataframe.')

parser.add_argument('-v', default = False, type = bool, \
    help = 'Whether to print status updates for the rewiring process, useful for large models using a lot of walltime')

parser.add_argument('-se', default = False, type = bool, \
    help = 'Whether to save the entire ensemble of configuration models to file.')

# Parser for includeing opv
parser.add_argument('-opv', default = "False", type = str, \
    help = "Include the OPV readings in the configuration model.")

parser.add_argument('-multip', default = "False", type = str, \
    help = "Boolean whether or not to use multiprocessing for rewiring the replicates across \
        different CPUS. Uses Pythons multiprocessing module. Uses the maximum number of CPUs \
            available.")

parser.add_argument('-bystool', default = "True", type = str, \
    help = "Boolean whether to perform the configuration  model by stool or by participant. \
        Defaults to True, which is by stool.")

parser.add_argument('-jobid', default = "Local or None Given", type = str, \
    help = "Job ID  as a string assigned by VACC PB Torque script. Defaults to 'Local or None \
        Given' if not provided.")
    
parser.add_argument('-script_name', default = "Local or None Given", type = str, \
    help = "Filename for the script, prints to the text output file.")

# Following Commented out 01Mar21
# parser.add_argument('-seasons', type = bool, default = False, help = "Include this boolean, to seperate \
#     observations by th season rather than parsing by age.")

# parser.add_argument('-bymonth', type = str, default = "False", help = "Segment the observations by calender month.")

parser.add_argument('-country', type = str, default = "none", \
    help = "Segment the data by country. Default is none, options are: Brazil, Tanzania,  \
        South Africa, Peru, Pakistan, Nepal, India, Brazil, Bangladesh.")

# parser.add_argument('-specific_partition', type = str, default = "No Partition Given", \
#     help = "How does the season get partitioned.")

parser.add_argument('-partition_type', type = str, default = "simple", \
    help = "The partitioning of samples across a timeseries. The default is 'simple', which does \
        no partition the samples, and pools them within their respective subset. Other options are: \
            monthbyage: Partition samples into months based on the age of of the partitipant. 372 days \
                is partitioned into 12 equal sized bins. \
            monthbycalender: Break into the based on the calender month they were collected.  \
            specific: Partition stool samples based on specific partitions see ___ for specific timepoints. \
            seasons: Partition stool samples by the season where the stool samples were collected.")
        

args = vars(parser.parse_args())

args['uuid'] = uuid.uuid4() if args['u'] else 'None_Given'
args['opv'] = True if args['opv'] == "True" else False
args['multip'] = True if args['multip'] == 'True' else False
args['bystool'] = True if args['bystool'] == 'True' else False
# args['bymonth'] = True  if args['bymonth'] == "True" else False
args['country'] = None if args['country'] == "none" else args['country']
# args['specific_partition'] = True if args['specific_partition'] == "True" else False

assert args['partition_type'] in ["simple", 'monthbyage', 'monthbycalender', 'specific', 'seasons'], \
    "ERROR: Invalid partition type specified."

if args['partition_type'] == "monthbycalender" and not args['bystool']:
    print("WARNING: You requested segmentation by month, but not breaking apart\
         stools. Will default to intergrating by subject and segementing based on\
              age, NOT Calender year.")

def clean_filters(study, stool):
    """Set the arguments for study and stool filters. Allows for easier argument entry. 
    This step performs the necessary cleaning to use the filter appropiately. 
    
    Arguments:
        study {string} -- The argument for the type of study. Accepts "provide" and "maled"\
            any other argument will be set to None, and no filtering will be done.
        stool {string} -- The argument for the stool type to apply filtering to, allows for \
            "sym" and "asym" which will convert to "Diarrhea" and "Asymptomatic respectively. 
    
    Returns:
        [string, string] -- The study filter followed by the stool filter in that order. 
    """
    if study != "provide" and study != 'maled':
        study = None
    if stool == 'sym':
        stool = "Diarrhea"
    elif stool == 'asym':
        stool = "Asymptomatic"
    else:
        stool = None
    return study, stool

#TODO: This is not good, fix (commented 01Mar21)
def check_files(args):
    # if args['type'] != 'simple' and "list" not in args['f'] and not args['bystool']:
    if args['partition_type'] in ['monthbycalender', 'monthbyage']:
        assert "list" in args['f'] and not args['bystool'],\
        "ERROR: Timeseries analysis with non-ts data, please fix."

if __name__ == '__main__':
    args = vars(parser.parse_args())
    
    print('N Replicates:', args['n'])
    print('N rewires:', args['r'])
    print('Stool type:', args['stool'])
    print('Study:', args['study'])
    print('Stool:', args['stool'])
    study, stool = clean_filters(args['study'], args['stool'])
    print('Study after:', study)
    print('Stool after:', stool)
    print("Specific Partition:", args['partition_type'])
