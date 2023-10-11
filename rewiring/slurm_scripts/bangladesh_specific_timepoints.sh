#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=12
#SBATCH --mem=32G
#SBATCH --time=30:00:00
#SBATCH --job-name=bangladesh_bothstudies_specific
# %x=job-name %j=jobid
#SBATCH --mail-user=cklopfer@uvm.edu
#SBATCH --mail-type=ALL
#SBATCH --output=/users/c/k/cklopfer/grad_research/config_models/output/%x_%j_output.out

echo "Init Commands Passed"
cd /users/c/k/cklopfer/grad_research/config_models
echo "Directory Changed"
source activate copath
echo "Env activated"

# Bagladesh MAL-ED
echo "Starting..."

python main.py -study maled -n 10000 -r 15000 -t "Maled_Asym_sepcific_tp" -u True -c "Partitioning the network in to month bins by age. This run expeludes the c.jejuni/coli and only uses campy pan. Using the 60m update from MAL-ED. Using specific timepoints." -f simple_noncondensed_all_countries_01Jun21.csv -stool asym -partition_type specific -se True -opv False -multip True -bystool True -jobid %j -script_name %x -country Bangladesh

python main.py -study maled -n 10000 -r 15000 -t "Maled_Sym_sepcific_tp" -u True -c "Partitioning the network in to month bins by age. This run expeludes the c.jejuni/coli and only uses campy pan. Using the 60m update from MAL-ED. Using specific timepoints." -f simple_noncondensed_all_countries_01Jun21.csv -stool sym -partition_type specific -se True -opv False -multip True -bystool True -jobid %j -script_name %x -country Bangladesh

# Balgadesh PROVIDE

python main.py -study provide -n 10000 -r 15000 -t "Provide_Sym_sepcific_tp" -u True -c "Partitioning the network in to month bins by age. This run expeludes the c.jejuni/coli and only uses campy pan. Using the 60m update from MAL-ED. Using specific timepoints." -f simple_noncondensed_all_countries_01Jun21.csv -stool sym -partition_type specific -se True -opv False -multip True -bystool True -jobid %j -script_name %x -country Bangladesh

python main.py -study provide -n 10000 -r 15000 -t "Provide_Asym_sepcific_tp" -u True -c "Partitioning the network in to month bins by age. This run expeludes the c.jejuni/coli and only uses campy pan. Using the 60m update from MAL-ED. Using specific timepoints." -f simple_noncondensed_all_countries_01Jun21.csv -stool asym -partition_type specific -se True -opv False -multip True -bystool True -jobid %j -script_name %x -country Bangladesh