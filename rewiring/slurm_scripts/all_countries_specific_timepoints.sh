#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=12
#SBATCH --mem=32G
#SBATCH --time=30:00:00
#SBATCH --job-name=all_countries_maled_specific
# %x=job-name %j=jobid
#SBATCH --mail-user=cklopfer@uvm.edu
#SBATCH --mail-type=ALL
#SBATCH --output=/users/c/k/cklopfer/grad_research/config_models/output/%x_%j_output.out

echo "Init Commands Passed"
cd /users/c/k/cklopfer/grad_research/config_models
echo "Directory Changed"
source activate copath
echo "Env activated"

# Tanzania 

python main.py -study maled -n 10000 -r 15000 -t "symptomatic_maled_tanzania_specific" -u True -c "All Symptomatic samples from MAL-ED running in parallel with no missing data, removing specific markers for c.jejuni/coli. Using the 60m updated dataset from MAL-ED. Only with samples from Tanzania. Timepoints are specifically set windows." -f simple_noncondensed_all_countries_01Jun21.csv -stool sym -partition_type specific -se True -opv False -multip True -country Tanzania -jobid %j -script_name %x
echo "MAL-ED Symptomatic Tanzania Completed"

python main.py -study maled -n 10000 -r 15000 -t "asymptomatic_maled_tanzania_specific" -u True -c "All Asymptomatic samples from MAL-ED running in parallel with no missing data, removing specific markers for c.jejuni/coli. Using the 60m updated dataset from MAL-ED. Only with samples from Tanzania. Timepoints are specifically set." -f simple_noncondensed_all_countries_01Jun21.csv -stool asym -partition_type specific -se True -opv False -multip True -country Tanzania -jobid %j -script_name %x
echo "MAL-ED Symptomatic Tanzania Completed"

# Peru 

python main.py -study maled -n 10000 -r 15000 -t "asymptomatic_maled_peru_specific" -u True -c "All Asymptomatic samples from MAL-ED running in parallel with no missing data, removing specific markers for c.jejuni/coli.  Using the 60m updated dataset from MAL-ED. Only samples from Peru. Timepoints are specifically set." -f simple_noncondensed_all_countries_01Jun21.csv -stool asym -partition_type specific -se True -opv False -multip True -country Peru -jobid %j -script_name %x
echo "MAL-ED Asymptomatic Peru Completed"

python main.py -study maled -n 10000 -r 15000 -t "symptomatic_maled_peru_specific" -u True -c "All Symptomatic samples from MAL-ED running in parallel with no missing data, removing specific markers for c.jejuni/coli.  Using the 60m updated dataset from MAL-ED. All samples from Peru. Timepoints are specifially set." -f simple_noncondensed_all_countries_01Jun21.csv -stool sym -partition_type specific -se True -opv False -multip True -country Peru -jobid %j -script_name %x
echo "MAL-ED Symptomatic Completed from Peru"

# South Africa 

python main.py -study maled -n 10000 -r 15000 -t "asymptomatic_maled_south_africa_specific" -u True -c "All asymptomatic samples from MAL-ED running in parallel with no missing data, removing specific markers for c.jejuni/coli.  Using the 60m updated dataset from MAL-ED. Using the samples from South Africa. Timepoints are specifically set." -f simple_noncondensed_all_countries_01Jun21.csv -stool asym -partition_type specific -se True -opv False -multip True -country "South Africa" -jobid %j -script_name %x
echo "MAL-ED Asymptomatic South Africa Completed"

python main.py -study maled -n 10000 -r 15000 -t "symptomatic_maled_south_africa_specific" -u True -c "All symptomatic samples from MAL-ED running in parallel with no missing data, removing specific markers for c.jejuni/coli.  Using the 60m updated dataset from MAL-ED. Using the samples from South Africa. Timepoints are specifically set." -f simple_noncondensed_all_countries_01Jun21.csv -stool sym -partition_type specific -se True -opv False -multip True -country "South Africa" -jobid %j -script_name %x
echo "MAL-ED Symptomatic South Afrcia Completed"

# Pakistan

python main.py -study maled -n 10000 -r 15000 -t "asymptomatic_maled_pakistan_specific" -u True -c "All asymptomatic samples from MAL-ED running in parallel with no missing data, removing specific markers for c.jejuni/coli.  Using the 60m updated dataset from MAL-ED. Using the samples from pakistan. Timepoints are specially set." -f simple_noncondensed_all_countries_01Jun21.csv -stool asym -partition_type specific -se True -opv False -multip True -country Pakistan -jobid %j -script_name %x
echo "MAL-ED Asymptomatic Pakistan Completed"

python main.py -study maled -n 10000 -r 15000 -t "symptomatic_maled_pakistan_specific" -u True -c "All symptomatic samples from MAL-ED running in parallel with no missing data, removing specific markers for c.jejuni/coli.  Using the 60m updated dataset from MAL-ED. Using the samples from Pakistan. Timepoints are specially set." -f simple_noncondensed_all_countries_01Jun21.csv -stool sym -partition_type specific -se True -opv False -multip True -country Pakistan -jobid %j -script_name %x
echo "MAL-ED Symptomatic Pakistan Completed"

# Nepal 

python main.py -study maled -n 10000 -r 15000 -t "asymptomatic_maled_nepal_specific" -u True -c "All asymptomatic samples from MAL-ED running in parallel with no missing data, removing specific markers for c.jejuni/coli.  Using the 60m updated dataset from MAL-ED. Using the samples from Nepal. Timepoints are specially set." -f simple_noncondensed_all_countries_01Jun21.csv -stool asym -partition_type specific -se True -opv False -multip True -country Nepal -jobid %j -script_name %x
echo "MAL-ED Asymptomatic Nepal Completed"

python main.py -study maled -n 10000 -r 15000 -t "symptomatic_maled_nepal_specific" -u True -c "All symptomatic samples from MAL-ED running in parallel with no missing data, removing specific markers for c.jejuni/coli.  Using the 60m updated dataset from MAL-ED. Using the samples from Nepal. Timepoints are specially set." -f simple_noncondensed_all_countries_01Jun21.csv -stool sym -partition_type specific -se True -opv False -multip True -country Nepal -jobid %j -script_name %x
echo "MAL-ED Symptomatic Nepal Completed"

# India 

python main.py -study maled -n 10000 -r 15000 -t "asymptomatic_maled_india_specific" -u True -c "All asymptomatic samples from MAL-ED running in parallel with no missing data, removing specific markers for c.jejuni/coli.  Using the 60m updated dataset from MAL-ED. Using the samples from India. Timepoints are specially set." -f simple_noncondensed_all_countries_01Jun21.csv -stool asym -partition_type specific -se True -opv False -multip True -country India -jobid %j -script_name %x
echo "MAL-ED Asymptomatic India Completed"

python main.py -study maled -n 10000 -r 15000 -t "symptomatic_maled_india_specific" -u True -c "All symptomatic samples from MAL-ED running in parallel with no missing data, removing specific markers for c.jejuni/coli.  Using the 60m updated dataset from MAL-ED. Using the samples from India. Timepoints are specially set." -f simple_noncondensed_all_countries_01Jun21.csv -stool sym -partition_type specific -se True -opv False -multip True -country India -jobid %j -script_name %x
echo "MAL-ED Symptomatic India Completed"

# Brazil

python main.py -study maled -n 10000 -r 15000 -t "asymptomatic_maled_brazil_specific" -u True -c "All asymptomatic samples from MAL-ED running in parallel with no missing data, removing specific markers for c.jejuni/coli.  Using the 60m updated dataset from MAL-ED. Using the samples from Brazil Timepoints are specially set." -f simple_noncondensed_all_countries_01Jun21.csv -stool asym -partition_type specific -se True -opv False -multip True -country Brazil -jobid %j -script_name %x
echo "MAL-ED Asymptomatic Brazil Completed"

python main.py -study maled -n 10000 -r 15000 -t "symptomatic_maled_brazil_specific" -u True -c "All symptomatic samples from MAL-ED running in parallel with no missing data, removing specific markers for c.jejuni/coli.  Using the 60m updated dataset from MAL-ED. Using the samples from Brazil. Timepoints are specially set." -f simple_noncondensed_all_countries_01Jun21.csv -stool sym -partition_type specific -se True -opv False -multip True -country Brazil -jobid %j -script_name %x
echo "MAL-ED Symptomatic Brazil Completed"

# Bagladesh MAL-ED

python main.py -study maled -n 10000 -r 15000 -t "Maled_Asym_sepcific_tp" -u True -c "Partitioning the network in to month bins by age. This run expeludes the c.jejuni/coli and only uses campy pan. Using the 60m update from MAL-ED. Using specific timepoints." -f simple_noncondensed_all_countries_01Jun21.csv -stool asym -partition_type specific -se True -opv False -multip True -bystool True -jobid %j -script_name %x

python main.py -study maled -n 10000 -r 15000 -t "Maled_Sym_sepcific_tp" -u True -c "Partitioning the network in to month bins by age. This run expeludes the c.jejuni/coli and only uses campy pan. Using the 60m update from MAL-ED. Using specific timepoints." -f simple_noncondensed_all_countries_01Jun21.csv -stool sym -partition_type specific -se True -opv False -multip True -bystool True -jobid %j -script_name %x

# Balgadesh PROVIDE

python main.py -study provide -n 10000 -r 15000 -t "Provide_Sym_sepcific_tp" -u True -c "Partitioning the network in to month bins by age. This run expeludes the c.jejuni/coli and only uses campy pan. Using the 60m update from MAL-ED. Using specific timepoints." -f simple_noncondensed_all_countries_01Jun21.csv -stool sym -partition_type specific -se True -opv False -multip True -bystool True -jobid %j -script_name %x

python main.py -study provide -n 10000 -r 15000 -t "Provide_Asym_sepcific_tp" -u True -c "Partitioning the network in to month bins by age. This run expeludes the c.jejuni/coli and only uses campy pan. Using the 60m update from MAL-ED. Using specific timepoints." -f simple_noncondensed_all_countries_01Jun21.csv -stool asym -partition_type specific -se True -opv False -multip True -bystool True -jobid %j -script_name %x