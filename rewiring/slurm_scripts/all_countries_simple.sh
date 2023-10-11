#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=12
#SBATCH --mem=32G
#SBATCH --time=30:00:00
#SBATCH --job-name=all_countries_maled
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

python main.py -study maled -n 10000 -r 15000 -t "symptomatic_maled_tanzania" -u True -c "All Symptomatic samples from MAL-ED running in parallel with no missing data, removing specific markers for c.jejuni/coli. Using the 60m updated dataset from MAL-ED. Only with samples from Tanzania." -f simple_noncondensed_all_countries_27Jan21.csv -stool sym -type simple -se True -opv False -multip True -country Tanzania
echo "MAL-ED Symptomatic Tanzania Completed"

# Peru 

python main.py -study maled -n 10000 -r 15000 -t "asymptomatic_maled_peru" -u True -c "All Asymptomatic samples from MAL-ED running in parallel with no missing data, removing specific markers for c.jejuni/coli.  Using the 60m updated dataset from MAL-ED. Only samples from Peru" -f simple_noncondensed_all_countries_27Jan21.csv -stool asym -type simple -se True -opv False -multip True -country Peru
echo "MAL-ED Asymptomatic Peru Completed"

python main.py -study maled -n 10000 -r 15000 -t "symptomatic_maled_peru" -u True -c "All Symptomatic samples from MAL-ED running in parallel with no missing data, removing specific markers for c.jejuni/coli.  Using the 60m updated dataset from MAL-ED. All samples from Peru" -f simple_noncondensed_all_countries_27Jan21.csv -stool sym -type simple -se True -opv False -multip True -country Peru
echo "MAL-ED Symptomatic Completed from Peru"

# South Africa 

python main.py -study maled -n 10000 -r 15000 -t "asymptomatic_maled_south_africa" -u True -c "All asymptomatic samples from MAL-ED running in parallel with no missing data, removing specific markers for c.jejuni/coli.  Using the 60m updated dataset from MAL-ED. Using the samples from South Africa" -f simple_noncondensed_all_countries_27Jan21.csv -stool asym -type simple -se True -opv False -multip True -country "South Africa"
echo "MAL-ED Asymptomatic South Africa Completed"

python main.py -study maled -n 10000 -r 15000 -t "symptomatic_maled_south_africa" -u True -c "All symptomatic samples from MAL-ED running in parallel with no missing data, removing specific markers for c.jejuni/coli.  Using the 60m updated dataset from MAL-ED. Using the samples from South Africa" -f simple_noncondensed_all_countries_27Jan21.csv -stool sym -type simple -se True -opv False -multip True -country "South Africa"
echo "MAL-ED Symptomatic South Afrcia Completed"

# Pakistan

python main.py -study maled -n 10000 -r 15000 -t "asymptomatic_maled_pakistan" -u True -c "All asymptomatic samples from MAL-ED running in parallel with no missing data, removing specific markers for c.jejuni/coli.  Using the 60m updated dataset from MAL-ED. Using the samples from pakistan" -f simple_noncondensed_all_countries_27Jan21.csv -stool asym -type simple -se True -opv False -multip True -country Pakistan
echo "MAL-ED Asymptomatic Pakistan Completed"

python main.py -study maled -n 10000 -r 15000 -t "symptomatic_maled_pakistan" -u True -c "All symptomatic samples from MAL-ED running in parallel with no missing data, removing specific markers for c.jejuni/coli.  Using the 60m updated dataset from MAL-ED. Using the samples from Pakistan" -f simple_noncondensed_all_countries_27Jan21.csv -stool sym -type simple -se True -opv False -multip True -country Pakistan
echo "MAL-ED Symptomatic Pakistan Completed"

# Nepal 

python main.py -study maled -n 10000 -r 15000 -t "asymptomatic_maled_nepal" -u True -c "All asymptomatic samples from MAL-ED running in parallel with no missing data, removing specific markers for c.jejuni/coli.  Using the 60m updated dataset from MAL-ED. Using the samples from Nepal." -f simple_noncondensed_all_countries_27Jan21.csv -stool asym -type simple -se True -opv False -multip True -country Nepal
echo "MAL-ED Asymptomatic Nepal Completed"

python main.py -study maled -n 10000 -r 15000 -t "symptomatic_maled_nepal" -u True -c "All symptomatic samples from MAL-ED running in parallel with no missing data, removing specific markers for c.jejuni/coli.  Using the 60m updated dataset from MAL-ED. Using the samples from Nepal." -f simple_noncondensed_all_countries_27Jan21.csv -stool sym -type simple -se True -opv False -multip True -country Nepal
echo "MAL-ED Symptomatic Nepal Completed"

# India 

python main.py -study maled -n 10000 -r 15000 -t "asymptomatic_maled_india" -u True -c "All asymptomatic samples from MAL-ED running in parallel with no missing data, removing specific markers for c.jejuni/coli.  Using the 60m updated dataset from MAL-ED. Using the samples from India." -f simple_noncondensed_all_countries_27Jan21.csv -stool asym -type simple -se True -opv False -multip True -country India
echo "MAL-ED Asymptomatic India Completed"

python main.py -study maled -n 10000 -r 15000 -t "symptomatic_maled_india" -u True -c "All symptomatic samples from MAL-ED running in parallel with no missing data, removing specific markers for c.jejuni/coli.  Using the 60m updated dataset from MAL-ED. Using the samples from India." -f simple_noncondensed_all_countries_27Jan21.csv -stool sym -type simple -se True -opv False -multip True -country India
echo "MAL-ED Symptomatic India Completed"

# Brazil

python main.py -study maled -n 10000 -r 15000 -t "asymptomatic_maled_brazil" -u True -c "All asymptomatic samples from MAL-ED running in parallel with no missing data, removing specific markers for c.jejuni/coli.  Using the 60m updated dataset from MAL-ED. Using the samples from Brazil" -f simple_noncondensed_all_countries_27Jan21.csv -stool asym -type simple -se True -opv False -multip True -country Brazil
echo "MAL-ED Asymptomatic Brazil Completed"

python main.py -study maled -n 10000 -r 15000 -t "symptomatic_maled_brazil" -u True -c "All symptomatic samples from MAL-ED running in parallel with no missing data, removing specific markers for c.jejuni/coli.  Using the 60m updated dataset from MAL-ED. Using the samples from Brazil" -f simple_noncondensed_all_countries_27Jan21.csv -stool sym -type simple -se True -opv False -multip True -country Brazil
echo "MAL-ED Symptomatic Brazil Completed"
