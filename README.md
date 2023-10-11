# copathogen-ensembles
Building Bipartite ensembles of randomly rewired networks, to look at patterns of pathogen coinfection while controlling for prevalence. This is used as the base code for the paper *Network-based analysis of pairwise co-occurence of enteric apthogens in Bangladeshi infants* foound in preprint: (paper under review). This is analysis repo, containing all the code for the analysis for all the figures and final analysis from the co-occurrence. 

## Objective

* Use random ensembles of bipartite graphs to indentify pathogen pairs that appear more or less than what we would expect given their individual prevalence. 

* Identify the impact of pathogen pairs by their burden of diarrhea, and the impact on the individuals by the number of days of diarrhea for both groups. 

## Datasets
Datasets were ongitudinal birth cohort studies. Datasets used: 

* [**PROVIDE**](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4385767/): The Performance of Rotavirus and Oral Polio Vaccines in Developeing Countries. Collaboration between the University of Virginia, University of Vermont, and the iccdr,b in Dhaka, Bangladesh. 

* [**MAL-ED**](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4385767/): The Etiology, Risk Factors, and Interactions of Enteric Infections and Malnutrition and the Consequences for Child Health (MAL-ED) Study. Longitudinal study looking at the effect of diarrhea, on undernutrition, and the combined effects of diarrhea and undernutrition on vaccine efficacy. 


### Analysis Scripts

Scripts are broken down as the following: 

* **descriptives.R** - Initial descriptives for the dataset. Contains the counts of participants and observatios in both datasets. 

* **kw_tests.R** - Kruskal-Wallis tests to test for a significant differences in the days of diarrhea betwen individual who experienced either or both pathogens. These are included in supplementary data. 

* **load_all_results.R** - Script for leading all results for further analysis. Uses named lists to specify the data being loaded. Data from the repositories listed above, or generated using the scripts descripted in the **Rewiring** section. Different imports:
    * `get_all_simple_results` : The default import, binarizes the qPCR data into a presence/absence dataset for all stool samples in the first 53 weeks of life. 
    * `get_all_country_results` : Gets the ensemble data from different sites used in the MAL-ED and PROVIDE datasets. Bangladesh is the only location with MAL-ED and PROVIDE data. 
    * `specific_partition_spec` : Partitions the MAL-ED/PROVIDE data according to study-specific time points. 
    * `month_byage_specs` : Partitions the MAL-ED/PROVIDE data into month partitions based on the age. 

* **ordered_distribution.R** : Scripts for generating figures depicting the distributions of co-occurences of pathogens in bipartite ensembles referenced in the manuscript. 

* **pie_charts_ver2.R** : Scripts for generating par charts depicting copathogen risk in diarrheal stools referenced in the manuscript. 

#### Running Analysis Scripts 

To run analysis scripts, run the script as a source file, changing the active directory to the main project root directory. The Root directory is the project directory. 

## Source Code for Ensemble Construction

The code for generating the ensembles of bipartite networks is found the the directory `rewiring`. The scripts are designed to be run from a slurm scripts, run on a linux server. For examples of how to run the analysis look at the scripts contained in the directory `rewiring/slurm_scripts`. Get a list of all the available arguments, type the command 
```
main.py -h
```

## Dependencies

In addition to the data repositories stated above, using this code requires additional libraries listed below: 

### R
* dplyr, ggplot2, gridExtra, ggtext
* [copathogenTools](https://github.com/connor-klopfer/copathogenTools) : A repository of tools written in R, for working with the MAL-ED and PROVIDE datasets mentioned above, includes data cleaning and joing scripts for merging the two datasets. Does not contain any data. 

### Python 
* Pandas, Numpy, datetime, cProfile, uuid, collections, math, numba, csv, time, scipy, argparse, math, multiprocessing, tqdm.


