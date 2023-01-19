# copathogen-ensembles
Building Bipartite ensembles of randomly rewired networks, to look at patterns of pathogen coinfection while controlling for prevalence. This is used as the base code for the paper *Network-based analysis of pairwise co-occurence of enteric apthogens in Bangladeshi infants* foound in preprint: (paper link here). This is analysis repo, containing all the code for the analysis for all the figures and final analysis from the co-occurrence. 

## Objective

* Use random ensembles of bipartite graphs to indentify pathogen pairs that appear more or less than what we would expect given their individual prevalence. 

* Identify the impact of pathogen pairs by their burden of diarrhea, and the impact on the individuals by the number of days of diarrhea for both groups. 

## Datasets
Datasets were ongitudinal birth cohort studies. Datasets used: 

* **PROVIDE**: The Performance of Rotavirus and Oral Polio Vaccines in Developeing Countries. Collaboration between the University of Virginia, University of Vermont, and the iccdr,b in Dhaka, Bangladesh. The study through the American Society for Tropical Medicine is available (here). 

* **MAL-ED**: The Etiology, Risk Factors, and Interactions of Enteric Infections and Malnutrition and the Consequences for Child Health (MAL-ED) Study. Longitudinal study looking at the effect of diarrhea, on undernutrition, and the combined effects of diarrhea and undernutrition on vaccine efficacy. 


### Analysis Scripts

Scripts are broken down as the following: 

* **descriptives.R** - Initial descriptives for the dataset. Contains the counts of participants and observatios in both datasets. 

* **kw_tests.R** - Kruskal-Wallis tests to test for a significant differences in the days of diarrhea betwen individual who experienced either or both pathogens. 

#### Running Analysis Scripts 

To run analysis scripts, run the script as a source file, changing the active directory to the main project root directory. The Root directory is the project directory. 